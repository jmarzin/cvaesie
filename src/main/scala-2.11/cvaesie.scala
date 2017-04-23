import java.io.File

import org.pdfclown.documents.contents.RotationEnum
import org.pdfclown.documents.{Page, Pages}
import org.pdfclown.files.SerializationModeEnum

import scala.collection.mutable
import sys.process._

/**
  * Created by jmarzin-cp on 22/04/2017.
  */
object cvaesie extends App with utilitairesSie{

  def getSiret(chaine: String): Option[String] = {
    var siretNumber: Option[String] = None
    val motifSiret = """.*SIRET : (\d{9} *\d{5}).*""".r
    chaine match {
      case motifSiret(numero) => siretNumber = Some(numero.replaceAll(" ",""))
      case _ =>
    }
    siretNumber
  }

  def addPage(dico : scala.collection.mutable.Map[String, (Boolean,List[Page])], cle : String, page: Page): Unit ={
    if (dico.get(cle).isDefined) {
      dico(cle) = (dico(cle)._1,dico(cle)._2 :+ page)
    } else {
      dico += (cle -> (false,List(page)))
    }
  }

  val dateHeure = getLaunchTime
  val retour = getDirectoryListing("Répertoire des relances CVAE à traiter")
  if (retour._2.isEmpty) System.exit(0)
  val repertoire = retour._1
  val listeFichiers = retour._2.get

  val clicEsi = showOptions(message = "Choisissez la transformation à appliquer",
    title = "Transformation",
    entries = List("Aucune","ClicEsiPlus"),
    initial = 1).getOrElse("")
  if(clicEsi.isEmpty)System.exit(0)

  val listeFichiersLus = new mutable.Stack[org.pdfclown.files.File]()
  val dicoRelances = scala.collection.mutable.Map[String, (Boolean,List[Page])]()
  val dico2807 = scala.collection.mutable.Map[String, (Boolean,List[Page])]()
  val dicoMajos5 = scala.collection.mutable.Map[String, (Boolean,List[Page])]()
  val dicoMajos02 = scala.collection.mutable.Map[String, (Boolean,List[Page])]()

  for(file <- listeFichiers) {
    if (file.getName.startsWith("courriers_")) {
      println("Fichier " + file.getName + " trouvé et non traité")
    } else {
      val fic = new org.pdfclown.files.File(file.getCanonicalPath)
      val fichier = fic.getDocument
      listeFichiersLus.push(fic)
      val pages = fichier.getPages
      var chaine = getStringPage(pages.get(0))
      if (chaine.contains("RESULTAT DE COMPARAISON CVAE") || chaine.contains("ETAT DE RESULTATS COMPARAISON CVAE")) {
        println("Fichier de comparaison CVAE trouvé")
        for (ipage <- 0 until pages.size) {
          val siretNumber = getSiret(getStringPage(pages.get(ipage)))
          if (siretNumber.isDefined) addPage(dico2807, siretNumber.get, pages.get(ipage))
        }
      } else if (chaine.contains("Cotisation sur la valeur ajoutée des entreprises. Régularisation au titre de l'année")) {
        println("Fichier de relances CVAE trouvé")
        for (ipage <- 0 until pages.size - 1) {
          val siretNumber = getSiret(getStringPage(pages.get(ipage)))
          if (siretNumber.isDefined) {
            addPage(dicoRelances, siretNumber.get, pages.get(ipage))
            addPage(dicoRelances, siretNumber.get, pages.get(ipage + 1))
          }
        }
      } else if (chaine.contains("Cotisation sur la valeur ajoutée des entreprises. Motivation de la majoration de 5%")) {
        println("Fichier des motivations de majoration à 5 %")
        for (ipage <- 0 until pages.size - 1) {
          val siretNumber = getSiret(getStringPage(pages.get(ipage)))
          if (siretNumber.isDefined) {
            addPage(dicoMajos5, siretNumber.get, pages.get(ipage))
            addPage(dicoMajos5, siretNumber.get, pages.get(ipage + 1))
          }
        }
      } else if (chaine.contains("Cotisation sur la valeur ajoutée des entreprises. Motivation de la majoration de 0,2%")) {
        println("Fichier des motivations de majoration à 0,2 %")
        for (ipage <- 0 until pages.size - 1) {
          val siretNumber = getSiret(getStringPage(pages.get(ipage)))
          if (siretNumber.isDefined) {
            addPage(dicoMajos02, siretNumber.get, pages.get(ipage))
            addPage(dicoMajos02, siretNumber.get, pages.get(ipage + 1))
          }
        }
      }
    }
  }

  val fichierCourriers = new org.pdfclown.files.File()
  val docsCourriers = fichierCourriers.getDocument
  val pagesCourriers = docsCourriers.getPages

  for(relance <- dicoRelances){
    var complet = true
    print("siret " + relance._1)
    if(dico2807.get(relance._1).isDefined) {
      dico2807(relance._1) = (true,dico2807(relance._1)._2)
      print(" comparaison trouvée")
      for(comparaison <- dico2807(relance._1)._2) {
        val chaine = getStringPage(comparaison)
        if(!chaine.matches(".*MONTANT TOTAL PENALITES SUR SOLDE 5% : *0,00.*")) {
          if(dicoMajos5.get(relance._1).isDefined) {
            dicoMajos5(relance._1) = (true,dicoMajos5(relance._1)._2)
            print(", majoration 5 % trouvée")
          } else {
            complet = false
            print(", majoration 5 % non trouvée")
          }
        }
        if(!chaine.matches(".*MONTANT MAJORATION 0\\.2% : *0,00.*")){
          if(dicoMajos02.get(relance._1).isDefined) {
            dicoMajos02(relance._1) = (true,dicoMajos02(relance._1)._2)
            print(", majoration 0,2 % trouvée")
          } else {
            complet = false
            print(", majoration 0,2 % non trouvée")
          }
        }
        println()
      }
    } else {
      complet = false
      println(" comparaisons non trouvées")
    }
    if(complet){
      dicoRelances(relance._1) = (true,relance._2._2)
      for(page <- relance._2._2) pagesCourriers.add(page.clone(docsCourriers))
      if(dico2807.get(relance._1).isDefined) for(page <- dico2807(relance._1)._2) {
        page.setRotation(RotationEnum.valueOf("Rightward"))
        pagesCourriers.add(page.clone(docsCourriers))
        pagesCourriers.add(new Page(docsCourriers))
      }
      if(dicoMajos5.get(relance._1).isDefined) for(page <- dicoMajos5(relance._1)._2) pagesCourriers.add(page.clone(docsCourriers))
      if(dicoMajos02.get(relance._1).isDefined) for(page <- dicoMajos02(relance._1)._2) pagesCourriers.add(page.clone(docsCourriers))
    }
  }

  val nomFichierCourriers = repertoire.getCanonicalPath + File.separatorChar +
    "courriers"  + "_" + dateHeure + ".pdf"
  docsCourriers.getFile.save(nomFichierCourriers,SerializationModeEnum.Standard)
  fichierCourriers.close()

  val repTraites = new File(repertoire.getCanonicalPath + File.separatorChar + "dejaTraites")
  if (!repTraites.exists() || repTraites.isFile) {
    repTraites.mkdir()
  }
  while(listeFichiersLus.nonEmpty){
    val fic = listeFichiersLus.pop
    fic.close()
    val nomFic = fic.getPath.split(File.separatorChar).last
    if(!(nomFic.startsWith("courriers_"))) {
      new File(fic.getPath).renameTo(new File(repertoire + File.separator + "dejaTraites" + File.separator + nomFic))
    }
  }

  dicoRelances.filter(!_._2._1).foreach(item => println("Relance Siret " + item._1 + " non traitée"))
  dico2807.filter(!_._2._1).foreach(item => println("2807 Siret " + item._1 + " non traitée"))
  dicoMajos5.filter(!_._2._1).foreach(item => println("Majo à 5 % Siret " + item._1 + " non traitée"))
  dicoMajos02.filter(!_._2._1).foreach(item => println("Majo à 0.2 % Siret " + item._1 + " non traitée"))

  //lancement de la fabrication des fichiers ClicEsi
  if(clicEsi != "Aucune") {
    var commande = "\"C:\\Program Files\\LibreOffice 4\\program\\sdraw\" " +
      "\"" + nomFichierCourriers + "\" " +
      "\"macro:///Standard.ClicEsi.ClicEsiPlus()\""
    var result = commande.!
  }

  println("fini")
}
