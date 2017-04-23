import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import javax.swing.{Icon, JOptionPane, UIManager}

import org.pdfclown.documents.Page
import org.pdfclown.documents.contents.ContentScanner
import org.pdfclown.documents.contents.objects.{ContainerObject, ContentObject, ShowText, Text}
import org.pdfclown.tools.TextExtractor

import scala.swing.{Component, Dialog, FileChooser, Swing}
import scala.swing.Swing.EmptyIcon

/**
  * Created by jmarzin-cp on 22/04/2017.
  */
trait utilitairesSie {

  def getLaunchTime: String = {
    val now = Calendar.getInstance().getTime
    val formatDateHeure = new SimpleDateFormat("yyyy-MM-dd_hh-mm-ss-SSS")
    formatDateHeure.format(now)
  }

  def getDirectoryListing(title: String = ""): (File, Option[Array[File]]) = {
    val chooser = new FileChooser(null)
    chooser.fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
    chooser.title = title
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      (chooser.selectedFile, Some(chooser.selectedFile.listFiles().filter(f => f.isFile && f.getName.contains(".pdf"))))
    } else (null, None)
  }

  def showOptions(
                   parent: Component = null,
                   message: Any,
                   title: String = UIManager.getString("OptionPane.titleText"),
                   messageType: Dialog.Message.Value = Dialog.Message.Question,
                   icon: Icon = EmptyIcon,
                   entries: Seq[String],
                   initial: Int): Option[String] = {
    val r = JOptionPane.showOptionDialog(
      if (parent == null) null else parent.peer,  message, title, 0,
      messageType.id, Swing.wrapIcon(icon),
      entries.toArray[AnyRef], initial)
    if (r < 0) None else Some(entries(r))
  }

  def getStringPage(page: Page): String = {
    var chaine = ""
    def extract(level: ContentScanner) : Unit = {
      if(level == null)
        return
      try {
        while (level.moveNext()) {
          val content = level.getCurrent
          content match {
            case showText: ShowText =>
              val font = level.getState.getFont
              // Extract the current text chunk, decoding it!
              chaine = chaine + " " + font.decode(content.asInstanceOf[ShowText].getText)
            case _: Text | _:ContainerObject => extract(level.getChildLevel)
            case _ =>
          }
        }
      } catch {
        case e: Exception =>
      }
    }
    extract(new ContentScanner(page))
//
//    var contents = page.getContents
//    var texte = new TextExtractor().extract(page.getContents)
//    var chaine = ""
//    for (i <- 0 until texte.get(null).size()) chaine += texte.get(null).get(i).getText
    chaine
  }
}
