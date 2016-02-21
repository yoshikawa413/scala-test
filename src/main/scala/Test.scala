import scalikejdbc._
import java.io.{ File, FileInputStream, FileOutputStream, BufferedOutputStream }
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths }
import java.security.MessageDigest
import java.util.zip.{ ZipInputStream }
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import scala.collection.JavaConversions._
import scala.collection.immutable.ListMap

object Permitted {
  val CHECK_LIST = ListMap("hogehoge" -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

  def checkSum(name: String, hash: String): Boolean = {
    val check = CHECK_LIST.get(name) match {
      case Some(s) => s
      case None => ""
    }
    if(check == hash) {
      true
    } else {
      false
    }
  }
}

case class JsFile(name: String, hash: String)
object Test {
  val CORRECT_ARGS_LENGTH = 2
  
  def extract(input: ZipInputStream, outputFile: File): Unit = {
    outputFile.getParentFile.mkdirs()
    using(new BufferedOutputStream(new FileOutputStream(outputFile))) { output =>
      Iterator.continually(input.read()).takeWhile(_ != -1)
        .foreach { output.write(_) }
    }
  }

  def unzip(zipPath: String, extractDir: String): Unit = {
    val zipFile = new File(zipPath)
    if (zipFile.canRead() & new File(extractDir).canRead()) {
      //ok file exist
    } else {
      return
    }

    using(new ZipInputStream(new FileInputStream(zipFile))) { input =>
      Iterator.continually(input.getNextEntry).takeWhile(_ != null)
        .filterNot { _.isDirectory() }.foreach(e => {
          val extractFile = new File(extractDir, e.getName)
          extract(input, extractFile)
        })
    }
  }

  def parseHtml(path: String): List[JsFile] = {
    var JsFiles: List[JsFile] = Nil
    val indexHtml = new File(path)
    val doc: Document = Jsoup.parse(indexHtml, StandardCharsets.UTF_8.name)
    for (element <- doc.getElementsByTag("script")) {
      val js = new File(indexHtml.getParentFile.getAbsolutePath, element.attr("src"))
      val byteArray: Array[Byte] = Files.readAllBytes(Paths.get(js.getAbsolutePath))
      val content = new String(byteArray, StandardCharsets.UTF_8)

      val md = MessageDigest.getInstance("SHA-256")
      val hash = md.digest(byteArray).map("%02x".format(_)).mkString
      val jsFile = JsFile(js.getName, hash)
      JsFiles = jsFile :: JsFiles
    }
    JsFiles
  }

  /**
   * args(0): input zip file
   * args(1): extract dir
   */
  def main(args: Array[String]): Unit = {
    args.length match {
      case CORRECT_ARGS_LENGTH => {
        unzip(args(0), args(1))

        val target = "input path"
        val jsFiles = parseHtml(target)
        jsFiles.foreach { f =>
          val check = Permitted.checkSum(f.name, f.hash)
          println(s"${f.name} => ${check}")
        }
      }
      case _ => {}
    }
  }
}
