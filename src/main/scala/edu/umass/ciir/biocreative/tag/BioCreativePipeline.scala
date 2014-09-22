package edu.umass.ciir.biocreative.tag

import edu.umass.ciir.strepsi.MainTools

import scala.reflect.io.{Directory, File}
import scala.xml.XML

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 9:52 AM
 */
class BioCreativePipeline(tagger:FastNameTagger) {
  System.setProperty("file.encoding","UTF-8")

  def processSingleDocument(file: File) = {
//    def create = XMLInputFactory.newInstance()
//      create.setProperty(XMLInputFactory.SUPPORT_DTD, java.lang.Boolean.FALSE)
//
//    val evReader = create.createXMLEventReader(new FileInputStream(new java.io.File(file.toAbsolute.path)))
//    evReader.

//    val factory = SAXParserFactory.newInstance()
//    factory.setValidating(false)
//    val parser = factory.newSAXParser()
//    val isValidating = parser.isValidating()
//    val xmlLoader = XML.withSAXParser(parser)
    val root = XML.loadFile(new java.io.File(file.toAbsolute.path))
//    val root = XML.loadXML(new InputSource(new FileReader(file.toAbsolute.path)), parser)

    val documentId  = ((root \\ "document").head \ "id").text

    println("=========================================")
    println("====== "+documentId+ " ===================")

    for(passage <- root \\ "passage"){
      val offset = (passage \ "offset").text.toInt
      val text = (passage \ "text").text
      val matches = tagger.tag(text)

      println(s"offset: $offset \t $text \n Matches: "+matches.mkString(", "))
    }
  }

  def processAllDocuments(dir: Directory): Unit = {
     for(file <- dir.list; if file.isFile && file.hasExtension("xml")) {
       processSingleDocument(file.toFile)
     }
  }

}

object BioCreativePipeline {
  def main(args:Array[String]): Unit = {
    val dictionaryFile = MainTools.strsPlainFromArgs(args, "--dictionary=").headOption.getOrElse(throw new IllegalArgumentException("required flag --dictionary={dictionaryfile}"))
    val articlesDir = MainTools.strsPlainFromArgs(args, "--articles=").headOption.getOrElse(throw new IllegalArgumentException("required flag --articles={dir}"))


    val tagger = new FastNameTagger(new java.io.File(dictionaryFile))
    val pipe = new BioCreativePipeline(tagger)
    pipe.processAllDocuments(Directory(articlesDir))
  }
}