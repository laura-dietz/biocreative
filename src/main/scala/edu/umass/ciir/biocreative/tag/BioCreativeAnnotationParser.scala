package edu.umass.ciir.biocreative.tag

import edu.umass.ciir.biocreative.tag.BioCreativeAnnotationParser.{BioCreativeAnnotation, BioCreativeAnnotatedPassage, BioCreativeAnnotatedDocument}

import scala.collection.mutable.ListBuffer
import scala.reflect.io.{Directory, File}
import scala.xml.XML

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 9:52 AM
 */
class BioCreativeAnnotationParser(tagger:FastNameTagger, doTrain:Boolean) {
  System.setProperty("file.encoding","UTF-8")

  def cleanGoldGeneSymbol(geneAnnotation: String) = {
    val offset = geneAnnotation.indexOf('(')
    if(offset >=0)
      geneAnnotation.substring(0, offset).trim
    else {
      System.err.println("Tried to clean gene annotation, but count not find delimiter \'(\'. "+geneAnnotation)
      geneAnnotation
    }
  }
  def cleanGoldGeneEntrez(geneAnnotation: String) = {
    val offset = geneAnnotation.indexOf('(')
    val endoffset = geneAnnotation.indexOf(')', offset)
    if(offset >=0 && endoffset >=0)
      geneAnnotation.substring(offset+1, endoffset).trim
    else {
      System.err.println("Tried to clean gene annotation, but count not find delimiters \'(\' or \')\'. "+geneAnnotation)
      geneAnnotation
    }
  }
  def cleanGoldGo(goAnnotation: String) = {
    val offset = goAnnotation.indexOf('|')
    if(offset>=0)
      goAnnotation.substring(0, offset).trim
    else {
      System.err.println("Tried to clean go annotation, but could not find delimiter \'|\'. "+goAnnotation)
      goAnnotation
    }
   }

  def processSingleDocumentTrain(file: File):BioCreativeAnnotatedDocument = {
    val root = XML.loadFile(new java.io.File(file.toAbsolute.path))

    val documentId  = ((root \\ "document").head \ "id").text

    println("=========================================")
    println("====== "+documentId+ " ===================")


    val passageList = new ListBuffer[BioCreativeAnnotatedPassage]

    for(passage <- root \\ "passage"){
      val offset = (passage \ "offset").text.toInt

      val annotationList = new ListBuffer[BioCreativeAnnotation]

      var passagetext = ""
      // only for training: nested annotation element
      for(annotation <- passage\\"annotation"){

        val text = (annotation \ "text").text
        passagetext = text

        val goldGeneSymbol = (for(infon <- (annotation \\ "infon"); if (infon\"@key").text == "gene") yield cleanGoldGeneSymbol(infon.text).toLowerCase).toSet
        val goldGeneEntrez = (for(infon <- (annotation \\ "infon"); if (infon\"@key").text == "gene") yield cleanGoldGeneEntrez(infon.text).toLowerCase).toSet
        val goldGo = (for(infon <- (annotation \\ "infon"); if (infon\"@key").text == "go-term") yield cleanGoldGo(infon.text).toLowerCase).toSet

        val annotationId = (annotation\ "@id").text

        annotationList += BioCreativeAnnotation(annotationId, goldGeneSymbol, goldGeneEntrez, goldGo)
      }
      if(annotationList.nonEmpty){
        passageList += BioCreativeAnnotatedPassage(offset, passagetext, Some(annotationList.result()))
        annotationList.clear()
      }
    }

    val document = BioCreativeAnnotatedDocument(documentId, passageList.result())
    passageList.clear()
    document
  }


  def processSingleDocumentTest(file: File):BioCreativeAnnotatedDocument = {
    val root = XML.loadFile(new java.io.File(file.toAbsolute.path))

    val passageList = new ListBuffer[BioCreativeAnnotatedPassage]

    val documentId  = ((root \\ "document").head \ "id").text

    for(passage <- root \\ "passage"){
      val offset = (passage \ "offset").text.toInt

      val text = (passage \ "text").text

      passageList += BioCreativeAnnotatedPassage(offset, text, None)

    }

    val document = BioCreativeAnnotatedDocument(documentId, passageList.result())
    passageList.clear()
    document
  }

  def processAllDocuments(dir: Directory): Iterator[BioCreativeAnnotatedDocument] = {
    val x =
      for(file <- dir.list; if file.isFile && file.hasExtension("xml")) yield {
         if (doTrain) {
           processSingleDocumentTrain(file.toFile)
         } else {
           processSingleDocumentTest(file.toFile)
         }
       }
    x
  }

}

object BioCreativeAnnotationParser {
  case class BioCreativeAnnotation(annotationId:String, goldGeneSymbol:Set[String], goldGeneEntrez:Set[String], goldGo:Set[String])
  case class BioCreativeAnnotatedPassage(passageOffset:Int, text:String, annotations:Option[Seq[BioCreativeAnnotation]])
  case class BioCreativeAnnotatedDocument(documentId:String, passages:Seq[BioCreativeAnnotatedPassage])
}

