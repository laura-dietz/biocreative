package edu.umass.ciir.biocreative.tag

import edu.umass.ciir.strepsi.{CountingTable, MainTools}

import scala.reflect.io.{Directory, File}
import scala.xml.XML

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 9:52 AM
 */
class BioCreativePipeline(tagger:FastNameTagger, doTrain:Boolean) {
  System.setProperty("file.encoding","UTF-8")

  val counting = new CountingTable[String]()

  def cleanGoldGene(geneAnnotation: String) = geneAnnotation.substring(0, geneAnnotation.indexOf('(')).trim
  def cleanGoldGo(goAnnotation: String) = goAnnotation.substring(0, goAnnotation.indexOf('|')).trim

  def processSingleDocumentTrain(file: File) = {
    val root = XML.loadFile(new java.io.File(file.toAbsolute.path))

    val documentId  = ((root \\ "document").head \ "id").text

    println("=========================================")
    println("====== "+documentId+ " ===================")



    for(passage <- root \\ "passage"){
      val offset = (passage \ "offset").text.toInt

      // only for training: nested annotation element
      for(annotation <- passage\\"annotation"){

        val text = (annotation \ "text").text
        val matches = tagger.tag(text)

        val goldGene = (for(infon <- (annotation \\ "infon"); if (infon\"@key").text == "gene") yield cleanGoldGene(infon.text).toLowerCase).toSet
        val goldGo = (for(infon <- (annotation \\ "infon"); if (infon\"@key").text == "go-term") yield cleanGoldGo(infon.text).toLowerCase).toSet

        val foundGene = matches.find(m => goldGene.contains(m.mention.toLowerCase))
        val foundGo = matches.find(m => goldGo.contains(m.mention.toLowerCase))
        if(foundGene.isDefined) counting.add("foundGene")
        if(foundGo.isDefined) counting.add("foundGo")
        counting.add("allGene")
        counting.add("allGo")

        println(s"offset: $offset \t $text \n Matches: "+matches.mkString(", ")+" \n " +
          "goldMatches Gene: "+goldGene.map(x => if(foundGene.get.mention == x) { "##"+x+"##"} else x).mkString(", ")+ " " +
          "Go:"+goldGo.mkString(","))
      }

    }

    val geneRecall = 1.0 * counting.getOrElse("foundGene", 0) / counting.getOrElse("allGene", 0)
    val goRecall = 1.0 * counting.getOrElse("foundGo", 0) / counting.getOrElse("allGo", 0)
    println(s" geneRecall=$geneRecall\tgoRecall=$goRecall")
  }

  def processSingleDocumentTest(file: File) = {
    val root = XML.loadFile(new java.io.File(file.toAbsolute.path))

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
       if (doTrain) {
         processSingleDocumentTrain(file.toFile)
       } else {
         processSingleDocumentTest(file.toFile)
       }
     }
  }

}

object BioCreativePipeline {
  def main(args:Array[String]): Unit = {
    val dictionaryFile = MainTools.strsPlainFromArgs(args, "--dictionary=").headOption.getOrElse(throw new IllegalArgumentException("required flag --dictionary={dictionaryfile}"))
    val articlesDir = MainTools.strsPlainFromArgs(args, "--articles=").headOption.getOrElse(throw new IllegalArgumentException("required flag --articles={dir}"))
    val doTrain = MainTools.strsPlainFromArgs(args, "--train").nonEmpty



    val tagger = new FastNameTagger(new java.io.File(dictionaryFile))
    val pipe = new BioCreativePipeline(tagger, doTrain)
    pipe.processAllDocuments(Directory(articlesDir))
  }
}