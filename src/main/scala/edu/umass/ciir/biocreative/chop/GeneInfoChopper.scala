//package edu.umass.ciir.biocreative.chop
//
//import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
//import java.util.zip.GZIPInputStream
//
//import edu.umass.ciir.biocreative.parse.BioThesaususParser
//import edu.umass.ciir.biocreative._
//import edu.umass.ciir.strepsi.MainTools
//
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//import scala.xml.Node
//
///**
// * User: dietz
// * Date: 11/4/14
// * Time: 3:11 PM
// */
//class GeneInfoChopper(geneInfoFile:String, chopOutputDir:String, fromLine:Int, toLine:Int, flushIntervall:Int) {
//
//
//
//  private final val stream: InputStream = {
//    val stream = new BufferedInputStream(new FileInputStream(geneInfoFile))
//    if (new File(geneInfoFile).getName.endsWith(".gz")) {
//      new GZIPInputStream(stream)
//    } else stream
//  }
//
//  def fillResultMap(source:io.Source, resultMap:mutable.HashMap[Name, Seq[Seq[String]]], fromLine:Int, toLine:Int, fileGetAbsolutePath:String) {
//    var prevId = ""
//    var counter: Int = 0
//    var startRecording = false
//    var bioNameBuffer = new ListBuffer[Seq[String]]()
//    for (line <- source.getLines()) {
//      val chunks = line.split('\t')
//      val id = chunks(0)
//      if (id != prevId) {
//        if(counter >= fromLine) {
//          startRecording = true
//          // submit
//          if (bioNameBuffer.nonEmpty) {
//            if (counter >= fromLine && counter <= toLine) {
//              resultMap += prevId -> bioNameBuffer
//            }
//            bioNameBuffer.clear()
//            if (counter > toLine) return
//          }
//          prevId = id
//          counter += 1
//
//          if (counter % 100000 == 0) println(s"Loaded $counter entries for $fileGetAbsolutePath  id = $id")
//        } else {
//          prevId = id
//          counter += 1
//          if (counter % 100000 == 0) println(s"skipping $counter entries for $fileGetAbsolutePath  id = $id")
//        }
//      }
//
//      if(startRecording) {
//        bioNameBuffer += chunks.slice(1, chunks.length)
//      }
//
//    }
//  }
//
//
//  val parser = new BioThesaususParser(stream)
//  val iter = parser.iterator()
//  val geneSymbolWriter = new ChopWriter(chopOutputDir, ".bio")
//  val writers = Seq(geneSymbolWriter)
//
//  def write() {
//    var count: Int = flushIntervall
//    var hasNext = nextDocument()
//    while (hasNext) {
//      if( count <=toLine){
//        if (count % 1000 == 0) print(".")
//        if (count % 100000 == 0) {
//          println()
//          writers.foreach(_.flush())
//        }
//        if (count == 0) {
//          //          println("\nStopped.")
//          //          hasNext = false
//          writers.foreach(_.flush())
//          count = flushIntervall
//        }
//        else {
//          hasNext = nextDocument()
//          count -= 1
//        }
//      } else {
//        writers.foreach(_.close())
//        return
//      }
//    }
//
//    //    writers.foreach(_.flush())
//    writers.foreach(_.close())
//  }
//
//  def nextDocument(): Boolean = {
//    def clean(seq:Seq[String]):Seq[String] ={
//      seq.map(_.replaceAll("\\s+"," "))
//    }
//
//    def cleanAndUnwrap(seq:Seq[String]):Seq[String] ={
//      seq.map({
//        _.replaceAllLiterally("\n"," ").replaceAllLiterally("\r"," ").replaceAllLiterally("\t"," ").replaceAll("\\s+"," ")
//      })
//    }
//
//    if (iter.hasNext) {
//      val entry: String = iter.next()
//      val dirty = parser.getNames(entry)
//      val bioDocument = BioNames("geneinfo_"+dirty.identifier,clean(dirty.names), dirty.otherIds, clean(dirty.species), clean(dirty.goTerms), cleanAndUnwrap(dirty.description))
//
//      val appendix = BioGalagoNames.serialize(bioDocument)
//      for(name <- bioDocument.names.distinct; if name.length>=3 && Seq(0,1,2).forall(name.charAt(_).isLetter)) {
//        geneSymbolWriter.append(name, appendix)
//      }
//
//      true
//    }
//    else false
//  }
//
//  def close() {
//    stream.close()
//  }
//}
//object GeneInfoChopper {
//
//  def main(args:Array[String]): Unit ={
//System.setProperty("file.encoding","UTF-8")
//    val inputFile =MainTools.strsPlainFromArgs(args, "-inputFile=").headOption.getOrElse(throw new Error("required argument -inputFile="))
//    val outputFile =MainTools.strsPlainFromArgs(args, "-chopOutputDir=").headOption.getOrElse(throw new Error("required argument -chopOutputDir="))
//    val fromLine =MainTools.strsPlainFromArgs(args, "-fromLine=").headOption.map(_.toInt).getOrElse(0)
//    val toLine =MainTools.strsPlainFromArgs(args, "-toLine=").headOption.map(_.toInt).getOrElse(Integer.MAX_VALUE)
//    val flushIntervall =MainTools.strsPlainFromArgs(args, "-flushIntervall=").headOption.map(_.toInt).getOrElse(500)
//
//    val chopper = new GeneInfoChopper(inputFile, outputFile,fromLine, toLine, flushIntervall)
//    chopper.write()
//    chopper.close()
//  }
//
//}
