package edu.umass.ciir.biocreative.chop

import java.io.{BufferedInputStream, File, FileInputStream, InputStream}
import java.util.zip.GZIPInputStream

import edu.umass.ciir.biocreative._
import edu.umass.ciir.strepsi.{MainTools, StringTools}

import scala.collection.mutable.ListBuffer

/**
* User: dietz
* Date: 11/4/14
* Time: 3:11 PM
*/
class GeneInfoChopper(geneInfoFile:String, chopOutputDir:String, fromLine:Int, toLine:Int, flushIntervall:Int) {



  private final val stream: InputStream = {
    val stream = new BufferedInputStream(new FileInputStream(geneInfoFile))
    if (new File(geneInfoFile).getName.endsWith(".gz")) {
      new GZIPInputStream(stream)
    } else stream
  }


  val iter = new TabLineIterator(io.Source.fromFile(geneInfoFile).getLines(), caseInsensitive = true)

  val geneSymbolWriter = new ChopWriter(chopOutputDir, ".bio")
  val writers = Seq(geneSymbolWriter)

  def write() {
    var count: Int = flushIntervall
    var hasNext = nextDocument()
    while (hasNext) {
      if( count <=toLine){
        if (count % 1000 == 0) print(".")
        if (count % 100000 == 0) {
          println()
//          writers.foreach(_.flush())
        }
        if (count == 0) {
          //          println("\nStopped.")
          //          hasNext = false
          writers.foreach(_.flush())
          count = flushIntervall
        }
        else {
          hasNext = nextDocument()
          count -= 1
        }
      } else {
        writers.foreach(_.flush())
        writers.foreach(_.close())
        return
      }
    }

    //    writers.foreach(_.flush())
    writers.foreach(_.flush())
    writers.foreach(_.close())
  }

  def nextDocument(): Boolean = {
    def clean(seq:Seq[String]):Seq[String] ={
      seq.map(_.replaceAll("\\s+"," "))
    }

    def cleanAndUnwrap(seq:Seq[String]):Seq[String] ={
      seq.map({
        _.replaceAllLiterally("\n"," ").replaceAllLiterally("\r"," ").replaceAllLiterally("\t"," ").replaceAll("\\s+"," ")
      })
    }

    if (iter.hasNext) {
      val entry: Seq[String] = iter.next()
      var identifier:String = ""
      val names = new ListBuffer[String]
      val ids = new ListBuffer[String]
      val desc = new ListBuffer[String]
      for(line <- entry){
        val chunks = StringTools.getTabSplits(line, min = 11).map( x => if(x.trim=="-") "" else x)
        identifier = chunks(0)
        names += chunks(3)
        names ++= {
          if(chunks(5).contains("|"))
            chunks(5).split("\\|").toSeq
          else Seq(chunks(5))
        }
        ids += chunks(2)
        desc += chunks(9)
      }
      val bioDocument = BioNames("geneinfo_"+identifier, names =names.distinct.filterNot(_.length==0), otherIds =Map("GeneId"->ids.distinct.filterNot(_.length==0)) , species = Seq.empty, goTerms = Seq.empty, description = desc.filterNot(_.length==0))

      //#Format: tax_id GeneID Symbol LocusTag Synonyms dbXrefs chromosome map_location description type_of_gene Symbol_from_nomenclature_authority Full_name_from_nomenclature_authority Nomenclature_status Other_designations Modification_date (tab is used a

      val appendix = BioGalagoNames.serialize(bioDocument)
      for(name <- bioDocument.names.distinct; if name.length>=3 && Seq(0,1,2).forall(name.charAt(_).isLetter)) {
//        geneSymbolWriter.append(name, appendix)
      }

      true
    }
    else false
  }

  def close() {
    stream.close()
  }
}
object GeneInfoChopper {

  def main(args:Array[String]): Unit ={
    System.setProperty("file.encoding","UTF-8")
    val inputFile =MainTools.strsPlainFromArgs(args, "-inputFile=").headOption.getOrElse(throw new Error("required argument -inputFile="))
    val outputFile =MainTools.strsPlainFromArgs(args, "-chopOutputDir=").headOption.getOrElse(throw new Error("required argument -chopOutputDir="))
    val fromLine =MainTools.strsPlainFromArgs(args, "-fromLine=").headOption.map(_.toInt).getOrElse(0)
    val toLine =MainTools.strsPlainFromArgs(args, "-toLine=").headOption.map(_.toInt).getOrElse(Integer.MAX_VALUE)
    val flushIntervall =MainTools.strsPlainFromArgs(args, "-flushIntervall=").headOption.map(_.toInt).getOrElse(500)

    val chopper = new GeneInfoChopper(inputFile, outputFile,fromLine, toLine, flushIntervall)
    chopper.write()
    chopper.close()
  }

}
