package edu.umass.ciir.biocreative.parse

import java.io._
import java.util.zip.GZIPInputStream

import edu.umass.ciir.strepsi.MainTools

import scala.xml.Node

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 3:25 PM
 */
class NameTaggingDictionaryFactory(outputForwardFile:File, outputReverseFile:File, biothesaurusFile:File) {
  private final val stream: InputStream = {
    val stream = new BufferedInputStream(new FileInputStream(biothesaurusFile))
    if(biothesaurusFile.getName.endsWith(".gz")){
      new GZIPInputStream(stream)
    } else stream
  }
  private final val bioThesaususIterator: Iterator[Node] = null
  private final val bioThesaususParser: BioThesaususParser = null
  private final val parsingTools: BioCreativeParsing = new BioCreativeParsing

  val parser = new BioThesaususParser(stream)
  val iter = parser.iterator()
  val outputForwardWriter = new FileWriter(outputForwardFile)
  val outputReverseWriter = new FileWriter(outputReverseFile)

  def write() {
    var count: Int = 10000
    var hasNext = nextDocument()
    while (hasNext) {
      if(count % 100==0) println()
      if (count <= 0) {
        println("\nStopped.")
        hasNext =false
      }
      else {
        hasNext = nextDocument()
        count -= 1
      }
    }
    outputForwardWriter.flush()
    outputForwardWriter.close()
    outputReverseWriter.flush()
    outputReverseWriter.close()
  }

  class IdProvider(init:Int) {
    var counter:Int = init
    def consume():Int = {
      val x = counter
      counter += 1
      x
    }
  }
  var entryIdProvider = new IdProvider(0)

  def nextDocument(): Boolean = {
    if (iter.hasNext) {
      val entry: Node = iter.next()
      val bioDocument = parser.getNames(entry)


      val foundIdType = Seq( "Entrez_Gene_ID", "GenBank_ID", "UniProtKB_ID", "Pfam_ID", "Locus_Tag").find(bioDocument.otherIds.keySet.contains(_))
      val id = foundIdType match {
        case None => "U:"+entryIdProvider.consume()
        case Some(idType) => idType+":"+bioDocument.otherIds(idType).head
      }


      val allids = bioDocument.otherIds.toSeq.flatMap(pair => pair._2.sorted.map(pair._1 -> _)).mkString(" ")
      for(name <- bioDocument.names.distinct) {
        outputForwardWriter.write(id + "\t" + name+"\n")
        outputReverseWriter.write(name + '\t' + id + '\t'+  bioDocument.goTerms.mkString(" ")+'\t' + allids+'\t'+bioDocument.species.mkString(" ")+'\n')
        print(".")
      }
      true
    }
    else false
  }

  def close() {
    stream.close()
  }
}

object NameTaggingDictionaryFactory {
  def main(args:Array[String]): Unit ={
    val outForwardFile = MainTools.strsFromArgs(args, "--outForward=", 1).head
    val outReverseFile = MainTools.strsFromArgs(args, "--outReverse=", 1).head
    val biothesaurusFile= MainTools.strsFromArgs(args, "--biothesaurus=", 1).head
    val factory = new NameTaggingDictionaryFactory(new File(outForwardFile), new File(outReverseFile), new File(biothesaurusFile))
    factory.write()
    factory.close()
  }
}
