package edu.umass.ciir.biocreative.parse

import java.io.{File, FileInputStream, BufferedInputStream, InputStream}
import java.util.zip.GZIPInputStream

import edu.umass.ciir.strepsi.MainTools

/**
 * User: dietz
 * Date: 9/25/14
 * Time: 3:24 PM
 */
object IdiotTest {


  def main(args:Array[String]): Unit = {
    val biothesaurusFileName = MainTools.strsFromArgsSimple(args, "--biothesaurus=").headOption.getOrElse("/home/dietz/biocreative/data/biothesaurus/iproclass.xml.gz")

    val biothesaurusFile = new File(biothesaurusFileName)

    val stream: InputStream = {
      val stream = new BufferedInputStream(new FileInputStream(biothesaurusFile))
      if(biothesaurusFile.getName.endsWith(".gz")){
        new GZIPInputStream(stream)
      } else stream
    }

    val xmlSegmentIter = new XmlSegmentIterator(stream)

    var size = 0
    while(xmlSegmentIter.hasNext){
      xmlSegmentIter.next()
      size += 1
      if(size % 1000 == 0) print(".")
      if(size % 100000 == 0) println()
      if(size % 1000000 == 0) println("\n--- "+size+" ("+size/1000000+" million) ---")
    }
    println(s"$size entries in ${biothesaurusFile.getAbsolutePath}")

  }

}
