package edu.umass.ciir.biocreative.load

import java.io._
import java.util.zip.GZIPInputStream

import edu.umass.ciir.biocreative.{Name, BioNames}
import edu.umass.ciir.strepsi.SeqTools

import scala.collection.mutable.ListBuffer

/**
 * User: dietz
 * Date: 9/23/14
 * Time: 10:53 PM
 */
object LoadBioDocument {
  def loadMap(file:File):Map[Name, BioNames] = {
    val source = io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(file)))
    var prevId = ""
    var bioNameBuffer = new ListBuffer[BioNames]()
    val resultMap = scala.collection.mutable.HashMap[Name, BioNames]()

    for(line <- source.getLines()){
      val chunks = line.split('\t')
      val id = chunks(0)
      if(id != prevId){
        // submit
        if(bioNameBuffer.nonEmpty) {
          resultMap += prevId -> BioNames.fromSeq(bioNameBuffer)
          bioNameBuffer.clear()
        }
        prevId = id
      }

      val names = chunks(2).split("\" \"")
      val otherIds = SeqTools.groupByKey( chunks(3).split("\\) \\(").map(_.split(",")).map(x => x(0) -> x(1))).map(pair => (pair._1 -> pair._2.toSeq))
      val goTerms = chunks(5).split(" ")
      val species = chunks(4).split("\" \"")
      val bioname = BioNames(id, names, otherIds, species, goTerms, Seq(chunks(6)))
      bioNameBuffer += bioname

    }
    source.close()
    resultMap.toMap
  }

}
