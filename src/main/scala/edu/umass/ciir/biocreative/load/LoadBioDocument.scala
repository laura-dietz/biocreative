package edu.umass.ciir.biocreative.load

import java.io._
import java.util.zip.GZIPInputStream

import edu.umass.ciir.biocreative.{BioNames, Name}
import edu.umass.ciir.strepsimur.DiskBacking

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

      val bioname = BioNames.deserialize(chunks)
      bioNameBuffer += bioname

    }
    source.close()
    resultMap.toMap
  }



  def convertToDiskBacker(inputFile:String, outputFile:String) = {

      val nameToBioNames = loadMap(new java.io.File(inputFile))
      val stringstringMap = nameToBioNames.map(x => x._1 -> x._2.toString)
      DiskBacking.createStringStringDiskBackingImm(stringstringMap, outputFile)
  }
}
