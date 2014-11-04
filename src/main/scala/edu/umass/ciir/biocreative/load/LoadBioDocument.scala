package edu.umass.ciir.biocreative.load

import java.io._
import java.util.zip.GZIPInputStream

import edu.umass.ciir.biocreative.{BioNames, Name}
import edu.umass.ciir.strepsi.MainTools
import edu.umass.ciir.strepsimur.DiskBacking

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * User: dietz
 * Date: 9/23/14
 * Time: 10:53 PM
 */
object LoadBioDocument {
  def loadMap(file:File, fromLine:Int=0, toLine:Int=Integer.MAX_VALUE):Map[Name, BioNames] = {
    val source = io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(file)))
    val resultMap = scala.collection.mutable.HashMap[Name, BioNames]()

    fillResultMap(source,resultMap, fromLine, toLine, file.getAbsolutePath)
    source.close()
    resultMap.toMap
  }


  def fillResultMap(source:io.Source, resultMap:mutable.HashMap[Name, BioNames], fromLine:Int, toLine:Int, fileGetAbsolutePath:String) {
    var prevId = ""
    var counter: Int = 0
    var startRecording = false
    var bioNameBuffer = new ListBuffer[BioNames]()
    for (line <- source.getLines()) {
      val chunks = line.split('\t')
      val id = chunks(0)
      if (id != prevId) {
        if(counter >= fromLine) {
          startRecording = true
          // submit
          if (bioNameBuffer.nonEmpty) {
            if (counter >= fromLine && counter <= toLine) {
              resultMap += prevId -> BioNames.fromSeq(bioNameBuffer)
            }
            bioNameBuffer.clear()
            if (counter > toLine) return
          }
          prevId = id
          counter += 1

          if (counter % 100000 == 0) println(s"Loaded $counter entries for $fileGetAbsolutePath  id = $id")
        } else {
          prevId = id
          counter += 1
          if (counter % 100000 == 0) println(s"skipping $counter entries for $fileGetAbsolutePath  id = $id")
        }
      }

      if(startRecording) {
        val bioname = BioNames.deserialize(chunks)
        bioNameBuffer += bioname
      }

    }
  }

  def convertToDiskBacker(inputFile:String, outputFile:String, fromLine:Int=0, toLine:Int=Integer.MAX_VALUE) = {

      val nameToBioNames = loadMap(new java.io.File(inputFile), fromLine, toLine)
      val stringstringMap = nameToBioNames.map(x => x._1 -> BioNames.serialize(x._2).mkString("\t"))
      DiskBacking.createStringStringDiskBackingImm(stringstringMap, outputFile)
  }

  def main(args:Array[String]): Unit ={
    val inputFile =MainTools.strsPlainFromArgs(args, "-inputFile=").headOption.getOrElse(throw new Error("required argument -inputFile="))
    val outputFile =MainTools.strsPlainFromArgs(args, "-outputFile=").headOption.getOrElse(throw new Error("required argument -outputFile="))
    val fromLine =MainTools.strsPlainFromArgs(args, "-fromLine=").headOption.map(_.toInt).getOrElse(0)
    val toLine =MainTools.strsPlainFromArgs(args, "-toLine=").headOption.map(_.toInt).getOrElse(Integer.MAX_VALUE)
    convertToDiskBacker(inputFile,outputFile,fromLine, toLine)
  }
}
