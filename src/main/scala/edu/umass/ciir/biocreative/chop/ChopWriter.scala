package edu.umass.ciir.biocreative.chop

import java.io.{File, FileWriter}

import edu.umass.ciir.strepsi.FileTools

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * User: dietz
 * Date: 11/4/14
 * Time: 3:32 PM
 */
class ChopWriter(pathPrefix:String, suffix:String) {
  val bufferMap = new mutable.HashMap[String, ListBuffer[String]]
  FileTools.makeDirs(pathPrefix)

  def append(key:String, text:String): Unit = {
    bufferMap.getOrElseUpdate(key, new ListBuffer[String]) += text
  }

  def scrub(s:String):String = {
    val str = s.replaceAll("[^a-zA-Z0-9\\._\\-+]","").toLowerCase
    str.substring(0,math.min(str.length, 200))
  }

  def flush(): Unit = {
    for((key, text) <- bufferMap){
      val scrubbedFilename = scrub(key)

      val subdir = scrubbedFilename.substring(0, 3).toLowerCase
      val file = new File(pathPrefix + File.separator +subdir+File.separator + scrubbedFilename + suffix)
      FileTools.makeNecessaryDirs(file.getAbsolutePath)
      val existed = file.exists()
      val pw = new FileWriter(file, true)
      if(!existed) {
        pw.append("<identifier>" + key + "</identifier>\n")
      }
      pw.append(text.mkString("\n"))
      pw.close()
    }


    bufferMap.clear()
  }

  def close() = flush()

}
