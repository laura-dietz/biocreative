package edu.umass.ciir.biocreative.load

import java.io._
import java.util.zip.GZIPInputStream

import edu.umass.ciir.biocreative.{NameId, Name, NameMap}
import edu.umass.ciir.strepsi.StringTools

/**
 * User: dietz
 * Date: 9/23/14
 * Time: 10:53 PM
 */
object LoadNameIds {

  def loadMap(file:File):NameMap = {
    val source =
      if(file.getName.endsWith(".gz")) io.Source.fromInputStream(new GZIPInputStream(new FileInputStream(file)))
      else io.Source.fromInputStream(new FileInputStream(file))
    val forwardMap = scala.collection.mutable.HashMap[Name, NameId]()

    var counter:Int = 0
    for(line <- source.getLines()){
      val chunks = StringTools.getTabSplits(line, 2)
      val nameId = chunks(0)
      val name = chunks(1)

      forwardMap += name -> nameId

      counter +=1
      if(counter %100000 == 0) println(s"Loaded $counter name entries from ${file.getAbsolutePath}")
    }
    val forwardMapResult = forwardMap.toMap
    val result = NameMap(forward = forwardMapResult)
    source.close()
    result
  }

}
