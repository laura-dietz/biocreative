package edu.umass.ciir.biocreative.chop

import scala.collection.mutable.ListBuffer

/**
 * User: dietz
 * Date: 11/4/14
 * Time: 8:01 PM
 */
class TabLineIterator(lineIterator: Iterator[String], caseInsensitive:Boolean) extends BufferedIterator[Seq[String]] {
  val lineIter = lineIterator
  var lineIterHead:String = if(lineIter.hasNext) lineIter.next() else ""



  def seekToNext():Option[Seq[String]] = {
    if(!lineIter.hasNext || !lineIterHead.contains("\t")) None
    else {
      val line = lineIterHead
      val prefix = {
        val pref = line.substring(0, line.indexOf("\t"))+"\t"
        if(caseInsensitive) pref.toLowerCase
        else pref
      }
      val batch = new ListBuffer[String]()
      batch += line
      while(if(caseInsensitive) lineIterHead.toLowerCase.startsWith(prefix) else lineIterHead.startsWith(prefix)) {
        batch += lineIterHead
        lineIterHead = if(lineIter.hasNext) lineIter.next() else ""
      }

//      val (batch, remaining) = lineIter.span(line => if(caseInsensitive) line.toLowerCase.startsWith(prefix) else line.startsWith(prefix))
//      lineIter = remaining.buffered
      Some(batch)
    }
  }

  var headOpt: Option[Seq[String]] = seekToNext()

  override def head:Seq[String] = headOpt.get

  override def next() = {
    val result = headOpt.get
    headOpt = seekToNext()
    result
  }

  override def hasNext = headOpt.isDefined
}



object TabLineIterator {
  def main(args:Array[String]): Unit = {
    val tli = new TabLineIterator(io.Source.fromFile("/home/dietz/biocreative/code/biocreative/data/gene/gene_info.sortedbysymbol.filtered3letters").getLines(), caseInsensitive = true)
    for(elem <- tli.take(10)) {
      println(elem.mkString("\n"))
      println()
    }
  }
}