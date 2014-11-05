package edu.umass.ciir.biocreative.chop

/**
 * User: dietz
 * Date: 11/4/14
 * Time: 8:01 PM
 */
class TabLineIterator(source:io.Source, caseInsensitive:Boolean) extends BufferedIterator[Seq[String]] {
  var lineIter = source.getLines().buffered


  def seekToNext():Option[Seq[String]] = {
    if(!lineIter.hasNext) None
    else {
      val line = lineIter.head
      val prefix = {
        val pref = line.substring(0, line.indexOf("\t"))+"\t"
        if(caseInsensitive) pref.toLowerCase
        else pref
      }
      val (batch, remaining) = lineIter.span(line => if(caseInsensitive) line.toLowerCase.startsWith(prefix) else line.startsWith(prefix))
      lineIter = remaining.buffered
      Some(batch.toSeq)
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
    val tli = new TabLineIterator(io.Source.fromFile("/home/dietz/biocreative/code/biocreative/data/gene/gene_info.sortedbysymbol.filtered3letters"), caseInsensitive = true)
    for(elem <- tli.take(10)) {
      println(elem.mkString("\n"))
      println()
    }
  }
}