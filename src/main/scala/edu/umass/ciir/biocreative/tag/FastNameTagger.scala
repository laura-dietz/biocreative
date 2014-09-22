package edu.umass.ciir.biocreative.tag

import java.io.{File, BufferedReader, InputStreamReader}
import java.lang.ProcessBuilder.Redirect

import edu.umass.ciir.biocreative.tag.MatchType.MatchType


object MatchType extends Enumeration {
  type MatchType = Value
  val Exact, CaseInsensitive = Value
}
case class Match(lower: Int, upper: Int, mention: String, matchType:MatchType)

/**
 * User: dietz
 * Date: 9/21/14
 * Time: 4:06 PM
 */
class FastNameTagger(val dictionaryFile:File) {
  val pb: ProcessBuilder = new ProcessBuilder("./name-tagger", "-w", dictionaryFile.getAbsolutePath)
  pb.redirectInput(Redirect.PIPE)
  pb.redirectOutput(Redirect.PIPE)
  val proc = pb.start()

  def tag(doc: String): Seq[Match] = {
    proc.getOutputStream.write(doc.getBytes("UTF-8"))
    proc.getOutputStream.write("\n".getBytes("UTF-8"))
    proc.getOutputStream.flush()
    val reader = new BufferedReader(new InputStreamReader(proc.getInputStream, "UTF-8"))
    var line: String = null
    var matches: Seq[Match] = Seq()
    while (true) {
      line = reader.readLine()
      if (line == "") {
        return matches
      }
      val parts = line.split('\t')
      assert(parts.length == 4, "response from name-tagger contained less than 4 parts "+parts)
      matches :+= Match(parts(0).toInt, parts(1).toInt, parts(2), if(parts(3).toBoolean)MatchType.Exact else MatchType.CaseInsensitive)
    }
    throw new RuntimeException("This shouldn't happen")
  }

  def close(): Unit = {
    proc.getOutputStream.close()
    proc.getInputStream.close()
    proc.waitFor()
  }
}

object FastNameTagger {
  def main(args:Array[String]): Unit = {
    val tagger = new FastNameTagger(new File("./data/names.txt"))

    tagger.tag("Laura Dietz lives in Massachusetts.")
    tagger.tag("In Amherst is the University of Massachusetts located. Where are you located?")
  }
}
