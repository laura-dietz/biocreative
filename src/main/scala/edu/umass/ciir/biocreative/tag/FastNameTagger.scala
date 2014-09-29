package edu.umass.ciir.biocreative.tag

import java.io.{File, BufferedReader, InputStreamReader}
import java.lang.ProcessBuilder.Redirect

import edu.umass.ciir.biocreative.scrub.TextScrubber
import edu.umass.ciir.biocreative.tag.MatchType.MatchType

import scala.collection.mutable.ListBuffer

object MatchType extends Enumeration {
  type MatchType = Value
  val Exact, CaseInsensitive = Value
}
case class Match(lower: Int, upper: Int, mention: String, matchType:MatchType, nameId:String) {
  def correctOffset(shift:Int=0, max:Int = Integer.MAX_VALUE, min:Int = Integer.MIN_VALUE):Match = {
    def minmax(i:Int) =if(i<min) min else if (i>max) max else i
    Match(minmax(lower+shift),minmax(upper+shift), mention, matchType, nameId)
  }
}

import scala.collection.JavaConversions._

/**
 * User: dietz
 * Date: 9/21/14
 * Time: 4:06 PM
 */
class FastNameTagger(val dictionaryFile:File, wholeWordMatch:Boolean, caseInsensitiveMatch:Boolean, textScrubber:(String)=>String) {
  val flags = new ListBuffer[String]()
  if(wholeWordMatch) flags += "-w"
  if(caseInsensitiveMatch) flags += "-i"
  private val command = List("./name-tagger") ++ flags ++ List(dictionaryFile.getAbsolutePath)
  val pb: ProcessBuilder = new ProcessBuilder(command)
  pb.redirectInput(Redirect.PIPE)
  pb.redirectOutput(Redirect.PIPE)
  pb.redirectError(Redirect.INHERIT)
  val proc = pb.start()

  def tag(doc: String): Seq[Match] = {
    val scrubDoc = " " + textScrubber(doc) + " "
    internalTag(scrubDoc).map(_.correctOffset(shift = -1, min = 0, max = doc.length))
  }
  protected def internalTag(doc:String): Seq[Match] = {
    proc.getOutputStream.write(doc.getBytes("UTF-8"))
    proc.getOutputStream.write("\n".getBytes("UTF-8"))
    proc.getOutputStream.flush()
    val reader = new BufferedReader(new InputStreamReader(proc.getInputStream, "UTF-8"))
    var line: String = null
    var matches: Seq[Match] = Seq()
    while (true) {
      line = reader.readLine()
      if (line == null || line == "") {
        return matches
      }
      //if(line == null || line.length ==0) System.err.println("Got empty response from name-tagger.")
      //else {
        val parts = line.split('\t')
        assert(parts.length == 6, "response from name-tagger does not contain exactly 6 parts " + parts.mkString(", "))
        matches :+= Match(parts(0).toInt, parts(1).toInt, parts(2), if (parts(3).toBoolean) MatchType.Exact else MatchType.CaseInsensitive, parts(5))
      //}
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
    val tagger = new FastNameTagger(new File("./data/names.txt"), wholeWordMatch = true, caseInsensitiveMatch = false, TextScrubber.scrubSentencePunctuation(_, virtualSpace = false))

    tagger.tag("Laura Dietz lives in Massachusetts.")
    tagger.tag("In Amherst is the University of Massachusetts located. Where are you located?")
  }
}
