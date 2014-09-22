package edu.umass.ciir.biocreative.tag

import java.io.{File, BufferedReader, InputStreamReader}
import java.lang.ProcessBuilder.Redirect


case class Match(lower: Int, upper: Int, mention: String)

/**
 * User: dietz
 * Date: 9/21/14
 * Time: 4:06 PM
 */
class BioCreativeRustTagging(val dictionaryFile:File) {
  val pb: ProcessBuilder = new ProcessBuilder("./name-tagger", dictionaryFile.getAbsolutePath)
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
      assert(parts.length == 3)
      matches :+= Match(parts(0).toInt, parts(1).toInt, parts(2))
    }
    throw new RuntimeException("This shouldn't happen")
  }

  def close(): Unit = {
    proc.getOutputStream.close()
    proc.getInputStream.close()
    proc.waitFor()
  }
}

object BioCreativeRustTagging {
  def main(args:Array[String]): Unit = {
    val tagger = new BioCreativeRustTagging(new File("./data/names.txt"))

    tagger.tag("Laura Dietz lives in Massachusetts.")
    tagger.tag("In Amherst is the University of Massachusetts located. Where are you located?")
  }
}
