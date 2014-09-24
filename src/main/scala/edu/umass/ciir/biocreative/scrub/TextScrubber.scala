package edu.umass.ciir.biocreative.scrub

/**
 * User: dietz
 * Date: 9/23/14
 * Time: 3:04 PM
 */
object TextScrubber {
  // replace sentence punctuation with spaces -- leaving the character offsets intact
  def scrubSentencePunctuation(text:String, virtualSpace:Boolean):String = {
      if (virtualSpace) addVirtualSpace(text, scrubSentencePunct)
      else scrubSentencePunct(text)

  }

  private def scrubSentencePunct(text:String):String = {
    val result =
      text
      .replaceAllLiterally("\n"," ")
      .replaceAllLiterally("\t"," ")
      .replaceAllLiterally("\""," ")
      .replaceAllLiterally(". ","  ")
      .replaceAllLiterally("; ","  ")
      .replaceAllLiterally(", ","  ")
      .replaceAllLiterally("? ","  ")
      .replaceAllLiterally("! ","  ")
      .replaceAllLiterally(", ","  ")
      .replaceAllLiterally(" - ","   ")
      .replaceAllLiterally(" -- ","    ")
      .replaceAllLiterally(" --- ","     ")
      .replaceAll("\\s\\(([^\\)]+)\\)\\s","  $1  ")  // replace parenthesis if they are on word boundaries

    assert(result.length == text.length, "text lengths do not match:\n"+text+"\n"+result)
    result
  }

  def addVirtualSpace(text:String, textMod:(String)=>String):String = {
    try{
      textMod(" " + text + " ").substring(1, 1+text.length)
    } catch {
      case ex: StringIndexOutOfBoundsException => {
        System.err.println(text)
        throw ex
      }

    }
  }

  def main(args:Array[String]): Unit = {
    println(scrubSentencePunctuation("Laura(2) needs (sometimes to go) home.! ((Where?))", virtualSpace = true))
  }
}
