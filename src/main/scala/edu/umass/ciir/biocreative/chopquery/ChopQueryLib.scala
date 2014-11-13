package edu.umass.ciir.biocreative.chopquery

import edu.umass.ciir.strepsi.StringTools
import edu.umass.ciir.strepsimur.galago.GalagoSearcher

/**
 * User: dietz
 * Date: 11/13/14
 * Time: 10:45 AM
 */
object ChopQueryLib {
  def queryGo(goNumber:String): String = {
    s"#combine:w=1.0( #dirichlet:lengths=go( #lengths:go:part=lengths() #counts:@/$goNumber/:part=field.go() ) )"
  }

  def querySingleTermInFieldWrapped(queryTerm:String, field:String): String = {
    s"#combine:w=1.0( ${querySingleTermInField(queryTerm, field)} )"
  }

  def querySingleTermInField(queryTerm:String, field:String): String = {
    s"#dirichlet:lengths=$field( #lengths:$field:part=lengths() #counts:@/$queryTerm/:part=field.$field() ) "
  }

  def sdmWithField(queryTerms:Seq[String], field:String):String = {
    val mid = queryTerms.map(_+"."+field).mkString(" ")
    s"#sdm ( $mid )"
  }

  /**
   *
   * @param queryTerms sequence of query terms, assumed to be tokenized with the collection tokenizer
   * @param field  field extends, all lower case!
   * @param windowType "unordered" or "ordered"
   * @param windowLen 1 for "no gaps", 8 for sdm windows.
   * @return
   */
  def windowWithFieldNorm(queryTerms:Seq[String], field:String, windowType:String, windowLen:Int):String = {
    val mid = queryTerms.map(q => s"#extents:@/$q/:part=field.$field()").mkString(" ")
    s"#combine:w=1.0( #dirichlet:lengths=$field( #lengths:$field:part=lengths() #$windowType:$windowLen( $mid )) )"
  }

  def sdmWithFieldNorm(queryTerms:Seq[String], field:String) : String = {
    val unigram =  queryTerms.map(q => querySingleTermInField(q, field)).mkString("#combine(", " ", " )")
    val bigram =  queryTerms.sliding(2).map( qs => windowWithFieldNorm(qs, field, "ordered", 1)).mkString("#combine(", " ", " )")
    val skipgram = queryTerms.sliding(2).map( qs => windowWithFieldNorm(qs, field, "unordered", 8)).mkString("#combine(", " ", " )")
    s"#combine:1=0.8:2=0.15:3=0.05( $unigram $bigram $skipgram )"
  }


  def main(args:Array[String]): Unit = {
    val g = GalagoSearcher("/home/dietz/biocreative/index/biochop")
    //#sdm ( intracellular.desc protein.desc kinease.desc cascade.desc mapk.desc mapkk.desc map3k.desc kinease.desc activated.desc mapkapk.desc phosphorylate.desc signal.desc cell.desc) {

    {
      val queryTokens = StringTools.getSepSplits("intracellular protein kinease cascade mapk mapkk map3k kinease activated mapkapk phosphorylate signal cell", sep = " ").toSeq
      println("queryTokens = "+queryTokens.mkString(", "))


      val queryDesc = ChopQueryLib.sdmWithFieldNorm(queryTokens, "desc")

      println("query = "+queryDesc)

      val descScoredDocs = g.retrieveScoredDocuments(query = queryDesc, resultCount = 20)
      val descDocs = g.fetchDocuments(descScoredDocs)
      println("==================\n desc \n ===============")
      for (fsd <- descDocs) {
        println("\n\n" + fsd.scored.rank + "\t" + fsd.scored.documentName + "\n" + fsd.doc.text.take(250) + "\n")
      }
    }

    println("\n\n\n")

    {
      val queryName = ChopQueryLib.sdmWithFieldNorm("intracellular protein kinease cascade mapk mapkk map3k kinease activated mapkapk phosphorylate signal cell".split(" "), "n")
      val nameScoredDocs = g.retrieveScoredDocuments(query = queryName, resultCount = 20)
      val nameDocs = g.fetchDocuments(nameScoredDocs)
      println("==================\n name \n ===============")
      for (fsd <- nameDocs) {
        println("\n\n" + fsd.scored.rank + "\t" + fsd.scored.documentName + "\n" + fsd.doc.text.take(250) + "\n")
      }
    }



  }
}
