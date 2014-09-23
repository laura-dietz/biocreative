package edu.umass.ciir.biocreative.parse

/**
 * User: dietz
 * Date: 9/5/14
 * Time: 5:46 PM
 */
case class GalagoBioDocument(identifier: String, meta: Map[String, String], nameExtent: String, idExtent: String, descExtent: String, goExtent: String, speciesExtent:String, functionIdExtent:String, typeExtent:String)

case class BioNames(identifier:String, names:Seq[String], otherIds:Map[String,Seq[String]], species:Seq[String], goTerms:Seq[String], description:Seq[String])

object GalagoBioDocument {
  def empty = GalagoBioDocument("",Map.empty, "","","","","","","")
}

