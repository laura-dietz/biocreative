package edu.umass.ciir.biocreative.parse

/**
 * User: dietz
 * Date: 9/5/14
 * Time: 5:46 PM
 */
case class GalagoBioDocument(identifier: String, meta: Map[String, String], nameExtent: String, idExtent: String, descExtent: String, goExtent: String, speciesExtent:String, functionIdExtent:String, typeExtent:String)

object GalagoBioDocument {
  def empty = GalagoBioDocument("",Map.empty, "","","","","","","")
}


