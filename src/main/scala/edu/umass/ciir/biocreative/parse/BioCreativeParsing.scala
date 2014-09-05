package edu.umass.ciir.biocreative.parse

import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

/**
 * User: dietz
 * Date: 9/5/14
 * Time: 5:31 PM
 */
class BioCreativeParsing {
  def toGalagoText(doc:GalagoBioDocument):java.lang.String = {
    s"""<name> ${doc.nameExtent} </name>
       <ids> ${doc.idExtent} </ids>
       <desc> ${doc.descExtent} </desc>
       <function> ${doc.functionIdExtent} </function>
       <go> ${doc.goExtent} </go>
       <species> ${doc.speciesExtent} </species>
       <entitytype> ${doc.typeExtent} </entitytype>
       |""".stripMargin
  }
  def toGalagoIdentifier(doc:GalagoBioDocument):java.lang.String = {
    doc.identifier
  }
  def toGalagoMeta(doc:GalagoBioDocument):java.util.HashMap[java.lang.String, java.lang.String] = {
    val map = new java.util.HashMap[String,String]()
    map.putAll(doc.meta)
    map
  }
}
