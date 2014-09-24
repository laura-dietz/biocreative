package edu.umass.ciir

import edu.umass.ciir.strepsi.SeqTools

/**
 * User: dietz
 * Date: 9/24/14
 * Time: 12:11 PM
 */
package object biocreative {
  case class BioNames(identifier:String, names:Seq[String], otherIds:Map[String,Seq[String]], species:Seq[String], goTerms:Seq[String], description:Seq[String])

  case class BioNamesWithNameIds(bionames:BioNames, namesNamesId:Seq[NameId], speciesNamesIds:Seq[NameId], goTermsNamesIds:Seq[NameId], descriptionNameIds:Seq[NameId]) {
    def identifier = bionames.identifier

    def names = bionames.names

    def otherIds = bionames.otherIds

    def species = bionames.species

    def goTerms = bionames.goTerms

    def description = bionames.description
    
    def contains(nameId:NameId):Option[String] = {
      if (namesNamesId.contains(nameId)) Some("names")
      else if (speciesNamesIds.contains(nameId)) Some("species") 
      else if (speciesNamesIds.contains(nameId)) Some("goTerms") 
      else if (speciesNamesIds.contains(nameId)) Some("description")
      else None
    }
  }


  object BioNames {
    def fromSeq(bioNameBuffer:Seq[BioNames]):BioNames = {
      val id = bioNameBuffer.head.identifier
      assert(bioNameBuffer.forall(_.identifier == id), "bionames from different ids. should be "+id+" but was "+bioNameBuffer)
      val names = bioNameBuffer.flatMap(_.names).distinct
      val otherIds = SeqTools.mergeMaps(bioNameBuffer.map(_.otherIds))
      val species = bioNameBuffer.flatMap(_.species).distinct
      val goTerms = bioNameBuffer.flatMap(_.goTerms).distinct
      val description = bioNameBuffer.map(_.description).mkString(" ")

      BioNames(id, names, otherIds, species, goTerms, Seq(description))

    }
  }


  type Name=String
  type NameId=String
  case class NameMap(forward:Map[Name,NameId]) {
    def reverse:Map[NameId,Name] = forward.map(x => x._2 -> x._1)
    def both:((Map[Name,NameId]), Map[NameId,Name]) = (forward, reverse)
  }


}
