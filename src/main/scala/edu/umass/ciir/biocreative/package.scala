package edu.umass.ciir

import edu.umass.ciir.strepsi.{StringTools, SeqTools}

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

    def serialize0(bioDocument:BioNames):Seq[String] = {
      val id = bioDocument.identifier
      val allNames = bioDocument.names.distinct.map("\"" + _.replaceAllLiterally("\"","") + "\"").mkString(" ")
      val allIds = bioDocument.otherIds.toSeq.flatMap(pair => pair._2.sorted.map(pair._1 -> _)).mkString(" ")
      val allGoTerms = bioDocument.goTerms.mkString(" ")
      val allSpecies = bioDocument.species.distinct.map("\"" + _.replaceAllLiterally("\"","") + "\"").mkString(" ")
      val description = bioDocument.description.mkString(" ").replaceAllLiterally("\n"," ").replaceAllLiterally("\t"," ")

      val serialTabs = Seq(id, allNames, allIds, allGoTerms, allSpecies, description)
      serialTabs
    }

    def deserialize0(serialTabs:Seq[String]):BioNames = {
      val chunks = serialTabs

      val id = chunks(0)
      val names = chunks(2).split("\" \"")
      val otherIds = SeqTools.groupByKey( chunks(3).split("\\) \\(").map(_.split(",")).map(x => x(0) -> x(1))).map(pair => (pair._1 -> pair._2.toSeq))
      val goTerms = chunks(5).split(" ")
      val species = chunks(4).split("\" \"")
      val bioname = BioNames(id, names, otherIds, species, goTerms, Seq(chunks(6)))
      bioname
    }

    def serialize(bioDocument:BioNames):Seq[String] = {
      val id = bioDocument.identifier
      val allNames = bioDocument.names.mkString("  ")
      val allIds = bioDocument.otherIds.toSeq.flatMap(pair => pair._2.sorted.map(pair._1+":"+ _)).mkString("  ")
      val allGoTerms = bioDocument.goTerms.mkString("  ")
      val allSpecies = bioDocument.species.mkString("  ")
      val description = bioDocument.description.mkString("  ")

      val serialTabs = Seq(id, allNames, allIds, allGoTerms, allSpecies, description)
      serialTabs
    }

    def deserialize(serialTabs:Seq[String]):BioNames = {
      val chunks = serialTabs

      val id = chunks(0)
      val names:Seq[String] = if(chunks(2).isEmpty) Seq.empty else chunks(2).split("  ")

      val otherIdChunks:Seq[(String,String)] = if(chunks(3).isEmpty) Seq.empty else chunks(3).split("  ").map(x=>StringTools.splitOnPattern(x,":").getOrElse(throw new Error("can't separate ID schema "+x)))
      val otherIds = SeqTools.mapValues[String, Iterable[String], Seq[String]]( SeqTools.groupByKey(otherIdChunks ), _.toSeq)
      val goTerms:Seq[String] = if(chunks(5).isEmpty) Seq.empty else chunks(5).split("  ")
      val species:Seq[String] = if(chunks(4).isEmpty) Seq.empty else chunks(4).split("  ")
      val descriptions:Seq[String] = if(chunks(6).isEmpty) Seq.empty else chunks(6).split("  ")
      val bioname = BioNames(id, names, otherIds, species, goTerms, descriptions)
      println(bioname)
      bioname
    }
  }


  type Name=String
  type NameId=String
  case class NameMap(forward:Map[Name,NameId]) {
    def reverse:Map[NameId,Name] = forward.map(x => x._2 -> x._1)
    def both:((Map[Name,NameId]), Map[NameId,Name]) = (forward, reverse)
  }


}
