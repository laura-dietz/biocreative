package edu.umass.ciir.biocreative.tag

import edu.umass.ciir.biocreative.NameId
import edu.umass.ciir.biocreative.load.{LoadBioDocument, LoadNameIds}
import edu.umass.ciir.biocreative.scrub.TextScrubber
import edu.umass.ciir.biocreative.tag.BioCreativeAnnotationParser.BioCreativeAnnotatedDocument
import edu.umass.ciir.strepsi.{CountingTable, MainTools}

import scala.reflect.io.Directory

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 9:52 AM
 */
class BioCreativePipeline(tagger:FastNameTagger, doTrain:Boolean) {
  System.setProperty("file.encoding","UTF-8")

  val counting = new CountingTable[String]()
  val entrezMap = LoadBioDocument.loadMap(new java.io.File("./name-tagger.bio/Entrez_Gene_ID.txt.gz.sorted.gz"))
  val (name2id, id2name) = LoadNameIds.loadMap(new java.io.File("./name-tagger.bio/nameDict.cleaned.txt")).both

  val biocreativeAnnotationParser = new BioCreativeAnnotationParser(tagger, doTrain) 
  
  val bioDocumentTagger = new BioDocumentTagger(tagger)
  def geneNameMatchesEntrez(nameId:NameId, entrezId:String):Boolean = {
    entrezMap.get(entrezId) match {
      case None => false
      case Some(bionames) => {
        val bioNamesWithNameIds = bioDocumentTagger.tag(bionames)
        val where = bioNamesWithNameIds.contains(nameId)
        if(where.isDefined) println(s"geneNameMatchesEntrez: $entrezId found $nameId in $where")
        where.isDefined
      }
    }
  }

  def processAllDocuments(dir: Directory): Unit = {
    for (doc <- biocreativeAnnotationParser.processAllDocuments(dir)) {
      if (doTrain) {
        processSingleDocumentTrain(doc)
      } else {
        processSingleDocumentTest(doc)
      }
    }
  }

  
  def processSingleDocumentTrain(doc:BioCreativeAnnotatedDocument)  {
    for(passage <- doc.passages; annotation <- passage.annotations.get) {

      println(doc.documentId + " " + passage.passageOffset + " " + annotation.annotationId)
      val matches = tagger.tag(passage.text)
      val canonicalMatchNames = matches.map(m => id2name.get(m.nameId))


      val foundGeneSymbol = matches.find(m => annotation.goldGeneSymbol.contains(m.mention.toLowerCase))
      val (foundGeneEntrez, missedGeneEntrez) = matches.partition(m => annotation.goldGeneEntrez.exists(gold => geneNameMatchesEntrez(m.nameId, gold)))
      val foundGo = matches.find(m => annotation.goldGo.contains(m.mention.toLowerCase))
      if (foundGeneSymbol.isDefined) counting.add("foundGeneSymbol")
      if (foundGeneEntrez.nonEmpty) counting.add("foundEntrez")
      if (foundGo.isDefined) counting.add("foundGo")
      counting.add("allGene")
      counting.add("allGo")
      counting.add("allEntrez")

      println(s"scrubbed ${TextScrubber.scrubSentencePunctuation(passage.text, virtualSpace = true)}")
      println(s"offset: ${passage.passageOffset} \t ${passage.text} \n Matches: " + matches.mkString(", ") + " \n " +
        "goldMatches Entrez: " + foundGeneEntrez.map("##" + _.mention + "##").mkString(", ") + " -- " +
        missedGeneEntrez.map(_.mention).mkString(", ") + "\n" +
        "canonical match names: " + canonicalMatchNames.mkString(", ") + "\n" +
        "goldMatches Gene: " + annotation.goldGeneSymbol.map(x => if (foundGeneSymbol.isDefined && foundGeneSymbol.get.mention == x) {
        "##" + x + "##"
      } else x).mkString(", ") + " " +
        "Go:" + annotation.goldGo.mkString(","))
    }



    val entrezRecall = 1.0 * counting.getOrElse("foundEntrez", 0) / counting.getOrElse("allEntrez", 0)
    val geneRecall = 1.0 * counting.getOrElse("foundGene", 0) / counting.getOrElse("allGene", 0)
    val goRecall = 1.0 * counting.getOrElse("foundGo", 0) / counting.getOrElse("allGo", 0)
    println(s" entrezRecall=$entrezRecall\tgeneRecall=$geneRecall\tgoRecall=$goRecall")

  }

  def processSingleDocumentTest(document: BioCreativeAnnotatedDocument) = {
    throw new NotImplementedError()
  }


}

object BioCreativePipeline {
  def main(args:Array[String]): Unit = {
    val dictionaryFile = MainTools.strsPlainFromArgs(args, "--dictionary=").headOption.getOrElse(throw new IllegalArgumentException("required flag --dictionary={dictionaryfile}"))
    val articlesDir = MainTools.strsPlainFromArgs(args, "--articles=").headOption.getOrElse(throw new IllegalArgumentException("required flag --articles={dir}"))
    val doTrain = MainTools.strsPlainFromArgs(args, "--train").nonEmpty

    val tagger = new FastNameTagger(new java.io.File(dictionaryFile), wholeWordMatch = true, caseInsensitiveMatch = true, TextScrubber.scrubSentencePunctuation(_,virtualSpace = false))
    val pipe = new BioCreativePipeline(tagger, doTrain)
    pipe.processAllDocuments(Directory(articlesDir))
  }
}