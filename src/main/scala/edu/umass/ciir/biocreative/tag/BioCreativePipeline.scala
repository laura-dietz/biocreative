package edu.umass.ciir.biocreative.tag

import edu.umass.ciir.biocreative
import edu.umass.ciir.biocreative.load.{LoadBioDocument, LoadNameIds}
import edu.umass.ciir.biocreative.scrub.TextScrubber
import edu.umass.ciir.biocreative.tag.BioCreativeAnnotationParser.BioCreativeAnnotatedDocument
import edu.umass.ciir.biocreative.{BioNamesWithNameIds, NameId}
import edu.umass.ciir.strepsi.{CountingTable, MainTools}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.io.Directory

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 9:52 AM
 */
class BioCreativePipeline(tagger:FastNameTagger, doTrain:Boolean, entrezMapFile:String) {
  System.setProperty("file.encoding","UTF-8")

  val counting = new CountingTable[String]()
  val entrezMap = LoadBioDocument.loadMap(new java.io.File(entrezMapFile))
//  val entrezMap = LoadBioDocument.loadMap(new java.io.File("./name-tagger.bio/Entrez_Gene_ID.txt.gz.sorted.gz"))
//  val goTermMap = LoadBioDocument.loadMap(new java.io.File("./name-tagger.bio/GO_ID.txt.gz.sorted.gz"))
  val (name2id, id2name) = LoadNameIds.loadMap(new java.io.File(tagger.dictionaryFile.getAbsolutePath)).both

  val anyname2EntrezName = {
    val map = new collection.mutable.HashMap[biocreative.Name, ListBuffer[biocreative.Name]]()
    for((entrezId, bionames) <- entrezMap; name <- bionames.names) {
      val list = map.getOrElseUpdate(name, new ListBuffer[biocreative.Name]())
      list += entrezId
    }
    map.result()
  }

  val biocreativeAnnotationParser = new BioCreativeAnnotationParser(tagger, doTrain) 
  
  val bioDocumentTagger = new BioDocumentTagger(tagger)
  def entrezEntry(entrezId:String):Option[BioNamesWithNameIds] = {
    entrezMap.get(entrezId).map(bioNames => bioDocumentTagger.tag(bioNames))
  }
//  def goTermEntry(goTerm:String):Option[BioNamesWithNameIds] = {
//    goTermMap.get(goTerm).map(bioNames => bioDocumentTagger.tag(bioNames))
//  }
  def existsInEntrez(nameId:NameId, entrezId:String):Boolean = {
    entrezEntry(entrezId) match {
      case None => false
      case Some(bioNamesWithNameIds) => {
        val where = bioNamesWithNameIds.contains(nameId)
//        if(where.isDefined) println(s"geneNameMatchesEntrez: $entrezId found $nameId in $where")
        where.isDefined
      }
    }
  }

  def countInEntry(nameId:NameId, entry:Option[BioNamesWithNameIds]):Double = {
    entry match {
      case None => 0.0
      case Some(bioNamesWithNameIds) => {
        val namesCount = bioNamesWithNameIds.namesNamesId.count(_ == nameId)
        val speciesCount = bioNamesWithNameIds.speciesNamesIds.count(_ == nameId)
        val goTermCount = bioNamesWithNameIds.goTerms.count(_ == nameId)
        val descCount = bioNamesWithNameIds.description.count(_ == nameId)
       
        namesCount * 10.0 + speciesCount + goTermCount + descCount
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
    val docCounting = new CountingTable[String]
    println("======================================")
    println("========"+doc.documentId+"===========")
    
    for(passage <- doc.passages) {
      val matches = tagger.tag(passage.text)
      val canonicalMatchNames = matches.map(m => id2name.get(m.nameId))

      val goldGeneEntrez = passage.annotations.get.flatMap(_.goldGeneEntrez).distinct
      val entrezAvgScore = matches.map(m => goldGeneEntrez.map(g => countInEntry(m.nameId, entrezEntry(g))).sum).sum / (matches.length*goldGeneEntrez.size)
      val (foundGeneEntrez, missedGeneEntrez) = matches.partition(m => goldGeneEntrez.exists(gold => existsInEntrez(m.nameId, gold)))

      val goldGeneSymbol = passage.annotations.get.flatMap(_.goldGeneSymbol).distinct
      val geneSymbolAvgScore = 1.0 * matches.map(m => goldGeneSymbol.count(gold =>{
        gold.toLowerCase.contains(m.mention.toLowerCase) || gold.toLowerCase.contains(id2name(m.nameId).toLowerCase)
      })).sum  / (matches.length*goldGeneSymbol.size)
      val (foundGeneSymbol, missedGeneSymbol) = matches.partition(m => goldGeneSymbol.exists(gold => {
        gold.toLowerCase.contains(m.mention.toLowerCase) || gold.toLowerCase.contains(id2name(m.nameId).toLowerCase)
      }))




//      val goldGoTerms = passage.annotations.get.flatMap(_.goldGoTerm).distinct
//      val goTermAvgScore = matches.map(m => goldGoTerms.map(g => countInEntry(m.nameId, goTermEntry(g))).sum).sum / (matches.length*goldGoTerms.size)


      println("----"+doc.documentId+" offset:"+passage.passageOffset+" -----")
      println(passage.text)
      println(matches.map(_.mention))
      println(canonicalMatchNames)
      println("-- goldGeneSymbol = "+goldGeneSymbol)
      println(s"foundGeneSymbol (${foundGeneSymbol.size}) = "+foundGeneSymbol.map(m => m.mention+"("+m.nameId+")"))
      println(s"missedGeneSymbol (${missedGeneSymbol.size})) = "+missedGeneSymbol.map(m => m.mention+"("+m.nameId+")"))
      println(s"geneSymbolAvgScore = $geneSymbolAvgScore")
      println("-- goldGeneEntrez = "+goldGeneEntrez)
      println(s"foundGeneEntrez (${foundGeneEntrez.size}) = "+foundGeneEntrez.map(m => m.mention+"("+m.nameId+")"))
      println(s"missedGeneEntrez (${missedGeneEntrez.size})) = "+missedGeneEntrez.map(m => m.mention+"("+m.nameId+")"))
      println(s"entrezAvgScore = $entrezAvgScore")
//      println("-- goldGoTerms = "+goldGoTerms)
//      println(s"goTermAvgScore = $goTermAvgScore")


//      val foundGoTerm = matches.find(m => goldGoTerms.contains(m.mention.toLowerCase))
      if (foundGeneSymbol.nonEmpty) counting.add("foundGeneSymbol")
      if (foundGeneEntrez.nonEmpty) counting.add("foundEntrez")
//      if (foundGoTerm.isDefined) counting.add("foundGoTerm")
      counting.add("allGeneSymbol")
      counting.add("allGoTerm")
      counting.add("allEntrez")

      if (foundGeneSymbol.nonEmpty) docCounting.add("foundGeneSymbol")
      if (foundGeneEntrez.nonEmpty) docCounting.add("foundEntrez")
      docCounting.add("allGeneSymbol")
      docCounting.add("allEntrez")


      val entrezMatches = new mutable.HashMap[biocreative.Name, ListBuffer[Match]]
      for(m <- matches) {
        id2name.get(m.nameId) match {
          case Some(nameId) => anyname2EntrezName.get(nameId) match {
            case Some(entrezNames) => {
              println("linked match "+m+" to "+entrezNames.distinct.length+" entrez entries "+entrezNames)
              for(name <- entrezNames) {
                entrezMatches.getOrElseUpdate(name, new ListBuffer[Match]()) += m
              }
            }
            case None => {}
          }
          case None => {}
        }
      }
      val sortedMatches = entrezMatches.toList.sortBy(- _._2.length)
      println(s"entrezMatch ranking for this paragraph: "+sortedMatches.take(20).map(pair => "match? "+goldGeneEntrez.contains(pair._1)+"\t\t"+ pair._1 +"\t\t"+pair._2).mkString("\n","\n","\n"))

      for(goldEntrez <- goldGeneEntrez) {
        val foundIdx = sortedMatches.map(_._1).indexOf(goldEntrez)
        if(foundIdx > -1) {
          if (foundIdx < 1 ) counting.add("entrezAt1")
          if (foundIdx < 5 ) counting.add("entrezAt5")
          if (foundIdx < 10 ) counting.add("entrezAt10")
          if (foundIdx < 20 ) counting.add("entrezAt20")
          if (foundIdx < 50 ) counting.add("entrezAt50")
          if (foundIdx < 100 ) counting.add("entrezAt100")
          counting.add("entrezAtSomewhere")
        }
        counting.add("entrezAtNorm")

      }
    }



    val entrezRecall = 1.0 * counting.getOrElse("foundEntrez", 0) / counting.getOrElse("allEntrez", 0)
    val geneRecall = 1.0 * counting.getOrElse("foundGeneSymbol", 0) / counting.getOrElse("allGeneSymbol", 0)
    val goRecall = 1.0 * counting.getOrElse("foundGoTerm", 0) / counting.getOrElse("allGoTerm", 0)
    val docEntrezRecall = 1.0 * docCounting.getOrElse("foundEntrez", 0) / docCounting.getOrElse("allEntrez", 0)
    val docGeneRecall = 1.0 * docCounting.getOrElse("foundGeneSymbol", 0) / docCounting.getOrElse("allGeneSymbol", 0)
    println(s" entrezRecall=$entrezRecall\tgeneSymbolRecall=$geneRecall\tgoRecall=$goRecall")
    println(s" DOC entrezRecall=$docEntrezRecall\tgeneSymbolRecall=$docGeneRecall")

    println(" EntrezPrecAtCount: "+Seq(1,5,10,20,50,100).map(level => level -> counting.getOrElse("entrezAt"+level,0)).mkString("  "))
    println(" EntrezPrecAtAvg  : "+Seq(1,5,10,20,50,100).map(level => level -> 1.0 * counting.getOrElse("entrezAt"+level,0)/counting.getOrElse("entrezAtNorm", 1)).mkString("  "))
  }

  def processSingleDocumentTest(document: BioCreativeAnnotatedDocument) = {
    throw new NotImplementedError()
  }


}

object BioCreativePipeline {
  def main(args:Array[String]): Unit = {
    val dictionaryFile = MainTools.strsPlainFromArgs(args, "--dictionary=").headOption.getOrElse(throw new IllegalArgumentException("required flag --dictionary={dictionaryfile}"))
    val articlesDir = MainTools.strsPlainFromArgs(args, "--articles=").headOption.getOrElse(throw new IllegalArgumentException("required flag --articles={dir}"))
    val entrezMapFile = MainTools.strsPlainFromArgs(args, "--entrezMapFile=").headOption.getOrElse(throw new IllegalArgumentException("required flag --entrezMapFile={file}"))
    val doTrain = MainTools.strsPlainFromArgs(args, "--train").nonEmpty

    val tagger = new FastNameTagger(new java.io.File(dictionaryFile), wholeWordMatch = true, caseInsensitiveMatch = true, TextScrubber.scrubSentencePunctuation(_,virtualSpace = false))
    val pipe = new BioCreativePipeline(tagger, doTrain, entrezMapFile = entrezMapFile)
    pipe.processAllDocuments(Directory(articlesDir))
  }
}