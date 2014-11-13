package edu.umass.ciir.biocreative.chop

import edu.umass.ciir.biocreative.chopquery.ChopQueryLib
import edu.umass.ciir.biocreative.scrub.TextScrubber
import edu.umass.ciir.biocreative.tag.BioCreativeAnnotationParser.BioCreativeAnnotatedDocument
import edu.umass.ciir.biocreative.tag._
import edu.umass.ciir.biocreative.{BioNamesWithNameIds, NameId}
import edu.umass.ciir.strepsi.{SeqTools, CountingTable, MainTools, StopWordList}
import edu.umass.ciir.strepsimur.galago.{FetchedScoredDocument, GalagoSearcher}
import org.lemurproject.galago.core.parse.TagTokenizer
import org.lemurproject.galago.utility.Parameters

import scala.collection.JavaConversions._
import scala.reflect.io.Directory

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 9:52 AM
 */
class BioChopPipeline(tagger:FastNameTagger, doTrain:Boolean, galagoChopIndex:String) {
  System.setProperty("file.encoding","UTF-8")

  val galago = GalagoSearcher(galagoChopIndex)
  val tokenizer = {
//    new TagTokenizer()

    val t = new TagTokenizer()
    t.addField("n")
    t.addField("geneid")
    t.addField("genbank_id")
    t.addField("entrez_gene_id")
    t.addField("Entrez_Gene_ID")
    t.addField("desc")
    t.addField("go")
    t.addField("spec")
    t.addField("bio")
    t

//    val params = Parameters.instance()
//    val tokenizerParam = Parameters.instance()
//    tokenizerParam.set("fields", List("n" , "geneid", "genbank_id", "entrez_gene_id", "desc", "go", "spec","bio"))
//    params.set("tokenizer", tokenizerParam)
//    new TagTokenizer(new FakeParameters(params))
  }

  val pullParams = {
    val p = new Parameters()
    p.set("tags", true)
    p.set("tag", true)
    p.set("terms", true)
    p
  }


  val counting = new CountingTable[String]()
//  val entrezMap = LoadBioDocument.loadMap(new java.io.File("./name-tagger.bio/Entrez_Gene_ID.txt.gz.sorted.gz"))
//  val goTermMap = LoadBioDocument.loadMap(new java.io.File("./name-tagger.bio/GO_ID.txt.gz.sorted.gz"))
//  val (name2id, id2name) = LoadNameIds.loadMap(new java.io.File(tagger.dictionaryFile.getAbsolutePath)).both
//  val entrezMap = LoadBioDocument.loadMap(new java.io.File(entrezMapFile))
//
//  val anyname2EntrezName = {
//    val map = new collection.mutable.HashMap[biocreative.Name, ListBuffer[biocreative.Name]]()
//    for((entrezId, bionames) <- entrezMap; name <- bionames.names) {
//      val list = map.getOrElseUpdate(name, new ListBuffer[biocreative.Name]())
//      list += entrezId
//    }
//    map.result()
//  }

  val biocreativeAnnotationParser = new BioCreativeAnnotationParser(tagger, doTrain) 
  
  val bioDocumentTagger = new BioDocumentTagger(tagger)


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

  def queryAndTokenize(query:String, numResults:Int):Seq[FetchedScoredDocument] = {
    val sd = galago.retrieveScoredDocuments(query, resultCount = numResults)
    val docs = galago.fetchDocuments(sd)
    val result =
      for ( FetchedScoredDocument(scored, doc) <- docs) yield {
        tokenizer.tokenize(doc)
        if(doc.tags != null && !doc.tags.isEmpty) {
          Some(FetchedScoredDocument(scored, doc))
        }  else None
      }
    result.flatten
  }
  
  def entrezFromFetchScoredDocuments(docs:Seq[FetchedScoredDocument]):Seq[(String,Double)] = {
    val result =
    for ( FetchedScoredDocument(scored, doc) <- docs) yield {
      if(doc.tags != null && !doc.tags.isEmpty) {
        val fulltext = doc.text
        val entrezTags = doc.tags.filter(tag => "entrez_gene_id".equals(tag.name))
        val entrezIds =
          entrezTags.map(tag => fulltext.substring(tag.charBegin, tag.charEnd))
        entrezIds.map(id => Pair(id, scored.score))
      }  else Seq.empty
    }
    SeqTools.distinctBy[(String,Double),String](result.flatten, _._1)
  }
  def geneSymbolFromFetchScoredDocuments(docs:Seq[FetchedScoredDocument]):Seq[(String,Double)] = {
    val result =
    for ( FetchedScoredDocument(scored, doc) <- docs) yield {
      if(doc.tags != null && !doc.tags.isEmpty) {
        val fulltext = doc.text
        val geneSymbolTags = doc.tags.find(tag => "n".equals(tag.name))
        val geneSymbol =
          geneSymbolTags.map(tag => fulltext.substring(tag.charBegin, tag.charEnd))
        geneSymbol.map(id => Pair(id, scored.score))
      }  else None
    }
    SeqTools.distinctBy[(String,Double),String](result.flatten, _._1)
  }
  
  def processSingleDocumentTrain(doc:BioCreativeAnnotatedDocument)  {
    val docCounting = new CountingTable[String]
    println("======================================")
    println("========"+doc.documentId+"===========")
    
    for(passage <- doc.passages) {
      val matches = tagger.tag(passage.text)
      if (matches.nonEmpty) {
        val canonicalMatchNames = matches.map(m => m.nameId)

        val canonicalTokenizedMatches = canonicalMatchNames.map(str => tokenizer.tokenize(str).terms)
        val namesForQuery = canonicalTokenizedMatches.flatten.distinct.filterNot(StopWordList.isStopWord).filter(_.length>2).map(_.toLowerCase)
        val passageTerms = tokenizer.tokenize(passage.text).terms.filterNot(StopWordList.isStopWord).filter(_.length > 2).map(_.toLowerCase)
        if(namesForQuery.nonEmpty ){
          val fsdByName = queryAndTokenize(ChopQueryLib.sdmWithFieldNorm(namesForQuery, "n"), 100)
          val entrezByName = entrezFromFetchScoredDocuments( fsdByName )
          val genesymbolByName = geneSymbolFromFetchScoredDocuments( fsdByName ) 

          val fsdByDesc = queryAndTokenize(ChopQueryLib.sdmWithFieldNorm(passageTerms, "desc"), 100)
          val entrezByDesc = entrezFromFetchScoredDocuments( fsdByDesc )
          val genesymbolByDesc = geneSymbolFromFetchScoredDocuments( fsdByDesc ) 

//        val entrezByDesc = queryEntrez(ChopQueryLib.sdmWithFieldNorm(passageTerms, "desc"), 100)


          val goldGeneEntrez = passage.annotations.get.flatMap(_.goldGeneEntrez).distinct
          val goldGeneSymbol = passage.annotations.get.flatMap(_.goldGeneSymbol).distinct
          val goldGeneSymbolLower = goldGeneSymbol.map(_.toLowerCase)


          val (foundGeneEntrezIrName, missedGeneEntrezIrName) = entrezByName.partition(id => goldGeneEntrez.contains(id))
          val (foundGeneSymbolIrName, missedGeneSymbolIrName) = genesymbolByName.map(p => (p._1.toLowerCase, p._2)).partition(id => goldGeneSymbolLower.contains(id))
          
          val (foundGeneEntrezIrDesc, missedGeneEntrezIrDesc) = entrezByDesc.partition(id => goldGeneEntrez.contains(id))
          val (foundGeneSymbolIrDesc, missedGeneSymbolIrDesc) = genesymbolByDesc.map(p => (p._1.toLowerCase, p._2)).partition(id => goldGeneSymbolLower.contains(id))
          
//          val (foundGeneEntrezIrDesc, missedGeneEntrezIrDesc) = entrezByDesc.partition(id => goldGeneEntrez.contains(id))

          /*
                val entrezAvgScore = matches.map(m => goldGeneEntrez.map(g => countInEntry(m.nameId, entrezEntry(g))).sum).sum / (matches.length*goldGeneEntrez.size)
                val (foundGeneEntrez, missedGeneEntrez) = matches.partition(m => goldGeneEntrez.exists(gold => existsInEntrez(m.nameId, gold)))

                val geneSymbolAvgScore = 1.0 * matches.map(m => goldGeneSymbol.count(gold =>{
                  gold.toLowerCase.contains(m.mention.toLowerCase) || gold.toLowerCase.contains(id2name(m.nameId).toLowerCase)
                })).sum  / (matches.length*goldGeneSymbol.size)
                val (foundGeneSymbol, missedGeneSymbol) = matches.partition(m => goldGeneSymbol.exists(gold => {
                  gold.toLowerCase.contains(m.mention.toLowerCase) || gold.toLowerCase.contains(id2name(m.nameId).toLowerCase)
                }))

          */



          println("----" + doc.documentId + " offset:" + passage.passageOffset + " -----")
          println(passage.text)
          println(matches.map(_.mention))
          println(canonicalMatchNames)
          println("-- goldGeneSymbol = " + goldGeneSymbol)
          println("-- goldGeneEntrez = " + goldGeneEntrez)
          println(s"foundGeneEntrezIrName (${foundGeneEntrezIrName.size}) = " + foundGeneEntrezIrName)
          println(s"missedGeneEntrezIrName (${missedGeneEntrezIrName.size})) = " + missedGeneEntrezIrName)
          println(s"foundGeneSymbolIrName (${foundGeneSymbolIrName.size}) = " + foundGeneSymbolIrName)
          println(s"missedGeneSymbolIrName (${missedGeneSymbolIrName.size})) = " + missedGeneSymbolIrName)
          println(s"foundGeneEntrezIrDesc (${foundGeneEntrezIrDesc.size}) = " + foundGeneEntrezIrDesc)
          println(s"missedGeneEntrezIrDesc (${missedGeneEntrezIrDesc.size})) = " + missedGeneEntrezIrDesc)
          println(s"foundGeneSymbolIrDesc (${foundGeneSymbolIrDesc.size}) = " + foundGeneSymbolIrDesc)
          println(s"missedGeneSymbolIrDesc (${missedGeneSymbolIrDesc.size})) = " + missedGeneSymbolIrDesc)


          if (foundGeneEntrezIrName.nonEmpty) counting.add("foundEntrezIrName")
          if (foundGeneSymbolIrName.nonEmpty) counting.add("foundSymbolIrName")
          if (foundGeneEntrezIrDesc.nonEmpty) counting.add("foundEntrezIrDesc")
          if (foundGeneSymbolIrDesc.nonEmpty) counting.add("foundSymbolIrDesc")
          counting.add("allGeneSymbol")
          counting.add("allGoTerm")
          counting.add("allEntrez")

          if (foundGeneEntrezIrName.nonEmpty) docCounting.add("foundEntrezIrName")
          if (foundGeneSymbolIrName.nonEmpty) docCounting.add("foundSymbolIrName")
          if (foundGeneEntrezIrDesc.nonEmpty) docCounting.add("foundEntrezIrDesc")
          if (foundGeneSymbolIrDesc.nonEmpty) docCounting.add("foundSymbolIrDesc")
          docCounting.add("allGeneSymbol")
          docCounting.add("allEntrez")


          val entrezByNameSortedMatches = entrezByName.toList.sortBy(-_._2)
          println(s"entrezMatch ByName ranking for this paragraph: " + entrezByNameSortedMatches.take(20).map(pair => "match? " + goldGeneEntrez.contains(pair._1) + "\t\t" + pair._1 + "\t\t" + pair._2).mkString("\n", "\n", "\n"))
          for (goldEntrez <- goldGeneEntrez) {
            val foundIdx = entrezByNameSortedMatches.map(_._1).indexOf(goldEntrez)
            if (foundIdx > -1) {
              if (foundIdx < 1) counting.add("entrezAt1")
              if (foundIdx < 5) counting.add("entrezAt5")
              if (foundIdx < 10) counting.add("entrezAt10")
              if (foundIdx < 20) counting.add("entrezAt20")
              if (foundIdx < 50) counting.add("entrezAt50")
              if (foundIdx < 100) counting.add("entrezAt100")
              counting.add("entrezAtSomewhere")
            }
            counting.add("entrezAtNorm")

          }


          val genesymbolByNameSortedMatches = genesymbolByName.toList.sortBy(-_._2)
          println(s"genesymbolMatch ByName ranking for this paragraph: " + genesymbolByNameSortedMatches.take(20).map(pair => "match? " + goldGeneEntrez.contains(pair._1) + "\t\t" + pair._1 + "\t\t" + pair._2).mkString("\n", "\n", "\n"))
          for (goldSymbol <- goldGeneSymbolLower) {
            val foundIdx = genesymbolByNameSortedMatches.map(_._1.toLowerCase).indexOf(goldGeneSymbolLower)
            if (foundIdx > -1) {
              if (foundIdx < 1) counting.add("symbolAt1")
              if (foundIdx < 5) counting.add("symbolAt5")
              if (foundIdx < 10) counting.add("symbolAt10")
              if (foundIdx < 20) counting.add("symbolAt20")
              if (foundIdx < 50) counting.add("symbolAt50")
              if (foundIdx < 100) counting.add("symbolAt100")
              counting.add("symbolAtSomewhere")
            }
            counting.add("symbolAtNorm")

          }
          
          val entrezByDescSortedMatches = entrezByDesc.toList.sortBy(-_._2)
          println(s"entrezMatch ByDesc ranking for this paragraph: " + entrezByDescSortedMatches.take(20).map(pair => "match? " + goldGeneEntrez.contains(pair._1) + "\t\t" + pair._1 + "\t\t" + pair._2).mkString("\n", "\n", "\n"))
          for (goldEntrez <- goldGeneEntrez) {
            val foundIdx = entrezByDescSortedMatches.map(_._1).indexOf(goldEntrez)
            if (foundIdx > -1) {
              if (foundIdx < 1) counting.add("entrezAt1")
              if (foundIdx < 5) counting.add("entrezAt5")
              if (foundIdx < 10) counting.add("entrezAt10")
              if (foundIdx < 20) counting.add("entrezAt20")
              if (foundIdx < 50) counting.add("entrezAt50")
              if (foundIdx < 100) counting.add("entrezAt100")
              counting.add("entrezAtSomewhere")
            }
            counting.add("entrezAtNorm")

          }


          val genesymbolByDescSortedMatches = genesymbolByDesc.toList.sortBy(-_._2)
          println(s"genesymbolMatch ByDesc ranking for this paragraph: " + genesymbolByDescSortedMatches.take(20).map(pair => "match? " + goldGeneEntrez.contains(pair._1) + "\t\t" + pair._1 + "\t\t" + pair._2).mkString("\n", "\n", "\n"))
          for (goldSymbol <- goldGeneSymbolLower) {
            val foundIdx = genesymbolByDescSortedMatches.map(_._1.toLowerCase).indexOf(goldGeneSymbolLower)
            if (foundIdx > -1) {
              if (foundIdx < 1) counting.add("symbolAt1")
              if (foundIdx < 5) counting.add("symbolAt5")
              if (foundIdx < 10) counting.add("symbolAt10")
              if (foundIdx < 20) counting.add("symbolAt20")
              if (foundIdx < 50) counting.add("symbolAt50")
              if (foundIdx < 100) counting.add("symbolAt100")
              counting.add("symbolAtSomewhere")
            }
            counting.add("symbolAtNorm")

          }
        }
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

object BioChopPipeline {
  def main(args:Array[String]): Unit = {
    val galagoChopIndex = MainTools.strsPlainFromArgs(args, "--chopindex=").headOption.getOrElse(throw new IllegalArgumentException("required flag --chopindex={indexdir}"))
    val dictionaryFile = MainTools.strsPlainFromArgs(args, "--dictionary=").headOption.getOrElse(throw new IllegalArgumentException("required flag --dictionary={dictionaryfile}"))
    val articlesDir = MainTools.strsPlainFromArgs(args, "--articles=").headOption.getOrElse(throw new IllegalArgumentException("required flag --articles={dir}"))
    val entrezMapFile = MainTools.strsPlainFromArgs(args, "--entrezMapFile=").headOption.getOrElse(throw new IllegalArgumentException("required flag --entrezMapFile={file}"))
    val doTrain = MainTools.strsPlainFromArgs(args, "--train").nonEmpty

    val tagger = new FastNameTagger(new java.io.File(dictionaryFile), wholeWordMatch = true, caseInsensitiveMatch = true, TextScrubber.scrubSentencePunctuation(_,virtualSpace = false))
    val pipe = new BioChopPipeline(tagger, doTrain, galagoChopIndex)
    pipe.processAllDocuments(Directory(articlesDir))
  }
}