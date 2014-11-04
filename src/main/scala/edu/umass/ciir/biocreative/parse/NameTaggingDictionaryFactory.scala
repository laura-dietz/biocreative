package edu.umass.ciir.biocreative.parse

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import edu.umass.ciir.biocreative.BioNames
import edu.umass.ciir.strepsi.{FileTools, MainTools}

import scala.xml.Node

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 3:25 PM
 */
class NameTaggingDictionaryFactory(outputDirectory:File, biothesaurusFile:File, val maxEntries:Int = -1) {
  FileTools.makeDirs(outputDirectory.getAbsolutePath)
  def writer(filename:String):Writer = {
    new OutputStreamWriter( new GZIPOutputStream( new FileOutputStream(filename)))
  }
  def plainWriter(filename:String):Writer = {
    new FileWriter(filename)
  }

  val nameWriter = writer(outputDirectory.getAbsolutePath+File.separator+"Name.dict.gz")
  val entrezGeneIdWriter = writer(outputDirectory.getAbsolutePath+File.separator+"Entrez_Gene_ID.txt.gz")
  val genBankIdWriter = writer(outputDirectory.getAbsolutePath+File.separator+"GenBank_ID.txt.gz")
  val goIdWriter = writer(outputDirectory.getAbsolutePath+File.separator+"GO_ID.txt.gz")
  val speciesWriter = writer(outputDirectory.getAbsolutePath+File.separator+"Species.txt.gz")
  val writers = Seq(nameWriter, entrezGeneIdWriter, genBankIdWriter, goIdWriter, speciesWriter)
  //  val uniProtKbIdWriter = writer(outputDirectory.getAbsolutePath+File.separator+"UniProtKB_ID.txt")
  //  val pfamIdWriter = writer(outputDirectory.getAbsolutePath+File.separator+"Pfam_ID.txt")
  //  val locusTagWriter = writer(outputDirectory.getAbsolutePath+File.separator+"Locus_Tag.txt")
//  val idWriters = Map( "Entrez_Gene_ID" -> entrezGeneIdWriter, "GenBank_ID" -> genBankIdWriter, "UniProtKB_ID" -> uniProtKbIdWriter,
//    "Pfam_ID" -> pfamIdWriter, "Locus_Tag" -> locusTagWriter)

  private final val stream: InputStream = {
    val stream = new BufferedInputStream(new FileInputStream(biothesaurusFile))
    if(biothesaurusFile.getName.endsWith(".gz")){
      new GZIPInputStream(stream)
    } else stream
  }
//  private final val bioThesaususIterator: Iterator[Node] = null
//  private final val bioThesaususParser: BioThesaususParser = null
//  private final val parsingTools: BioCreativeParsing = new BioCreativeParsing

  val parser = new BioThesaususParser(stream)
  val iter = parser.iterator()

  def write() {
    var count: Int = maxEntries
    var hasNext = nextDocument()
    while (hasNext) {
      if(count % 1000 ==0)print(".")
      if(count % 100000==0) {
        println()
        writers.foreach(_.flush())
      }
      if (count == 0) {
        println("\nStopped.")
        hasNext =false
      }
      else {
        hasNext = nextDocument()
        count -= 1
      }
    }

    writers.foreach(_.flush())
    writers.foreach(_.close())
  }

  class IdProvider(init:Int) {
    var counter:Int = init
    def consume():Int = {
      val x = counter
      counter += 1
      x
    }
  }
  var entryIdProvider = new IdProvider(0)

  def nextDocument(): Boolean = {
    def clean(seq:Seq[String]):Seq[String] ={
      seq.map(_.replaceAll("\\s+"," "))
    }

    def cleanAndUnwrap(seq:Seq[String]):Seq[String] ={
      seq.map({
        _.replaceAllLiterally("\n"," ").replaceAllLiterally("\r"," ").replaceAllLiterally("\t"," ").replaceAll("\\s+"," ")
      })
    }

    if (iter.hasNext) {
      val entry: Node = iter.next()
      val dirty = parser.getNames(entry)
      val bioDocument = BioNames(dirty.identifier,clean(dirty.names), dirty.otherIds, clean(dirty.species), clean(dirty.goTerms), cleanAndUnwrap(dirty.description))

      val appendix = BioNames.serialize(bioDocument).mkString("\t")
      for(name <- (bioDocument.names ++ bioDocument.species).distinct){
        nameWriter.write(name+"\t" + bioDocument.identifier +"\n")
      }

      for( geneid <- bioDocument.otherIds.getOrElse("Entrez_Gene_ID", Seq.empty); if !geneid.trim.isEmpty ) {
        entrezGeneIdWriter.write(geneid.trim()+"\t"+appendix+"\n")
      }
      for( geneid <- bioDocument.otherIds.getOrElse("GenBank_ID", Seq.empty); if !geneid.trim.isEmpty){
        genBankIdWriter.write(geneid.trim()+"\t"+appendix+"\n")
      }
      
      for( goterm <- bioDocument.goTerms){
        goIdWriter.write(goterm.trim+"\t"+appendix+"\n")
      }
      
      for( species <- bioDocument.species){
        speciesWriter.write(species.trim+"\t"+appendix+"\n")
      }
      

      true
    }
    else false
  }

  def close() {
    stream.close()
  }
}

object NameTaggingDictionaryFactory {
  def main(args:Array[String]): Unit ={
    val outDir = MainTools.strsFromArgsSimple(args, "--outDir=", 1).head
    val biothesaurusFile= MainTools.strsFromArgsSimple(args, "--biothesaurus=", 1).head
    val maxEntries= MainTools.strsFromArgsSimple(args, "--maxEntries=", 0).headOption.getOrElse("-1").toInt

    println(s"Running: NameTaggingDictionaryFactory --outDir=$outDir --biothesaurus=$biothesaurusFile --maxEntries=$maxEntries")
    val factory = new NameTaggingDictionaryFactory(new File(outDir),new File(biothesaurusFile), maxEntries = maxEntries)
    factory.write()
    factory.close()


    println("now run this!")
    println("cd "+outDir)
    println("for f in *txt.gz; do zcat $f | sort --temporary-directory=/mnt/scratch2/tmp/ --compress-program=gzip --parallel=3 -o $f.sorted; done")
    println("zcat Name.dict.gz | cut -f1 | sort -u --temporary-directory=/mnt/scratch2/tmp/ --compress-program=gzip --parallel=3 | aw\nk '{ print FNR \"\\t\" $0 }' > nameDict.txt")
    println("grep -e \"[a-Z][a-Z]\" nameDict.txt > nameDict.cleaned.txt")

    // zcat biothesarus/Name.txt.bak.gz |head -n 1000 | cut -f1 | sort -u --temporary-directory=/mnt/scratch2/tmp/ --compress-program=gzip --parallel=3 | awk '{ print FNR "\t" $0 }' | gzip > biothesarus/nameDict.bak.txt.gz


  }
}
