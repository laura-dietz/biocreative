package edu.umass.ciir.biocreative.parse

import java.io._
import java.util.zip.GZIPInputStream

import edu.umass.ciir.strepsi.{FileTools, MainTools}

import scala.xml.Node

/**
 * User: dietz
 * Date: 9/22/14
 * Time: 3:25 PM
 */
class NameTaggingDictionaryFactory(outputDirectory:File, biothesaurusFile:File) {
  FileTools.makeDirs(outputDirectory.getAbsolutePath)
  val nameWriter = new FileWriter(outputDirectory.getName+File.separator+"Name.txt")
  val entrezGeneIdWriter = new FileWriter(outputDirectory.getName+File.separator+"Gene_ID.txt")
  val genBankIdWriter = new FileWriter(outputDirectory.getName+File.separator+"GenBank_ID.txt")
  val goIdWriter = new FileWriter(outputDirectory.getName+File.separator+"GO_ID.txt")
  val speciesWriter = new FileWriter(outputDirectory.getName+File.separator+"Species.txt")
  val writers = Seq(nameWriter, entrezGeneIdWriter, genBankIdWriter, goIdWriter, speciesWriter)
  //  val uniProtKbIdWriter = new FileWriter(outputDirectory.getName+File.separator+"UniProtKB_ID.txt")
  //  val pfamIdWriter = new FileWriter(outputDirectory.getName+File.separator+"Pfam_ID.txt")
  //  val locusTagWriter = new FileWriter(outputDirectory.getName+File.separator+"Locus_Tag.txt")
//  val idWriters = Map( "Entrez_Gene_ID" -> entrezGeneIdWriter, "GenBank_ID" -> genBankIdWriter, "UniProtKB_ID" -> uniProtKbIdWriter,
//    "Pfam_ID" -> pfamIdWriter, "Locus_Tag" -> locusTagWriter)

  private final val stream: InputStream = {
    val stream = new BufferedInputStream(new FileInputStream(biothesaurusFile))
    if(biothesaurusFile.getName.endsWith(".gz")){
      new GZIPInputStream(stream)
    } else stream
  }
  private final val bioThesaususIterator: Iterator[Node] = null
  private final val bioThesaususParser: BioThesaususParser = null
  private final val parsingTools: BioCreativeParsing = new BioCreativeParsing

  val parser = new BioThesaususParser(stream)
  val iter = parser.iterator()

  def write() {
    var count: Int = -1
    var hasNext = nextDocument()
    while (hasNext) {
      if(count % 1000 ==0)print(".")
      if(count % 100000==0) println()
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
    if (iter.hasNext) {
      val entry: Node = iter.next()
      val bioDocument = parser.getNames(entry)

      val id = bioDocument.identifier
            
      val allNames = bioDocument.names.distinct.map("\"" + _.replaceAllLiterally("\"","") + "\"").mkString(" ")
      val allIds = bioDocument.otherIds.toSeq.flatMap(pair => pair._2.sorted.map(pair._1 -> _)).mkString(" ")
      val allGoTerms = bioDocument.goTerms.mkString(" ")
      val allSpecies = bioDocument.species.distinct.map("\"" + _.replaceAllLiterally("\"","") + "\"").mkString(" ")
      val description = bioDocument.description.mkString(" ").replaceAllLiterally("\n"," ").replaceAllLiterally("\t"," ")

      val appendix = Seq(id, allNames, allIds, allGoTerms, allSpecies, description).mkString("\t")
      for(name <- (bioDocument.names ++ bioDocument.species).distinct){
        nameWriter.write(name+"\t"+id+"\n")
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
    val outDir = MainTools.strsFromArgs(args, "--outDir=", 1).head
    val biothesaurusFile= MainTools.strsFromArgs(args, "--biothesaurus=", 1).head
    val factory = new NameTaggingDictionaryFactory(new File(outDir),new File(biothesaurusFile))
    factory.write()
    factory.close()


    println(" run this in "+outDir)
    println("cd "+outDir)
    println("for f in *txt; do sort -u -t\\t $f > $f.sorted; done")
    println("cut -f1 Name.txt.sorted | uniq | nl -nln -s\\t  > nameDict.txt")
  }
}
