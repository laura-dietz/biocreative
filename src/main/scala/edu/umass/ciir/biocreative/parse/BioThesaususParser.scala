package edu.umass.ciir.biocreative.parse

import java.io.{InputStream, BufferedInputStream, FileInputStream}
import java.util.zip.GZIPInputStream

//import javax.xml.stream.XMLEventReader

import scala.xml.pull.XMLEvent
import scala.xml.{Node, XML}
/**
 * User: dietz
 * Date: 9/4/14
 * Time: 4:35 PM
 */
class BioThesaususParser(stream:InputStream) extends BioParser[Node] {
  type XmlEventBlock = Seq[XMLEvent]

  val xmlSegmentIter = new XmlSegmentIterator(stream)

  def iterator():Iterator[Node] = {

    xmlSegmentIter.map(nextSegment => {
      val x = XML.loadString("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> <iProClassDatabase xmlns=\"http://pir.georgetown.edu/iproclass\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \nxsi:schemaLocation=\"http://pir.georgetown.edu/iproclass http://pir.georgetown.edu/iproclass/iproclass.xsd\" \nrelease_date=\"14-May-14\" version=\"4_27\"> " +
        "\n" + nextSegment + "</iProClassDatabase>")
      x.child(1)
    })
  }



  def convert(elem:Node):GalagoBioDocument = {
//    val printer = new scala.xml.PrettyPrinter(90,2)
//    val debug = new StringBuilder()
//    printer.format(elem, debug)
//    println(debug.toString())

    val identifier = (elem \ "@Entry_ID").text

    def extent(fields:Seq[String]):String = {
      val elems =
        for(names <- fields.flatMap(elem \\ _ )) yield {
          "<"+names.label+">"+names.text+"</"+names.label+">" // get the whole thing with tags
        }
      elems.mkString(" ")
    }

    def metadata(fields2key:Seq[(String, String)]):Map[String,String] = {
      (
        for((field,key) <- fields2key) yield {
          val entries =
            for (node <- elem \\ field) yield {
              node.text // just the value
            }
          key -> entries.mkString(" ")
        }).toMap
    }

    val nameExtents = extent(Seq("Gene_Name", "Protein_Name"))
    val idExtents = extent(Seq("GenBank_ID", "Entrez_Gene_ID", "UniProtKB_ID", "Pfam_ID", "Locus_Tag"))
    val descExtent = extent(Seq("keyword", "Function_Info", "Gene_Desc", "Pfam_Desc", "InterPro_Desc", "GO_Term", "ISG_Desc",
      "Category", "Nomenclature", "KEGG_pathway_Desc", "EcoCyc_pathway_Name", "Feature_Desc", "PIRSF_Name",
      "Prosite_Desc", "InterPro_Desc", "Tissue_Specificity"))
    val speciesExtent = extent(Seq("Organism", "Source_Organism", "Taxon_Group", "Taxon"))
    val goidExtent = extent(Seq("GO_ID"))
    val functionIdExtent = extent(Seq("EC_num", "KEGG_pathway_ID", "EcoCyc_pathway_ID"))
    val typeExtent = "protein"

    val meta = metadata(Seq("UniProtKB_ID", "GenBank_ID", "Entrez_Gene_ID", "Pfam_ID", "Locus_Tag").map(x => (x,x)))


    GalagoBioDocument(identifier, meta, nameExtents, idExtents, descExtent, goidExtent, speciesExtent, functionIdExtent, typeExtent)

  }

  def getNames(elem:Node):BioNames = {
//    val printer = new scala.xml.PrettyPrinter(90,2)
//    val debug = new StringBuilder()
//    printer.format(elem, debug)
//    println(debug.toString())

    val identifier = (elem \ "@Entry_ID").text


    def contents(fields:Seq[String], splitFn:(String) => Iterable[String] = x => Seq(x), prefixFn:(String, String)=>String = (name,label) => name):Seq[String] = {
      val elems =
        for(names <- fields.flatMap(elem \\ _ )) yield {
          for( n <- splitFn(names.text)) yield {
            prefixFn(n, names.label)
          }
        }
      elems.flatten.toSeq
    }
    val nameExtents = contents(Seq("Gene_Name", "Protein_Name"), splitFn=_.split(';').map(_.trim))
    val idExtents =
      Seq("GenBank_ID", "Entrez_Gene_ID", "UniProtKB_ID", "Pfam_ID", "Locus_Tag")
        .map( field => {
          val content = contents(Seq(field))
          if(content.nonEmpty) Some( Tuple2(field, content) )
          else None
        })
        .flatten.toMap

    val species = contents(Seq("Organism", "Source_Organism", "Taxon_Group", "Taxon"))
    val goTerms = contents(Seq("GO_ID"))

    BioNames(identifier,  nameExtents, idExtents, species, goTerms)

  }
}

object BioThesaususParser extends App {
  System.setProperty("file.encoding", "UTF8")

  try {
    val filename = "./data/biothesaurus/iproclass.xml.gz"
    val p = new BioThesaususParser(new BufferedInputStream(new GZIPInputStream(new FileInputStream(filename))))

    val iter = p.iterator()
    iter.take(10000).foreach(x => {
      p.convert(x)

    })
  }
  catch {
    case  x: Throwable => throw new RuntimeException(x)
  }
}

class XmlSegmentIterator(stream:InputStream) extends BufferedIterator[String] {
  val src= io.Source.fromInputStream(stream)
  val buffer = new Array[Char](1024*1024)
  val sb = new StringBuilder()
  val reader = src.bufferedReader()
  var head_ = ""
  var avail = 1

  def findNext():Option[String] = {
    val begin = sb.indexOf("<iProClassEntry")
    if (begin > -1) {
      val end = sb.indexOf("</iProClassEntry>", begin + 1)
      if (end > -1) {
        val endend = end + "</iProClassEntry>".length
        head_ = sb.substring(begin, endend)
        sb.delete(0, endend)
        return Some(head_)
      }
    }
    return None
  }

  def seekToNext() {
    var found = false
    val opt = findNext()
    if (opt.isDefined) {
      found = true
      head_ = opt.get
    } else {

      avail = reader.read(buffer)
      sb.appendAll(buffer, 0, avail)
      while (avail > 0 && !found) {
        val opt = findNext()
        if (opt.isDefined) {
          found = true
          head_ = opt.get
        }
        if (!found) {
          avail = reader.read(buffer)
          sb.appendAll(buffer, 0, avail)
        }
      }
      if(avail <=0 && !found) {
        head_ = ""
      }
    }
  }

  seekToNext()
  override def head = head_
  override def next() = {
    val result = head_
    seekToNext()
    result
  }

  override def hasNext = head_ != ""

}

