package edu.umass.ciir.biocreative

import scala.xml.Node

/**
 * User: dietz
 * Date: 9/4/14
 * Time: 4:56 PM
 */
package object parse {





  trait BioParser[IterElem] {
    def iterator(): Iterator[IterElem]

    def convert(elem: IterElem): GalagoBioDocument
  }

}
