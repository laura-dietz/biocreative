package edu.umass.ciir.biocreative.tag

import edu.umass.ciir.biocreative.{BioNamesWithNameIds, BioNames}

/**
 * User: dietz
 * Date: 9/24/14
 * Time: 12:22 PM
 */
class BioDocumentTagger(val tagger:FastNameTagger) {
  def tag(biodocument:BioNames):BioNamesWithNameIds = {

    val namesNamesId = biodocument.names.flatMap(tagger.tag(_).map(_.nameId))
    val speciesNamesId = biodocument.species.flatMap(tagger.tag(_).map(_.nameId))
    val goTermsNamesId = biodocument.goTerms.flatMap(tagger.tag(_).map(_.nameId))
    val descriptionNamesId = biodocument.description.flatMap(tagger.tag(_).map(_.nameId))
    BioNamesWithNameIds(biodocument,
      namesNamesId = namesNamesId,
      speciesNamesIds = speciesNamesId,
      goTermsNamesIds = goTermsNamesId,
      descriptionNameIds = descriptionNamesId)
  }

}
