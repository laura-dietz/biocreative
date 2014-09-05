package edu.umass.ciir.biocreative.parse;

import org.lemurproject.galago.core.parse.Document;
import org.lemurproject.galago.core.parse.DocumentStreamParser;
import org.lemurproject.galago.core.types.DocumentSplit;
import org.lemurproject.galago.utility.Parameters;
import scala.collection.Iterator;
import scala.xml.Node;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.util.HashMap;

/**
 * User: dietz
 * Date: 9/5/14
 * Time: 5:12 PM
 */
public class BioThesaurusGalagoParser extends DocumentStreamParser {

    private final BufferedInputStream stream;
    private final Iterator<Node> bioThesaususIterator;
    private final BioThesaususParser bioThesaususParser;
    private final BioCreativeParsing parsingTools = new BioCreativeParsing();

    public BioThesaurusGalagoParser(DocumentSplit split, Parameters p) throws IOException {
        super(split, p);
        stream = getBufferedInputStream(split);

        bioThesaususParser = new BioThesaususParser(stream);
        bioThesaususIterator = bioThesaususParser.iterator();
    }
    @Override
    public Document nextDocument() throws IOException {
        if(bioThesaususIterator.hasNext()){
            Node entry = bioThesaususIterator.next();
            GalagoBioDocument bioDocument = bioThesaususParser.convert(entry);

            String identifier = parsingTools.toGalagoIdentifier(bioDocument);
            String text = parsingTools.toGalagoText(bioDocument);
            HashMap<String, String> meta = parsingTools.toGalagoMeta(bioDocument);

            Document doc = new Document(identifier, text);
            doc.metadata = meta;

            return doc;
        } else {
            return null;
        }
    }

    @Override
    public void close() throws IOException {
        stream.close();
    }
}
