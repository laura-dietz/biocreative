package edu.umass.ciir.biocreative.parse;

import org.lemurproject.galago.core.parse.Document;
import org.lemurproject.galago.core.parse.DocumentStreamParser;
import org.lemurproject.galago.core.types.DocumentSplit;
import org.lemurproject.galago.utility.Parameters;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;

public class SingleFileChopParser extends DocumentStreamParser {

    protected BufferedReader reader;
    protected String identifier;

    public SingleFileChopParser(DocumentSplit split, Parameters pp) throws IOException {
        super(split, pp);
        this.reader = getBufferedReader(split);

        if (reader != null) {
            String identifierLine = reader.readLine();
            identifier = identifierFromLine(identifierLine);
        } else {
            identifier = "";
        }
    }

    private String identifierFromLine(String identifierLine) {
        // <identifier>RUT</identifier>
        if (identifierLine == null || identifierLine.isEmpty()) return "";
        int beginbegin = identifierLine.indexOf("<identifier>");
        if(beginbegin<0) return "";
        int begin = beginbegin + "<identifier>".length();

        int end = identifierLine.indexOf("</identifier>", begin);
        if(end<0) return "";
        String identifier = identifierLine.substring(begin, end);
        return identifier;
    }


    public Document nextDocument() throws IOException {
        String line;

        if (reader == null) {
            return null;
        }

        if (identifier.length() == 0) {
            return null;
        }

        StringBuilder buffer = new StringBuilder();

        int lines = 0;
        while ((line = reader.readLine()) != null) {
            buffer.append(line);
            buffer.append('\n');
            lines ++;
        }

        if(lines == 0) return null;

        //        System.out.println(identifier+ "\t\t"+lines+" lines.");
        return new Document(identifier, buffer.toString());
    }

    @Override
    public void close() throws IOException {
        if (this.reader != null) {
            this.reader.close();
            this.reader = null;
        }
    }
}
