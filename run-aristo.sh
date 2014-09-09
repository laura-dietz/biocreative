#!/bin/sh

java -cp target/biocreative-s2.10.2-g3.7-SNAPSHOT-1.4-SNAPSHOT-jar-with-dependencies.jar -Xmx5g -Dfile.encoding=utf8 org.lemurproject.galago.core.tools.App build ./indexingparams/aristo.json
