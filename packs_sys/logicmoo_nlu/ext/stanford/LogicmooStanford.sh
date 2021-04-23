#!/bin/bash

echo "${BASH_SOURCE[0]} Starting..."

if [[ ! -f deps/stanford-corenlp-4.2.0-models.jar ]]; then
(
 echo "${BASH_SOURCE[0]} Downloading..."
 cd deps
 set +x +e
 wget -N http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models.jar
 wget -N http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english.jar
 wget -N http://nlp.stanford.edu/software/stanford-corenlp-4.2.0-models-english-kbp.jar
)
fi

#set +x +e

export OPTS="edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 4090 -preload -status_port 4091 -timeout 15000 $@"

export OPTS="edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 4090 -preload -status_port 4091 -timeout 15000"

# -annotators tokenize,quote,ssplit,pos,lemma,ner,truecase,parse,hcoref,relation
echo java -mx4g -cp "deps/*" $OPTS
java -mx4g -cp "deps/*" $OPTS

echo "${BASH_SOURCE[0]} Exiting..."


