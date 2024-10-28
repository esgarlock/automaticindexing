#!/bin/bash
export PATH=${HOME}/edirect:${PATH}
efetch -db pubmed -id "$(cat query_sets/pmid_query.txt)" -format xml |\
xtract -pattern PubmedArticle -tab "|" -sep "," -def " " -element MedlineCitation/PMID -element MedlineCitation@IndexingMethod -element ArticleTitle Journal/Title ISOAbbreviation ISSN -element Keyword -element Chemical/NameOfSubstance -block PubDate -tab "|" -sep "-" -element Year,Month,Day -block DateCompleted -tab "|" -sep "-" -element Year,Month,Day -block DateRevised -tab "|" -sep "-" -element Year,Month,Day -block MeshHeading -tab "_" -sep ":" -element DescriptorName,QualifierName >datasets/pubmed_data_0109.txt