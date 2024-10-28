#THIS IS THE SCRIPT THAT WILL FIND THE OVERLAP BETWEEN DIFFERENT FIELDS 
#WRITTEN BY EMMA GARLOCK, MSC, MIST 

library(tidyverse)
library(SnowballC)
library(tokenizers)

#load the dataset that was created using the data_cleanup.R script 
data_longformat=read.csv("../Data/processed/data_longformat_102024.csv")


data_wideformat=(data_longformat)%>%
  mutate(term=str_to_lower(str_squish(term)))%>%
  mutate(term=str_replace_all(term, "[[:punct:]]", " "))%>%
  pivot_wider(id_cols=PMID,names_from=type,values_from = term)%>%
  drop_na(kw,mh,ent)
  
 
 
#there is going to be 3 "repeats" (string match, token match, stem match) of 5 different analyses (list below). in total, 15 different comparisons. 
# 1. Keywords+Mesh 
# 2. Keywords+Title
# 3. Keyword+ Entry Term 
# 4. MeSH+title 
# 5. Title+Entry 

###Exact string match####

string_match_data=data_wideformat#making a dataset that will hold the info for the string match. 

#before we can run the loop, we need to make columns for the results to go in 
string_match_data$keyword_mesh=NA #column 6
string_match_data$keyword_title=NA #column 7
string_match_data$keyword_entry=NA #column 8
string_match_data$mesh_title=NA #column 9
string_match_data$title_entry=NA #column 10



for(i in 1:nrow(string_match_data)) { #for every row in the dataframe, do this: 
  kw_list=unlist(string_match_data[i,2])#unlist the content in row i, column 2(keywords)
  mh_h_list=unlist(string_match_data[i,3])#unlist the content in row i, column 3(mesh headings main 
  #we need another loop to sort out the entry terms 
  ent_list=unlist(string_match_data[i,4])
  ti_list=string_match_data[i,5]#not unlisting here, because we want to search the whole title for the occurance of a term 
  #now lets look at the combos 
  # 1. Keywords+Mesh 
  k_mh_matches=Reduce(intersect,list(kw_list,mh_h_list))#look for matches between keywords and mesh headings 
  string_match_data[i,6]=toString(k_mh_matches)#if there are any matches, add them to row  i column 8
  # 2. Keywords+Title
  kw_ti_matches=sapply(kw_list, grepl,ti_list)
  kw_ti_matches_num=length(which(sapply(kw_ti_matches, isTRUE)))
  string_match_data[i,7]=toString(kw_ti_matches_num)
  # 3. Keyword+Entry Term 
  k_ent_matches=Reduce(intersect,list(kw_list,ent_list))
  string_match_data[i,8]=toString(k_ent_matches)
  # 4. MeSH+title 
  mh_ti_matches=sapply(mh_h_list,grepl,ti_list)
  mh_ti_matches_num=length(which(sapply(mh_ti_matches,isTRUE)))
  string_match_data[i,9]=toString(mh_ti_matches_num)
  # 5. Title+Entry
  ent_ti_matches=sapply(ent_list,grepl,ti_list)
  ent_ti_matches_num=length(which(sapply(ent_ti_matches,isTRUE)))
  string_match_data[i,10]=toString(ent_ti_matches_num)
  
}#CLOSE LOOP 

string_match_data_character=string_match_data%>%mutate(across(everything(), as.character))
write.csv(string_match_data_character,"../Data/processed/string_match_data_102024.csv",row.names = FALSE)
#TOKENS 

token_data_wideformat=(data_longformat)%>%
  mutate(term=str_to_lower(str_squish(term)))%>%
  mutate(term=tokenize_words(term))%>%
  unnest_longer(term)%>%
  pivot_wider(id_cols=PMID,names_from=type,values_from = term)%>%
  drop_na(kw,mh,ent)

token_string_match_data=(token_data_wideformat)#making a dataset that will hold the info for the string match. 

#before we can run the loop, we need to make columns for the results to go in 
token_string_match_data$keyword_mesh=NA #column 6
token_string_match_data$keyword_title=NA #column 7
token_string_match_data$keyword_entry=NA #column 8
token_string_match_data$mesh_title=NA #column 9
token_string_match_data$title_entry=NA #column 10


for(i in 1:nrow(token_string_match_data)) { #for every row in the dataframe, do this: 
  kw_list=unlist(token_string_match_data[i,2])#unlist the content in row i, column 2(keywords)
  mh_h_list=unlist(token_string_match_data[i,3])#unlist the content in row i, column 3(mesh headings main 
  #we need another loop to sort out the entry terms 
  ent_list=unlist(token_string_match_data[i,4])
  ti_list=token_string_match_data[i,5]#not unlisting here, because we want to search the whole title for the occurance of a term 
  #now lets look at the combos 
  # 1. Keywords+Mesh 
  k_mh_matches=Reduce(intersect,list(kw_list,mh_h_list))#look for matches between keywords and mesh headings 
  token_string_match_data[i,6]=toString(k_mh_matches)#if there are any matches, add them to row  i column 8
  # 2. Keywords+Title
  kw_ti_matches=sapply(kw_list, grepl,ti_list)
  kw_ti_matches_num=length(which(sapply(kw_ti_matches, isTRUE)))
  token_string_match_data[i,7]=toString(kw_ti_matches_num)
  # 3. Keyword+Entry Term 
  k_ent_matches=Reduce(intersect,list(kw_list,ent_list))
  token_string_match_data[i,8]=toString(k_ent_matches)
  # 4. MeSH+title 
  mh_ti_matches=sapply(mh_h_list,grepl,ti_list)
  mh_ti_matches_num=length(which(sapply(mh_ti_matches,isTRUE)))
  token_string_match_data[i,9]=toString(mh_ti_matches_num)
  # 5. Title+Entry
  ent_ti_matches=sapply(ent_list,grepl,ti_list)
  ent_ti_matches_num=length(which(sapply(ent_ti_matches,isTRUE)))
  token_string_match_data[i,10]=toString(ent_ti_matches_num)
  
}#CLOSE LOOP 

token_string_match_data_character=token_string_match_data%>%mutate(across(everything(), as.character))
write.csv(token_string_match_data_character,"../Data/processed/token_string_match_data_102024.csv",row.names = FALSE)
##Stem and tokenize 

stem_data_wideformat=(data_longformat)%>%
  mutate(term=str_to_lower(str_squish(term)))%>%
  mutate(term=tokenize_word_stems(term))%>%
  unnest_longer(term)%>%
  pivot_wider(id_cols=PMID,names_from=type,values_from = term)%>%
  drop_na(kw,mh,ent)

stem_match_data=stem_data_wideformat#making a dataset that will hold the info for the string match.

#before we can run the loop, we need to make columns for the results to go in 
stem_match_data$keyword_mesh=NA #column 6
stem_match_data$keyword_title=NA #column 7
stem_match_data$keyword_entry=NA #column 8
stem_match_data$mesh_title=NA #column 9
stem_match_data$title_entry=NA #column 10


for(i in 1:nrow(stem_match_data)) { #for every row in the dataframe, do this: 
  kw_list=unlist(stem_match_data[i,2])#unlist the content in row i, column 2(keywords)
  mh_h_list=unlist(stem_match_data[i,3])#unlist the content in row i, column 3(mesh headings main 
  #we need another loop to sort out the entry terms 
  ent_list=unlist(stem_match_data[i,4])
  ti_list=stem_match_data[i,5]#not unlisting here, because we want to search the whole title for the occurance of a term 
  #now lets look at the combos 
  # 1. Keywords+Mesh 
  k_mh_matches=Reduce(intersect,list(kw_list,mh_h_list))#look for matches between keywords and mesh headings 
  stem_match_data[i,6]=toString(k_mh_matches)#if there are any matches, add them to row  i column 8
  # 2. Keywords+Title
  kw_ti_matches=sapply(kw_list, grepl,ti_list)
  kw_ti_matches_num=length(which(sapply(kw_ti_matches, isTRUE)))
  stem_match_data[i,7]=toString(kw_ti_matches_num)
  # 3. Keyword+Entry Term 
  k_ent_matches=Reduce(intersect,list(kw_list,ent_list))
  stem_match_data[i,8]=toString(k_ent_matches)
  # 4. MeSH+title 
  mh_ti_matches=sapply(mh_h_list,grepl,ti_list)
  mh_ti_matches_num=length(which(sapply(mh_ti_matches,isTRUE)))
  stem_match_data[i,9]=toString(mh_ti_matches_num)
  # 5. Title+Entry
  ent_ti_matches=sapply(ent_list,grepl,ti_list)
  ent_ti_matches_num=length(which(sapply(ent_ti_matches,isTRUE)))
  stem_match_data[i,10]=toString(ent_ti_matches_num)
  
}#CLOSE LOOP 

stem_match_data_character=stem_match_data%>%mutate(across(everything(), as.character))
write.csv(stem_match_data_character,"../Data/processed/stem_match_data_102024.csv",row.names = FALSE)
