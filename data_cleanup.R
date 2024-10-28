#AUTOMATIC INDEXING PROJECT-DATA CLEANUP AND PREP
#WRITTEN BY EMMA GARLOCK, MSC, MIST

library(tidyverse)#load package
#load the data from pubmed
raw_data <- read.csv("../Data/raw/pubmed_data_072024.csv",header = FALSE,na.strings=c("","NA"))%>%
  separate(V1,into=c("PMID","Indexing","Title","Journal","ISO","ISSN","Keyword","Substances","PubDate","DateCompleted","DateRevised","MeSH"),sep="\\|")
#convert the blank spaces to NA in the data file 
raw_data[raw_data == ' '] <- NA

raw_data_chem=raw_data%>%
  drop_na(Substances)

###data restructuring 
longformat_keyword=raw_data_chem%>%
  dplyr::select(PMID,Indexing,Keyword)%>% #get columns needed
  distinct()%>%#get only unique
  mutate(Keyword = strsplit(Keyword, ","))%>%#split into single keywords
  unnest(Keyword) %>%
  group_by(PMID) %>% #keep the kewords with the PMID
  mutate(v = paste0("kw.", row_number())) %>% #add column names to each keyword for PMID
  pivot_wider(id_cols=PMID, names_from = "v", values_from = "Keyword") %>%#go wide and make new columns 
  ungroup()%>%#ungroup 
  pivot_longer(cols=c(2:46),names_to = "type",values_to="term")%>%#pivot to longer format 
  drop_na(term)%>%
  mutate(type="kw")#make new column to indicate keyword later on 



#repeat same process for mesh headings 
#this will only be the full heading/subheading combos, not individually 
longformat_mesh=raw_data_chem%>%
  dplyr::select(PMID,MeSH)%>%
  distinct()%>%
  mutate(MeSH= strsplit(MeSH, "_"))%>%
  unnest(MeSH) %>%
  group_by(PMID) %>%
  mutate(v = paste0("mh.", row_number())) %>%
  pivot_wider(id_cols=PMID, names_from = "v", values_from = "MeSH") %>%
  ungroup()%>%
  pivot_longer(cols=c(2:32),names_to = "type",values_to="term")%>%
  drop_na(term)%>%
  mutate(type="mh")

#this is going to split up things into the main headings and subheadings
longformat_mesh_headings=longformat_mesh%>%
  dplyr::select(PMID,term)%>%
  distinct()%>%
  separate(term,into=c("mh_h","mh_s"),sep=":",remove=TRUE)%>% #_h was for the main heading. _s was for subheadings 
  pivot_longer(cols = c(2:3),names_to = "type",values_to = "term")%>%
  drop_na(term)%>%
  dplyr::filter(type=="mh_h")#I only want to use the main headings going forward 


#now grabbing the titles
titles=raw_data_chem%>%
  dplyr::select(PMID,Title)%>%
  mutate(type="ti")%>%#just adding in another column so we can rbind later 
  dplyr::select(PMID,type,term=Title)#changing the order of the columns so they match the others 

#now grab the relevant entry terms 

mesh_df=read.csv("../Data/raw/MESH.csv")#full mesh data

entry_terms=mesh_df[,2:3]%>%
  mutate(Preferred.Label=str_sub(Preferred.Label,3,-3))%>%
  mutate(Preferred.Label=str_to_lower(str_squish(Preferred.Label)))#just the actual heading and their entry terms now 


#extract only the Mesh/entries that are found in this dataset 
#note: I tried doing the next two datasets at once, but it made my R session quit, so its not as elegant as it could be. 
mesh_merge=longformat_mesh%>% #all the mesh terms we have for this analysis
  dplyr::select(term)%>% 
  mutate(term=str_to_lower(str_squish(term)))%>%
  distinct()%>%
  left_join(entry_terms,by=c("term"="Preferred.Label"))%>% #match them to the entry term data 
  mutate(Synonyms = strsplit(Synonyms, "\\|"))%>%
  drop_na(Synonyms)%>%
  unnest_longer(Synonyms)#drop any rows where there are no entry terms 

#now were assigning the entry terms to their PMIDS
longformat_entry=longformat_mesh_headings%>%
  mutate(term=str_to_lower(str_squish(term)))%>%
  left_join(mesh_merge,by="term",relationship="many-to-many")%>%
  mutate(type="ent")%>%
  dplyr::select(PMID,type,term=Synonyms)





#bind everything together so that there is one file that has all the keywords, mesh terms (main headings), titles and entry terms into one dataset 
data_longformat=rbind(longformat_keyword,longformat_mesh,longformat_entry,titles)%>%
  mutate(across(everything(), as.character))
 
  

write.csv(data_longformat,"../Data/processed/data_longformat_102024.csv",row.names = FALSE)
