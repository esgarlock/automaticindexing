#this is the script that generates the figures and other summary information 
#written by Emma Garlock

library(tidyverse)

string_match_data=read.csv("../Data/processed/string_match_data_102024.csv")
string_match_data[string_match_data==""]=NA
metadata=read.csv("../Data/raw/pubmed_data_072024.csv",header = FALSE,na.strings=c("","NA"))%>%
  separate(V1,into=c("PMID","Indexing","Title","Journal","ISO","ISSN","Keyword","Substances","PubDate","DateCompleted","DateRevised","MeSH"),sep="\\|")
metadata[metadata == ' '] <- NA



string_match_cleaned=string_match_data%>%
  dplyr::select(1,6:10)%>%
  mutate(keyword_mesh=ifelse(is.na(keyword_mesh),0,str_count(keyword_mesh, ',')+1))%>%
  mutate(keyword_entry=ifelse(is.na(keyword_entry),0,str_count(keyword_entry, ',')+1))%>%
  mutate(PMID=as.character(PMID))%>%
  pivot_longer(cols=c(2:6))%>%
  left_join(metadata,by="PMID")%>%
  dplyr::select(PMID,name,value,Indexing,11:13)%>%
  mutate(Indexing=ifelse(is.na(Indexing),"Manual",Indexing))

indexing_summary=string_match_cleaned%>%
  dplyr::select(PMID,Indexing)%>%
  distinct()%>%
  group_by(Indexing)%>%
  count(n_distinct(PMID))%>%
  dplyr::select(Indexing,ind_group_total=n)

#Automatic=3148
#curated=658
#manual=496

#string match plots 
string_match_summary=string_match_cleaned%>%
  dplyr::select(PMID,Indexing,name,value)%>%
  group_by(Indexing,name,value)%>%
  count(n_distinct(PMID))%>%
  dplyr::select(-4)%>%
  left_join(indexing_summary,by="Indexing")%>%
  mutate(percentage=round((n/ind_group_total)*100,2))%>%
  ungroup()

names=as.list(string_match_summary%>%dplyr::select(name)%>%distinct())[[1]]

for (i in 1:length(names)){
title=paste0("Exact String Match Overlaps for ",names[i])

string_match_plot=string_match_summary%>%
  filter(name==names[[i]])%>%
  ggplot(aes(x=value,y=percentage,fill=Indexing))+ #start plotting 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label = percentage),colour = "black", size = 2.5, position = position_dodge(1),vjust=-0.9,angle=45,hjust=-0.2)+
  xlab("Number of Overlaps Between Fields")+
  ylab("Percentage of Citations")+
  ggtitle(title)+
  scale_y_continuous(expand=expansion(c(0,0.1)))+
  scale_x_continuous(breaks = seq(0, 12, by = 1))+
  scale_fill_manual(values = c("#D81B60", "#1F88E5", "#FFC105"))+
  theme_bw()

ggsave(paste0("../Data/plots/string_overlaps_",names[[i]],".png"),plot=string_match_plot,width = 20, height = 20, units = "cm")

}

#token match plots 

token_match_data=read.csv("../Data/processed/token_string_match_data_102024.csv")
token_match_data[token_match_data==""]=NA

token_match_cleaned=token_match_data%>%
  dplyr::select(1,6:10)%>%
  mutate(keyword_mesh=ifelse(is.na(keyword_mesh),0,str_count(keyword_mesh, ',')+1))%>%
  mutate(keyword_entry=ifelse(is.na(keyword_entry),0,str_count(keyword_entry, ',')+1))%>%
  mutate(PMID=as.character(PMID))%>%
  pivot_longer(cols=c(2:6))%>%
  left_join(metadata,by="PMID")%>%
  dplyr::select(PMID,name,value,Indexing,11:13)%>%
  mutate(Indexing=ifelse(is.na(Indexing),"Manual",Indexing))

token_match_summary=token_match_cleaned%>%
  dplyr::select(PMID,Indexing,name,value)%>%
  group_by(Indexing,name,value)%>%
  count(n_distinct(PMID))%>%
  dplyr::select(-4)%>%
  left_join(indexing_summary,by="Indexing")%>%
  mutate(percentage=round((n/ind_group_total)*100,2))%>%
  ungroup()

for (i in 1:length(names)){
  title=paste0("Token Match Overlaps for ",names[i])
  
 token_match_plot=token_match_summary%>%
    filter(name==names[[i]])%>%
    ggplot(aes(x=value,y=percentage,fill=Indexing))+ #start plotting 
    geom_bar(position="dodge", stat="identity")+
    geom_text(aes(label = percentage),colour = "black", size = 2.5, position = position_dodge(1),vjust=-0.9,angle=45,hjust=-0.2)+
    xlab("Number of Overlaps Between Fields")+
    ylab("Percentage of Citations")+
    ggtitle(title)+
    scale_y_continuous(expand=expansion(c(0,0.1)))+
    scale_x_continuous(breaks = seq(0, 400, by = 1))+
    scale_fill_manual(values = c("#D81B60", "#1F88E5", "#FFC105"))+
    theme_bw()
  
  ggsave(paste0("../Data/plots/token_overlaps_",names[[i]],".png"),plot=token_match_plot,width = 20, height = 20, units = "cm")
  
}


#stemmed 

stem_match_data=read.csv("../Data/processed/stem_match_data_102024.csv")
stem_match_data[stem_match_data==""]=NA

stem_match_cleaned=stem_match_data%>%
  dplyr::select(1,6:10)%>%
  mutate(keyword_mesh=ifelse(is.na(keyword_mesh),0,str_count(keyword_mesh, ',')+1))%>%
  mutate(keyword_entry=ifelse(is.na(keyword_entry),0,str_count(keyword_entry, ',')+1))%>%
  mutate(PMID=as.character(PMID))%>%
  pivot_longer(cols=c(2:6))%>%
  left_join(metadata,by="PMID")%>%
  dplyr::select(PMID,name,value,Indexing,11:13)%>%
  mutate(Indexing=ifelse(is.na(Indexing),"Manual",Indexing))

stem_match_summary=stem_match_cleaned%>%
  dplyr::select(PMID,Indexing,name,value)%>%
  group_by(Indexing,name,value)%>%
  count(n_distinct(PMID))%>%
  dplyr::select(-4)%>%
  left_join(indexing_summary,by="Indexing")%>%
  mutate(percentage=round((n/ind_group_total)*100,2))%>%
  ungroup()

for (i in 1:length(names)){
  title=paste0("Stem Match Overlaps for ",names[i])
  
 stem_match_plot=stem_match_summary%>%
    filter(name==names[[i]])%>%
    ggplot(aes(x=value,y=percentage,fill=Indexing))+ #start plotting 
    geom_bar(position="dodge", stat="identity")+
    geom_text(aes(label = percentage),colour = "black", size = 2.5, position = position_dodge(1),vjust=-0.9,angle=45,hjust=-0.2)+
    xlab("Number of Overlaps Between Fields")+
    ylab("Percentage of Citations")+
    ggtitle(title)+
    scale_y_continuous(expand=expansion(c(0,0.1)))+
    scale_x_continuous(breaks = seq(0, 700, by = 1))+
    scale_fill_manual(values = c("#D81B60", "#1F88E5", "#FFC105"))+
    theme_bw()
  
  ggsave(paste0("../Data/plots/stem_overlaps_",names[[i]],".png"),plot=stem_match_plot,width = 20, height = 20, units = "cm")
  
}

#BINARY COMPASISONS (YES MATCH/NO)
string_match_rbindprep=string_match_cleaned%>%
  mutate(level="string")
token_match_rbindprep=token_match_cleaned%>%
  mutate(level="token")
stem_match_rbindprep=stem_match_cleaned%>%
  mutate(level="stem")
all_matches=rbind(string_match_rbindprep,token_match_rbindprep,stem_match_rbindprep)


all_matches_summary=all_matches%>%
  dplyr::select(1:4,8)%>%
  mutate(value_binary=ifelse(value==0,"No","Yes"))%>%
  group_by(level,Indexing,name,value_binary)%>%
  count(n_distinct(PMID))%>%
  dplyr::select(-5)%>%
  left_join(indexing_summary,by="Indexing")%>%
  mutate(percentage=round((n/ind_group_total)*100,2))%>%
  ungroup()

for (i in 1:length(names)){
title=paste0("Summary Overlaps for ",names[i])
all_matches_summary_plot=all_matches_summary%>%
  filter(name==names[[i]])%>%
  mutate(level=fct_relevel(level,c("string","token","stem")))%>%
  ggplot(aes(x=(value_binary),y=percentage,fill=Indexing))+
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label = percentage),colour = "black", size = 2.5, position = position_dodge(1),vjust=-0.9,angle=45,hjust=-0.2)+
  ggtitle(title)+
  ylab('Percentage of Citations')+
  xlab("Is there any overlap between the fields?")+
  scale_y_continuous(expand=expansion(c(0,0.1)))+
  scale_fill_manual(values = c("#D81B60", "#1F88E5", "#FFC105"))+
  theme_bw()+
  facet_wrap(~level)
ggsave(paste0("../Data/plots/summary_overlaps_",names[[i]],".png"),plot=all_matches_summary_plot,width = 20, height = 20, units = "cm")
}

