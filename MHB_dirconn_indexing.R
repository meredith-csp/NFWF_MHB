#Set up directed connectivity file

library(dplyr)

setwd("E:/Projects/NFWF_IntegratedLI/data/working")

index<-read.csv("HUC12_ids_conversion.csv")
dirconn<-read.table("MHB_tree_file.txt",sep='\t',header=FALSE)
colnames(dirconn)<-c("To","From")

join<-dirconn %>%
  left_join(index,by=c("To"="HUC12")) %>%
  left_join(index,by=c("From"="HUC12")) %>%
  filter(!is.na(ID.x)) %>%
  select(ID.x,ID.y)

join[,2]<-ifelse(is.na(join[,2]),-1,join[,2])

write.table(join,file="E:/Projects/NFWF_IntegratedLI/code/R/NFWF_MHB/MHB_MarkdownSetup/runs/data/ZonationData/MHB_dirconn.txt",sep='\t',row.names=FALSE,col.names=FALSE)
