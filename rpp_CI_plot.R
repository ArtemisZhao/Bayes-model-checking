library(dplyr)
library(ggplot2)

d<-read.table("rpp_extract_new.dat", head=T)

attach(d)

rpp_CI<-sapply(1:nrow(d),function(x) bayespval_beta_meta(beta=c(beta_orig[x], beta_rep[x]),sd=c(se_orig[x],  se_rep[x]),report_CI=T))

rpp_CI<-t(rpp_CI)
rpp_CI<-cbind(Study, rpp_CI, beta_rep)
rpp_CI<-data.frame(rpp_CI)
colnames(rpp_CI)<-c("Study","CIlow","CIhigh","pval","observed")

rpp_CI[,1]<-as.factor(rpp_CI[,1])

data_new<- rpp_CI[order(rpp_CI$pval,decreasing = T),]

data_new$Study2<- factor(data_new$Study,as.character(data_new$Study))

p <- ggplot(data_new)+
  geom_pointrange(aes(x=Study2, y=observed, ymin=CIlow, ymax=CIhigh),color="darkblue",fill="red",shape=21,fatten=2)+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point(aes(x=Study2,y=(CIlow+CIhigh)/2),shape=3,color="darkblue")+
  coord_flip()+
  xlab('Study_ID (sorted by p-value)')+
  ylab("Estimated confidence interval")+
  theme_bw()

p
