library(dplyr)
library(ggplot2)

#d<-read.table("rpp_extract_new.dat", head=T)

#attach(d)
#ruleoutlist<-c(12,13,17,22,25,43,46,50,55,64,73,
 #              80,82,84,86,104,117,132,139,140,142,143,165)
#dnew<-d[-which(d$Study %in% ruleoutlist),]

#write.table(dnew,file="rpp_73.dat",quote=F,row.names=T, col.names = T)
dnew<-read.table("rpp_73.dat",header=T)

dnew
plot(dnew$beta_orig,dnew$beta_rep)

ggplot(dnew,aes(x=beta_orig,y=beta_rep))+
  geom_point(color="darkblue")+
  geom_abline(intercept=0,slope=1)+
  geom_hline(yintercept=0,linetype=2)+
  theme_bw()+
  xlim(-0.5,1.5)+
  labs(x="Original effect size", y="Replication effect size")


df$irre<-0
df$irre[which(result$ind_prob[,2]>0.9)]<-1
df$irre<-as.factor(df$irre)
attach(dnew)

rpp_CI<-sapply(1:nrow(dnew),function(x) bayespval_beta_meta(beta=c(beta_orig[x], beta_rep[x]),sd=c(se_orig[x],  se_rep[x]),report_CI=T))

rpp_CI<-t(rpp_CI)
rpp_CI<-cbind(Study, rpp_CI, beta_rep)
rpp_CI<-data.frame(rpp_CI)
colnames(rpp_CI)<-c("Study","CIlow","CIhigh","pval","observed")

rpp_CI[,1]<-as.factor(rpp_CI[,1])

data_new<- rpp_CI[order(rpp_CI$pval,decreasing = T),]

data_new$Study2<- factor(data_new$Study,as.character(data_new$Study))

data_new$significance = as.factor((data_new$pval>0.05)+0)

p <- ggplot(data_new)+
  geom_pointrange(aes(x=Study2, y=(CIlow+CIhigh)/2, ymin=CIlow, ymax=CIhigh),color="darkblue",shape=3,fatten=2)+
  geom_hline(yintercept = 0, linetype=2)+
  geom_point(aes(x=Study2,y=observed,color=significance))+
  labs( x = "Study_ID", y = "95% predictive interval", color = "Replication\n p value") +
  scale_color_manual(labels = c("< 0.05", "> 0.05"), values = c("red", "blue")) +
  coord_flip()+
  theme_bw()

p

