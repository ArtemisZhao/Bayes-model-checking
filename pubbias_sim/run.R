library(metafor)
###egger regression
eggerres<-sapply(1:nrow(betafinal),function(x) 
  regtest(betafinal[x,],vi=sdfinal[x,])$pval)

library(altmeta)
##skewness test
skewres<-data.frame(skew=sapply(1:nrow(betafinal),function(x) 
  metapb(betafinal[x,],sdfinal[x,])$skewness.pval))

###posterior checking
posres<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(betafinal[x,],sdfinal[x,],test="egger"))

posnewres<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(betafinal[x,],sdfinal[x,],test="egger-hetero"))

posskewres<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(betafinal[x,],sdfinal[x,],test="skew")) 

posqres<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(betafinal[x,],sdfinal[x,],test="Q")) 

####plotting
# library(ggplot2)
# library(egg)
# eggerres<- data.frame(egger=eggerres)
# posnewres<-data.frame(bayes_pval_egger=posnewres)
# p1<-ggplot(posnewres,aes(x=bayes_pval_egger))+
#   geom_histogram(color="black",fill="white")+
#   theme_bw()
# data3<-cbind(eggerres,posnewres)
# p2<- ggplot(data=data3, aes(x=egger,y=bayes_pval_egger))+geom_point()+theme_bw()
# ggarrange(p1,p2,nrow=1,labels=c("Bayes_p with egger","Comparison"),
#           label.args = list(gp = grid::gpar(font = 2, cex =0.8)))
# 
# 
# p1<-ggplot(eggerres,aes(x=egger))+geom_histogram(color="black",fill="white")
# 
# p2<-ggplot(skewres,aes(x=skew))+geom_histogram(color="black",fill="white")

#p3<-ggplot(posres,aes(x=pos_diff))+geom_histogram(color="black",fill="white")

# p4<-ggplot(posskewres,aes(x=pos_skew))+geom_histogram(color="black",fill="white")
# ggarrange(p1,p2,p4,nrow=1,labels=c("Egger regression","Skewness-based test","Bayesp with skew"),
#           label.args = list(gp = grid::gpar(font = 2, cex =0.8)))
length(which(eggerres<0.05))/100
length(which(skewres<0.05))/100
length(which(posres<0.05))/100
length(which(posskewres<0.05))/100



####publication bias application data from package altmeta
##### Hróbjartsson and Gøtzsche (2010) all tests imply significant publication bias
data("dat.ha")
hares<-bayes_posterior_check(dat.ha[,1],dat.ha[,2],test="egger-hetero",print_test_dist = T)
haskewres<-bayes_posterior_check(dat.ha[,1],dat.ha[,2],test="Q",print_test_dist = T)

###### Stead et al. (2012) A Meta-Analysis on the Effect of Nicotine Gum for Smoking Cessation
data("dat.slf")
slfres<-bayes_posterior_check(dat.slf[,1],dat.slf[,2],test="egger-hetero",print_test_dist = T)
slfskewres<-bayes_posterior_check(dat.slf[,1],dat.slf[,2],test="Q",print_test_dist = T)

bayes_posterior_check(dat.slf[-c(1,2),1],dat.slf[-c(1,2),2],test="skew")

####Liu and Latham (2009)
data("dat.lcj")
lcjres<-bayes_posterior_check(dat.lcj[,1],dat.lcj[,2],test="egger-hetero",print_test_dist = T)
lcjskewres<-bayes_posterior_check(dat.lcj[,1],dat.lcj[,2],test="skew",print_test_dist = T)



