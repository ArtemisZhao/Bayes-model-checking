library(metafor)
library(egg)
library(ggplot2)

############plottting for with pubbias

load(file="./data_multiple_groups/sim.pub_bias.multiple.beta.RData")
load(file="./data_multiple_groups/sim.pub_bias.multiple.sd.RData")

eggerres<-sapply(1:nrow(betafinal),function(x) 
  regtest(betafinal[x,],sei=sdfinal[x,],control=list(maxiter=5000))$pval)
pval_egger<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(beta=betafinal[x,],sd=sdfinal[x,],
                        test = "egger-hetero",r_vec = c(0))$pval)

data<-cbind(egger=eggerres,pos_pval=pval_egger)
data=data.frame(data)
pp1<-ggplot(data,aes(x=egger,y=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+xlab("Egger regression p-value")+ylab("Bayesian p-value with egger test statistics")

#p2=ggplot(data,aes(x=egger))+geom_histogram(color="darkblue",bins=20,fill="white")+xlab("egger_regression_pval")
p3=ggplot(data,aes(x=pos_pval))+
  geom_histogram(color="darkblue",bins=10,fill="white")+
  xlab("Bayesian p-value with egger test statistics")+theme_bw()+ylim(0,1000)
ggarrange(p3,p1,nrow=1)


###comparison with Q test
qtest=sapply(1:nrow(betafinal),function(x) 
  rma.uni(yi=betafinal[x,],sei=sdfinal[x,])$QEp)

pval_Q<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(beta=betafinal[x,],sd=sdfinal[x,],test = "Q",r_vec = c(0))$pval)

data2<-cbind(qtest=qtest,pos_pval=pval_Q)
data2=data.frame(data2)
pp2<-ggplot(data2,aes(x=qtest,y=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+xlab("Cochran's Q p-value")+ylab("Bayesian p-value with Q test statistics")
ggarrange(pp1,pp2,nrow=1)
restest<-rma(yi=betafinal2[1,],sei=sdfinal2[1,])
funnel(restest)

length(which(eggerres<0.05))/5000
length(which(pval_egger<0.05))/5000

length(which(qtest<0.05))/5000
length(which(pval_Q<0.05))/5000


#######plotting for without pubbias data

load(file="./data_multiple_groups/sim.control.multiple.beta.RData")
load(file="./data_multiple_groups/sim.control.multiple.sd.RData")

eggerres2<-sapply(1:nrow(betafinal2),function(x) 
  regtest(betafinal2[x,],vi=sdfinal2[x,]^2)$pval)
pval_egger2<-sapply(1:nrow(betafinal2),function(x) 
  bayes_posterior_check(beta=betafinal2[x,],sd=sdfinal2[x,],
                        test = "egger-hetero",r_vec=c(0))$pval)

data3<-cbind(egger=eggerres2,pos_pval=pval_egger2)
data3=data.frame(data3)
p1<-ggplot(data3,aes(x=pos_pval,y=egger))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0, slope = 1) +
  ylab("Egger regression p-value")+xlab("Bayes p-value with egger test statistics")+
  theme_bw()
#p2=ggplot(data,aes(x=egger))+geom_histogram(color="darkblue",bins=20,fill="white")+xlab("egger_regression_pval")
p3=ggplot(data3,aes(x=pos_pval))+
  geom_histogram(color="darkblue",bins=10,fill="white")+
  xlab("Bayes p-value with egger test statistics")+theme_bw()+ylim(0,1000)
ggarrange(p3,p1,nrow=1)

###comparison with Q test
qtest2=sapply(1:nrow(betafinal2),function(x) 
  rma(yi=betafinal2[x,],sei=sdfinal2[x,])$QEp)

pval_Q2<-sapply(1:nrow(betafinal2),function(x) 
  bayes_posterior_check(beta=betafinal2[x,],sd=sdfinal2[x,],test = "Q",r_vec=c(0))$pval)

data4<-cbind(qtest=qtest2,pos_pval=pval_Q2)
data4=data.frame(data4)
pp2<-ggplot(data4,aes(y=qtest,x=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0,slope=1)+
  ylab("Cochran's Q test p-value")+xlab("Bayes p-value with Q test statistics")+
  theme_bw()
pp3=ggplot(data4,aes(x=pos_pval))+
  geom_histogram(color="darkblue",bins=10,fill="white")+
  xlab("Bayes p-value with Q test statistics")+theme_bw()+ylim(0,1000)

ggarrange(pp3,pp2,nrow=1)
#restest<-rma(yi=betafinal2[1,],sei=sdfinal2[1,])
#funnel(restest)

length(which(eggerres2<0.05))/5000
length(which(pval_egger2<0.05))/5000

length(which(qtest2<0.05))/5000
length(which(pval_Q2<0.05))/5000
