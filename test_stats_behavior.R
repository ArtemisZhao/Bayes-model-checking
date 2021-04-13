
library(metafor)
library(egg)
library(ggplot2)

sim_data<-function(r, omg, n=10, sd=rep(1,10)){
  phi = sqrt(r/(1-r))*omg 
  
  beta = rnorm(1,mean=0,sd=omg)
  
  bv = rnorm(n, mean = beta, sd=phi)
  
  zv = rnorm(n, mean = bv, sd = sd)
  
  return(matrix(zv,nrow=1))
}

repdata<-t(sapply(1:1000, function(x) sim_data(r=1e-5,omg=1,n=5,sd=rep(1,5))))

pvalegger<-sapply(1:1000,function(x) 
  bayes_posterior_check(beta=repdata[x,],sd=rep(1,5),test="egger-hetero",L=2000)$pval)

eggerres<-sapply(1:1000,function(x) 
  regtest(repdata[x,],sei=rep(1,5),control=list(maxiter=5000))$pval)

data<-cbind(egger=eggerres,pos_pval=pvalegger)
data=data.frame(data)
pp1<-ggplot(data,aes(x=egger,y=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()+xlab("Egger regression p-value")+ylab("Bayesian p-value with egger test statistics")


pvalQ<-sapply(1:1000,function(x) 
  bayes_posterior_check(beta=repdata[x,],sd=rep(1,5),test="Q",L=2000)$pval)

qtest<-sapply(1:1000,function(x) 
  rma.uni(yi=repdata[x,],sei=rep(1,5))$QEp)

data2<-cbind(qtest=qtest,pos_pval=pvalQ)
data2=data.frame(data2)
pp2<-ggplot(data2,aes(x=qtest,y=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()+xlab("Cochran's Q p-value")+ylab("Bayesian p-value with Q test statistics")

           
