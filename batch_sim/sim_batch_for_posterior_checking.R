#### n number of repeats; m number of study in each repeat
n=1000
m=5

#####parameters
###proportion of biased study
p_bias = 0.4
##batch effect magnitude

phi_list = seq(0,1,0.1)
## n in case/control
n_arms = 40
### varaince magnitude
sd=1

#####case: with & without batch effect

beta<-rep(1,n) ### fixed at 1

x<-rep(c(0:1),each=n_arms)

###bias label
bias_or_not<-c(rep(1,m*p_bias),rep(0,m*(1-p_bias)))

res<-c()
for (phi in phi_list){ 
  beta_list<-c()
  sd_list<-c()
 for (j in bias_or_not){
 if (j==1){
  batchlabel0<-rbinom(n_arms,1,prob=0.2)
  batchlabel1<-rbinom(n_arms,1,prob=0.8)
  batchlabels<-c(batchlabel0,batchlabel1)
  batcheffect<-c(rnorm(n,0,phi)) 

  Ybatch<-sapply(1:length(beta), function(i) 
    x*beta[i]+batchlabels*batcheffect[i]+rnorm(2*n_arms,0,sd))
  estbatch<-t(sapply(1:ncol(Ybatch),function(j) 
    summary(lm(Ybatch[,j]~x))$coefficient[2,])) 
  beta_list<-cbind(beta_list,estbatch[,1])
  sd_list<-cbind(sd_list,estbatch[,2])
  }
  else{
    #beta=rnorm(n,1,1)
    Y<-sapply(1:length(beta), function(i) x*beta[i]+rnorm(2*n_arms,0,sd))
    est1<-t(sapply(1:ncol(Y),function(j)
      summary(lm(Y[,j]~x))$coefficient[2,]))
    beta_list<-cbind(beta_list,est1[,1])
    sd_list<-cbind(sd_list,est1[,2])
}
 }

pos_pval_diff<-sapply(1:nrow(beta_list),function(x) 
  bayes_posterior_check(beta=beta_list[x,],sd=sd_list[x,],r_vec=c(1e-5),test = "Q")$pval)
res<-cbind(res,pos_pval_diff)

}

resnew<-cbind(phi=rep(c(0,0.2,0.4,0.6),each=1000),pval=matrix(res[,c(1,3,5,7)],ncol=1))
resnew<-data.frame(resnew)
names(resnew)<-c("phi","bayes_pval")
p2<-ggplot(data=resnew[,],aes(x=bayes_pval))+
  geom_histogram(color="black",fill="white",bins = 20)+
  facet_grid(.~phi,labeller = label_both)+theme_bw()
p2
