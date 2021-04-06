#### n number of repeats; m number of study in each repeat
library(ggplot2)
library(egg)
n=5000
m=5

#####parameters
###proportion of biased study
p_bias = 0.4
##batch effect magnitude

phi_list = seq(0,1,0.1)
## n in case/control
n_arms = 100
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

  save(beta_list,file=paste0("./data_multiple_groups/beta_list_",phi,".RData"))
  save(sd_list,file=paste0("./data_multiple_groups/sd_list_",phi,".RData"))

}

