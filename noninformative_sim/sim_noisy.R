set.seed(123)

##### noisy level
sdlist=c(2,5,10,20)
n=5000

for (i in 1:length(sdlist)){
  sd<-sdlist[i]
  beta1<-rep(1,n)
  ##
  n_arms=100
  x<-rep(c(0:1),each=n_arms)
  #xsmall<-rep(c(0:1),each=20)
  
  batchlabel0<-c(rep(1,0.2*n_arms),rep(0,0.8*n_arms))
  batchlabel1<-c(rep(1,0.8*n_arms),rep(0,0.2*n_arms))
  batchlabels<-c(batchlabel0,batchlabel1)
  
  # medium batch effect
  batcheffect<-c(rnorm(n,0,0.6)) 
  
  Ybatch<-sapply(1:length(beta1), function(i) 
    x*beta1[i]+batchlabels*batcheffect[i]+rnorm(n_arms*2))
  
  Yrep<-sapply(1:length(beta1), function(i) x*beta1[i]+rnorm(n_arms*2,0,sd))
  

  est<-t(sapply(1:ncol(Y),function(j)
    summary(lm(Yrep[,j]~x))$coefficient[2,]))
  
  #estnoisy<-t(sapply(1:ncol(Ysmall),function(j)
  # summary(lm(Ysmall[,j]~xsmall))$coefficient[2,]))
  
  estbatch<-t(sapply(1:ncol(Ybatch),function(j) 
    summary(lm(Ybatch[,j]~x))$coefficient[2,]))
  
  data_batch_vs_noisy<-cbind(estbatch[,c(1,2)],est[,c(1,2)])
  write.table(data_batch_vs_noisy,file=paste0("./data/noisy_",sd,".dat"),
              quote=F,row.names = F,
              col.names = c("beta_o","sd_o","beta_r","sd_r"))
  
}