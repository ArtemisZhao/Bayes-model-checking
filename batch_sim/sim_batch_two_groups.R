set.seed(123)

###batch experiment simulation
##batch magnitude
etalist=seq(0,1,0.1)
###repeats
n=5000
### number of individuals in each group
n_arms = 100
for (i in 1:length(etalist)){

  eta<-etalist[i]
  beta<-rep(1,n) ##fixed at 1
  x<-rep(c(0:1),each=n_arms)
  
  batchlabel0<-c(rep(1,n_arms*0.2),rep(0,n_arms*0.8))
  batchlabel1<-c(rep(1,n_arms*0.8),rep(0,n_arms*0.2))
  batchlabels<-c(batchlabel0,batchlabel1)
  
  # batch effect --- magnitude is eta
  batcheffect<-c(rnorm(n,0,eta)) ###rep(sd,1000)
  
  # simulate two studies: 
  # first with batch effects 
  Ybatch<-sapply(1:length(beta), function(i) 
    x*beta[i]+batchlabels*batcheffect[i]+rnorm(n_arms*2))
 
  # second without batch effects
  Y<-sapply(1:length(beta), function(i) x*beta[i]+rnorm(n_arms*2))
  
  # Estimation
  est<-t(sapply(1:ncol(Y),function(j)
    summary(lm(Y[,j]~x))$coefficient[2,]))
  
  estbatch<-t(sapply(1:ncol(Ybatch),function(j) 
    summary(lm(Ybatch[,j]~x))$coefficient[2,]))
  
  data_batch_vs_nobatch<-data.frame(cbind(estbatch[,c(1,2)],est[,c(1,2)]))
  write.table(data_batch_vs_nobatch,file=paste0("batch_",eta,".dat"),
              quote=F,row.names = F,
              col.names = c("beta_o","sd_o","beta_r","sd_r"))
}




