set.seed(123)
###noisy experiment simulation

beta<-rep(1,1000)
x<-rep(c(0:1),each=200)

xsmall<-rep(c(0:1),each=20)

Y<-sapply(1:length(beta), function(i) x*beta[i]+rnorm(400))
Ysmall<-sapply(1:length(beta), function(i) xsmall*beta[i]+rnorm(40))

est<-t(sapply(1:ncol(Y),function(j)
  summary(lm(Y[,j]~x))$coefficient[2,]))

estnoisy<-t(sapply(1:ncol(Ysmall),function(j)
  summary(lm(Ysmall[,j]~xsmall))$coefficient[2,]))

data_nobatch_vs_noisy<-data.frame(cbind(est[,c(1,2)],estnoisy[,c(1,2)]))
write.table(data_nobatch_vs_noisy,file="noisy.txt",quote=F,row.names = F,
            col.names = c("beta_o","sd_o","beta_r","sd_r"))



###batch experiment simulation
etalist=seq(0.1,1,0.1)

for (i in 1:length(etalist)){
  eta<-etalist[i]
  beta<-rep(1,1000)
  x<-rep(c(0:1),each=200)
  
  batchlabel0<-c(rep(1,40),rep(0,160))
  batchlabel1<-c(rep(1,160),rep(0,40))
  batchlabels<-c(batchlabel0,batchlabel1)
  
  # batch effect --- magnitude is eta
  batcheffect<-c(rnorm(1000,0,eta)) ###rep(sd,1000)
  
  # simulate two studies: 
  # first without batch effects 
  Y<-sapply(1:length(beta), function(i) x*beta[i]+rnorm(400))
 
  # third with batch effects
  Ybatch<-sapply(1:length(beta), function(i) 
    x*beta[i]+batchlabels*batcheffect[i]+rnorm(400))
  
  # Estimation
  est<-t(sapply(1:ncol(Y),function(j)
    summary(lm(Y[,j]~x))$coefficient[2,]))
  
  estbatch<-t(sapply(1:ncol(Ybatch),function(j) 
    summary(lm(Ybatch[,j]~x))$coefficient[2,]))
  
  data_nobatch_vs_batch<-data.frame(cbind(est[,c(1,2)],estbatch[,c(1,2)]))
  write.table(data_nobatch_vs_batch,file=paste0("batch_",eta,".txt"),
              quote=F,row.names = F,
              col.names = c("beta_o","sd_o","beta_r","sd_r"))
}
