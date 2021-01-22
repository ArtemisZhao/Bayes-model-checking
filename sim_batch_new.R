set.seed(123)


sd=0.5
beta1<-rep(0,1000)
x<-rep(c(0:1),each=200)
xsmall<-rep(c(0:1),each=20)

batchlabel0<-c(rep(1,40),rep(0,160))
batchlabel1<-c(rep(1,160),rep(0,40))
batchlabels<-c(batchlabel0,batchlabel1)


# small batch effect
batcheffect<-c(rnorm(1000,0,sd))

# simulate two studies: first without batch effects second with batch effects
Y<-sapply(1:length(beta1), function(i) x*beta1[i]+rnorm(400))

Ysmall<-sapply(1:length(beta1), function(i) xsmall*beta1[i]+rnorm(40))

Ybatch<-sapply(1:length(beta1), function(i) 
  x*beta1[i]+batchlabels*batcheffect[i]+rnorm(400))

# Estimation

est<-t(sapply(1:ncol(Y),function(j)
  summary(lm(Y[,j]~x))$coefficient[2,]))

estnoisy<-t(sapply(1:ncol(Ysmall),function(j)
  summary(lm(Ysmall[,j]~xsmall))$coefficient[2,]))

estbatch<-t(sapply(1:ncol(Ybatch),function(j) 
  summary(lm(Ybatch[,j]~x))$coefficient[2,]))