library(mvtnorm)
library(e1071)
bayes_posterior_check<-function(beta,sd2,L=1000,r_vec = c(1e-5, 6e-3, 0.024),test="skew",print_test_dist=FALSE){
    
  m<-length(beta)  ###number of replicates
  
  chis1<-qchisq(c(0.25,0.5,0.75),df=1)
  eta2_vec = c(min(beta)^2/chis1,mean(beta)^2/chis1)
  
  rv = r_vec
  
  make_grid <-function(eta2){
    grid = sapply(rv, function(x)  c(eta2*(1-x), eta2*x))
    return(t(grid))
  }
  
  grid = c()
  
  for (i in 1:length(eta2_vec)){
    grid = rbind(grid, make_grid(eta2_vec[i]))
  }
  
  omg2_list = grid[,1]
  phi2_list = grid[,2]
  
  wts<-c()
  count=0
  for (i in 1:length(omg2_list)){
    omg2<-omg2_list[i]
    phi2<-phi2_list[i]
    Sigma<-matrix(omg2,ncol=m,nrow=m)+diag(c(sd2+phi2),nrow=m)
    wtsi<-dmvnorm(beta, mean=rep(0,m), sigma=Sigma)
    wts<-c(wts,wtsi)
  }
  wts<-wts/sum(wts)
  
  
  dist_list<-c()
  
  for (t in 1:L){
  k<-sample(1:length(omg2_list),1,prob=wts)
  
  phi2<-phi2_list[k]
  omg2<-omg2_list[k]
  
  barbeta_pos_var<-1/(1/omg2+sum(1/(sd2+phi2)))  
  barbeta_pos_mean<-barbeta_pos_var*sum(beta/(sd2+phi2))
  barbeta<-rnorm(1,barbeta_pos_mean,sqrt(barbeta_pos_var)) 

  betanewjs<-c()
  tnewjs<-c()
  for (j in 1:m){
    #print(c(i,j))
    betaj_var<-1/(1/phi2+1/sd2[j])
    betaj_mean<-betaj_var*(barbeta/phi2+beta[j]/sd2[j])
    betaj<-rnorm(1,betaj_mean,sqrt(betaj_var))
    #print(betaj)
    
    betanewj = betaj+rnorm(1,0,sqrt(sd2[j]))
    betanewjs<-c(betanewjs,betanewj)
  }
  ###test 4: Cochran's Q test
  if (test == "Q"){
    q = sum((betanewjs - mean(betanewjs))^2 / (sd2 + phi2))
    q_orig = sum((beta - mean(beta))^2 / (sd2 + phi2))
    dist_list<- c(dist_list,q)
    count = count + (q>q_orig)
  }
  ####test statistics 3 egger regression with heterogeneous param
  if (test == "egger-hetero"){
    y = betanewjs / sqrt(sd2 + phi2)
    x = 1 / sqrt(sd2 + phi2)
    a = abs(summary(lm(y ~ x))$coefficients[1,1])
    dist_list = c(dist_list,a)
    
    y_orig = beta / sqrt(sd2 + phi2)
    x_orig = 1 / sqrt(sd2 + phi2)
    com = abs(summary(lm(y_orig~x_orig))$coefficients[1,1])
    
    count = count + (a>com)
  }
  ###test statistics 2 skewness:
  if (test=="skew"){
    y = betanewjs / sqrt(sd2 + phi2)
    x = 1 / sqrt(sd2 + phi2)
    muhat = summary(lm(y ~ x))$coefficients[2,1]
    dis = (betanewjs - muhat)/sqrt(sd2+phi2)
    skew<-abs(skewness(dis))
    dist_list = c(dist_list,skew)
    
    y_orig = beta / sqrt(sd2 + phi2)
    x_orig = 1 / sqrt(sd2 + phi2)
    muhat = summary(lm(y_orig ~ x_orig))$coefficients[2,1]
    dis = (beta - muhat)/sqrt(sd2+phi2)
    com = abs(skewness(dis))
    count=count+(skew>com)
  }
  if (test == "diff")
  { ###test statistics 1 naive: 
    ###max min difference
  dist<-max(betanewjs)-min(betanewjs)
  dist_list<-c(dist_list,dist)
  com = max(beta)-min(beta)
  count = count+(dist>com)
  }
}
 if (print_test_dist){
    print(length(dist_list))
    hist(dist_list)
  }
  
  return( count/L)
}


set.seed(123)
sim_data<-function(k, omg, n=5, sd=1){
  beta = rnorm(1,mean=0,sd=omg)
  
  bv = rnorm(n, mean = beta, sd=k*abs(beta))
  
  zv = rnorm(n, mean = bv, sd = sd)
  
  return(matrix(zv,nrow=1))
}

#null_data = t(sapply(1:4000, function(x) sim_data(k=0,omg=0,r=0.6)))
fix_data = t(sapply(1:1000, function(x) sim_data(k=0, omg=1, n=10)))
rep_data = t(sapply(1:1000, function(x) sim_data(k=0.1, omg=1, n=10)))
irr_data = t(sapply(1:1000,  function(x) sim_data(k=3, omg=1, n=10)))

fix_p<-sapply(1:1000, function(x) bayes_posterior_check(beta=fix_data[x,],sd2=rep(1,10),test="diff"))

rep_p<-sapply(1:1000, function(x) bayes_posterior_check(beta=rep_data[x,],sd2=rep(1,10),test="diff"))

irr_p<-sapply(1:1000, function(x) bayes_posterior_check(beta=irr_data[x,],sd2=rep(1,10),test="diff"))

pp1<-data.frame(bayes_pval=fix_p)
#p1<-ggplot(pp1,aes(x=bayes_pval))+geom_histogram(color="black",fill="white")
pp2<-data.frame(bayes_pval=rep_p)
#p2<-ggplot(pp2,aes(x=bayes_pval))+geom_histogram(color="black",fill="white")
pp3<-data.frame(bayes_pval=irr_p)
#p3<-ggplot(pp3,aes(x=bayes_pval))+geom_histogram(color="black",fill="white")
#ggarrange(p1,p2,p3,ncol=3,labels=c("fix","rep","irre"),
#         label.args = list(gp = grid::gpar(font = 2, cex =0.8)))
data<-data.frame(type=c(rep("fix",1000),rep("rep",1000),rep("irre",1000)),bayes_pval=rbind(pp1,pp2,pp3))
data[,1]<-factor(data[,1],levels=c("fix","rep","irre"))
plot1<-ggplot(data=data,aes(x=bayes_pval))+
  geom_histogram(color="black",fill="white")+
  facet_grid(.~type)+theme_bw()
plot1


