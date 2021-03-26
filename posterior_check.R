library(mvtnorm)
library(e1071)
bayes_posterior_check<-function(beta,sd,L=1000,r_vec = c(1e-5, 6e-3, 0.024),test="diff",print_test_dist=FALSE){
  res<-list()
  sd2=sd^2
  m<-length(beta)  ###number of replicates
  
  #chis1<-qchisq(c(0.25,0.5,0.75),df=1)
  ## test1 normal
  #eta2_vec = c(mean(beta),min(beta),max(beta))^2
  
  ## test 2 weighted average
  
  wts = 1/sd2
  center = sum(wts*beta)/sum(wts)
  eta2_vec = c(center, center-sqrt(1/sum(wts)),center+sqrt(1/sum(wts)))^2
   
   ##test 3 
  #eta2_vec = c(center)^2
  res[["eta_grid"]] = eta2_vec
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
  dist_list2<-c()
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
    dist_list2<-c(dist_list2,q-q_orig)
    count = count + (q>q_orig)
  }
  ####test statistics 3 egger regression with heterogeneous param
  else if (test == "egger-hetero"){
    y = betanewjs / sqrt(sd2 + phi2)
    x = 1 / sqrt(sd2 + phi2)
    a = abs(summary(lm(y ~ x))$coefficients[1,1])
    dist_list = c(dist_list,a)
   
    y_orig = beta / sqrt(sd2 + phi2)
    x_orig = 1 / sqrt(sd2 + phi2)
    com = abs(summary(lm(y_orig~x_orig))$coefficients[1,1])
    dist_list2= c(dist_list2,a-com)
    count = count + (a>com)
  }
  ###test statistics 2 skewness:
  else if (test=="skew"){
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
    dist_list2= c(dist_list2,skew-com)
    count=count+(skew>com)
  }
  else if (test == "diff")
  { ###test statistics 1 naive: 
    ###max min difference
  dist<-max(betanewjs)-min(betanewjs)
  dist_list<-c(dist_list,dist)
  com = max(beta)-min(beta)
  dist_list2= c(dist_list2,dist-com)
  count = count+(dist>com)
  }
  else{
    dist = test(betanewjs) ####what about phi,omega?
    com = test(beta)
    dist_list2<-c(dist_list2,dist-com)
    count = count+(dist>com)
  }
}
 if (print_test_dist){
    #print(length(dist_list))
    hist(dist_list2)
 }
  res[["n_sim"]]=L
  res[["test_stats_dif"]]=dist_list2
  res[["pval"]]=count/L
  res[["test"]]=test
  return( res)
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

fix_p<-sapply(1:1000, function(x) bayes_posterior_check(beta=fix_data[x,],sd=rep(1,10),test="Q"))

rep_p<-sapply(1:1000, function(x) bayes_posterior_check(beta=rep_data[x,],sd=rep(1,10),test="Q"))

irr_p<-sapply(1:1000, function(x) bayes_posterior_check(beta=irr_data[x,],sd=rep(1,10),test="Q"))




