library(mvtnorm)

bayes_posterior_check<-function(data,L=1000,r_vec = c(0, 1e-5, 6e-3, 0.024)){
    
  m<-ncol(data)/2  ###number of replicates
  beta<-c() 
  sd2<-c()
  
  for (i in 1:m){
    beta<-c(beta,data[,2*i-1])
    sd2<-c(sd2,data[,2*i]^2)
  }
  
  
  eta2_vec = (min(beta)^2+min(sd2))*c(1,2,4)
  
  #eta2_vec = c(1.0, 2.0, 4.0, 8.0)
  ######no p=1
  #pv = c( 0.99, 0.975, 0.95)
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
  for (i in 1:length(omg2_list)){
    omg2<-omg2_list[i]
    phi2<-phi2_list[i]
    Sigma<-matrix(omg2,ncol=m,nrow=m)+diag(c(sd2+phi2),nrow=m)
    wtsi<-dmvnorm(beta, mean=rep(0,m), sigma=Sigma)
    wts<-c(wts,wtsi)
  }
  wts<-wts/mean(wts)
  
  simbeta<-c()
  dist_list<-c()
  for (t in 1:L){
  k<-sample(1:length(omg2_list),1,prob=wts)
  
  phi2<-phi2_list[k]
  omg2<-omg2_list[k]
  barbeta_pos_var<-1/(1/omg2+sum(1/(sd2+phi2)))  
  barbeta_pos_mean<-barbeta_pos_var*sum(beta/(sd2+phi2))
  barbeta<-rnorm(1,barbeta_pos_mean,sqrt(barbeta_pos_var)) 
  
  betanewjs<-c()
  for (j in 1:m){
    #print(c(i,j))
    betaj_var<-1/(1/phi2+1/sd2[j])
    betaj_mean<-betaj_var*(barbeta/phi2+beta[j]/sd2[j])
    betaj<-rnorm(1,betaj_mean,sqrt(betaj_var))
    #print(betaj)
    betanewjs<-c(betanewjs,betaj+rnorm(1,0,sqrt(sd2[j])))
  }
  
  ###test statistics : max-mean
  dist<-max(betanewjs)-mean(betanewjs)
  simbeta<-rbind(simbeta,betanewjs)
  dist_list<-c(dist_list,dist)
  }
  com<-max(beta)-mean(beta)
 
  return( length(which(dist_list>com))/L)
}

