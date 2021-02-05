bayesCI_beta_meta<-function(beta,sd,r_vec = c(0, 1e-5, 6e-3, 0.024)){
  
  beta_o<-beta[1]
  beta_r<-beta[2]
  
  sigma_o<-sd[1]^2
  
  sigma_r<-sd[2]^2
  
  ####old grid
  #eta2_vec = c(sigma_o)
  
  #eta2 = 2*sigma_o
  #while(eta2<= beta_o^2 + sigma_o){
  #  eta2_vec = c(eta2_vec, eta2)
  #  eta2 = 2*eta2
  #}
  
  ####new grid
  eta2_vec = (beta_o^2+sigma_o)*c(1,2,4)
  
  #pv = c(1.0, 0.99, 0.975, 0.95)
  rv = r_vec
  
  make_grid <-function(eta2){
    grid = sapply(rv, function(x)  c(eta2*(1-x), eta2*x))
    return(t(grid))
  }
  
  grid = c()
  
  for (i in 1:length(eta2_vec)){
    grid = rbind(grid, make_grid(eta2_vec[i]))
  }
  
  omg2 = grid[,1]
  phi2 = grid[,2]
  
  mean<-sapply(1:length(omg2),function(x) 
    beta_o/(sigma_o/omg2[x]+phi2[x]/omg2[x]+1))
  
  var<-sapply(1:length(omg2),function(x) 
    (sigma_o+phi2[x])*omg2[x]/(sigma_o+phi2[x]+omg2[x])+phi2[x]+sigma_r)
  
   #pval<-sapply(1:length(mean),function(x) pnorm(beta_r, mean=mean[x],sd=sqrt(var[x]))) 
  #integrate(normal_pdf(t,mean=mean[x],sd=sqrt(var[x])),lower=-Inf,upper=x )
  wts = dnorm(beta_o, mean=0, sd=sqrt(omg2+sigma_o+phi2))
  wts = wts/sum(wts)

  mixture_CDF_right<-Vectorize(function(t) wts%*%pnorm(t,mean=mean,sd=sqrt(var))-0.975)
  root_right<-uniroot(mixture_CDF_right,c(mean(mean),10))$root  
 
   mixture_CDF_left<-Vectorize(function(t) wts%*%pnorm(t,mean=mean,sd=sqrt(var))-0.025)
  root_left<-uniroot(mixture_CDF_left,c(-10,mean(mean)))$root
  return(c(root_left,root_right))
}

d<-read.table("rpp_extract.dat", head=T)
attach(d)
rpp_CI<-sapply(1:nrow(d),function(x) bayesCI_beta_meta(beta=c(beta_orig[x], beta_rep[x]),sd=c(se_orig[x],  se_rep[x])))

rbind(rpp_CI,beta_rep)

