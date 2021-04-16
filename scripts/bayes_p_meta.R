###prior_prp
prior_prp<-function(beta,se,r_vec = c(0, 1e-5, 6e-3, 0.024),test="two_sided",report_CI=F){
  reslist<-list()
  beta_o<-beta[1]
  beta_r<-beta[2]
  
  se2_o<-se[1]^2
  
  se2_r<-se[2]^2
  
  ####new grid
  #chis1<-qchisq(c(0.25,0.5,0.75),df=1)
  #eta2_vec = beta_o^2/chis1
  eta2_vec = (se2_o+beta_o^2)/qchisq(c(0.25, 0.5, 0.75), df=1)
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
  reslist[["grid"]] = grid
  
  omg2 = grid[,1]
  phi2 = grid[,2]

  mean<-sapply(1:length(omg2),function(x) 
    beta_o/(se2_o/omg2[x]+phi2[x]/omg2[x]+1))
  
  var<-sapply(1:length(omg2),function(x) 
    (se2_o+phi2[x])*omg2[x]/(se2_o+phi2[x]+omg2[x])+phi2[x]+se2_r)
  

  pval<-sapply(1:length(mean),function(x) pnorm(beta_r, mean=mean[x],sd=sqrt(var[x]))) 
  
  wts = dnorm(beta_o, mean=0, sd=sqrt(omg2+se2_o+phi2))
  wts = wts/sum(wts)
  
  pval_wt<-wts%*%pval
  
  res = NA
  reslist["test_statistics"]=test
  if (test=="pub_bias"){
    mean_scale=mean/beta_o
    sd_scale=sqrt(var)/abs(beta_o)
    pval_new<-sapply(1:length(mean),function(x) pnorm(beta_r/beta_o, mean=mean_scale[x],sd=sd_scale[x])) 
    res = wts%*%pval_new
    reslist[["pvalue"]]=res
  }
  if (test=="two_sided"){
    res = 2*min(pval_wt,1-pval_wt)
    reslist[["pvalue"]]=res
    if (report_CI){
       mixture_CDF_right<-Vectorize(function(t) wts%*%pnorm(t,mean=mean,sd=sqrt(var))-0.975)
       root_right<-uniroot(mixture_CDF_right,c(mean(mean),10))$root  
    
       mixture_CDF_left<-Vectorize(function(t) wts%*%pnorm(t,mean=mean,sd=sqrt(var))-0.025)
       root_left<-uniroot(mixture_CDF_left,c(-10,mean(mean)))$root
    
       reslist[["CI"]]=c(CI_left=root_left,CI_right=root_right)
       }
  }
  return(reslist)
}


# d<-read.table("rpp_extract_new.dat", head=T)
# attach(d)
# rpp_data<-d[,c("beta_orig","se_orig","beta_rep","se_rep")]
# rpp_pval<-sapply(1:nrow(d),function(x) 
# bayespval_beta_meta(beta=c(beta_orig[x], beta_rep[x]),sd=c(se_orig[x],  se_rep[x]),report_CI=T))


