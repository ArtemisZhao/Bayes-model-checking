#####bayes p for CEFN 

#####################CEFN model
integrate1<-function(x,k,omega,beta_o,sigma_o){
  nom1<-(1/sqrt(2*pi*omega^2))*exp(-x^2/(2*omega^2))
  med<-k^2*x^2+sigma_o^2
  nom2<-(1/sqrt(2*pi*med))*exp(-(beta_o-x)^2/(2*med))
  return(nom1*nom2)
}

posterior_barbeta<-function(barbeta,k,omega,beta_o,sigma_o){
  nom1<-(1/sqrt(2*pi*omega^2))*exp(-barbeta^2/(2*omega^2))
  med<-k^2*barbeta^2+sigma_o^2
  nom2<-(1/sqrt(2*pi*med))*exp(-(beta_o-barbeta)^2/(2*med))
  denom<-integrate(integrate1,-Inf,Inf,k=k,
                   omega=omega,beta_o=beta_o,sigma_o=sigma_o)
  return(nom1*nom2/denom$value)
}

integrate2<-function(x,betar,k,omega,beta_o,sigma_o){
  val<-(1/sqrt(2*pi*k^2*x^2))*posterior_barbeta(x,k,omega,beta_o,sigma_o)*exp(-(betar-x)^2/(2*k^2*x^2))
  return(val)
}

posterior_betar<-function(betar,k,omega,beta_o,sigma_o){
  val<-integrate(integrate2,-Inf,Inf,betar=betar,k=k,omega=omega,beta_o=beta_o,sigma_o=sigma_o)
  return(val$value)
}

integrate3<-function(x,betahat,k,omega,beta_o,sigma_o,sigma_r){
  val= (1/sqrt(2*pi*sigma_r^2))*posterior_betar(x,k,omega,beta_o,sigma_o)* exp(-(betahat-x)^2/(2*sigma_r^2))
  return(val)
}

posterior_betahat<-function(betahat,k,omega,beta_o,sigma_o,sigma_r){
  tol=1e-8
  #val<-integrate(Vectorize(integrate3),-Inf,Inf,betahat=betahat,k=k,omega=omega,beta_o=beta_o,
                 #sigma_o=sigma_o,sigma_r=sigma_r)
  val<-integrate(Vectorize(integrate3),-Inf,Inf,stop.on.error = F,betahat=betahat,k=k,omega=omega,beta_o=beta_o,
         sigma_o=sigma_o,sigma_r=sigma_r)
  return(val$value)
}

bayespval_cefn<-function(data){
  print(1)
  beta_o<-data[,1]
  sigma_o<-data[,2]
  beta_r<-data[,3] ###target
  sigma_r<-data[,4]
  k<-c(0.462)
  omega=2
  pval<-integrate(Vectorize(posterior_betahat),beta_r,Inf,k=k,omega=omega,beta_o=beta_o,sigma_o=sigma_o,sigma_r=sigma_r)$value
  return(min(pval,1-pval))
}

rpp_pval<-sapply(1:17,function(x) bayespval_cefn(data=rpp_data[x,]))

rpp_pval18<-bayespval_cefn(data=rpp_data[18,])
rpp_pval2<-sapply(19:38,function(x) bayespval_cefn(data=rpp_data[x,]))

rpp_pval3<-sapply(39:60,function(x) bayespval_cefn(data=rpp_data[x,]))

rpp_pval4<-sapply(61:72,function(x) bayespval_cefn(data=rpp_data[x,]))
rpp_pval73<-bayespval_cefn(data=rpp_data[73,])
rpp_pval5<-sapply(74:nrow(rpp_data),function(x) bayespval_cefn(data=rpp_data[x,]))


rpp_pval_cefn<-c(rpp_pval,rpp_pval18,rpp_pval2,rpp_pval3,rpp_pval4
                 ,rpp_pval73,rpp_pval5)

plot(rpp_pval_cefn,rpp_pval_twoside/2)

