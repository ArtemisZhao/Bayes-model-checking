set.seed(123)
or<-c(2/3)

studynum<-c(1,1)
indnum<-c(100, 100)

wipi<-function(p){
  if (p<0.05){return(1)}
  else{return(0)}
}

betafinal<-c()
sdfinal<-c()
rep=5000


for (k in 1:rep){
  for (i in 1:length(or)){ 
    curor<-or[i]
    betalist<-c()
    sdlist<-c()
    for (j in 1:length(studynum)){
      
      cur_snum<-studynum[j]
      cur_indnum<-indnum[j]
      beta_cur_est<-c()
      sd_cur_est<-c()
      tim<-0
      while (tim<cur_snum){
        pcontrol<-runif(1,min=0.3,max=0.5)
        oddscontrol = pcontrol/(1-pcontrol)
        oddstreat= oddscontrol * curor
        ptreat=oddstreat/(1+oddstreat)

        
        control<-rbinom(cur_indnum,1,pcontrol)
        treat<-rbinom(cur_indnum,1,ptreat)
        
        datay<-c(control,treat)
        datax<-c(rep(0,cur_indnum),rep(1,cur_indnum))
        est_p<-summary(glm(datay~datax,
                           family="binomial"))$coefficient[2,c(1,2,4)]
       	beta_cur_est<-c(beta_cur_est,est_p[1])
        sd_cur_est<-c(sd_cur_est,est_p[2])
        tim<-tim+1
      }
      betalist<-c(betalist,beta_cur_est)
      sdlist<-c(sdlist,sd_cur_est)
    }
  }
  betafinal<-rbind(betafinal,betalist)
  sdfinal<-rbind(sdfinal,sdlist)
}




outd = cbind(rep(1:rep), betafinal[,1], sdfinal[,1], betafinal[,2], sdfinal[,2])

write(file="sim_data/sim.control.2grp.dat", t(outd), ncol=5)

