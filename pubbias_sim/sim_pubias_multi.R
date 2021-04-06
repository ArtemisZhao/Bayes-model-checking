#logodds<-c(0.0,-0.405,-0.693,-1.386)
set.seed(123)
or<-c(2/3)

##### p/(1-p) * or  = pcase/(1-pcase)

####10 studies
studynum<-c(5,3,2)
####sample size in each study
indnum<-c(50,200,300) ###50 400 1000
#nlist<-unlist(sapply(1:length(studynum), function(x) rep(indnum[x],studynum[x])))

####with publication bias
#### stronger censoring!
wipi<-function(p){
  return(exp(-15*p^(1.5)))
}

###without publication bias
# wipi<-function(p){
#     return(1)
#  }

betafinal<-c()
sdfinal<-c()
###repeats for 100 times
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
        
        #ptreat<-pcontrol*curor
        
        control<-rbinom(cur_indnum,1,pcontrol)
        treat<-rbinom(cur_indnum,1,ptreat)
        
        datay<-c(control,treat)
        datax<-c(rep(0,cur_indnum),rep(1,cur_indnum))
        est_p<-summary(glm(datay~datax,
                           family="binomial"))$coefficient[2,c(1,2,4)]
        if (rbinom(1,1,wipi(est_p[3])) == 1){
          beta_cur_est<-c(beta_cur_est,est_p[1])
          sd_cur_est<-c(sd_cur_est,est_p[2])
          tim<-tim+1
        }
      }
      betalist<-c(betalist,beta_cur_est)
      sdlist<-c(sdlist,sd_cur_est)
    }
 }
  betafinal<-rbind(betafinal,betalist)
  sdfinal<-rbind(sdfinal,sdlist)
}

save(betafinal,file="./data_multiple_groups/sim.pub_bias.multiple.beta.RData")
save(sdfinal,file="./data_multiple_groups/sim.pub_bias.multiple.sd.RData")