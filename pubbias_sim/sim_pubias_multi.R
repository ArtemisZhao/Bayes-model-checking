#logodds<-c(0.0,-0.405,-0.693,-1.386)
or<-c(2/3)

####10 studies
studynum<-c(5,3,2)
####sample size in each study
indnum<-c(50,200,300)
#nlist<-unlist(sapply(1:length(studynum), function(x) rep(indnum[x],studynum[x])))

####with publication bias
wipi<-function(p){
  return(exp(-4*p^(1.5)))
}

###without publication bias
# wipi<-function(p){
#     return(1)
#  }

betafinal<-c()
sdfinal<-c()
###repeats for 100 times
rep=500
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
        ptreat<-pcontrol*curor
        
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


####comparison with egger regression
library(metafor)
library(egg)
eggerres<-sapply(1:nrow(betafinal),function(x) 
  regtest(betafinal[x,],vi=sdfinal[x,]^2)$pval)
pval_egger<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(beta=betafinal[x,],sd=sdfinal[x,],test = "egger-hetero")$pval)

data<-cbind(egger=eggerres,pos_pval=pval_egger)
data=data.frame(data)
ggplot(data,aes(x=egger,y=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()
#p2=ggplot(data,aes(x=egger))+geom_histogram(color="darkblue",bins=20,fill="white")+xlab("egger_regression_pval")
#p3=ggplot(data,aes(x=pos_pval))+geom_histogram(color="darkblue",bins=20,fill="white")+xlab("Bayes_pval")
#ggarrange(p2,p3,p1,nrow=1)

###comparison with Q test
qtest=sapply(1:nrow(betafinal),function(x) 
  rma.uni(yi=betafinal[x,],sei=sdfinal[x,])$QEp)

pval_Q<-sapply(1:nrow(betafinal),function(x) 
  bayes_posterior_check(beta=betafinal[x,],sd=sdfinal[x,],test = "Q")$pval)

data2<-cbind(qtest=qtest,pos_pval=pval_Q)
data2=data.frame(data2)
ggplot(data2,aes(x=qtest,y=pos_pval))+
  geom_point(color="darkblue",size=0.5)+
  geom_abline(intercept = 0,slope=1)+
  theme_bw()

restest<-rma(yi=betafinal2[1,],sei=sdfinal2[1,])
funnel(restest)

