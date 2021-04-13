###Application for posterior checking
####Data generation
library(metafor)
calculate_sd<-function(rr,rr_left){
  sd=(log(rr)-log(rr_left))/1.96
  beta=log(rr)
  return(c(beta,sd))
}

data_cereb= matrix(c(1.01,0.30,
        3.43,1.35,
        2.23,1.61,
        2.65,1.89,
        2.13,0.82),ncol=2,byrow=T)

data_cereb_trans=t(sapply(1:nrow(data_cereb),function(x) calculate_sd(rr=data_cereb[x,1],rr_left=data_cereb[x,2])))

res1<-rma(yi=data_cereb_trans[,1],vi=data_cereb_trans[,2]^2)
funnel(res1)
data_cereb_severe=matrix(c(1.71,0.73,
                           0.80,0.34,
                           7.93,4.22,
                           1.15,0.80,
                           1.22,0.58,
                           1.30,0.53,
                           3.43,2.30),ncol=2,byrow=T)
data_cereb_severe_trans=t(sapply(1:nrow(data_cereb_severe),function(x) calculate_sd(rr=data_cereb_severe[x,1],rr_left=data_cereb_severe[x,2])))
res2<-rma(yi=data_cereb_severe_trans[,1],vi=data_cereb_severe_trans[,2]^2)
funnel(res2)


data_cardi=matrix(c(3.02,0.71,
                           2.43,1.04,
                           4.16,1.75,
                           1.80,1.32,
                           0.72,0.19,
                           2.92,1.44),ncol=2,byrow=T)
data_cardi_trans=t(sapply(1:nrow(data_cardi),function(x) calculate_sd(rr=data_cardi[x,1],rr_left=data_cardi[x,2])))
res3<-rma(yi=data_cardi_trans[,1],vi=data_cardi_trans[,2]^2)
funnel(res3)


data_cardi_severe=matrix(c(4.06,1.73,
                    2.77,1.13,
                    1.15,0.80,
                    3.23,2.13,
                    1.61,1.15,
                    2.80,1.80),ncol=2,byrow=T)
data_cardi_severe_trans=t(sapply(1:nrow(data_cardi_severe),function(x) calculate_sd(rr=data_cardi_severe[x,1],rr_left=data_cardi_severe[x,2])))

res4<-rma(yi=data_cardi_severe_trans[,1],vi=data_cardi_severe_trans[,2]^2)
funnel(res4)

cereb = bayes_posterior_check(beta=data_cereb_trans[,1],sd=data_cereb_trans[,2], test = "egger-hetero",print_test_dist = T)

cereb_severe = bayes_posterior_check(beta=data_cereb_severe_trans[,1],sd=data_cereb_severe_trans[,2],test = "egger-hetero",print_test_dist = T)

cardi = bayes_posterior_check(beta=data_cardi_trans[,1],sd=data_cardi_trans[,2], test = "egger-hetero",print_test_dist = T)

cardi_severe = bayes_posterior_check(beta=data_cardi_severe_trans[,1],sd=data_cardi_severe_trans[,2], test = "egger-hetero",print_test_dist = T)


#######forest plot for data_cardi_trans
study_names<-c("Akbari 2020","Bai T 2020","Cao J 2020","Chen T 2020","Fu L 2020","Yuan M 2020")
data_cardi1<-data.frame(data_cardi_trans)
data_cardi1<-cbind(study_names,data_cardi1)
names(data_cardi1)<-c("Study","beta","sd")

p <- ggplot(data_cardi1)+
  geom_pointrange(aes(x=Study, y=beta, ymin=beta-1.96*sd, ymax=beta+1.96*sd),color="darkblue",shape=3,fatten=2)+
  geom_hline(yintercept = 0, linetype=2)+
  labs( x = "Study", y = "95% CI for log risk ratio") +
  coord_flip()+
  theme_bw()

p


########forest plot for data_cardi_severe_trans
study_names2<-c("Li Q 2020","Liu Jingyuan 2020","Qin 2020","Wan 2020","Wang Dan 2020","Zhang Guqin 2020")
data_cardi2<-data.frame(data_cardi_severe_trans)
data_cardi2<-cbind(study_names2,data_cardi2)
names(data_cardi2)<-c("Study","beta","sd")

p2 <- ggplot(data_cardi2)+
  geom_pointrange(aes(x=Study, y=beta, ymin=beta-1.96*sd, ymax=beta+1.96*sd),color="darkblue",shape=3,fatten=2)+
  geom_hline(yintercept = 0, linetype=2)+
  labs( x = "Study", y = "95% CI for log risk ratio") +
  coord_flip()+
  theme_bw()

p2