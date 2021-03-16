library(ggplot2)
library(egg)
sdlist=c(2,5,10)

pval_noisy_list <- c()

for (i in 1:length(sdlist)){
  sd=sdlist[i]
  data<-read.table(paste0("noisy_",sd,".dat"),header=T)
  pval_noisy<-sapply(1:nrow(data),function(x) bayespval_beta_meta(beta=c(data$beta_o[x],data$beta_r[x]),
                                                                  sd=c(data$sd_o[x],data$sd_r[x])))
  
  pval_noisy_list<-cbind(pval_noisy_list,pval_noisy)
}


##pick up some values to plot: 2,5 10
replication_pval=c(pval_noisy_list[,1],pval_noisy_list[,2],pval_noisy_list[,3])
data_plot<- data.frame(sd=rep(c(2,5,10),each=1000),rep_pval=replication_pval)

data_plot[,1]<-factor(data_plot[,1])
p3<-ggplot(data=data_plot,aes(x=rep_pval))+
  geom_histogram(color="black",fill="white")+
  facet_grid(.~sd,labeller = label_both)+theme_bw()
p3