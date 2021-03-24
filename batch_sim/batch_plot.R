library(ggplot2)
library(egg)
etalist=seq(0.2,1,0.2)

pval_batch_list <- c()

for (i in 1:length(etalist)){
  eta=etalist[i]
  data<-read.table(paste0("batch_",eta,".dat"),header=T)
  pval_batch<-sapply(1:nrow(data),function(x) bayespval_beta_meta(beta=c(data$beta_o[x],data$beta_r[x]),
                      sd=c(data$sd_o[x],data$sd_r[x])))

  pval_batch_list<-cbind(pval_batch_list,pval_batch)
}


##pick up some values to plot: 0.2, 0.4, 0.6, 0.8
replication_pval=c(pval_batch_list[,1],pval_batch_list[,2],pval_batch_list[,3],pval_batch_list[,4])
data_plot<- data.frame(eta=rep(c(0.2,0.4,0.6,0.8),each=1000),rep_pval=replication_pval)

data_plot[,1]<-factor(data_plot[,1])
p2<-ggplot(data=data_plot,aes(x=rep_pval))+
  geom_histogram(color="black",fill="white")+
  facet_grid(.~eta,labeller = label_both)+theme_bw()
p2