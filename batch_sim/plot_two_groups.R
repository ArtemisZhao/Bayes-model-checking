library(ggplot2)
library(egg)
etalist=seq(0,1,0.1)
n=5000
pval_batch_list <- c()

for (i in 1:length(etalist)){
  eta=etalist[i]
  data<-read.table(paste0("./data_two_groups/","batch_",eta,".dat"),header=T)
  pval_batch<-sapply(1:nrow(data),function(x) bayespval_beta_meta(beta=c(data$beta_o[x],data$beta_r[x]),
                      sd=c(data$sd_o[x],data$sd_r[x]),r_vec=0))

  pval_batch_list<-cbind(pval_batch_list,pval_batch)
}


##pick up some values to plot: 0, 0.2, 0.4, 0.6, 0.8, 1
replication_pval=c(pval_batch_list[,1],pval_batch_list[,3],pval_batch_list[,5],
                   pval_batch_list[,7],pval_batch_list[,9],pval_batch_list[,11])
data_plot<- data.frame(eta=rep(c(0,0.2,0.4,0.6,0.8,1),each =n),rep_pval=replication_pval)

data_plot[,1]<-factor(data_plot[,1])

p2<-ggplot(data=data_plot[0:(2*n),],aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  facet_grid(.~eta,labeller = label_both)+theme_bw()+xlab("")+ylim(0,3100)
#hist(pval_batch_list[,1])

p3<-ggplot(data=data_plot[(2*n+1):(4*n),],aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  facet_grid(.~eta,labeller = label_both)+theme_bw()+xlab("")+ylim(0,3100)

p4<-ggplot(data=data_plot[(4*n+1):(6*n),],aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  facet_grid(.~eta,labeller = label_both)+theme_bw()+xlab("Replication p-value")+ylim(0,3100)

ggarrange(p2,p3,p4,nrow=3)

percentage<-sapply(1:ncol(pval_batch_list),function(x) length(which(pval_batch_list[,x]<0.05))/5000)

data_plot2<-data.frame(eta=etalist,sensitivity=percentage)

ggplot(data=data_plot2,aes(x=etalist,y=sensitivity))+
  geom_line(color="darkblue")+
  geom_point(color="darkblue")+
  theme_bw()+
  xlab("batch magnitude")

