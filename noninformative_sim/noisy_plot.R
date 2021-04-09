library(ggplot2)
library(egg)
library(ggplot2)
sdlist=c(2,5,10,20)
n=5000
pval_noisy_list <- c()

for (i in 1:length(sdlist)){
  sd=sdlist[i]
  data<-read.table(paste0("./data/noisy_",sd,".dat"),header=T)
  pval_noisy<-sapply(1:nrow(data),function(x) 
    bayespval_beta_meta(beta=c(data$beta_o[x],data$beta_r[x]),
                        sd=c(data$sd_o[x],data$sd_r[x]),r_vec = c(0)))
  
  pval_noisy_list<-cbind(pval_noisy_list,pval_noisy)
}


##pick up some values to plot: 2,5 10
replication_pval=c(pval_noisy_list[,1],pval_noisy_list[,2],pval_noisy_list[,3],pval_noisy_list[,4])
data_plot<- data.frame(se=rep(c(2,5,10,20),each=n),rep_pval=replication_pval)

data_plot[,1]<-factor(data_plot[,1])
p3<-ggplot(data=data_plot[0:(2*n),],aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  ylim(0,1600)+
  facet_grid(.~se,labeller = label_both)+theme_bw()+xlab("")
p4<-ggplot(data=data_plot[(2*n+1):(4*n),],aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+ylim(0,1600)+
  scale_x_continuous(limits = c(0, 1))+
  facet_grid(.~se,labeller = label_both)+theme_bw()+xlab("Replication p-value")
ggarrange(p3,p4,nrow=2)

sensitivity<-sapply(1:ncol(pval_noisy_list),function(x) length(which(pval_noisy_list[,x]<0.05))/n)

data_plot2<-data.frame(sd=sdlist,sensitivity=sensitivity)

ggplot(data=data_plot2,aes(x=sd,y=sensitivity))+
  geom_line(color="darkblue")+
  geom_point(color="darkblue")+
  theme_bw()+
  xlab("noise magnitude")



