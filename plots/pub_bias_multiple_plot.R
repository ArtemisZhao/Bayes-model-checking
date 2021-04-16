library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
filename = args[1]

c_param = as.numeric(args[2])

d = read.table(filename)
#d=read.table("pubbias_multigrp_cparam_10.prp.out")
d = data.frame(d)

names(d)<-c("V1", "pval_Q","pval_egger")

pdf(file = paste0("output/",filename,"_pval_Q.plot.pdf"),width=4,height=4)

ggplot(data=d,aes(x=pval_Q))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  theme_bw()+
  ggtitle(paste0("test: Q, censoring param: ", c_param))+
  theme(plot.title = element_text(size=12,hjust = 0.5))+
  xlab("Posterior-predictive replication p-value")+ylim(0,4500)
dev.off()

pdf(file = paste0("output/",filename,"_pval_egger.plot.pdf"),width=4,height=4)
ggplot(data=d,aes(x=pval_egger))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  theme_bw()+
  ggtitle(paste0("test: Egger, censoring param: ", c_param))+
  theme(plot.title = element_text(size=12,hjust = 0.5))+
  xlab("Posterior-predictive replication p-value")+ylim(0,4500)
dev.off()