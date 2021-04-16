library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
filename = args[1]
bb = args[2]

d = read.table(filename)
#d = read.table("batch_2gp_bb_sd_0.2.prp.out")

d=data.frame(d)
names(d)<-c("V1", "rep_pval")

if (grepl("2gp",filename)){
  type="Prior-predictive replication p-value"
}
if(grepl("multigrp",filename) & grepl("prp",filename)){
  type="Posterior-predictive replication p-value"
}
if(grepl("multigrp",filename) & grepl("classic",filename)){
  type="Cochran's Q test p-value"
}

pdf(file = paste0("output/",filename,".plot.pdf"),width=4,height=4)
#pdf(file="~/Desktop/plot.pdf",width=4,height=4)

ggplot(data=d,aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  ggtitle(paste0("batch magnitude: ", bb))+
  theme_bw()+
  theme(plot.title = element_text(size=12,hjust = 0.5))+
  xlab(paste0(type))+
  ylim(0,4500)

dev.off()