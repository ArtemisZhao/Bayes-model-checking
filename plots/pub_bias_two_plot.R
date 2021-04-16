library(ggplot2)

args = commandArgs(trailingOnly=TRUE)
filename = args[1]
p_thresh = as.numeric(args[2])

filename="pubbias_2grp_pthresh_0.01.prp.out"
p_thresh=0.01
d = read.table(filename)

d = data.frame(d)

names(d)<-c("V1", "default","pub_bias")

pdf(file = paste0("output/",filename,"_default.plot.pdf"),width=4,height=4)
#pdf(file = "~/Desktop/plot.pdf",width=4,height=4)
ggplot(data=d,aes(x=default))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  theme_bw()+
  ggtitle(paste0("test: default, p-value censoring: ", p_thresh))+
  theme(plot.title = element_text(size=12,hjust = 0.5))+
  xlab("Prior-predictive replication p-value")+ylim(0,4500)

dev.off()

pdf(file = paste0("output/",filename,"_pub_bias.plot.pdf"),width=4,height=4)

ggplot(data=d,aes(x=pub_bias))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=0)+
  scale_x_continuous(limits = c(0, 1))+
  theme_bw()+
  ggtitle(paste0("test: pub_bias, p-value censoring: ", p_thresh))+
  theme(plot.title = element_text(size=12,hjust = 0.5))+
  xlab("Prior-predictive replication p-value")+ylim(0,4500)

dev.off()