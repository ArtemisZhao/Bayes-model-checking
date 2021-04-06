data_control=read.table("sim.control.2grp.dat")

pvalcontrol<-sapply(1:nrow(data_control), function(x) 
  bayespval_beta_meta(beta=c(data_control[x,1],data_control[x,3]),
                      sd = c(data_control[x,2],data_control[x,4]),test = "pub_bias",r_vec = 0))

hist(pvalcontrol)


data_case=read.table("sim.pub_bias.2grp.dat")

pvalcase<-sapply(1:nrow(data_case), function(x) 
  bayespval_beta_meta(beta=c(data_case[x,1],data_case[x,3]),
                      sd = c(data_case[x,2],data_case[x,4]),test = "pub_bias",r_vec = 0))

hist(pvalcase)

data_plot<-data.frame(pubbias=rep(c("without","with"),each=5000),rep_pval=c(pvalcontrol,pvalcase))

p<-ggplot(data=data_plot,aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10)+
  facet_grid(.~pubbias,labeller = label_both)+theme_bw()+xlab("Replication p-value")
p