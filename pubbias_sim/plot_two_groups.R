data_control=read.table("./data_two_groups/sim.control.2grp.dat")

pvalcontrol<-sapply(1:nrow(data_control), function(x) 
  prior_prp(beta=c(data_control[x,1],data_control[x,3]),
                      sd = c(data_control[x,2],data_control[x,4]),test = "pub_bias"))
pvalcontrol2<-sapply(1:nrow(data_control), function(x) 
  prior_prp(beta=c(data_control[x,1],data_control[x,3]),
            sd = c(data_control[x,2],data_control[x,4])))
hist(pvalcontrol2)
data_case=read.table("./data_two_groups/sim.pub_bias.2grp.dat")

pvalcase<-sapply(1:nrow(data_case), function(x) 
  prior_prp(beta=c(data_case[x,1],data_case[x,3]),
                      sd = c(data_case[x,2],data_case[x,4]),test = "pub_bias"))
pvalcase2<-sapply(1:nrow(data_case), function(x) 
  prior_prp(beta=c(data_case[x,1],data_case[x,3]),
            sd = c(data_case[x,2],data_case[x,4]),r_vec = 0))

#hist(pvalcase2)

data_plot<-data.frame(pub_bias=rep(c("without","with"),each=10000),
                      test_statistics=rep(c("two_sided","pub_bias","two_sided","pub_bias"),each=5000),
                      rep_pval=c(pvalcontrol2,pvalcontrol,pvalcase2,pvalcase))

p<-ggplot(data=data_plot,aes(x=rep_pval))+
  geom_histogram(color="darkblue",fill="white",bins=10,boundary=1)+
  scale_x_continuous(limits=c(0,1))+
  facet_grid(pub_bias~test_statistics,labeller = label_both)+theme_bw()+xlab("Replication p-value")
p