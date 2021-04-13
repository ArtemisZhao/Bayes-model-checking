
res<-c()
phi_list = seq(0,1,0.1)
for (phi in phi_list){
  load(file=paste0("./data_multiple_groups/beta_list_",phi,".RData"))
  load(file=paste0("./data_multiple_groups/sd_list_",phi,".RData"))
               
  pos_pval_diff<-sapply(1:nrow(beta_list),function(x) 
    bayes_posterior_check(beta=beta_list[x,],sd=sd_list[x,],r_vec=c(0),test = "Q")$pval)
  res<-cbind(res,pos_pval_diff)
}


resnew<-cbind(phi=rep(c(0,0.2,0.4,0.6,0.8,1),each=n),pval=matrix(res[,c(1,3,5,7,9,11)],ncol=1))
resnew<-data.frame(resnew)
names(resnew)<-c("phi","bayes_pval")

p2<-ggplot(data=resnew[1:(2*n),],aes(x=bayes_pval))+
  geom_histogram(color="darkblue",fill="white",bins = 10,boundary=0)+
  scale_x_continuous(limits=c(0,1))+
  facet_grid(.~phi,labeller = label_both)+theme_bw()+xlab("")+ylim(0,4200)
p3<-ggplot(data=resnew[(2*n+1):(4*n),],aes(x=bayes_pval))+
  geom_histogram(color="darkblue",fill="white",bins = 10,boundary=0)+
  scale_x_continuous(limits=c(0,1))+
  facet_grid(.~phi,labeller = label_both)+theme_bw()+xlab("")+ylim(0,4200)
p4<-ggplot(data=resnew[(4*n+1):(6*n),],aes(x=bayes_pval))+
  geom_histogram(color="darkblue",fill="white",bins = 10,boundary=0)+
  scale_x_continuous(limits=c(0,1))+
  facet_grid(.~phi,labeller = label_both)+theme_bw()+xlab("Bayesian p-value")+ylim(0,4200)
ggarrange(p2,p3,p4,nrow=3)


percentage<-sapply(1:ncol(res),function(x) length(which(res[,x]<0.05))/5000)

data_plot2<-data.frame(eta=phi_list,sensitivity=percentage)

ggplot(data=data_plot2,aes(x=eta,y=sensitivity))+
  geom_line(color="darkblue")+
  geom_point(color="darkblue")+
  theme_bw()+
  xlab("batch magnitude")

