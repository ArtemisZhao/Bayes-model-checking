source("../prior_checking/prior_checking.R")
d = read.table("sim_data/sim.pub_bias.2grp.dat")
beta = cbind(d$V2, d$V4)
se = cbind(d$V3,d$V5)
p = length(d$V2)
pval = sapply(1:p, function(x) replication_pvalue_pubbias(bhat = beta[x,], se = se[x,]))
pdf("output/2grp_with_pub_bias.pdf", width=8, height=5, bg="white")
hist(pval,breaks=10,col="gray")
dev.off()

d = read.table("sim_data/sim.control.2grp.dat")
p = length(d$V2)
beta = cbind(d$V2, d$V4)
se = cbind(d$V3,d$V5)
pval = sapply(1:p, function(x) replication_pvalue_pubbias(bhat = beta[x,], se = se[x,]))


pdf("output/2grp_no_pub_bias.pdf", width=8, height=5, bg="white")
hist(pval,breaks=10,col="gray")
dev.off()


