set.seed(123)

source("~/project/reproducibility/code/rep_pval.R")
N = 5000
se1 = 0.2
se2 = 0.5
bbar =rnorm(N)

bhat1 = bbar+rnorm(N,sd=se1) 
bhat2 = bbar+rnorm(N,sd=se2)

cat("replication p two-sided:\n")
pval = sapply(1:N, function(x) replication_pvalue(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))

cat("\nreplication p left-side:\n")
pval = sapply(1:N, function(x) replication_pvalue_left(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))


cat("\nreplication p right-side:\n")
pval = sapply(1:N, function(x) replication_pvalue_right(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))



cat("\nfixed effect flat prior\n")
pval = sapply(1:N, function(x) replication_pvalue_ff(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))



cat("\npub bias:\n")
pval = sapply(1:N, function(x) replication_pvalue_pubbias(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))
pval = sapply(1:N, function(x) replication_pvalue_pubbias2(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))


cat("\npub bias ff :\n")
pval = sapply(1:N, function(x) replication_pvalue_ff_pubbias(bhat=c(bhat1[x], bhat2[x]), se=c(se1,se2)))
summary(pval)
length(which(pval<0.05))

