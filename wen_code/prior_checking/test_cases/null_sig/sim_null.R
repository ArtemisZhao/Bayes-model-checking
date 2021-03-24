#set.seed(123)

source("~/project/reproducibility/code/rep_pval.R")
N = 5000
se = 1
bhat1 = rnorm(N,sd=se) 
bhat2 = rnorm(N,sd=se)

cat("replication p two-sided\n")
pval = sapply(1:N, function(x) replication_pvalue(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))


cat("\nreplication p left-side\n")
pval = sapply(1:N, function(x) replication_pvalue_left(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))


cat("\nreplication p right-side\n")
pval = sapply(1:N, function(x) replication_pvalue_right(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))


cat("\npub bias p\n")
pval = sapply(1:N, function(x) replication_pvalue_pubbias(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))
pval = sapply(1:N, function(x) replication_pvalue_pubbias2(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nfixed eff flat prior p\n")
pval = sapply(1:N, function(x) replication_pvalue_ff(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

