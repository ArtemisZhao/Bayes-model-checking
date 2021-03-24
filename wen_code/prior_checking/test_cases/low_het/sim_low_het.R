set.seed(123)

source("~/project/reproducibility/code/rep_pval.R")
N = 5000
se =  1.5
ssd = 1
bbar =rnorm(N,sd=ssd)

b1 = bbar + rnorm(N, sd = ssd*sqrt(0.022/(1-0.022)))
b2 = bbar + rnorm(N, sd = ssd*sqrt(0.022/(1-0.022)))

bhat1 = b1+rnorm(N,sd=se) 
bhat2 = b2+rnorm(N,sd=se)

cat("two-sided:\n")
pval = sapply(1:N, function(x) replication_pvalue(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nfixed effect flat prior:\n")
pval = sapply(1:N, function(x) replication_pvalue_ff(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nleft-tail:\n")
pval = sapply(1:N, function(x) replication_pvalue_left(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nright-tail:\n")
pval = sapply(1:N, function(x) replication_pvalue_right(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\npub bias:\n")
pval = sapply(1:N, function(x) replication_pvalue_pubbias(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))
pval = sapply(1:N, function(x) replication_pvalue_pubbias2(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))


