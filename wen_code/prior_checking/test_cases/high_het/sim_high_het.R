set.seed(123)

source("~/project/reproducibility/code/rep_pval.R")
N = 5000
se = 1
ssd = 2
bbar =rnorm(N,sd=ssd)

b1 = bbar + rnorm(N, sd =abs(bbar) )
b2 = bbar + rnorm(N, sd = abs(bbar))

bhat1 = b1+rnorm(N,sd=se) 
bhat2 = b2+rnorm(N,sd=se)

cat("Two-sided\n")
pval = sapply(1:N, function(x) replication_pvalue(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nFixed effect flat prior\n")
pval = sapply(1:N, function(x) replication_pvalue_ff(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))


cat("\nLeft side\n")
pval = sapply(1:N, function(x) replication_pvalue_left(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nRight side\n")
pval = sapply(1:N, function(x) replication_pvalue_right(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))

cat("\nPublication bias\n")
pval = sapply(1:N, function(x) replication_pvalue_pubbias(bhat=c(bhat1[x], bhat2[x]), se=c(se,se)))
summary(pval)
length(which(pval<0.05))


