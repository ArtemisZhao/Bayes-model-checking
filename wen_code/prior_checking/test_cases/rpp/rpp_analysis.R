source("~/project/reproducibility/code/rep_pval.R")
d = read.table("rpp_73.dat", head=T)
attach(d)

cat("Replication p-value (default):\n")
pval_rpp1 = sapply(1:dim(d)[1], function(x) replication_pvalue(bhat=c(beta_orig[x], beta_rep[x]),  se=c(se_orig[x],  se_rep[x]) ))
summary(pval_rpp1)
length(which(pval_rpp1<= 0.05))
cat("\n")

cat("Replication p-value (fixed effect and flat prior):\n")
pval_rpp2 = sapply(1:dim(d)[1], function(x) replication_pvalue_ff(bhat=c(beta_orig[x], beta_rep[x]),  se=c(se_orig[x],  se_rep[x]) ))
summary(pval_rpp2)
length(which(pval_rpp2<= 0.05))
cat("\n")

cat("Replication p-value (publication bias):\n")
pval_rpp3 = sapply(1:dim(d)[1], function(x) replication_pvalue_pubbias(bhat=c(beta_orig[x], beta_rep[x]),  se=c(se_orig[x],  se_rep[x]) ))
summary(pval_rpp3)
length(which(pval_rpp3<= 0.05))

#pval_rpp3 = sapply(1:dim(d)[1], function(x) replication_pvalue_pubbias2(bhat=c(beta_orig[x], beta_rep[x]),  se=c(se_orig[x],  se_rep[x]) ))
#summary(pval_rpp3)
#length(which(pval_rpp3<= 0.05))

cat("\n")

which(pval_rpp2<=0.05)
which(pval_rpp3<=0.05)

