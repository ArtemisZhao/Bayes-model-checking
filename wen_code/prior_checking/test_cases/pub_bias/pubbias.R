source("~/project/reproducibility/code/rep_pval.R")
ud = read.table("unbias.dat")
bd = read.table("bias.dat")

cat("unbias ** Replication p-value (default):\n")
pval_rpp1 = sapply(1:dim(ud)[1], function(x) replication_pvalue(bhat=c(ud[x,2], ud[x,4]),  se=c(ud[x,3],  ud[x,5]) ))
summary(pval_rpp1)
length(which(pval_rpp1<= 0.05))
cat("\n")

cat("unbias ** pubbias p-value:\n")
pval_rpp2 = sapply(1:dim(ud)[1], function(x) replication_pvalue_pubbias(bhat=c(ud[x,2], ud[x,4]),  se=c(ud[x,3],  ud[x,5]) ))
summary(pval_rpp2)
length(which(pval_rpp2<= 0.05))

#pval_rpp2 = sapply(1:dim(ud)[1], function(x) replication_pvalue_pubbias2(bhat=c(ud[x,2], ud[x,4]),  se=c(ud[x,3],  ud[x,5]) ))
#summary(pval_rpp2)
#length(which(pval_rpp2<= 0.05))

cat("\n")



cat("bias ** Replication p-value (default):\n")
pval_rpp2 = sapply(1:dim(bd)[1], function(x) replication_pvalue(bhat=c(bd[x,2], bd[x,4]),  se=c(bd[x,3],  bd[x,5]) ))
summary(pval_rpp2)
length(which(pval_rpp2<= 0.05))
cat("\n")


pval_rpp2 = sapply(1:dim(bd)[1], function(x) replication_pvalue_pubbias(bhat=c(bd[x,2], bd[x,4]),  se=c(bd[x,3],  bd[x,5]) ))
summary(pval_rpp2)
length(which(pval_rpp2<= 0.05))

#pval_rpp2 = sapply(1:dim(bd)[1], function(x) replication_pvalue_pubbias2(bhat=c(bd[x,2], bd[x,4]),  se=c(bd[x,3],  bd[x,5]) ))
#summary(pval_rpp2)
#length(which(pval_rpp2<= 0.05))

cat("\n")






