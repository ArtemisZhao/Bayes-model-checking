q_test <- function(bhat, se){

	m = length(bhat)
	se2 = se^2
	wv = 1/se2

	fix_mean = sum(wv*bhat)/sum(wv)
	q = sum(wv*(bhat-fix_mean)^2)
	p = 1 - pchisq(q, df=m-1)
	p
}


args = commandArgs(trailingOnly = T)
input_file = args[1]
d = read.table(input_file)

nc = dim(d)[2]

seq_b = seq(2,nc,2)
seq_s = seq(3,nc,2) 

beta = d[,seq_b]
sd = d[,seq_s]


p = dim(d)[1]
pval_q = sapply(1:p, function(x) q_test(bhat = as.numeric(beta[x,]), se = as.numeric(sd[x,])))
summary(pval_q)
hist(pval_q)

