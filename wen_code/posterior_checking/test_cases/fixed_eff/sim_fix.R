set.seed(123)

source("~/project/reproducibility/code/posterior_checking/posterior_checking.R")
N = 500
p = 5 

bbar =rnorm(N)
sev = runif(p, min = 0.5, max=1) 


bhat_matrix = t( sapply(1:N, function(x) bbar[x] + rnorm(p, sd=sev)))


cat("Q-statistic:\n")
pval = apply(bhat_matrix, 1, function(x) posterior_checking(beta=x, se=sev))
summary(pval)
length(which(pval<0.05))



