set.seed(123)
args = commandArgs(trailingOnly=TRUE)
input = as.numeric(args[1])






replication_pvalue<-function(bhat, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){

	bhat_o = bhat[1]
	bhat_r = bhat[2]
	s2_o = se[1]^2
	s2_r = se[2]^2


	eta2_vec = (bhat_o^2 + s2_o)/qchisq(c(0.25, 0.5, 0.75), df=1)
	rv = r_vec

	make_grid <-function(eta2){
		grid = sapply(rv, function(x)  c(eta2*(1-x), eta2*x))
		return(t(grid))
	}

	grid = c()
	for (i in 1:length(eta2_vec)){
		grid = rbind(grid, make_grid(eta2_vec[i]))
	}


	omg2_vec = grid[,1]
	phi2_vec = grid[,2]

	sv2 = rep(s2_o, length(omg2_vec))
	sv2 = phi2_vec + sv2
	ev2 = omg2_vec

	bbar_mean_vec = bhat_o*ev2/(ev2+sv2)
	bbar_var_vec = ev2*sv2/(ev2+sv2)

	# update weights for each grid

	wts = dnorm(bhat_o, mean=0, sd=sqrt(sv2+ev2))
	wts = wts/sum(wts)

	br_mean_vec = bbar_mean_vec
	br_sd_vec = sqrt(bbar_var_vec + phi2_vec + s2_r)





	# compute p-value for bhat_r	

	right_tail_vec= sapply(1:dim(grid)[1], function(x) 1-pnorm(bhat_r, mean=br_mean_vec[x],sd=br_sd_vec[x]))
	left_tail_vec = sapply(1:dim(grid)[1], function(x) pnorm(bhat_r, mean=br_mean_vec[x],sd=br_sd_vec[x]))


	right_tail_prob = sum(right_tail_vec*wts)
	left_tail_prob = sum(left_tail_vec*wts)

	pval = 2*min(right_tail_prob, left_tail_prob)

	pval

}


# replication p-value w/ fixed-effect and the flat prior
replication_pvalue_ff<-function(bhat, se){


	bhat_o = bhat[1]
	bhat_r = bhat[2]

	var = se[1]^2 + se[2]^2
	right_tail_prob = 1-pnorm(bhat_r, mean=bhat_o, sqrt(var))
	left_tail_prob = pnorm(bhat_r, mean=bhat_o, sqrt(var))

	pval = 2*min(right_tail_prob, left_tail_prob)

	pval
}










shuffle <- function(xv, rep=40){

	yv = xv
	for (i in 1:rep){
	  index = sample(1:length(xv),2, replace=F)
	  temp = yv[index[1]] 
	  yv[index[1]] = yv[index[2]]
	  yv[index[2]] = temp
	}
	return(yv)
}	



# simulate pure batch contamination
sim_batch<-function(bt=0, bb){

	y1 = bt*gv1+ bb*batch + rnorm(N)
	y2 = bt*gv2 + rnorm(N)
	rst1 = summary(lm(y1~gv1))
	rst2 = summary(lm(y2~gv2))


	return(c(rst1$coef[2,1], rst1$coef[2,2], rst2$coef[2,1], rst2$coef[2,2]))
}


N = 100
gv1 = rbinom(N, 1, 0.4)
gv2 = rbinom(N, 1, 0.4)
batch = shuffle(gv1, rep=floor(N*0.20))

run_sim<-function(sd){

    rsv = sim_batch(bt = 0.5, bb=rnorm(1,sd = sd))
    replication_pvalue(bhat=c(rsv[1],rsv[3]), se=c(rsv[2], rsv[4]))
}


pvec = sapply(1:5000, function(x) run_sim(sd=input))
pdf(file = "hist.pdf", width=10, height=5.5, bg="white")
hist(pvec,breaks=20)
dev.off()
