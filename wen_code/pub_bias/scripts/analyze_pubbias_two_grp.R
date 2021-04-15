prior_prp<-function(beta, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){
	
	bhat = beta
	bhat_o = bhat[1]
	bhat_r = bhat[2]
	s2_o = se[1]^2
	s2_r = se[2]^2


	eta2_vec = (bhat_o^2 + s2_o)/qchisq(c(0.25, 0.5, 0.75), df=1)
	#eta2_vec = mean(bhat_o,bhat_r)^2/qchisq(c(0.25, 0.5, 0.75), df=1)
	#eta2_vec = max(bhat)^2/qchisq(c(0.25, 0.5, 0.75), df=1)
	#eta2_vec = 0.5*(bhat_o^2+bhat_r^2)/qchisq(c(0.25, 0.5, 0.75), df=1)
	#eta2_vec = c(eta2_vec, c(bhat_r^2)/qchisq(c(0.25, 0.5, 0.75), df=1))

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


# Ratio statistic for detecting publication 
prior_prp_pubbias<-function(beta, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){
	
	bhat = beta
	bhat_o = bhat[1]
	bhat_r = bhat[2]
	s2_o = se[1]^2
	s2_r = se[2]^2


	eta2_vec = (s2_o+bhat_o^2)/qchisq(c(0.25, 0.5, 0.75), df=1)

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

	## rescaling
	br_mean_vec = br_mean_vec/bhat_o
	br_sd_vec = br_sd_vec/abs(bhat_o)

	# compute p-value for bhat_r        


	left_tail_vec = sapply(1:dim(grid)[1], function(x) pnorm(bhat_r/bhat_o, mean=br_mean_vec[x],sd=br_sd_vec[x]))
	pval = left_tail_prob = sum(left_tail_vec*wts)

	pval

}






args = commandArgs(trailingOnly=TRUE)
input_file = args[1]
p_thresh = as.numeric(args[2])



d = read.table(input_file)
beta = as.matrix(cbind(d$V2, d$V4))
se = as.matrix(cbind(d$V3,d$V5))
p = length(d$V2)

pval1 = sapply(1:p, function(x) prior_prp_pubbias(beta = beta[x,], se = se[x,]))
pval2 = sapply(1:p, function(x) prior_prp(beta = beta[x,], se = se[x,]))
filename = paste0("output/pubbias_2grp_pthresh_", round(p_thresh,2) , ".prp.out" )
outd = cbind(rep(1:p), pval2, pval1)
write(file=filename, t(outd), ncol=3)

