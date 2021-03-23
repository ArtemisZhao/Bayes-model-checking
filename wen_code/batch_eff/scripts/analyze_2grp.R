replication_pvalue<-function(bhat, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){

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



args = commandArgs(trailingOnly=TRUE)
filename = args[1]
info = read.table(text=filename, sep="_")
bb = info[4]
rnse = info[6]
bt = info[8]




d = read.table(filename)
attach(d)
N = dim(d)[1]

pvec = sapply(1:N, function(x) replication_pvalue(bhat=c(V2[x], V4[x]), se=c(V3[x], V5[x])))

outfile = paste0("output/batch_2gp.bb_",round(bb,2),".rnse_",rnse,".bt_",bt,".pdf")

pdf_file = pdf(file=outfile, width=10, height=5, bg="white")
hist(pvec,breaks=20, main=paste0("lambda = ", round(bb,2),", rnse = ", round(rnse,2)), col="gray"  ) 
dev.off()
