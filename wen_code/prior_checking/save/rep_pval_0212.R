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





replication_pvalue_left<-function(bhat, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){

	bhat_o = bhat[1]
	bhat_r = bhat[2]
	s2_o = se[1]^2
	s2_r = se[2]^2


	eta2_vec = (bhat_o^2+s2_o)/qchisq(c(0.25, 0.5, 0.75), df=1)

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







	# read left tail if bhat_o > 0
	left_tail_vec = sapply(1:dim(grid)[1], function(x) pnorm(bhat_r, mean=br_mean_vec[x],sd=br_sd_vec[x]))

	pval = left_tail_prob = sum(left_tail_vec*wts)

	pval

}



replication_pvalue_right<-function(bhat, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){

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





	right_tail_vec= sapply(1:dim(grid)[1], function(x) 1-pnorm(bhat_r, mean=br_mean_vec[x],sd=br_sd_vec[x]))
	pval = right_tail_prob = sum(right_tail_vec*wts)

	pval

}







# one-sided p-value for detecting publication bias
replication_pvalue_pubbias<-function(bhat, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){

	bhat_o = bhat[1]
	bhat_r = bhat[2]
	s2_o = se[1]^2
	s2_r = se[2]^2


	eta2_vec = (bhat_o^2+s2_o)/qchisq(c(0.25, 0.5, 0.75), df=1)

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

	pval= 1

	# read left tail if bhat_o > 0
	if(bhat_o > 0){
		left_tail_vec = sapply(1:dim(grid)[1], function(x) pnorm(bhat_r, mean=br_mean_vec[x],sd=br_sd_vec[x]))

		pval = left_tail_prob = sum(left_tail_vec*wts)

	}

	if(bhat_o<=0){
		right_tail_vec= sapply(1:dim(grid)[1], function(x) 1-pnorm(bhat_r, mean=br_mean_vec[x],sd=br_sd_vec[x]))
		pval = right_tail_prob = sum(right_tail_vec*wts)
	}    

	pval

}


# Ratio statistic for detecting publication bias
replication_pvalue_pubbias2<-function(bhat, se, r_vec = c(0, 1e-5, 6e-3, 0.024)){

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







# replication p-value w/ fixed-effect and the flat prior
replication_pvalue_ff_pubbias<-function(bhat, se){


        bhat_o = bhat[1]
        bhat_r = bhat[2]

        var = se[1]^2 + se[2]^2
        left_prob = pnorm(bhat_r/bhat_o, mean=1, sqrt(var/bhat_o^2))
        right_prob = 1-pnorm(bhat_r/bhat_o, mean=1, sqrt(var/bhat_o^2))

	pval = left_prob
        #pval = 2*min(left_prob, right_prob)
	pval

}



