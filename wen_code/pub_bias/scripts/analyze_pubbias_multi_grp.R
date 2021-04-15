library(mvtnorm)

posterior_prp<-function(beta,se,L=1000,r_vec = c(1e-5, 6e-3, 0.024),test="diff",print_test_dist=FALSE){
	res<-list()
	se2=se^2
	m<-length(beta)  ###number of replicates

	#chis1<-qchisq(c(0.25,0.5,0.75),df=1)
	## test1 normal
	#eta2_vec = c(mean(beta),min(beta),max(beta))^2

	## test 2 weighted average

	wts = 1/se2
	center = sum(wts*beta)/sum(wts)
	eta2_vec = c(center, center-sqrt(1/sum(wts)),center+sqrt(1/sum(wts)))^2

	##test 3 
	#eta2_vec = c(center)^2
	res[["eta_grid"]] = eta2_vec
	rv = r_vec

	make_grid <-function(eta2){
		grid = sapply(rv, function(x)  c(eta2*(1-x), eta2*x))
		return(t(grid))
	}

	grid = c()

	for (i in 1:length(eta2_vec)){
		grid = rbind(grid, make_grid(eta2_vec[i]))
	}

	omg2_list = grid[,1]
	phi2_list = grid[,2]

	wts<-c()
	count=0
	for (i in 1:length(omg2_list)){
		omg2<-omg2_list[i]
		phi2<-phi2_list[i]
		Sigma<-matrix(omg2,ncol=m,nrow=m)+diag(c(se2+phi2),nrow=m)
		wtsi<-dmvnorm(beta, mean=rep(0,m), sigma=Sigma)
		wts<-c(wts,wtsi)
	}
	wts<-wts/sum(wts)


	dist_list<-c()
	dist_list2<-c()
	for (t in 1:L){
		k<-sample(1:length(omg2_list),1,prob=wts)

		phi2<-phi2_list[k]
		omg2<-omg2_list[k]

		barbeta_pos_var<-1/(1/omg2+sum(1/(se2+phi2)))  
		barbeta_pos_mean<-barbeta_pos_var*sum(beta/(se2+phi2))
		barbeta<-rnorm(1,barbeta_pos_mean,sqrt(barbeta_pos_var)) 

		betanewjs<-c()

		for (j in 1:m){
			#print(c(i,j))
			betaj_var<- (phi2*se2[j])/(phi2+se2[j])
			betaj_mean<- (barbeta*se2[j] + phi2*beta[j])/(phi2+se2[j])
			betaj<-rnorm(1,betaj_mean,sqrt(betaj_var))
			#print(betaj)

			betanewj = betaj+rnorm(1,0,sqrt(se2[j]))
			betanewjs<-c(betanewjs,betanewj)
		}

		###test 4: Cochran's Q test
		if (test == "Q"){
			q_sim = sum((betanewjs - barbeta)^2 / (se2 + phi2))
			q_orig = sum((beta - barbeta)^2 / (se2 + phi2))

			#cat(t, " Q:  ",barbeta, " ", phi2, "  ", q_sim, " ",q_orig,"\n")

			dist_list2<-c(dist_list2,(q_sim -q_orig))
			count = count + (q_sim>q_orig)
		}
		####test statistics 3 egger regression with heterogeneous param
		else if (test == "egger"){

			y = betanewjs/sqrt(se2+phi2)
			x = 1/sqrt(se2+phi2)
			Sxx = sum( (x-mean(x))*x)
			Sxy = sum( (x-mean(x))*y)
			Syy = sum( (y-mean(y))*y)

			b1 = Sxy/Sxx
			b0 = mean(y)- b1*mean(x)
			s2 = (Syy - b1^2*Sxx)/m
			vb0 = s2*(1/m+mean(x)^2/Sxx)


			egger_sim = b0^2/vb0


			y = beta/sqrt(se2+phi2)
			Sxy = sum( (x-mean(x))*y)
			Syy = sum( (y-mean(y))*y)

			b1 = Sxy/Sxx
			b0 = mean(y)- b1*mean(x)
			s2 = (Syy - b1^2*Sxx)/m
			vb0 = s2*(1/m+mean(x)^2/Sxx)


			egger_orig = b0^2/vb0


			dist_list2= c(dist_list2, (egger_sim - egger_orig))
			count = count + ( egger_sim > egger_orig )
		}
	}
	if (print_test_dist){
		#print(length(dist_list))
		hist(dist_list2)
	}
	res[["n_sim"]]=L
	res[["test_stats_dif"]]=dist_list2
	res[["pval"]]=count/L
	res[["test"]]=test
	return( res)

}


args = commandArgs(trailingOnly=TRUE)
input_file = args[1]
c_param = as.numeric(args[2])



d = read.table(input_file)

nc = dim(d)[2]

seq_b = seq(2,nc,2)
seq_s = seq(3,nc,2) 

beta = as.matrix(d[,seq_b])
se = as.matrix(d[,seq_s])


p = dim(d)[1]
pval_egger =c()
pval_Q = c()

for (i in 1:p){

	pval1 = posterior_prp(beta = beta[i,], se = se[i,], test="egger")$pval
	pval_egger = c(pval_egger,pval1)

	pval2 = posterior_prp(beta = beta[i,], se = se[i,], test="Q")$pval
	pval_Q = c(pval_Q, pval2)

}


filename = paste0("output/pubbias_multigrp_cparam_", round(c_param,1) , ".prp.out" )
outd = cbind(rep(1:p), pval_Q, pval_egger)
write(file=filename, t(outd), ncol=3)



