library(mvtnorm)
library(e1071)

posterior_checking<-function(beta, se, r_vec = c(1e-5, 6e-3, 0.024), L= 1000, test="Q", print_test_dist=FALSE) {

	m<-length(beta)  ###number of replicates
	

	wv = 1/se^2
	fix_mean = sum(wv*beta)/sum(wv)
	fix_var = 1/sum(wv)
	#chis1<-qchisq(c(0.25,0.5,0.75),df=1)
	#eta2_vec = c((fix_mean+fix_var)/chis1)
	
	eta2_vec=c(fix_mean, fix_mean-sqrt(fix_var), fix_mean+sqrt(fix_var))^2
	#cat(eta2_vec, "\n")

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

	sd2 = se^2

	wts<-c()
	count=0
	for (i in 1:length(omg2_list)){
		omg2<-omg2_list[i]
		phi2<-phi2_list[i]
		Sigma<-matrix(omg2,ncol=m,nrow=m)+diag(c(sd2+phi2),nrow=m)
		wtsi<-dmvnorm(beta, mean=rep(0,m), sigma=Sigma)
		wts<-c(wts,wtsi)
	}
	wts<-wts/sum(wts)

	simbeta<-c()
	dist_list<-c()
	dist_list2<-c()
	for (t in 1:L){
		k<-sample(1:length(omg2_list),1,prob=wts)

		phi2<-phi2_list[k]
		omg2<-omg2_list[k]


		barbeta_pos_var<-1/(1/omg2+sum(1/(sd2+phi2)))  
		barbeta_pos_mean<-barbeta_pos_var*sum(beta/(sd2+phi2))
		barbeta<-rnorm(1,barbeta_pos_mean,sqrt(barbeta_pos_var)) 

		betanewjs<-c()
		tnewjs<-c()
		for (j in 1:m){
			#print(c(i,j))
			betaj_var<-1/(1/phi2+1/sd2[j])
			betaj_mean<-betaj_var*(barbeta/phi2+beta[j]/sd2[j])
			betaj<-rnorm(1,betaj_mean,sqrt(betaj_var))
			#print(betaj)

			betanewj = betaj+rnorm(1,0,sqrt(sd2[j]))
			betanewjs<-c(betanewjs,betanewj)
		}
		if (test == "Q"){
			q = sum((betanewjs - mean(betanewjs))^2 / (sd2 + phi2))
			q_orig = sum((beta - mean(beta))^2 / (sd2 + phi2))
			dist_list<- c(dist_list,q)
			count = count + (q>q_orig)
			##the difference
			#dist=q-q_orig
			#dist_list2<-c(dist_list2,dist)
		}
		if (test == "egger-hetero"){
			y = betanewjs / sqrt(sd2 + phi2)
			x = 1 / sqrt(sd2 + phi2)
			a = abs(summary(lm(y ~ x))$coefficients[1,1])
			dist_list = c(dist_list,a)

			y_orig = beta / sqrt(sd2 + phi2)
			x_orig = 1 / sqrt(sd2 + phi2)
			com = abs(summary(lm(y_orig~x_orig))$coefficients[1,1])

			count = count + (a>com)
		}
		###test statistics 2 skewness:
		if (test=="skew"){
			y = betanewjs / sqrt(sd2 + phi2)
			x = 1 / sqrt(sd2 + phi2)
			muhat = summary(lm(y ~ x))$coefficients[2,1]
			dis = (betanewjs - muhat)/sqrt(sd2+phi2)
			skew<-abs(skewness(dis))
			dist_list = c(dist_list,skew)

			y_orig = beta / sqrt(sd2 + phi2)
			x_orig = 1 / sqrt(sd2 + phi2)
			muhat = summary(lm(y_orig ~ x_orig))$coefficients[2,1]
			dis = (beta - muhat)/sqrt(sd2+phi2)
			com = abs(skewness(dis))
			count=count+(skew>com)
		}
		if (test== "egger")
		{ ### test statistics 3 egger regression:
			y = betanewjs/sqrt(sd2)
			x= 1/sqrt(sd2)
			a = summary(lm(y ~ x))$coefficients[1,1]
			dist_list = c(dist_list,abs(a))
		}
		if (test == "diff")
		{ ###test statistics 1 naive: 
			###max mean difference
			dist<-max(betanewjs)-mean(betanewjs)
			dist_list<-c(dist_list,dist)
		}
	}
	if (print_test_dist){
		print(length(dist_list))
		hist(dist_list)
	}
	if (test == "skew"){
		p = (count/L)
	}
	if (test == "egger-hetero" || test == "egger"){
		p=(count/L)
	}
	if (test == "Q"){
		p = count/L
	}
	if (test == "egger"){
		y = beta / sqrt(sd2)
		x = 1 / sqrt(sd2)
		com = abs(summary(lm(y~x))$coefficients[1,1])
	}
	if (test == "diff"){
		com = max(beta)-mean(beta)
		p = length(which(dist_list>com))/L
	}
	return(p)
}


