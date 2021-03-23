set.seed(123)
args = commandArgs(trailingOnly=TRUE)


# key simulation parameters

rnse = 1.0  # replication noise level/se
bt_eff = 0.5 # beta true effect
bb_sd = 1.0 # beta batch effect, sd

if(length(args)>=2){
	for (i in 1:length(args)){

		if(all(args[i] == "-rnse")){
			i = i+1
			rnse = as.numeric(args[i])
		}

		if(all(args[i] == "-bt")){
			i = i+1
			bt_eff = as.numeric(args[i])
		}

		if(all(args[i] == "-bb")){
			i = i+1
			bb_sd = as.numeric(args[i])
		}
	}
}



shuffle <- function(xv, rep){

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
sim_batch<-function(bt=0, bb_sd, rnse = 1){
	
	btv = rep(bt,grp_num)
	bbv = rep(0, grp_num)
	bbv[1:bc_grp_num]=rnorm(bc_grp_num, sd=bb_sd)

	rst = c()
	for (i in 1:grp_num){
		gv = Gv[i,]
		batch = Batch[i,]
		y = btv[i]*gv + bbv[i]*batch + rnorm(N, sd=rnse)
	 	m = summary(lm(y~gv))
 		rst = c(rst, m$coef[2,1], m$coef[2,2])
	}	

	return(rst)
}



N = 100

grp_num = 5
bc_grp_num = 2 # batch contaminated groups

Gv = t(sapply(1:grp_num, function(x) rbinom(N,1,0.4)))


Batch = t(apply(Gv,1, function(x) shuffle(x, rep=floor(N*0.20))))

rst = t(sapply(1:5000, function(x) sim_batch(bt=bt_eff, bb_sd = bb_sd)))



outd = cbind(1:5000, rst)
out_name = paste0("sim_data/batch_multigrp.bb_",round(bb_sd,2),"_rnse_",round(rnse,2),"_bt_",bt_eff, "_params.dat")
write(file= out_name, ncol=1+2*grp_num, t(outd))



