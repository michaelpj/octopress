x <- c(37,15,20,10,404,642,1377,631,3113,4417,183,284,150,48,838,1440,587,2566,1699,1937,
2617,930,2712,141,159,4185,1062,47,15,120,6,89,3027,733,23520,296,922,84,82,673,192,37,
57,52449,156,121,47,39,688,36793,2028,9,398,129,4530,15,22,160,11,7,19,17,147,86,87,88,
4,12632,14,671,15869,409,789,345,924,37,734,1132,31114,136,9834,11920,3,149,1977,81,
1458,2128,1411,396,353,22,6269,39,2449,21,68,318,301,13158,207,102,197,7,225,42,117,73)
x <- log(x)

# Functions

library(MCMCpack)

# Figure out the probability of getting the data given our distribution parameters
eval <- function(x,k){
	sum(log(dnorm(x,k[1],k[2])))
}

# Sampling functions for MCMC: Gibbs sampling
update1 <- function(x,k){
temp <- k[[2]]
for(i in 1:length(x)){ 
	temp2 <- (1/k[[1]][2]^2+1/k[[1]][3]^2)^-1
	temp1 <- (k[[1]][1]/k[[1]][2]^2+x[i]/k[[1]][3]^2)*temp2
	temp[i] <- rnorm(1,temp1,sqrt(temp2)) 
	}
return(temp)
}

update2 <- function(k){
	temp2 <- (length(k[[2]])/k[[1]][2]^2)^-1
	temp1 <- (sum(k[[2]])/k[[1]][2]^2)*temp2
	temp <- rnorm(1,temp1,sqrt(temp2)) 
	return(temp)
}

update3 <- function(x,k){
	temp <- sqrt(rinvgamma(1,length(x)/2,sum((x-k[[1]][1])^2)/2)) 
	return(temp)
}

# Script

# We're going to vary the proportion of the variance due to error in res-many steps
res <- 25
# dcp_benefit holds the benefit for running another DCP trial, for each of the error proportions we're
# going to use
dcp_benefit <- rep(0,res)
# current_benefit holds the same, but for the intervention currently believed to be best (according to x)
current_benefit <- rep(0,res)

times_til_improvement <- rep(0,res)

for(l in 1:res){

	stores <- 1000
	repeat_times <- 10
	# proportion of variance that is due to DCP error
	error_proportion <- (l - 0.5)/res

	# k[[1]] holds our current best guess at the parameters of the true underlying distribution
	# for x, given error_ proportion, and for the variance of the error distribution
	# k[[2]] holds our current best guess at the true effectiveness of the i-th charity; initially this
	# is just x
	k <- list(NULL,NULL)
	k[[1]] <- c(mean(x),sd(x)*sqrt((1-error_proportion)),sd(x)*sqrt(error_proportion))
	k[[2]] <- x

	# We do MCMC using Gibbs sampling to get some samples from the distributions of 
	# the parameters and the data, given the data we actually have.
	# samples1 holds our successive samples for k[[1]] (the distribution parameters)
	# samples2 holds the samples for k[[2]] (the data)
	samples1 <- matrix(0,stores,length(k[[1]]))
	samples2 <- matrix(0,stores,length(k[[2]]))
	for(i in 1:stores*repeat_times){
		k[[2]] <- update1(x,k)
		k[[1]][1] <- update2(k)
		temp <- update3(x,k)/sqrt(sum(k[[1]][2:3]^2))
		k[[1]][2:3] <- k[[1]][2:3]*temp
		if(i%%repeat_times==0){ 
			samples1[i/repeat_times,] <- k[[1]]
			samples2[i/repeat_times,] <- k[[2]]
		}
	}



	gains <- rep(0,25*stores)
	curtot <- 0
	for(j in 1:25){
		for(i in 1:stores){
			# temp holds a sample taken from the i-th set of parameters for the true distribution
			temp <- rnorm(1,samples1[i,1],samples1[i,2])
			# temp2 adds in the error (i.e. samples around temp with variance taken from the i-th 
			# parameters)
			temp2 <- rnorm(1,temp,samples1[i,3])
			# current holds the true effectiveness (according to the i-th samples) of the intervention
			# that is *believed* to be best according to our original values in x
			current <- exp(-samples2[i,order(x)[1]])
			# potential holds the estimated true effectiveness of our sampled intervention
			potential <- exp(-temp)
			# if our sample *looks* like it's better than the current one (taking into account error),
			# then we calculate the gain in *actual* estimated effectiveness that we get
			if(temp2<min(x)) gains[(j-1)*stores+i] <- potential - current
			# curtot keeps track of the average estimated value of the intervention that currently
			# appears to be best
			curtot <- curtot + current/stores/25
		}
	}
	

	a <- mean(gains)	#expected increase in dalys per dollar for asessing a new intervention
	# money moved looks log-normally distributed with median 200m, 5th pct 20m,
	# 95th pct 2000m, so overall mean of 530m
	b <- 530e6	#money moved by DCP suggestions over, say, a ten year period
	d <- 1e5	#cost of DCP assessing an additional intervention

	dcp_benefit[l] <- a*b/d#estimate of dalys per dollar for dcp
	current_benefit[l] <- curtot
	print(l)
	times_til_improvement[l] <- stores/length(gains[gains>0])
}

#plot((1:25-0.5)/25,dcp_benefit*d/b,xlab="Percentage of variance in reported efficencies due to error",ylab="DALYs per $ transfered")
#abline(h=0)

variance_prior <- dbeta((1:res-0.5)/res,2,4)

png("estimates-by-error.png")
plot((1:25-0.5)/25,dcp_benefit,xlab="Percentage of variance in reported efficencies that is due to error",ylab="DALYs per $",type="l",col=2)
abline(h=0,lty=2)
lines((1:99)/100,dbeta((1:99)/100,2,4),col=1)
lines((1:res-0.5)/res,current_benefit,col=3)
#lines((1:99)/100,dbeta((1:99)/100,3,3),col=3)
legend(0.6,4.7,c("DCP donations","Direct donations","Variance prior"),lty=c(1,1,1),col=c(2,3,1))
dev.off()

mean(dcp_benefit*variance_prior)
mean(current_benefit*variance_prior)


#mean(dcp_benefit*dbeta((1:25-0.5)/25,3,3))
plot((1:25-0.5)/25,dcp_benefit/current_benefit,type="l",col=2)
lines((1:99)/100,dbeta((1:99)/100,2,4),col=1)
abline(h=0,lty=2)
abline(h=1,lty=2)