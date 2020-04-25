smm_dexp <- function(y, se, B1 = 5000, B2 = 25000){

	K <- length(y)   # number of studies
	is2 <- 1/(se*se)
	
	# BUGS model code

	rmamodel.2 <- function() {

	for (j in 1:K) {
		y[j] ~ dnorm(theta[j], is2m[j])
		theta[j] ~ dnorm(mu, it2)

		is2m[j] <- is2[j] / lambda[j]	# double exponential error
		lambda[j] ~ dexp(0.5)
	}
	
	## Priors
	mu ~ dnorm(0, 0.0001)
	it2 ~ dgamma(0.01, 0.01)
	tau2 <- 1/it2
		
	# predictive distribution
	theta.new ~ dnorm(mu, it2)
	
	}


	# BUGS implementation

	rmadata <- list(y = y, is2 = is2, K = K)
	
	rmainits <- function(){
		list(mu = 0, it2 = 1)
	}

	rmaout.2 <- bugs(data = rmadata, inits = rmainits, parameters.to.save = c("lambda", "mu", 
    "tau2", "theta.new"), model.file = rmamodel.2, n.chains = 1, n.iter = B2, n.burnin=B1)
	
	out.2 <- rmaout.2$sims.matrix	# posterior samples

	R1 <- NULL
	for(i in 1:K) R1 <- rbind(R1,c(i,mean(out.2[,i]),quantile(out.2[,i],c(0.025,0.975)),mean(out.2[,i]>=1)))

	Q <- list(bugsm=rmaout.2,lambda=R1)

	return(Q)
	
}

