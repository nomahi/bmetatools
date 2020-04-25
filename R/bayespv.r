bayespv <- function(y, se, B1 = 5000, B2 = 25000){

	K <- length(y)   # number of studies
	is2 <- 1/(se*se)
	
	# BUGS model code

	rmamodel.1 <- function() {

		for (j in 1:K) {
			y[j] ~ dnorm(theta[j], is2[j])
			theta[j] ~ dnorm(mu, it2)
		}
		
		## Priors
		mu ~ dnorm(0, 0.0001)
		it2 ~ dgamma(0.01, 0.01)
		
		# predictive distribution
		theta.new ~ dnorm(mu, it2)
	
	}


	# BUGS implementation

	rmadata <- list(y = y, is2 = is2, K = K)
	
	rmainits <- function(){
		list(mu = 0, it2 = 1)
	}

	rmaout.1 <- bugs(data = rmadata, inits = rmainits, parameters.to.save = c("mu", 
		"it2", "theta.new"), model.file = rmamodel.1, n.chains = 1, n.iter = B2, n.burnin=B1)
	
	out.1 <- rmaout.1$sims.matrix	# posterior samples



	# Bayesian p-value

	tn.1 <- mean(out.1[,3])
	Vtn.1 <- var(out.1[,3])

	P <- numeric(K)

	for(i in 1:K) {

		y.i_new <- rnorm((B2-B1),out.1[,3],dat1$se[i])
  
		D.i <- (dat1$y[i] - tn.1)^2/Vtn.1
		D.i_new <- (y.i_new - tn.1)^2/Vtn.1
	
		P[i] <- mean(D.i < D.i_new)

	}

	R1 <- data.frame(study=1:K,pvalue=P)
	return(R1)
	
}

