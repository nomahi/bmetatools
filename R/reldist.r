reldist <- function(y, se, B1 = 5000, B2 = 25000, Cores=detectCores()){

	K <- length(y)   # number of studies
	is2 <- 1/(se*se)
	
	cl <- makeSOCKcluster(Cores)
	registerDoSNOW(cl)
	
	# BUGS model code

	rmamodel.1 <- function() {

		for (j in 1:K) {
			y[j] ~ dnorm(theta[j], is2[j])
			theta[j] ~ dnorm(mu, it2)
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

	rmaout.1 <- bugs(data = rmadata, inits = rmainits, parameters.to.save = c("mu", 
		"tau2", "theta.new"), model.file = rmamodel.1, n.chains = 1, n.iter = B2, n.burnin=B1)
	
	out.1 <- rmaout.1$sims.matrix	# posterior samples
	
	mu.1 <- mean(out.1[,1])



	# Relative distance and standardized residuals

	opts <- list(progress = function(x) print(paste0("The leave-one-out analysis for study ", x, " is completed.")))

 	R1 <- foreach(i = 1:K, .combine = rbind, .packages=c("MASS","R2OpenBUGS"), .options.snow = opts) %dopar% {
 
		rmadata <- list(y = y[-i], is2 = is2[-i], K = K-1)

		rmaout.i <- bugs(data = rmadata, inits = rmainits, parameters.to.save = c("mu", 
			"tau2", "theta.new"), model.file = rmamodel.1, n.chains = 1, n.iter = B2, n.burnin=B1)
	
		out.i <- rmaout.i$sims.matrix	# posterior samples
	
		mu.i <- mean(out.i[,1])
	
		tn.i <- mean(out.i[,3])
		stn.i <- sd(out.i[,3])
	
		RD <- abs((mu.1 - mu.i)/mu.1)
		STR <- (y[i] - tn.i)/stn.i

		c(i,RD,STR)
	
	}
	
	return(R1)
	
}

