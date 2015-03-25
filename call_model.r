		#  Call frogs analysis
		#  02/2015
		#  Lukacs Lab
####################################################################################################
		#  Load packages
		require(R2jags)
   
####################################################################################################
		#  Load data
		load("C:/frogs/frog_dat_list.RData")		
####################################################################################################
		#  Manipulate raw data
		#  Skipped for now...
		
		#  Format for JAGS
		source("C:/frogs/data_fun.r")
		jags_dat <- data_fun(frog_dat$ehj)
		#  Cheap fix
		jags_dat$eh <- sapply(jags_dat$eh, as.numeric)
		
		#  Create miscellaneous JAGS stuff
		#  Inits
		jags_inits <- function(){list(
			"mu.phi" = runif(1, qlogis(0.4), qlogis(0.6)),
			"mu.p" = runif(1, qlogis(0.1), qlogis(0.3)),
			"z" = jags_dat$z.init
		)}
		
		#  Parameters to save
		parms <- c("mean.phi", "mean.p")
		
		#  Call JAGS model
		out <- try(jags(
			data = jags_dat,
			inits = jags_inits,
			parameters = parms,
			"C:/frogs/model_skeleton.txt",
			n.chains = 3,
			n.thin = 1,
			n.burnin = 50,
			n.iter = 100		
		), silent = T)
		
		#  Retry if fail
		counter <- 1
		while(class(out) == "try-error" & counter < 10){
			counter <- counter + 1
			out <- try(jags(
				data = jags_dat,
				inits = jags_inits,
				parameters = parms,
				"C:/frogs/model_skeleton.txt",
				n.chains = 3,
				n.thin = 1,
				n.burnin = 500,
				n.iter = 1000		
			), silent = T)
		}
####################################################################################################
		#  If desired save output of JAGS run
		save(out, file = paste("frogs_surv_", format(as.POSIXlt(Sys.time()), "%d%b%y_%H%M%S"), 
				".RData", sep = ""))
####################################################################################################
		#  Plotting scripts
		source("C:/frogs/plot_jags.r")
		plot_jags(out)
		#  If desired save plots
		savePlot()
		