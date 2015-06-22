		#  Model prep functions for JAGS frog analysis
		#  04/2015
		#  Lukacs Lab
####################################################################################################
		require(R2jags)
		require(mcmcplots)
		setwd("C:/frog")
		user <- Sys.info()["login"]
####################################################################################################

		#  Source Anna's functions
		source(file.path("C:/Users", 
							Sys.info()["login"],
				"Documents/GitHub/Columbia-Spotted-Frogs/data_manip/extract_fun.R"))
		
		#  Source and run Charlie's function and define objects
		source(file.path("C:/Users", 
							Sys.info()["login"],
						"Documents/GitHub/Columbia-Spotted-Frogs/data_manip/Data_Check_Prep.R"))
				
		fEH <- get_data()
		fsex <- get_sex_data() 
		
		y <- as.numeric(fEH$cap)	
		ind <- as.factor(fEH$Index)
		ind <- as.numeric(ind)
		prim <- as.factor(fEH$prim)
		prim <- as.numeric(prim)
		sec <- as.factor(fEH$sec)
		sec <- as.numeric(sec)
	
		n_ind <- length(unique(fEH$Index))
		n_prim <- length(unique(prim))
		n_sec <- rep(NA,n_prim)	
		for(i in 1:n_prim){
			n_sec[i] <- length(unique(sec[prim == i]))
		}
		n_obs <- nrow(fEH)
	
		first_occ <- as.numeric(tapply(prim, ind, min))
		
		toe <- as.numeric(tapply(as.numeric(fEH$toe), ind, unique))
		toe <- as.numeric(scale(toe))
	
		weight <- as.numeric(tapply(as.numeric(fEH$sc_wt), ind, unique))
				
		length <- as.numeric(tapply(as.numeric(fEH$sc_len), ind, unique))
				
		############# Data selection for all covariates when using sex ############################
		
		#  There are fewer that have sex recorded than in the total data set. So, we have to recreate all
		#   the data and covariate vectors with this smaller subset of data, and it has to be taken for
		#   the correct individuals (i.e., not just the first 463 individuals, but the 463 that also have
		#   sex recorded)
		sex_num <- tapply(fsex$Sex, as.numeric(as.factor(fsex$Index)), function(x){
				ifelse(unique(x) == "M", 0, 1)
		})
		sex_num <- as.data.frame(sex_num)
		
		sex_index <- fsex %>%  
							group_by(Index) %>% 
							summarise(sex_info = any(!is.na(Sex)))	
							
		temp <- bind_cols(sex_index, sex_num)
		
		sex_dat <- inner_join(fsex, temp, by = "Index")
		sex_dat <- select(sex_dat, -sex_info)
		
		
		y <- as.numeric(sex_dat$cap)	
		ind<- as.factor(sex_dat$Index)
		ind <- as.numeric(ind)
		prim <- as.factor(sex_dat$prim)
		prim <- as.numeric(prim)
		sec <- as.factor(sex_dat$sec)
		sec <- as.numeric(sec)
	
		n_obs <- nrow(sex_dat)
		n_ind <- length(unique(sex_dat$Index))
		n_prim <- length(unique(prim))
		n_sec <- rep(NA,n_prim)	
		for(i in 1:n_prim){
			n_sec[i] <- length(unique(sec[prim == i]))
		}
		
		first_occ <- as.numeric(tapply(prim, ind, min))
		
		sex <- as.numeric(tapply(as.numeric(sex_dat$sex_num), ind, unique))
		toe <- as.numeric(tapply(as.numeric(sex_dat$toes), ind, unique))
		toe <- as.numeric(scale(toe))
		length <- as.numeric(tapply(as.numeric(sex_dat$sc_len), ind, unique))
		weight <- as.numeric(tapply(as.numeric(sex_dat$sc_wt), ind, unique))

		
	
		
		# Bundle data
		data <- list("y", 
						"ind", 
						"prim", 
						"sec", 
						"n_ind", 
						"n_obs", 
						"n_prim", 
						"n_sec", 
						"toe", 
						"weight", 
						"length", 
						"sex",
						"first_occ")

							
			
		# Get model name with function
		foo <- function(weight = T, length = T, sex = T, intx1 = T, 
					intx2 = T, name = T, mod_name = NA){
	
			#  A function to create model name or directory from user inputs
			#  Takes:  A series of logical statements about what to include in 
			#   the model.  Each component represents a component of the model
			#   except for name, which determines whether the model name or the
			#   directory containing models is returned.  The directory currently
			#   relies on the user having the working directory set prior to 
			#   running this function.
			#  Returns:  A character string describing the model name or the 
			#   model directory with the model name appended to the directory.  
			#   Currently only text (.txt) file names are created...every model 
			#   name will include .txt.
			
			wt <- ifelse(weight, "weight", "n")
					
			len <- ifelse(length, "length", "n")
			
			sx <- ifelse(sex, "sex", "n")
							
			ws <- ifelse(intx1, "ws", "n")
							
			ls <- ifelse(intx2, "ls", "n")
			
			if(name){
				mod_name <- paste("toe", wt, len, sx, ws, ls, sep = "_")
				return(mod_name)
			}else{
				#  Assumes working directory is set to place where models are 
				#   stored
				mod_path <- file.path(getwd(), mod_name)
				return(mod_path)
			}
		}
	
		# Execute foo - function to create model name	

		mod_name <- foo(weight = F, 
						length = F, 
						sex = T, 
						intx1 = F, 
						intx2 = F, 
						name = T)
		
		#Initial values 
		#Function to create a matrix of initial vales for latent state z
		# z.init <- function(){
			
			# z <- matrix(NA, nrow = n_ind, ncol = n_prim))
			# for(i in 1:n_obs){
				# z[ind[i], prim[i]] <- max(y[ind == ind[i] & 
													# prim == prim[i]])
			# }			
		# z
		# }
		
		z.init <- function(n_ind=n_ind, first_occ=first_occ, n_prim=n_prim){
			z <- matrix(1, nrow = n_ind, ncol = n_prim)
			for(i in 1:n_ind){
				z[i, (1: (first_occ[i])-1)] <- NA
			}
			z
		}

	
		inits <- function(){
			list("mu_phi" = runif(1, -1, 1),
				 "mu_p" =   runif(1, -1, 1), 
				 "beta1" =  runif(1, -5, 5),
				 "beta2" =  runif(1, -5, 5),
				 "beta3" =  runif(1, -5, 5),
				 "beta4" =  runif(1, -5, 5),
				 "z" =  z.init(n_ind=n_ind, first_occ=first_occ, n_prim=n_prim)) # Latent (true) state of individual i at time t
		}
		
		debug_inits <- function(){
			list("mu_phi" = runif(1, -1, 1),
				 "mu_p" =   runif(1, -1, 1), 
				 "beta1" =  runif(1, -5, 5),
				 "beta2" =  runif(1, -5, 5),
				 "beta3" =  runif(1, -5, 5),
				 "beta4" =  runif(1, -5, 5),
				 "z" =  z.init(100, rep(1,100),14)) # Latent (true) state of individual i at time t
		}
				
#################################################################################
		#  Call JAGS
		call_jags <- function(mod_name, 
								parallel = F,
								ni = 100,
								nt = 1,
								nb = 10,
								nc = 3,
								debug_mode = T,
								return_fit = T){

			if(debug_mode){
				data <- list()
				data$y <- y[ind %in% 1:100]
				data$ind <- ind[ind %in% 1:100] 
				data$prim <- prim[ind %in% 1:100] 
				data$sec <- sec[ind %in% 1:100] 
				data$n_ind <- 100
				data$n_obs <- length(data$y)
				data$n_prim <- max(data$prim)
				data$n_sec <- n_sec
				data$toe <- toe[ind %in% 1:100]  
				data$weight <- weight[ind %in% 1:100]  
				data$length <- length[ind %in% 1:100]  
				data$sex <- sex[ind %in% 1:100] 
				data$first_occ <- first_occ[ind %in% 1:100] 
				print(names(data))
				lapply(data, head)
				
				print(head(debug_inits()))
				
				out <- try(jags(data = data, 
							inits = debug_inits, 
							parameters.to.save = parameters,
							model.file = paste("C:/Users/", 
												user, 
												"/Documents/GitHub/Columbia-Spotted-Frogs/models/",
												mod_name,
												".txt", 
												sep=""),
							n.chains = nc, 
							n.thin = nt, 
							n.iter = ni, 
							n.burnin = nb), silent = T)
				
			}else{

				if(parallel){
					out <- try(jags.parallel(data = data, 
											inits = inits, 
											parameters.to.save = parameters,
											model.file = paste("C:/Users/", user, 
																"/Documents/GitHub/Columbia-Spotted-Frogs/models/",
																mod_name, 
																".txt", 
																sep=""),
											n.chains = nc, 
											n.thin = nt, 
											n.iter = ni, 
											n.burnin = nb), silent = T)			
				}else{
					out <- try(jags(data = data, 
								inits = inits, 
								parameters.to.save = parameters,
								model.file = paste("C:/Users/", 
													user, 
													"/Documents/GitHub/Columbia-Spotted-Frogs/models/",
													mod_name,
													".txt", 
													sep=""),
								n.chains = nc, 
								n.thin = nt, 
								n.iter = ni, 
								n.burnin = nb), silent = T)
				}
				
				alarm()
			
				# Save output of JAGS run
				if(class(out) != "try-error"){
					save(out, file = file.path("C:/Users", 
												user, 
												"Documents/GitHub/Columbia-Spotted-Frogs/Results", 
												paste(user, 
														"_", 
														mod_name,
														"_",
														format(as.POSIXlt(Sys.time()), 
																"%d%b%Y_%H%M%S"), 
														".RData", sep = "")))
				}
			}
			
			if(return_fit){
				return(out)
			}else{
				return(NULL)
			}
		}
		
#################################################################################
		# #  Example call		
		# fit <- call_jags(mod_name,
							# parallel = F,
							# ni = 100,
							# nt = 1,
							# nb = 10,
							# nc = 3,
							# debug_mode = T,
							# return_fit = T)
				
		
		# # Traceplots
		# traceplot(fit)
		# mcmcplot(fit)		
	
		# # Histograms of parameters
		# hist(fit$BUGS$sims.list$mean.phi)
		# hist(fit$BUGS$sims.list$mean.p)
		# hist(fit$BUGS$sims.list$p_star)
		# hist(fit$BUGS$sims.list$beta1)
		# hist(fit$BUGS$sims.list$beta2)
		# hist(fit$BUGS$sims.list$beta3)
		# hist(fit$BUGS$sims.list$beta4)

	
		## See Output
		#ls()
		#fit
		#names(fit)
		#str(fit)