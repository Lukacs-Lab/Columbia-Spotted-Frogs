		#  Analysis script
		#  Lukacs Lab
		#  05/18/2015
#################################################################################
		#  Source prep scripts
		source(file.path("C:/Users", 
							Sys.info()["login"],
							"Documents/GitHub/Columbia-Spotted-Frogs/call_jags.R"))
#################################################################################
		#  Call models
		#  Example debug call
		parameters <- c("mean_phi", "mean_p", "p_star", "beta1", "pred_surv")
		fit <- call_jags("toe_n_n_n_n_n",
					parallel = F,
					ni = 5000,
					nt = 1,
					nb = 1000,
					nc = 3,
					debug_mode = T,
					return_fit = T)
					
		mcmcplot(fit)
					
		#########  parameters to monitor not likely correct !!!!  ###############
		#########  ni and nb too low given example runs  ########################
		#########  call_jags does not handle data correctly for sex models  ##### "mean_p"
		
		#  Scenario 1
		parameters <- c("mean_phi", "mean_p", "p_star", "beta1", "pred_surv")
		call_jags("toe_n_n_n_n_n",
					parallel = F,
					ni = 5000,
					nt = 1,
					nb = 1000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 2
		parameters <- c("mean_phi", "mean_p", "p_star", "beta1", "beta2", 
						"beta3")
		call_jags("toe_n_length_sex_n_ls",
					parallel = T,
					ni = 10000,
					nt = 1,
					nb = 5000,
					nc = 3,
					debug_mode = F,
					return_fit = F)		
					
		#  Scenario 3
		parameters <- c("mean_phi", "mean_p", "p_star", "beta1", "beta2")
		call_jags("toe_n_n_sex_n_n",
					parallel = T,
					ni = 10000,
					nt = 1,
					nb = 5000,
					nc = 3,
					debug_mode = F,
					return_fit = F)		
					
		#  Scenario .....