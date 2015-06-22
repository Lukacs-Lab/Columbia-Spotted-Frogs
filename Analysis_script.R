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
		parameters <- c("mean_phi", "mu_p", "p_star", "beta1", "pred_surv")
		fit <- call_jags("toe_n_n_n_n_n",
					parallel = F,
					ni = 5000,
					nt = 1,
					nb = 1000,
					nc = 3,
					debug_mode = T,
					return_fit = T)
					
		mcmcplot(fit)
					
		#########  parameters to monitor not likely correct !!!!  ####################
		#########  ni and nb too low given example runs  ########################
		#########  call_jags does not handle data correctly for sex models  ######### 
		######### "mean_p" not estimable in current form of model script ###########
		#  Scenario 0 - Null
		parameters <- c("mean_phi", "p_star", "pred_surv")
		fit <- call_jags("n_n_n_n_n_n",
					parallel = F,
					ni = 5000,
					nt = 1,
					nb = 1000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
		
		
		#  Scenario 1 - Toe only
		parameters <- c("mean_phi", "p_star", "beta1", "pred_surv")
		fit <- call_jags("toe_n_n_n_n_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 2 - Toe and length
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "pred_surv")
		fit <- call_jags("toe_n_length_n_n_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 3 - Toe and weight
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "pred_surv")
		fit <- call_jags("toe_weight_n_n_n_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 4 - Toe and sex
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "pred_surv")
		fit <- call_jags("toe_n_n_sex_n_n",
					parallel = F,
					ni = 5000,
					nt = 1,
					nb = 1000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		