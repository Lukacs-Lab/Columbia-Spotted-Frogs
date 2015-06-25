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
		
		#######  For the following scenarios (with sex covariate), you must use data  ##########
		#######   subsetted just the individuals for which sex is recorded   ###################
		#  Scenario 4 - Toe and sex
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "pred_surv")
		fit <- call_jags("toe_n_n_sex_n_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 5 - Toe, sex and length
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "beta3", "pred_surv")
		fit <- call_jags("toe_n_length_sex_n_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 6 - Toe, sex and weight
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "beta3", "pred_surv")
		fit <- call_jags("toe_weight_n_sex_n_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 7 - Toe, sex, length and interaction length*sex
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "beta3", "beta4", "pred_surv")
		fit <- call_jags("toe_n_length_sex_n_ls",
					parallel = F,
					ni = 30000,
					nt = 12,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
					
		#  Scenario 8 - Toe, sex, weight and interaction weight*sex
		parameters <- c("mean_phi", "p_star", "beta1", "beta2", "beta3", "beta4", "pred_surv")
		fit <- call_jags("toe_weight_n_sex_ws_n",
					parallel = F,
					ni = 30000,
					nt = 2,
					nb = 15000,
					nc = 3,
					debug_mode = F,
					return_fit = T)		
