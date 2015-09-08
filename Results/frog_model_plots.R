# Frog survival - length model results plot
# Lukacs Lab
# 9/2/2015
#################################################################################

#  Load packages
library(stringr)
library(dplyr)
library(tidyr)

#  Read in data
x <- read.csv("C:/frog/Frog.csv", as.is = T)

#  Run data extraction function
source(file.path("C:/Users", 
				Sys.info()["login"],
				"Documents/GitHub/Columbia-Spotted-Frogs/data_manip/extract_fun.R"))
#  Run data check function
source(file.path("C:/Users", 
				Sys.info()["login"],
				"Documents/GitHub/Columbia-Spotted-Frogs/data_manip/Data_Check_Prep.R"))

fEH <- get_data()
#################################################################################

#  For scaled and centered toe+length, and unscaled survival. 
#   Mean parameter estimates 
beta1 <- -0.151 # toe
beta2 <- 0.190 # length
mean_phi <- 0.156 # mean survival
unscale <- qlogis(mean_phi) # unscaled mean survival

#  Get a single length for each individual, then calculate survival for mean_phi + 0*toe + beta2*length
frog <- group_by(fEH, Index) %>%
  summarise(len = mean(sc_len)) %>%
  arrange(len) %>%
  mutate(sc_len = as.numeric(scale(len, center = T)),
					surv = unscale + beta2 * sc_len)
#################################################################################
	 
#  Create credible intervals on plots using iterations from MCMC.
#   Load saved MCMC values as R objects for mean_phi, beta1, and beta2
load(file.path("C:/Users", 
										Sys.info()["login"],
										"Documents/GitHub/Columbia-Spotted-Frogs/Results/sim_reps_mean_phi.RData"))
				
load(file.path("C:/Users", 
										Sys.info()["login"],
										"Documents/GitHub/Columbia-Spotted-Frogs/Results/sim_reps_beta1.RData"))
				
load(file.path("C:/Users", 
										Sys.info()["login"],
										"Documents/GitHub/Columbia-Spotted-Frogs/Results/sim_reps_beta2.RData"))

#   Unscale mean_phi parameter values for all iterations										
iter_unscale <- qlogis(sim_reps_mean_phi) # unscaled mean survival over all iteration reps

#  Give a name to the object for the iterations of beta2
iter_beta2 <- sim_reps_beta2

#   Get value for length, scale and arrange
iter_len <- group_by(fEH, Index) %>%
						summarise(len = mean(sc_len)) %>%
						arrange(len) %>%
						mutate(sc_len = as.numeric(scale(len, center = T)))
#   Separate out just the column "sc_len" and make a numeric vector for use in plot.
iter_len <- iter_len$sc_len				
#################################################################################

#  Create plot
#   Number of pulls out of MCMC iterations.
n <- 5000
keep <- sample(1:length(sim_reps_mean_phi), n, replace = F)

plot(iter_len, seq(0,1,length.out=length(iter_len)), 
		type="n", 
		ylim=c(0.05, 0.3),
		main = "Columbia Spotted Frog Annual Survival",
		xlab = "Length (scaled and centered)",
		ylab = "Annual Survival", 
		bty="L")

for (j in 1:n){
		iter_surv <- iter_unscale[keep[j]] + iter_beta2[keep[j]] * iter_len
		lines(iter_len, plogis(iter_surv), col= grey(.9,.3))
	}

	#  Add line for length and survival, scaled and centered
	lines (frog$sc_len, plogis(frog$surv), col="grey40", lwd = 2)
	
	
 lines (lowess(plogis(frog$surv) ~ frog$sc_len), col="red", lwd = 2)

 seq_len <- seq(min(frog$sc_len), max(frog$sc_len), length.out=5000)
 y <- unscale + beta2*seq_len
 lines(seq_len, plogis(y), lwd = 1)