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

#  For scaled and centered toe+length, and transformed survival (not on real probability scale)
#   Mean parameter estimates 
beta1 <- -0.151 # toe
beta2 <- 0.190 # length
mean_phi <- 0.156 # mean survival
trans <- qlogis(mean_phi) # unscaled mean survival

#  Look at change in survival across lengths
surv1 <- trans + beta2*(-1.845) # scaled and centered value for 20mm length
surv2 <- trans + beta2*(0.416667) # scaled and centered value for 30mm length
#   Transform to real probability scale
diff_surv <- plogis(surv2) - plogis(surv1)


#  Get a single length for each individual, then calculate survival for mean_phi + 0*toe + beta2*length
frog <- fEH%>%
            group_by(Index) %>%
            summarise(len = mean(sc_len)) %>%
            arrange(len) %>%
            mutate(scaled_len = as.numeric(scale(len, center = T)), surv = trans + beta2 * scaled_len) %>%
            ungroup(.) %>%
            as.data.frame(.)
#   Separate out just the column "scaled_len" and make a numeric vector for use in plot.
iter_len <- frog$scaled_len

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

#   Untransformed mean_phi parameter values for all iterations                    
iter_trans <- qlogis(sim_reps_mean_phi) # untransformed mean survival over all iteration reps
#  Give a name to the object for the iterations of beta2
iter_beta2 <- sim_reps_beta2
#################################################################################

#  Create plot
#   Number of pulls out of MCMC iterations.
n <- 5000
keep <- sample(1:length(sim_reps_mean_phi), n, replace = F)

par(mar=c(5,5,4,2))
plot(iter_len, seq(0,1,length.out=length(iter_len)), 
        type="n", 
        ylim=c(0.10, 0.25),
        xaxt="n",
        #main = "Columbia Spotted Frog Annual Survival",
        xlab = "Length at first capture (mm)",
        ylab = "Mean annual survival probability", 
        cex.lab=1.5,
        cex.axis=1.5,
        bty="L")
axis(1, at=c(-1.845, -0.714, 0.417, 1.548) ,labels=c("20.0", "25.0", "30.0", "35.0"), cex.axis = 1.5)


for (j in 1:n){
    iter_surv <- iter_trans[keep[j]] + iter_beta2[keep[j]] * iter_len
    lines(iter_len, plogis(iter_surv), col= grey(.9,.3))
  }

#  Add line for length and survival, scaled and centered
lines (frog$scaled_len, plogis(frog$surv), col="grey40", lwd = 2)





lines (lowess(plogis(frog$surv) ~ frog$scaled_len), col="red", lwd = 2)
seq_len <- seq(min(frog$scaled_len), max(frog$scaled_len), length.out=5000)
y <- trans + beta2*seq_len
lines(seq_len, plogis(y), lwd = 1)