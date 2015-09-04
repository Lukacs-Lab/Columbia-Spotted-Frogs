# Frog graphs
# Anna Moeller
# 9/2/2015

# Load packages
library(stringr)
library(dplyr)
library(tidyr)

# Read in data
x <- read.csv("C:/frog/Frog.csv", as.is = T)

# Run data extraction function
source("C:/Users/anna.moeller/Documents/GitHub/Columbia-Spotted-Frogs-1/data_manip/extract_fun.R")

# Run data check function
source("C:/Users/anna.moeller/Documents/GitHub/Columbia-Spotted-Frogs-1/data_manip/Data_Check_Prep.R")
fEH <- get_data()

# For scaled and centered toe+length, 
beta1 <- -0.151 # toe
beta2 <- 0.190 # length
mean_phi <- 0.156 # mean survival
unscale <- qlogis(mean_phi) # unscaled mean survival

# Get a single length for each individual, then calculate survival for mean_phi + 0*toe + beta2*length
frog <- group_by(fEH, Index) %>%
  summarise(len = mean(sc_len)) %>%
  arrange(len) %>%
  mutate(sc_len = as.numeric(scale(len, center = T)),
         surv = unscale + beta2 * sc_len)

# Make line for length and survival, scaled and centered
plot(frog$sc_len, plogis(frog$surv),
     main = "Columbia Spotted Frog Annual Survival",
     xlab = "Length (scaled and centered)",
     ylab = "Annual Survival",
     type = "l")
# Add confidence intervals

# Summarize length, weight by sex
sex <- get_sex_data()
sex2 <- group_by(sex, Sex) %>%
  summarise(len = mean(sc_len), len_sd = sd(sc_len), wt = mean(sc_wt), wt_sd = sd(sc_wt))

# Sex      len   len_sd       wt    wt_sd
# (chr)    (dbl)    (dbl)    (dbl)    (dbl)
# 1     F 31.68683 4.451490 3.127518 1.372645
# 2     M 31.31998 4.266771 3.078286 1.402304


