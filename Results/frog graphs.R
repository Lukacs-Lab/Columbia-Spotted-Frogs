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

