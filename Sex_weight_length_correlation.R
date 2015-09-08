# Investigating issues with sex covariate because of length and weight requirements for 
#  sex determination.
# Lukacs Lab
#  9/4/2015
#################################################################################

# Load packages
library(stringr)
library(dplyr)
library(tidyr)

#  Run data extraction function
source(file.path("C:/Users", 
				Sys.info()["login"],
				"Documents/GitHub/Columbia-Spotted-Frogs/data_manip/extract_fun.R"))

#  Run data check function
source(file.path("C:/Users", 
				Sys.info()["login"],
				"Documents/GitHub/Columbia-Spotted-Frogs/data_manip/Data_Check_Prep.R"))

#  Load data sets and filter for unique individuals
#   Load data set for all individuals.
#   Create column for sex_num (whether sex was determined or not).
#   Set all rows to 0 for sex_num (because this is the data set where sex was not determined).
fEH <- get_data()
fEH["sex_num"] <- NA
fEH$sex_num <- 0

#  Load data set for individuals where sex WAS determined.
#   Create column for sex_num (whether sex was determined or not).
#   Set all rows to 1 for sex_num (because this is the data set where sex WAS determined).
fsex <- get_sex_data() 
temp_sex <- mutate(fsex, sex_num = ifelse(Sex == "M", 1, 1))
temp_sex <- select(temp_sex, -Sex)

#  Bind both data sets together (all rows).
all_dat <- bind_rows(fEH, temp_sex)

#  Arrange in descending order by sex_num then select top row for each indivudal.
temp_dat <- all_dat %>%
						group_by(Index) %>% 
						arrange(desc(sex_num)) %>%
						as.data.frame(.)
uni_dat <- distinct(temp_dat, Index)

#  Center and scale this uniqified data set for all covariates.	
uni_dat $sc_wt <- as.numeric(scale(uni_dat $sc_wt))
uni_dat $sc_len <- as.numeric(scale(uni_dat $sc_len))
uni_dat $toes <- as.numeric(scale(uni_dat $toes))

#  Split up into 2 datasets - one for indivudals where sex was not determined and one for indiviuals
#   where sex was determined (sex_num = 0 and 1).
uni_sex_dat <- filter(uni_dat, sex_num > 0)
uni_all_dat <-  filter(uni_dat, sex_num < 1)

#  Look at means of these two groups
mean(uni_sex_dat$sc_wt)				
mean(uni_all_dat$sc_wt)
#  0.690
#	-0.114

mean(uni_sex_dat$sc_len)	
mean(uni_all_dat$sc_len)
#  0.683
#  -0.113

#  Plot
par(mfrow=c(2,2))
boxplot(uni_sex_dat$sc_wt, main = "Weigth - sex determined individuals")
boxplot(uni_all_dat$sc_wt, main = "Weigth - sex NOT determined individuals")
boxplot(uni_sex_dat$sc_len, main = "Length - sex determined individuals")
boxplot(uni_all_dat$sc_len, main = "Length - sex NOT determined individuals")

summary(uni_sex_dat$sc_wt)
summary(uni_all_dat$sc_wt)

fit_wt <- lm(uni_dat$sc_wt ~ uni_dat$sex_num, data=uni_dat)
fit_len <- lm(uni_dat$sc_len ~ uni_dat$sex_num, data=uni_dat)
summary(fit_wt) 
summary(fit_len)
		