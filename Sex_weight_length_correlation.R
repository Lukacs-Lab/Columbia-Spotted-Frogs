		#  Analysis script
		# Sara Williams
		#  9/4/2015
#################################################################################

# Load packages
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

#  Load data sets  
fEH <- get_data()
fsex <- get_sex_data() 

mean(sex_dat$sc_wt)				
mean(fEH$sc_wt)
#   0.3879728
#	-0.1694245

mean(sex_dat$sc_len)	
mean(fEH$sc_len)
#   0.7524703
#   0.0299451

boxplot(sex_dat$sc_wt)
boxplot(fEH$sc_wt)

summary(sex_dat$sc_wt)
summary(fEH$sc_wt)



		sex_num <- tapply(fsex$Sex, as.numeric(as.factor(fsex$Index)), function(x){
		ifelse(unique(x) == "M", 0, 1)
		})
		
		sex_num <- as.data.frame(sex_num)
		
		sex_index <- fsex %>%  
							# group_by(Index) %>% 
							# summarise(sex_info = any(!is.na(Sex)))	
							
		temp <- bind_cols(sex_index, sex_num)
		
		sex_dat <- inner_join(fsex, temp, by = "Index")
		
		cor_dat <- left_join(fEH, sex_dat, by = "Index")
		new <- select(cor_dat, Index, sc_wt.x,  sc_len.x, toes.x, sex_num)
		z <- distinct(new)
		z$sex_num[is.na(z$sex_num)] <- "Unknown"
		z$sex_num[z$sex_num==1] <- "F"
		z$sex_num[z$sex_num==0] <- "M"
		names(z) <- c("index", "weight", "length", "toes", "sex")
		
		z %>%
		group_by(sex) %>%
		summarise(avg = mean(length))
		
		plot(z$weight, z$sex, data=z)



		
		y <- z %>%
                mutate(sex_info = ifelse(as.character(sex_num) == 2, 0, 1))
									
		
		fit <- aov(sc_wt.x ~ sex_info, data=y)
		summary(fit) # display Type I ANOVA table
		drop1(fit,~.,test="F") # type III SS and F Tests
		plot(fit)
		