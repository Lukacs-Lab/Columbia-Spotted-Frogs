# Charlie Henderson
# 4/8/2015
# Frog Data Extraction
################################################################################
library(stringr)
library(dplyr)
library(tidyr)
###############################################################################
# Read in data
x <- read.csv("C:/frog/Frog.csv", as.is = T)

# put frogs in table format for easy viewing
frog <- tbl_df(x)

# source Anna's code for extracting 
anna <- "C:/Users/charles.henderson/Documents/GitHub/Columbia-Spotted-Frogs/data_manip/Data_Extraction.R"
source(anna)

# look at list
lapply(xobs, head)

##############################################################################
# make dataframe of individuals who are juveniles at first capture
##################################################################

# number of individuals in each category F, J, M, N, S
table(xobs$eh$eh)
eh <- extract_fun("_Sec", "eh")
# extract individuals captured as juveniles
t2 <- filter(eh, eh == "J" )
nrow(t2)
# group by Index
t2b <- group_by(t2, Index)
# determine number of unique individuals captured as juveniles
summarise(t2b, nid = n_distinct(Index))
#####################################################
# extract sex/age and weight
wt_age <- data.frame(extract_fun("_Sec", "eh"),
                   extract_fun("Weight", "wt")[,-1],
                   extract_fun("First.Marked", "first")) %>%
                      
first <- mutate(wt_age, fm = paste(Index.1,Prim.Sec.First.Marked, sep = "_")) %>%
            filter(eh =="J" & !is.na(wt))

head(first)

j_wt_age <- filter(wt_age, eh == "J" & !is.na(wt))
head(j_wt_age, 50)
summarise(j_wt_age, avg = mean(wt),
                    min = min(wt),
                    max = max(wt))

#################################################################
#################################################################

# extracts when each individual is first marked
first <- extract_fun("First.Marked", "first")

test <- data.frame(extract_fun("First.Marked", "first"),
                   sex = extract_fun("Sex", "sex")[,-1])

# make dataframe of extracted weight info
wt <- extract_fun("Weight", "weight")
head(wt, 20)