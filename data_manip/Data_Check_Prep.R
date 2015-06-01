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
# anna <- "C:/Users/sara.williams/Documents/GitHub/Columbia-Spotted-Frogs/data_manip/extract_fun.R"
# source(anna)

#########################################################################

# # Example call
# xobs <- list("eh" = extract_fun("_Sec", "eh"), 
#              "weight" = extract_fun("Weight", "weight"), 
#              "length" = extract_fun("SVL", "length"),
#              "toes" = extract_fun("toes", "toes"),
#              "first" = extract_fun("First.Marked", "first"))
# 

# look at list
# lapply(xobs, head)

get_data <- function(){

  
##############################################################################
# make dataframe of individuals who are juveniles at first capture
##################################################################


# extracts when each individual is first marked
first <- extract_fun("First.Marked", "first")

# extracts age at capture extracts juveniles                      
age <- data.frame(extract_fun("_Sec", "eh")) %>%                       
  filter(eh == "J") 

# joins first marked and age "J" to create a dataframe of individuals that were
# juveniles at first capture
fcap <- inner_join(first, age, by = "Index") %>%
  mutate(fm = paste(prim, sec, sep = "")) %>%
  filter(Prim.Sec.First.Marked == fm)




##############################################################################
# extract weight at first capture data
#############################################################################

wt <- data.frame(extract_fun("Weight", "wt")) %>%
          mutate(fm2 = paste(prim, sec, sep = "")) 
          
fwt <- inner_join(fcap, wt, by = "Index") %>%
          filter(Prim.Sec.First.Marked == fm2 & !is.na(wt)) %>%
          # change weight filter here
          filter( wt >= 0.5 & wt <= 7) %>%
          mutate(sc_wt = as.numeric(scale(wt, center = T, scale = T))) %>%
          select(Index, eh, fm, wt, sc_wt)

##############################################################################
# extract length at first capture data
#############################################################################

svl <- data.frame(extract_fun("SVL", "length")) %>%
            mutate(fm2 = paste(prim, sec, sep = ""))

fsvl <- inner_join(fcap, svl, by = "Index") %>%
          filter(Prim.Sec.First.Marked == fm2 & !is.na(length)) %>%
          mutate(length = as.numeric(length)) %>%
          # change length filter here
          filter( length >= 20 & length <= 38) %>%
          mutate(sc_len = as.numeric(scale(length, center = T, scale = T))) %>%
          select(Index, eh, fm, length, sc_len)


#############################################################################
# join wt and length data frames create weight to length ratio
############################################################################

fwl <- inner_join(fwt, fsvl, by = "Index") %>%
              # add column with weight/length ratio
              mutate( ratio = wt/length, eh = eh.x,
                     fm = fm.x) %>%
              select(Index, eh, fm, wt, sc_wt, length, sc_len, ratio )
              
 
  
#############################################################################
# toe clip data from individuals that were juveniles at first capture
############################################################################

toe <- extract_fun("toes", "toes")

ftoe <- inner_join(fwl, toe, by = "Index") %>%
            # number of toes removed <= 7 to eliminate individuals that lost 
            # toes to something other than marking
            filter(Number_Toes_Removed <= 7) %>%
            mutate(toes = as.numeric(Number_Toes_Removed)) %>%
            select(-Number_Toes_Removed)
                 
#############################################################################
# get encounter history for individuals meeting all capture 
# weight, length, and toe requirements
#############################################################################

  
EH <- data.frame(extract_fun("_Sec", "eh")) 

fEH <- inner_join(ftoe, EH, by = "Index") %>%
            mutate(cap = as.numeric(eh.y %in% c("J", "F", "M", "N", "S"))) %>%
            select(Index, cap, prim, sec, sc_wt, sc_len, toes, ratio) %>%
            group_by(Index) %>%
            filter(cumsum(cap) >= 1) 
            
fEH <- as.data.frame(fEH)

stopifnot(all(fEH$cap %in% c("0", "1")))

return(fEH)
# end get_data function
}

#############################################################################
#############################################################################
#############################################################################
# function to get sex data

  get_sex_data <- function(){
  
  
  ##############################################################################
  # make dataframe of individuals who are juveniles at first capture
  ##################################################################
  
  
  # extracts when each individual is first marked
  first <- extract_fun("First.Marked", "first")
  
  # extracts age at capture extracts juveniles                      
  age <- data.frame(extract_fun("_Sec", "eh")) %>%                       
    filter(eh == "J") 
  
  # joins first marked and age "J" to create a dataframe of individuals that were
  # juveniles at first capture
  fcap <- inner_join(first, age, by = "Index") %>%
    mutate(fm = paste(prim, sec, sep = "")) %>%
    filter(Prim.Sec.First.Marked == fm)
  
  
  
  
  ##############################################################################
  # extract weight at first capture data
  #############################################################################
  
  wt <- data.frame(extract_fun("Weight", "wt")) %>%
    mutate(fm2 = paste(prim, sec, sep = "")) 
  
  fwt <- inner_join(fcap, wt, by = "Index") %>%
    filter(Prim.Sec.First.Marked == fm2 & !is.na(wt)) %>%
    # change weight filter here
    filter( wt >= 0.5 & wt <= 7) %>%
    mutate(sc_wt = as.numeric(scale(wt, center = T, scale = T))) %>%
    select(Index, eh, fm, wt, sc_wt)
  
  ##############################################################################
  # extract length at first capture data
  #############################################################################
  
  svl <- data.frame(extract_fun("SVL", "length")) %>%
    mutate(fm2 = paste(prim, sec, sep = ""))
  
  fsvl <- inner_join(fcap, svl, by = "Index") %>%
    filter(Prim.Sec.First.Marked == fm2 & !is.na(length)) %>%
    mutate(length = as.numeric(length)) %>%
    # change length filter here
    filter( length >= 20 & length <= 38) %>%
    mutate(sc_len = as.numeric(scale(length, center = T, scale = T))) %>%
    select(Index, eh, fm, length, sc_len)
  
  
  #############################################################################
  # join wt and length data frames create weight to length ratio
  ############################################################################
  
  fwl <- inner_join(fwt, fsvl, by = "Index") %>%
    # add column with weight/length ratio
    mutate( ratio = wt/length, eh = eh.x,
            fm = fm.x) %>%
    select(Index, eh, fm, wt, sc_wt, length, sc_len, ratio )
  
  
  
  #############################################################################
  # toe clip data from individuals that were juveniles at first capture
  ############################################################################
  
  toe <- extract_fun("toes", "toes")
  
  ftoe <- inner_join(fwl, toe, by = "Index") %>%
    # number of toes removed <= 7 to eliminate individuals that lost 
    # toes to something other than marking
    filter(Number_Toes_Removed <= 7) %>%
    mutate(toes = as.numeric(Number_Toes_Removed)) %>%
    select(-Number_Toes_Removed)
  
  #############################################################################
  # get encounter history for individuals meeting all capture 
  # weight, length, and toe requirements
  #############################################################################
  
  
  EH <- data.frame(extract_fun("_Sec", "eh")) 
  

  fEH <- inner_join(ftoe, EH, by = "Index") %>%
    mutate(cap = as.numeric(eh.y %in% c("J", "F", "M", "N", "S"))) %>%
    select(Index, cap, prim, sec, sc_wt, sc_len, toes, ratio) %>%
    group_by(Index) %>%
    filter(cumsum(cap) >= 1) 
  
    
  fEH <- as.data.frame(fEH)
  
  stopifnot(all(fEH$cap %in% c("0", "1")))
  
  #############################################################################
  # sex of individuals that were juveniles at first capture
  ############################################################################
  
    sex <- extract_fun("Sex", "sex")
    
    fsex <- inner_join(fEH, sex, by = "Index") %>%
      filter(Sex == "M" | Sex == "F")
  
    
    
    return(fsex)
    
  # end get_sex_data function
}   