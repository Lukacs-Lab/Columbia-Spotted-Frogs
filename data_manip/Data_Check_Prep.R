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
#anna <- "C:/Users/charles.henderson/Documents/GitHub/Columbia-Spotted-Frogs/data_manip/Data_Extraction.R"
#source(anna)

#########################################################################
# Anna's fixed code to use until I figure out how to download from github
# then can uncomment above code

extract_fun <- function(col_ext, col_new, sub_0 = T){
  #  Takes pattern to match using contains(), must be a character string that matches some column(s) of the data
  #  Returns a new list containing only the requested data
  if(sub_0){
    x[x == 0] <- NA
  }
  
  tmp <- x %>%
    select(Index, contains(col_ext))
  
  if(ncol(tmp) > 2){
    out <- tmp %>%
      gather_("occ", col_new, 2:ncol(.), convert = T) %>%
      arrange(Index) %>%
      mutate(prim = gsub("[A-Za-z]*([1-9][0-9]?)_.*", "\\1", occ),
             sec = gsub(".*_[A-Za-z]*([1-9]).*", "\\1", occ)) %>%
      select(-occ)
    
  }else{
    out <- tmp %>%
      arrange(Index)
  }
}

# Example call
xobs <- list("eh" = extract_fun("_Sec", "eh"), 
             "weight" = extract_fun("Weight", "weight"), 
             "length" = extract_fun("SVL", "length"),
             "toes" = extract_fun("toes", "toes"),
             "first" = extract_fun("First.Marked", "first"))


# look at list
lapply(xobs, head)

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
          select(Index, eh, fm, wt)

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
          select(Index, eh, fm, length)


#############################################################################
# join wt and length data frames create weight to length ratio
############################################################################

fwl <- inner_join(fwt, fsvl, by = "Index") %>%
              # add column with weight/length ratio
              mutate(length = as.numeric(length), ratio = wt/length, eh = eh.x,
                     fm = fm.x) %>%
              select(Index, eh, fm, wt, length, ratio )
              
hist(wt_len$ratio)  
  
#############################################################################
# toe clip data from individuals that were juveniles at first capture
############################################################################

toe <- extract_fun("toes", "toes")

ftoe <- inner_join(fwl, toe, by = "Index") %>%
            # number of toes removed <= 6 to eliminate individuals that lost 
            # toes to something other than marking
            filter(Number_Toes_Removed <= 7) %>%
            mutate(toes = Number_Toes_Removed) %>%
            select(-Number_Toes_Removed)
                 
hist(ftoe$Number_Toes_Removed)

#############################################################################
# sex of individuals that were juveniles at first capture
############################################################################

sex <- extract_fun("Sex", "sex")

fsex <- inner_join(fwl, sex, by = "Index") %>%
            filter(Sex == "M" | Sex == "F") 
            


#############################################################################
#############################################################################
