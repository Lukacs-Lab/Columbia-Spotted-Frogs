# Anna Moeller
# 3/25/2015
# Frog Data Extraction
################################################################################
library(stringr)
library(dplyr)
library(tidyr)

# Read in data
x <- read.csv("C:/frog/Frog.csv", as.is = T)

# Make a list where each element is a long data frame of a variable we want
# col_ext is a pattern to match in original column names
# col_new is the name of the new column in the long dataset, made from all the wide columns
# sub_0 is replaces 0s with NA
# If there are multiple columns that need to be combined into one column, then we do gather
#     and add prim, sec columns
# If there is only one column to begin with, we just arrange it
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
            mutate(prim = gsub(".*([1-9][0-9]?)_.*([1-9]).*", "\\1", occ),
                   sec = gsub(".*([1-9][0-9]?)_.*([1-9]).*", "\\2", occ)) %>%
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

# Things to check: 
# Is NA different than <NA>?
# Do we need 0s or NAs in eh for the model?
# Should we change U in encounter history to NA?
