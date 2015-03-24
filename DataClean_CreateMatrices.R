  require(stringr)

  setwd("C:/frogs")
  x <- read.csv("Frog.csv", as.is = T)
  
  #  Unique id for individuals
  uid <- x$Index
  
  #  Make encounter history matrix
  col.index <- grep("Sec", names(x))[-1]
  eh <- x[,col.index]

  #  Find and look at the one N values
  eh[which(apply(eh, 1, function(x) any(x == "N") )),]

  #  The N is a J, so change it
  eh[eh == "N"] <- "J"

  #  Subset to juveniles
  age.index <- apply(eh, 1, function(x){
    any(x == "J" | x == "j")
  })
  ehj <- eh[age.index,]

  #  Fill blanks
  #  Create CJS like capture history, recode M,S,J to alive, 
  #  dead  
  ehj[ehj == ""] <- 0
  ehj[ehj != 0] <- 1

  #  Capture occassion
  cap <- apply(ehj, 1, function(x){
    min(which(x == 1))
  })
  capture <- colnames(ehj)[cap]
  primary <- substr(capture, 5, str_locate(capture, "_")-1)
  secondary <- substr(capture, str_locate(capture, "_")+4,
                      nchar(capture))
  #
  x2 <- x[age.index,]
  weight.nm <- paste("X", primary, "_", secondary, "Weight", 
                  sep = "")
  weight <- vector("numeric")
  for(i in 1:nrow(ehj)){
    weight[i] <- x2[i, weight.nm[i]]
  }
  length.nm <- paste("X", primary, "_", secondary, "MeanSVL", 
                  sep = "")
  length <- vector("numeric")
  for(i in 1:nrow(ehj)){
    length[i] <- x2[i, length.nm[i]]
  }
  
  toes <- x2$Number_Toes_Removed
  
  frog_dat <- list("ehj" = ehj,
                   "weight" = weight,
                   "length" = length,
                   "toes" = toes)
  save(frog_dat, file = "frog_dat_list.RData")