
######################################################
### Functions used in ROTFL analysis
######################################################

################################################################
# maps ks.test comparing Landsat overpasses with full Q distriubtion

myks <- function(full,sat){
  x = full %>%
    filter(!is.na(q),q > 0) %>%
    pull(q) %>%
    jitter()
  
  y = sat %>%
    filter(!is.na(q), q > 0) %>%
    pull(q) %>%
    jitter()
  
  tk <- ks.test(x,y)
  out = tibble(d=tk$statistic,ks_p.value=tk$p.value,nsat=length(y), npop=length(x),sdq=sd((x)),
               mq=mean((x)), mq_sat=mean((y)), sdq_sat=sd((y)))
  return(out)
}

############################################

myboots <- function(full,sat){
  x = full %>%
    filter(!is.na(q),q > 0) %>%
    pull(q)
  y = sat %>%
    filter(!is.na(q), q > 0) %>%
    pull(q)
  
  boot <- Matching::ks.boot(x,y,nboots=500)
  
  out = tibble(d_boot = boot$ks$statistic, pvalue_boot = boot$ks.boot.pvalue  )
  
  return(out)
}

##################################

mywilcox <- function(full,sat){
  x = full %>%
    dplyr::filter(!is.na(q),q > 0) %>%
    pull(q) %>%
    jitter()
  
  y = sat %>%
    dplyr::filter(!is.na(q), q > 0) %>%
    pull(q) %>%
    jitter()
  
  tk <- wilcox.test(x,y, paired = F, correct=T)
  
  out = tibble(w=tk$statistic, wc_p.value=tk$p.value)
  
  return(out)
}


###########################################

RBIcalc <- function(data){##FUNCTION.RBIcalc.START
  
  time.start <- proc.time();
  
  Q <- data %>% 
    pull(q) 
  
  Q[Q==0] <- NA
  # Size
  myLen <- length(Q)
  # Add previous record in second column
  Qprev <- c(NA,Q[-myLen])
  # Create dataframe.
  myData <- as.data.frame(cbind(Q,Qprev))
  # delta (absolute)
  myData[,"AbsDelta"] <- abs(myData[,"Q"] - myData[,"Qprev"])
  # SumQ
  SumQ <- sum(myData[,"Q"],na.rm=TRUE)
  # Sum Delta
  SumDelta <- sum(myData[,"AbsDelta"], na.rm=TRUE)
  
  RBIsum <- SumDelta / SumQ
  
  time.elaps <- proc.time()-time.start
  # Return RBI value 
  return(RBIsum)
}


##############################

mycvm <- function(full, sat) {
  require(twosamples)
  
  x = full %>%
    filter(!is.na(q),q > 0) %>%
    pull(q) %>%
    jitter()
  
  y = sat %>%
    filter(!is.na(q), q > 0) %>%
    pull(q) %>%
    jitter()
  
  cvm <- cvm_test(x,y, nboot=50)
  out = tibble(stat = cvm[1], cvm_p.value=cvm[2])
  return(out)
}