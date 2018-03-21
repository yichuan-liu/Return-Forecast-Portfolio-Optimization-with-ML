# Miscellaneous helper functions for experts
# expertReset: resets the expert database
# expertWeights: computes expert weights

expertCount <- function() {
  # Counts the number of experts
  # Args:
  #   None
  #
  # Returns:
  #   Number of experts
  
  return(0)
}

expertReset <- function() {
  # Resets the expert database
  suppressWarnings(rm(list=c("expdb","expnf"), envir = globalenv()))
}

expertWeights <- function(now, experts, brate=P_EXPBR, wn="iwt", bn="bdex", pn="perf")
{
  # Computes expert weight(s) for one or many experts
  #
  # Args:
  #   now: the time variable representing the current period
  #   experts: the table containing information about experts
  #   brate: number of periods between the births of two consecutive experts
  #   parms: list of column names: wn = initial weights; bn = birth periods; pn = performance measures
  #
  # Returns:
  #   Vector of weights
  
  # Initial weights
  iwt = experts[,wn]
  # Birthdays
  bdex = experts[,bn]
  # Performance measures
  perf = experts[,pn]
  # Next expert's birthdays
  ndex = bdex + brate
  # Weights: 0 if just born or not yet born; otherwise use formula
  # suppressWarnings(ifelse(now<=bdex, 0, iwt * pmin( (now-bdex)/(ndex-bdex), 1) * exp(50* perf/sqrt(now - bdex))))
  
  # NO RAMP
  suppressWarnings(ifelse(now<=bdex, 0, iwt * exp(10* perf/sqrt(now - bdex))))
  # suppressWarnings(ifelse(now<=bdex, 0, exp(perf)))
}

# expertWeights(now, z_expnf)
