# MISCELLANEOUS HELPER FUNCTIONS FOR EXPERTS
# Miscellaneous helper functions for experts

expWeights <- function(now, experts, brate=12, parms=list(wn="iwt", bn="bdex", pn="perf"))
{
  # Compute expert weight(s) for one or many experts
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
  # Weights: if just born
  suppressWarnings(ifelse(now<=bdex, 0, iwt * pmin( (now-bdex)/(ndex-bdex), 1) * exp(perf/sqrt(now - bdex))))
}

