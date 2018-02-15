rankToWeights <- function(df, name, pct=0.3, sep=F, rkn=P_RKN, cpn=P_CPN, ew=T) {
  # Converts rankings to long-short portfolio weights (equally- and value-weighted)
  #
  # Args:
  #   df: data frame containing ranking and cap information
  #   name: name to call the output portfolio
  #   pct: percentage of sample to include in long and short portions
  #   sep: long and short weights are in separate columns if true
  #   rkn: column name for ranks
  #   cpn: column name for caps
  #   ew: include equally-weighted portfolios if true
  #
  # Returns:
  #   Table containing ids, equal weights and value weights
  #
  
  if(pct>0.5)
  {
    cat("ERROR in rankToWeights: pct cannot exceed 50%.\n")
    return(NULL)
  }
  
  # Percentiles
  df[,"pct"] <- df[,rkn]/length(df[,rkn])
  
  # Up/down dummies
  df[,"pfu"] <- (df[,"pct"]>=(1-pct)) * 1
  df[,"pfd"] <- (df[,"pct"]<=pct) * 1
  # Equal weights
  if(ew) df[,paste0(name,".ew")] <- df[,"pfu"]/sum(df[,"pfu"]) - df[,"pfd"]/sum(df[,"pfd"])
  # Value weights
  if (!is.null(cpn)) {
    df[,paste0(name,".vw")] <- df[,"pfu"]*df[,cpn]/sum(df[,"pfu"]*df[,cpn]) - 
      df[,"pfd"]*df[,cpn]/sum(df[,"pfd"]*df[,cpn])
  }
  # print(head(df))

  # Clean up
  df[,c("pct", "pfu", "pfd")] <- NULL
  return(df)
}
