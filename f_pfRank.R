pfRank <- function(df, pct=0.3, sep=F, caps=NULL, ew=T) {
  # Ranks variables and computes portfolio weights
  #
  # Args:
  #   df: data frame containing sorting variables
  #   pct: percentage of sample to include in long and short portions
  #   sep: long and short weights are in separate columns if true
  #   caps: list of asset capitalization; vw portfolio weights are produced if not null
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
  
  # If the first column is IDs
  if(names(df)[1] == P_IDN) {
    weights <- as.data.frame(df[,1])
    df <- df[,-1]
  } else {
    weights <- as.data.frame(1:dim(df)[1])
  }
  names(weights) <- P_IDN
  
  # Percentile Rank
  for(vcol in 1:dim(df)[2]) {
    # Variable name
    name <- names(df)[vcol]
    # Quantile ranks
    qtl <- rank(df[,vcol], na.last = "keep")/sum(!is.na(df[,vcol]))
    # Give a rank of 0.5 to NA entries
    qtl[is.na(qtl)] <- 0.5
    # Up/down dummies
    d.up <- (qtl>(1-pct)) * 1
    d.dn <- (qtl<=pct) * 1
    # Equal weights
    if(ew) weights[,paste0(name,".ew")] <- d.up/sum(d.up) - d.dn/sum(d.dn)
    # Value weights
    if (!is.null(caps)) {
      weights[,paste0(name)] <- d.up*caps/sum(d.up*caps) - d.dn*caps/sum(d.dn*caps)
    }
    
  }
  
  return(weights)
}

# df <- data.frame(10001:10010)
# names(df) <- P_IDN
# df$v1 <- sample(1:10)
# df$v2 <- sample(1:10)
# df$v3 <- sample(1:10)
# print(df)
# ranks <- pfRank(df, caps=c(1:10), ew=F)
# print(ranks)
