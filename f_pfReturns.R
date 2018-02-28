library(tidyr)

pfReturns <- function(weights, hp, fill=0, rdb=NULL, dxn=P_DXN, idn=P_IDN, rtn=P_RTN) {
  # Computes portfolio returns for the specified weights and holding period (hp)
  #
  # Args:
  #   weights: data frame containing ids and K weights
  #   hp: holding period (a number or list of numbers)
  #   fill: fill in NA returns (default = 0: invest in the risk-free asset instead)
  #   rdb: return data set (use global default if NULL)
  #   dxn: column name for time index
  #   idn: column name for IDs
  #   rtn: column name for returns
  #
  # Returns:
  #   Portfolio return for the given period
  #
  
  # Keep track of time
  pt <- proc.time()
  
  # Get the default return data set if nothing is supplied
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  
  # Get the set of IDs
  ids <- unique(weights[,idn])
  ids <- sort(ids)
  
  # Check that IDs are unique and sorted in weights
  if(!all(weights[,idn]==ids))
  {
    cat("ERROR in pfReturns: IDs in weights must be sorted (asc) and unique.\n")
    return()
  }
  
  # Take the slice of the return data that includes the holding period 
  # and IDs with weights
  slc <- rdb[rdb[,dxn]>=min(hp) & rdb[,dxn]<=max(hp),c(dxn, idn, rtn)]
  rm(rdb)
  
  # Make return panel
  rets = as.data.frame(ids)
  names(rets) <- idn

  # Fill return panel
  for (cd in hp) {
    # add in the current-period returns
    rets <- merge(rets, slc[slc[,dxn]==cd, c(idn, rtn)], by=idn, all.x=T)
    # Fill NA entries
    rets[is.na(rets[,rtn]),rtn] <- 0
    # Rename the column to r##
    rets[,paste0('r',cd)] <- rets[,rtn]
    rets[,rtn] <- NULL
    # print(head(rets))
  }
  
  # Alternative code
  # idf = as.data.frame(ids)
  # names(idf) <- idn
  # rets <- merge(idf, spread(slc, dxn, rtn), by=idn, all.x=T)
  # rets[is.na(rets)] <- 0
  
  # print(dim(weights[,-1]))
  # print(dim(rets[,-1]))
  # print(length(rets[,-1]))
  
  # Compute portfolio returns for each portfolio-period combination
  out <- t(as.matrix(rets[,-1])) %*% as.matrix(weights[,-1])
  
  # Add in time indices
  out <- as.data.frame(cbind(hp, out))
  names(out) <- names(weights)
  
  # Record the time
  note("pfReturns: runtime =", (proc.time() - pt)[1], lvl=3)
  
  return(out)
  
}


# umd.rets <- pfReturns(weights, 501:502)
# print(umd.rets)
# rm(umd.rets)