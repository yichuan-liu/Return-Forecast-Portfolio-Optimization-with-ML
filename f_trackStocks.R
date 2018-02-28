trackStocks <- function(cur.perf) {
  # Tracks the performance of individual assets in the L-S portfolio
  #
  # Args:
  #   cur.perf: Nx3 data frame containing IDs, current return and L-S portfolio return
  #
  # Returns:
  #   None
  
  if (is.null(cur.perf) || is.null(dim(cur.perf)) || dim(cur.perf)[2]!=3) {
    print("WARNING in trackStocks: invalid input.")
    return()
  }
  
  # Keep only complete records
  cur.perf <- cur.perf[complete.cases(cur.perf),]
  
  # Get name
  rname <- names(cur.perf)[2]
  pname <- names(cur.perf)[3]
  
  # Add return squared
  cur.perf[,"ret.sq"] <- cur.perf[,2]^2
  cur.perf[,"retxpf"] <- cur.perf[,2]*cur.perf[,3]
  cur.perf[,"addcnt"] <- 1
  
  # Create individual performance record if it doesn't exist already
  if (!exists("z_indperf", envir = globalenv()) || is.null(z_indperf)) {
    z_indperf <<- cur.perf
    names(z_indperf) <<- c(P_IDN, "t.r", "t.p", "t.r2", "t.rp", "t.cnt")
  } else {
    # Merge performance with existing record
    z_indperf <<- merge(z_indperf, cur.perf, by=P_IDN, all = T)
    
    # Fill with zero
    z_indperf[is.na(z_indperf)] <<- 0
    
    # Process new information
    z_indperf[,"t.r"] <<- z_indperf[,"t.r"] + z_indperf[,rname] 
    z_indperf[,"t.p"] <<- z_indperf[,"t.p"] + z_indperf[,pname] 
    z_indperf[,"t.r2"] <<- z_indperf[,"t.r2"] + z_indperf[,"ret.sq"]
    z_indperf[,"t.rp"] <<- z_indperf[,"t.rp"] + z_indperf[,"retxpf"]
    z_indperf[,"t.cnt"] <<- z_indperf[,"t.cnt"] + z_indperf[,"addcnt"]
    
    # Keep only the t. columns
    z_indperf <<- z_indperf[,c(P_IDN, "t.r", "t.p", "t.r2", "t.rp", "t.cnt")]
  }
  
}


trackStocksReset <- function() {
  # Resets the stock tracker
  if(exists("z_indperf", envir = globalenv())) {
    rm(z_indperf, envir = globalenv())
  }
}

trackStocksGetPerf <- function(ids = NULL, v = F) {
  # Computes stock performance saved in the tracker
  #
  # Args:
  #   ids: list of IDs (default = NULL - use all available records)
  #   v: verbose mode if true
  #
  # Returns:
  #   data frame consisting of IDs, mean, variance and covariance
  
  if (!exists("z_indperf", envir = globalenv()) || is.null(z_indperf) || is.null(dim(z_indperf))) {
    if (v) print("WARNING in trackStocksGetPerf: no valid stock records found.")
    return()
  }
  
  if (is.null(ids)) {
    report <- z_indperf
  } else {
    report <- z_indperf[z_indperf[,P_IDN] %in% ids, ]
  }
  
  
  report[,"m"] <- report[,"t.r"]/report[,"t.cnt"]
  report[,"v"] <- report[,"t.r2"]/report[,"t.cnt"] - report[,"m"]^2
  report[,"c"] <- report[,"t.rp"]/report[,"t.cnt"] - report[,"m"] * report[,"t.p"]/report[,"t.cnt"]
  report[,"n"] <- report[,"t.cnt"]
  
  return(report[,c(P_IDN, "m", "v", "c", "n")])
}


# trackStocksReset()
# n1 <- data.frame(permno=1:10)
# n1$ret <- 2:11
# n1$pret <- 3
# trackStocks(n1)
# n2 <- data.frame(permno=2:10)
# n2$ret <- 4:12
# n2$pret <- 5
# trackStocks(n2)
# print(trackStocksGetPerf())