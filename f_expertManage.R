expertManage <- function(now, expdb = NULL, expnf = NULL, rdb = NULL, v = T) {
  # Manages and evaluates the existing pool of experts
  #
  # Args:
  #   now: current dex
  #   expdb: data base containing expert models (default: use global settings)
  #   expnf: data base containing expert information (default: use global settings)
  #   rdb: return database that will be passed down to the predictor function (default: NULL)
  #   v: verbose mode
  #
  # Returns:
  #   None
  
  # Initialization ----
  
  # Log file
  logfile = "f_expertManage.txt"
  if(v) cat("Managing experts at time", now, "\n", file=logfile, append=T)
  
  # Return if there are no experts alive.
  if(is.null(expdb) && !exists(P_EXPDB, envir = globalenv())) return()
  
  # Load default expert database if none are supplied
  if(is.null(expdb)) expdb <- get(P_EXPDB)
  if(is.null(expnf)) expnf <- get(P_EXPNF)
  
  # Evaluate existing experts ----
  
  # Get current performance data
  if(dim(expnf)[1]>0) old.wts <- expnf$w / sum(expnf$w)
  else old.wts <- 0
  
  # Compute L-S portfolio returns based on predicted values
  if (sum(old.wts)>0) {
    pf.rets <- expertRFPredict(now, exp.wts = old.wts, rdb = rdb)
  } else {
    pf.rets <- expertRFPredict(now, exp.wts = NULL, rdb = rdb)
  }
  
  # Update performance record of experts
  exp.rets <- as.numeric(pf.rets[1,2:(length(expnf$eid)+1)])
  
  # print(expnf$perf)
  # print(exp.rets)
  expnf$perf <- P_LAMBDA * expnf$perf + (1 - P_LAMBDA) * exp.rets
  expnf$w  <- expertWeights(now, expnf)
  # print(expnf$w)
  
  # Store current portfolio performance data ----
  cur.perf <- as.data.frame(cbind(now, c(expnf$eid, "med", "avg", "exp"), as.numeric(pf.rets[,-1]) ))
  names(cur.perf) <- c(P_DXN, "eid", "ret")
  
  # print(cur.perf)
  
  # Save performance data to tracker
  track("perf", cur.perf)
  
  # Trim old experts ----
  # Cutoff point (default = 10 years)
  cutoff <- now - 120
  cutoff.row <- 0
  del.cnt <- 0
  for(i in 1:dim(expnf)[1]) {
    # If the expert is born before the cutoff date...
    if(expnf[i,"bdex"]<cutoff) {
      # Locate the model location in the database
      loc <- expnf[i,"loc"]
      # Remove the model
      expdb[[loc]] <- 0
      # Set cutoff row
      cutoff.row <- i
    } else {
      break
    }
  }
  expnf <- expnf[(cutoff.row+1):dim(expnf)[1],]
  del.cnt <- cutoff.row
  if(del.cnt>0 && v) cat("- ", del.cnt, "experts deleted.\n", file=logfile, append=T)
  
  # Garbage collection
  gc()
  
  # Set global variables
  assign(P_EXPDB, expdb, envir = globalenv())
  assign(P_EXPNF, expnf, envir = globalenv())
}

# expertManage(now)