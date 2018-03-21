expertManage <- function(now, rdb = NULL, v = T) {
  # Manages and evaluates the existing pool of experts (expdb and expnf)
  #
  # Args:
  #   now: current dex
  #   rdb: return database that will be passed down to the predictor function (default: NULL)
  #   v: verbose mode
  #
  # Returns:
  #   None
  
  # Keep track of time
  st <- Sys.time()
  
  # Initialization ----
  note("expertManage: initializing", lvl = 1)
  
  # Return if there are no experts alive.
  if(!exists("expdb") || is.null(expdb) ) {
    note("expertManage: there are no experts alive; returning...", lvl = 1)
    return()
  }
  
  # Return if there is only one expert alive.
  if(dim(expnf)[1] == 1) {
    note("expertManage: there is one expert alive; awaiting a second opinion...", lvl = 1)
    return()
  }
  
  
  # Evaluate existing experts ----
  
  # Number of experts
  exp.cnt <- dim(expnf)[1]
  note("expertManage: managing", exp.cnt, "experts...", lvl = 1)
  
  # Get current performance data
  if(dim(expnf)[1] > 0 && sum(expnf$w) > 0) old.wts <- expnf$w / sum(expnf$w)
  else old.wts <- 0
  
  # Compute L-S portfolio returns based on predicted values
  if (sum(old.wts)>0) {
    preds <- expertRFPredict(now, rdb = rdb)
    pf.rets <- expertPortfolios(now, preds, exp.wts = old.wts, rdb = rdb)
  } else {
    preds <- expertRFPredict(now, rdb = rdb)
    pf.rets <- expertPortfolios(now, preds, exp.wts = NULL, rdb = rdb)
  }
  # Get the extra portfolio names
  ext.names <- names(pf.rets)[(length(expnf$eid) + 2):dim(pf.rets)[2]]
  
  # Get the return of each expert's L-S portfolio
  exp.rets <- as.numeric(pf.rets[1,2:(length(expnf$eid) + 1)])
  
  # Make note of the old weights
  note("expertManage: old wts: H", round(max(old.wts), 4), "L", round(min(old.wts), 4), 
       "M", round(mean(old.wts), 4), "SD", round(sd(old.wts),4), lvl = 1)
  note("expertManage: exp rts: H", round(max(exp.rets), 4), "L", round(min(exp.rets), 4), 
       "M", round(mean(exp.rets), 4), "SD", round(sd(exp.rets), 4), lvl = 1)
  note("expertManage: old prf: H", round(max(expnf$perf), 4), "L", round(min(expnf$perf), 4), 
       "M", round(mean(expnf$perf), 4), "SD", round(sd(expnf$perf),4), lvl = 1)
  
  # Performance measure = weighted average of last period's performance measure and current return
  expnf$perf <<- P_LAMBDA *  exp.rets + (1 - P_LAMBDA) * expnf$perf
  # Compute new weights based on updated performance measures
  expnf$w  <<- expertWeights(now, expnf)
  
  # Make note of the new weights
  note("expertManage: cur prf: H", round(max(expnf$perf), 4), "L", round(min(expnf$perf), 4), 
       "M", round(mean(expnf$perf), 4), "SD", round(sd(expnf$perf), 4), lvl = 1)
  note("expertManage: new wts: H", round(max(expnf$w),4), "L", round(min(expnf$w), 4), 
       "M", round(mean(expnf$w), 4), "SD", round(sd(expnf$w), 4), lvl = 1)
  
  
  # Track long-short portfolio returns ----
  
  # Put returns in a nice table
  cur.perf <- as.data.frame(cbind(now, c(expnf$eid, ext.names), as.numeric(pf.rets[,-1]) ))
  names(cur.perf) <- c(P_DXN, "eid", "ret")
  
  # Save returns to a tracking table
  track("perf", cur.perf)
  
  # Remove old experts (older than P_EXPLF) ----
  
  # Cutoff point
  cutoff <- now - P_EXPLF
  cutoff.row <- 0
  del.cnt <- 0
  for(i in 1:dim(expnf)[1]) {
    # If the expert is born before the cutoff date...
    if(expnf[i,"bdex"]<cutoff) {
      # Locate the model's position index in the database
      loc <- expnf[i,"loc"]
      # Remove the model
      expdb[[loc]] <<- 0
      # Set cutoff row
      cutoff.row <- i
    } else {
      break
    }
  }
  
  # Remove rows of old experts in the information table
  expnf <<- expnf[(cutoff.row+1):dim(expnf)[1],]
  # Number of experts removed
  del.cnt <- cutoff.row
  
  # Note the number of experts deleted
  if(del.cnt > 0) note("expertManage: removed", del.cnt, "experts;", dim(expnf)[1], "remain.", lvl = 1)
  
  # Record the time
  note("expertManage: runtime =", as.numeric(Sys.time() - st), lvl = 1)
}

# # Debug code
# expertManage(now)