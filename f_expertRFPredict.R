library(randomForest)
library(ranger)

expertRFPredict <- function(now, rdb = NULL, fdb = NULL, sets = 1,
                            dxn = P_DXN, idn = P_IDN, rtn = P_RTN, v = F) {
  # Predicts the returns of all assets with each expert
  #
  # Args:
  #   now: predicted period (using data up to now-1)
  #   rdb: data frame containing return data (default = use global default)
  #   fdb: data frame containing financial ratio data (default = use global default)
  #   sets: predictor sets, containing values in (1, 2) (default = 1)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for ids (default = P_IDN)
  #   rtn: column name for returns (default = P_RTN)
  #   v: verbose mode if true
  #
  # Returns:
  #   Data frame containing predicted returns of all experts
  
  # Validate set
  if (!all(sets %in% c(1, 2))) {
    print("WARNING in expertRFPredict: invalid set IDs; SETS #1 & #2 will be used.")
    sets <- c(1, 2)
  }
  set1 <- (1 %in% sets)
  set2 <- (2 %in% sets)
  
  # Keep track of time
  st <- Sys.time()
  
  # Get return data set
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  
  # Get the data panels & test sets
  if (set1) {
    # data panel
    panel <- gatherData(F, now, rdb = rdb, fdb = fdb, set = 1, 
                        dxn = dxn, idn = idn, rtn = rtn, v = F)
    
    note("expertRFPredict: SET #1 has", dim(panel)[1], "rows and", dim(panel)[2] - 1, "cols.", lvl = 2)
  } else {
    ts.x <- NULL # Placeholder
  }
  if (set2) {
    # data panel
    s2.panel <- gatherData(F, now, rdb = rdb, fdb = fdb, set = 2, 
                           dxn = dxn, idn = idn, rtn = rtn, v = F)
    note("expertRFPredict: SET #2 has", dim(s2.panel)[1], "rows and", dim(s2.panel)[2] - 1, "cols.", lvl = 2)
  } else {
    s2.ts.x <- NULL # Placeholder
  }
  
  # aggregate predictor sets
  if (set1 && !set2) {
    ts.x <- panel
  } else if (set2 && !set1) {
    ts.x <- s2.panel
  } else if (set1 && set2) {
    ts.x <- merge(panel, s2.panel, by = idn)
  }
  
  # test set
  ts.x <- ts.x[order(ts.x[, idn]), ]
  ids  <- ts.x[, idn]
  ts.x <- ts.x[,-1] # Remove the first column (ID)
  note("expertRFPredict: the test set has", dim(ts.x)[1], "rows and", dim(ts.x)[2], "cols.", lvl = 2)
  
  # Create the table to hold prediction results
  preds <- as.data.frame(ids)
  names(preds) <- idn
  
  # Let the experts predict ----
  
  # Loop over all experts
  tm()
  exp.cnt <- 0 # Count of experts that have predicted
  for(e in expdb)
  {
    if (class(e) == "ranger") {
      # predictor names, used to match predictor set
      ivn <- e$forest$independent.variable.names
      # Predict
      preds <- cbind(preds, predict(e, data = ts.x[, ivn])$predictions)
      # Number the predictor
      names(preds)[dim(preds)[2]] <- exp.cnt
      # Increment count
      exp.cnt <- exp.cnt + 1
      
    } else if (class(e)=="lm") {
      exp.cnt <- exp.cnt + 1
      preds <- cbind(preds, as.matrix(ts.x) %*% e$coefficients[-1] + e$coefficients[1])
      names(preds)[dim(preds)[2]] <- exp.cnt
    }
  }
  note("expertRFPredict: expert prediction took", tm(F), "secs.", lvl = 2)
  
  note("expertRFPredict: runtime =", as.numeric(Sys.time()-st), "secs.", lvl = 2)
  
  return(preds)
}

# # Debug code
# slice <- c2[c2$dex>=1400 & c2$dex<=1416,c(P_DXN, P_IDN, P_RTN, P_CPN)]
# tm()
# rets <- expertRFPredict(now, rdb = slice)
# print(tm(F))
