library(randomForest)
library(ranger)

expertRFPredict <- function(now, rdb = NULL, fdb = NULL, 
                            dxn = P_DXN, idn = P_IDN, rtn = P_RTN, v = F) {
  # Predicts the returns of all assets with each expert
  #
  # Args:
  #   now: predicted period (using data up to now-1)
  #   rdb: data frame containing return data (default = use global default)
  #   fdb: data frame containing financial ratio data (default = use global default)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for ids (default = P_IDN)
  #   rtn: column name for returns (default = P_RTN)
  #   v: verbose mode if true
  #
  # Returns:
  #   Data frame containing predicted returns of all experts
  
  # Keep track of time
  st <- Sys.time()
  
  # Get return data set
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  
  # Get the data panel
  panel <- gatherData(F, now, rdb = rdb, fdb = fdb, 
                      dxn = dxn, idn = idn, rtn = rtn, v = F)
  
  # Produce the test set
  ts.x <- panel[,-1] # Remove the first column (ID)
  # ts.x <- panel.rk[,2:(dim(panel.rk)[2])]
  # ts.x <- panel[,2:(dim(panel)[2])]
  note("expertRFPredict: the test set has", dim(ts.x)[1], "rows and", dim(ts.x)[2], "cols.", lvl = 2)
  
  # Create the table to hold prediction results
  preds <- as.data.frame(panel[,1])
  names(preds) <- idn
  
  # Let the experts predict ----
  
  # Loop over all experts
  tm()
  exp.cnt <- 0 # Count of experts that have predicted
  for(e in expdb)
  {
    if(class(e) == "randomForest") {
      exp.cnt <- exp.cnt + 1
      preds <- cbind(preds, predict(e, ts.x))
      names(preds)[dim(preds)[2]] <- exp.cnt
    } else if (class(e) == "ranger") {
      exp.cnt <- exp.cnt + 1
      ivn <- e$forest$independent.variable.names
      preds <- cbind(preds, predict(e, data = ts.x[,ivn])$predictions)
      names(preds)[dim(preds)[2]] <- exp.cnt
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
