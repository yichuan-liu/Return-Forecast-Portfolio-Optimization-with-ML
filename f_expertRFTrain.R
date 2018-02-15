library(randomForest)
library(strip)

expertRFTrain <- function(now, rdb = NULL, fdb = NULL, 
                          dxn = P_DXN, idn = P_IDN, rtn = P_RTN, v = F) {
  # Trains an expert using random forest on past data (now or earlier)
  #
  # Args:
  #   now: the latest time index that the model can access.
  #   rdb: data frame containing return data (default = use global default)
  #   fdb: data frame containing financial ratio data (default = use global default)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for ids (default = P_IDN)
  #   rtn: column name for returns (default = P_RTN)
  #   v: verbose mode if true
  #
  # Returns:
  #   One trained random forest model object
  
  # Log file
  logfile = "f_expertRFTrain.txt"
  if(v) cat("Training RF/LM at time", now, "\n", file=logfile, append=T)
  
  # Get default data sets if none are supplied
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  if(is.null(fdb)) fdb <- get(P_FRDB, globalenv())
  
  # Past return panel
  panel <- NULL
  for (lkbk in 0) {
    # List of date indices for past returns
    pdex <- rev(now-lkbk-(0:12))
    # Past returns
    p.rets <- getReturns(pdex, rdb = rdb, cum = F)
    # Remove rows containing NA returns
    p.rets$chk <- rowMeans(p.rets[,-1])
    p.rets <- p.rets[!is.na(p.rets$chk),]
    p.rets$chk <- NULL
    # Remove column names
    names(p.rets) <- NULL
    prets <- as.matrix(p.rets)
    # Add the current slice to the overall panel
    panel <- rbind(panel, p.rets)
  }
  
  if(v) cat("  Past return panel dimensions:", dim(panel), "\n", file=logfile, append=T)
  
  # Training set
  tr.y <- panel[,dim(panel)[2]]
  tr.x <- panel[,2:(dim(panel)[2]-1)]
  
  if(v) cat("Training set X dimensions:", dim(tr.x), "\n", file=logfile, append=T)
  
  # (Optionally) trim away outliers
  core <- (tr.y<=quantile(tr.y,0.99)) & (tr.y>=quantile(tr.y,0.01))
  tr.y <- tr.y[core]
  tr.x <- tr.x[core,]
  
  if(v) cat("  Trimmed panel dimensions:", dim(tr.x), "\n", file=logfile, append=T)
  
  # Train random forest model
  rf.mdl <- randomForest(tr.x, tr.y, ntree=500)
  
  if(v) cat("  Correlation:", cor(predict(rf.mdl), tr.y), "\n", file=logfile, append=T)
  
  # Train OLS model
  ls.mdl <- lm(tr.y~as.matrix(tr.x))
  
  # Strip models to reduce storage requirement
  rf.mdl <- strip(rf.mdl, keep = "predict")
  ls.mdl <- strip(ls.mdl, keep = "predict")
  
  # Output
  out <- NULL
  out <- list()
  out[[1]] <- rf.mdl
  out[[2]] <- ls.mdl
  
  return(out)
}

# models <- expertRFTrain(900, v=T)

