gatherData <- function(train, now, rdb = NULL, fdb = NULL, 
                           dxn = P_DXN, idn = P_IDN, rtn = P_RTN, v = F) {
  # Gathers data for training and predicting
  #
  # Args:
  #   train: if true, gather data for training the model (includes the y variable)
  #          if false, gather data for predicting the model (no y variable)
  #   now: if train=T, now is the latest time index that the model can access.
  #        if train=F, now-1 is the latest time index that the predictor can access.
  #   rdb: data frame containing return data (default = use global default)
  #   fdb: data frame containing financial ratio data (default = use global default)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for ids (default = P_IDN)
  #   rtn: column name for returns (default = P_RTN)
  #   v: verbose mode if true
  #
  # Returns:
  #   Data frame containing all predictors and, if train=T, the y variable.
  #   (WARNING: the first column of the returned df is always the ID column (idn), 
  #    so it must be removed before training/predicting)
  
  # Parameters ----
  
  # Note level (default = 3)
  note.lvl <- 3
  # % missing thresholds for removing a ratio column
  fr.threshold <- if (train) 0.5 else 0.75
  # Preserve only the top N firms by capitalization
  cap.threshold <- if (train) 1200 else 1000
  # Whether or not to fill NA cells with column median
  fill.na <- T
  # Turn predictors into quantiles
  x.qtl <- F
  y.qtl <- F
  
  # Keep track of time
  st <- Sys.time()
  
  # Procuring data ----
  # Log file
  note("gatherData: gathering", (if (train) "training" else "predicting"), "data at time", now, lvl = note.lvl)
  
  # Get default data sets if none are supplied
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  if(is.null(fdb)) fdb <- get(P_FRDB, globalenv())
  
  # Past 12-month average excluding the most recent month
  note("gatherData: gathering past 12-month return...", lvl = note.lvl)
  pdex <- rev(now - (2:12))
  p.rets <- getReturns(pdex, rdb = rdb, cum = T, gm = T)
  names(p.rets) <- c(idn, "c12")
  
  # Past return (most recent 1 month)
  note("gatherData: gathering past 1-month return...", lvl = note.lvl)
  p.ret1 <- getReturns(now - 1, rdb = rdb, cum = F)
  names(p.ret1) <- c(idn, "r1")
  
  # Financial ratios
  note("gatherData: gathering WRDS financial ratios...", lvl = note.lvl)
  
  # Financial ratios (1 month prior)
  fin.ratios <- fdb[fdb[, dxn] == (now - 1),]
  fin.ratios[, dxn] <- NULL
  fr.names <- names(fin.ratios[, -1])
  # Remove ratios that are NA for more than 50% of stocks
  for (fn in fr.names) {
    pct.missing <- missing(fin.ratios[, fn])
    if (pct.missing > fr.threshold) {
      fin.ratios[, fn] <- NULL
      note("gatherData: removed", fn, "w/", round(pct.missing, 2), "missing", lvl = note.lvl)
    }
  }
  
  
  # Assembling the panel ----
  
  # Merge predictors
  m1 <- merge(p.rets, p.ret1, by=idn)
  m2 <- merge(m1, fin.ratios, by=idn)
  
  # Final assembly
  panel <- m2
  # predictor columns
  pred.cols <- 2:dim(panel)[2]
  note("gatherData: the raw predictor set has", dim(panel)[1], "obs and", 
       dim(panel)[2] - 1, "vars", lvl = note.lvl)
  
  # Add in the y variable if train=T
  if (train) {
    # Get the y variable
    y.var <- getReturns(now, rdb = rdb, cum = F)
    names(y.var) <- c(idn, "y")
    # Include the y variable
    panel <- merge(m2, y.var, by = idn)
    # Remove rows in which the y-var is missing
    panel <- panel[!is.na(panel$y), ]
  }
  
  # Use only the top N stocks by market cap
  caps <- rdb[rdb[, P_DXN] == (now - 1), c(P_IDN, P_CPN)]
  caps[, P_CPN] <- rank(-caps[, P_CPN])
  panel2 <- merge(panel, caps, by = P_IDN)
  panel <- panel2[panel2[, P_CPN] <= cap.threshold, ]
  panel[, P_CPN] <- NULL
  
  # Fill with column median
  if (fill.na) {
    for (col in 2:dim(panel)[2]) {
      panel[is.na(panel[, col]), col] <- median(panel[, col], na.rm = T)
    }
  } else {
    # Keep only non-NA rows
    panel <- panel[complete.cases(panel), ]
  }
  
  # Turn quantities into percentile ranks
  if (y.qtl) {
    panel$y <- rank(panel$y, ties.method = "average") / length(panel$y)
  }
  if (x.qtl) {
    for (col in pred.cols)
    {
      panel[,col] <- rank(panel[,col]) / length(panel[,col], ties.method = "average")
    }
  }

  note("gatherData: the final predictor set has", dim(panel)[1], "obs", lvl = note.lvl)
  
  
  # Note the run time ----
  note("gatherData: runtime =", as.numeric(Sys.time() - st), lvl = note.lvl)
  
  return(panel)
}

