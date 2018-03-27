gatherData <- function(train, now, rdb = NULL, fdb = NULL, set = 1, 
                           dxn = P_DXN, idn = P_IDN, cpn = P_CPN, rtn = P_RTN, v = F) {
  # Gathers data for training and predicting
  #
  # Args:
  #   train: if true, gather data for training the model (includes the y variable)
  #          if false, gather data for predicting the model (no y variable)
  #   now: if train=T, now is the latest time index that the model can access.
  #        if train=F, now-1 is the latest time index that the predictor can access.
  #   rdb: data frame containing return data (default = use global default)
  #   fdb: data frame containing financial ratio data (default = use global default)
  #   set: predictor set = one of (1, 2) (default = 1)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for ids (default = P_IDN)
  #   cpn: column name for cap (default = P_CPN)
  #   rtn: column name for returns (default = P_RTN)
  #   v: verbose mode if true
  #
  # Returns:
  #   Data frame containing all predictors and, if train=T, the y variable.
  #   (WARNING: the first column of the returned df is always the ID column (idn), 
  #    so it must be removed before training/predicting)
  
  # Parameters ----
  
  # Validate set
  if (!(set %in% c(1,2))) {
    print("WARNING in gatherData: invalid set ID; SET #1 will be used.")
    set <- 1
  }
  
  # Note level (default = 3)
  note.lvl <- 3
  # % missing thresholds for removing a ratio column
  fr.threshold <- if (train) 0.5 else 0.75
  # Preserve only the top N firms by capitalization
  cap.threshold <- if (train) 1200 else 1000
  # Whether or not to fill NA cells with column median
  fill.med <- T
  # Turn predictors into quantiles
  x.qtl <- F
  y.qtl <- F
  
  # Keep track of time
  st <- Sys.time()
  
  # Get default data sets if none are supplied
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  if(is.null(fdb)) fdb <- get(P_FRDB, globalenv())
  
  
  # Procuring data ----
  
  # Note
  note("gatherData: gathering", (if (train) "training" else "predicting"), 
       "data at time", now, lvl = note.lvl)
  
  if (set == 1) {
    # Past 12-month average excluding the most recent month
    note("gatherData: (SET 1) gathering past 12-month return...", lvl = note.lvl)
    pdex <- rev(now - (2:12))
    p.rets <- getReturns(pdex, rdb = rdb, cum = T, gm = T)
    names(p.rets) <- c(idn, "c12")
    
    # Past return (most recent 1 month)
    note("gatherData: (SET 1) gathering past 1-month return...", lvl = note.lvl)
    p.ret1 <- getReturns(now - 1, rdb = rdb, cum = F)
    names(p.ret1) <- c(idn, "r1")
    
    # Max return (1 month)
    note("gatherData: (SET 1) gathering 1-month max & min return...", lvl = note.lvl)
    p.max1 <- rdb[rdb[, dxn] == (now - 1), c(idn, P_MXN, P_MNN)]
    names(p.max1) <- c(idn, "max1", "min1")
    
    # Financial ratios (1 month prior)
    note("gatherData: (SET 1) gathering WRDS financial ratios...", lvl = note.lvl)
    fin.ratios <- fdb[fdb[, dxn] == (now - 1),]
    fin.ratios[, dxn] <- NULL
    fr.names <- names(fin.ratios[, -1])
    # Remove ratios that have too many NA entries
    for (fn in fr.names) {
      pct.missing <- missing(fin.ratios[, fn])
      if (pct.missing > fr.threshold) {
        fin.ratios[, fn] <- NULL
        note("gatherData: (SET 1) removed", fn, "w/", round(pct.missing, 2), "missing", lvl = note.lvl)
      }
    }
  } else if (set == 2) {
    # Note
    note("gatherData: (SET 2) gathering", (if (train) "training" else "predicting"), 
         "data at time", now, lvl = note.lvl)
    
    # Average of past returns from (now - 23) to (now - 13)
    note("gatherData: (SET 2) gathering average return 2 years ago...", lvl = note.lvl)
    pdex <- rev(now - (13:23))
    p.rets <- getReturns(pdex, rdb = rdb, cum = T, gm = T)
    names(p.rets) <- c(idn, "c24")
    
    # Past return at time (now - 12)
    note("gatherData: (SET 2) gathering 1-month return a year ago...", lvl = note.lvl)
    p.ret1 <- getReturns(now - 12, rdb = rdb, cum = F)
    names(p.ret1) <- c(idn, "r12")
    
    # Financial ratios at time (now - 12)
    note("gatherData: (SET 2) gathering WRDS financial ratios...", lvl = note.lvl)
    fin.ratios <- fdb[fdb[, dxn] == (now - 12),]
    fin.ratios[, dxn] <- NULL
    # Add the suffix "12" to differentiate these predictors from set 1
    names(fin.ratios)[-1] <- paste0(names(fin.ratios)[-1], "12") 
    fr.names <- names(fin.ratios[, -1])
    # Remove ratios that have too many NA entries
    for (fn in fr.names) {
      pct.missing <- missing(fin.ratios[, fn])
      if (pct.missing > fr.threshold) {
        fin.ratios[, fn] <- NULL
        note("gatherData: (SET 2) removed", fn, "w/", round(pct.missing, 2), "missing", lvl = note.lvl)
      }
    }
  }
  
  
  # Assembling the panel ----
  
  if (set == 1 || set == 2) {
    # Merge predictors
    m1 <- merge(p.rets, p.ret1, by = idn)
    m2 <- merge(m1, p.max1, by = idn)
    m3 <- merge(m2, fin.ratios, by = idn)
    # Final assembly
    panel <- m3
    
  } else if (set == 2) {
    # Merge predictors
    m1 <- merge(p.rets, p.ret1, by = idn)
    m2 <- merge(m1, fin.ratios, by = idn)
    # Final assembly
    panel <- m2
  }
  
  
  
  # predictor columns
  pred.cols <- 2:dim(panel)[2]
  note("gatherData: the raw predictor set has", dim(panel)[1], "obs and", 
       dim(panel)[2] - 1, "vars", lvl = note.lvl)
  
  
  # Add in the y variable if train=T ----
  if (train) {
    if (set == 1) {
      # Get the y variable
      y.var <- getReturns(now, rdb = rdb, cum = F)
    } else if (set == 2) {
      # Get the y variable
      y.var <- getReturns(rev(now - (0:11)), rdb = rdb, cum = T, gm = T, fillna = 0)
    }
    # Set column names
    names(y.var) <- c(idn, "y")
    # Include the y variable
    panel <- merge(panel, y.var, by = idn)
    # Remove rows in which the y-var is missing
    panel <- panel[!is.na(panel$y), ]
  }
  
  # Post-processing ----
  
  # Use only the top N stocks by market cap
  if (set == 1) {
    caps <- rdb[rdb[, dxn] == (now - 1), c(idn, cpn)]
  } else if (set == 2) {
    caps <- rdb[rdb[, dxn] == (now - 12), c(idn, cpn)]
  }
  caps[, cpn] <- rank(-caps[, cpn])
  panel2 <- merge(panel, caps, by = idn)
  panel <- panel2[panel2[, cpn] <= cap.threshold, ]
  panel[, cpn] <- NULL
  
  # Fill with column median
  if (fill.med) {
    for (col in 2:dim(panel)[2]) {
      panel[is.na(panel[, col]), col] <- median(panel[, col], na.rm = T)
    }
  } else {
    # Keep only non-NA rows
    panel <- panel[complete.cases(panel), ]
  }
  
  # Turn quantities into percentile ranks
  if (train & y.qtl) {
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

# tmp <- gatherData( T, 900, set = 1)