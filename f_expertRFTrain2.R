library(randomForest)
library(strip)

expertRFTrain2 <- function(now, rdb = NULL, fdb = NULL, 
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
  
  # Keep track of time
  pt <- proc.time()
  
  # Log file
  note("expertRFTrain: training RF/OLS models at time", now, lvl=2)
  
  # Get default data sets if none are supplied
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  if(is.null(fdb)) fdb <- get(P_FRDB, globalenv())
  
  # Most recent month's return (y variable)
  y.var <- getReturns(now, rdb = rdb, cum = F)
  
  # Past return (12 months prior)
  note("expertRFTrain: gathering past 12-month return...", now, lvl=2)
  pdex <- rev(now-(1:12))
  p.rets <- getReturns(pdex, rdb = rdb, cum = T, gm = T)
  
  # Past return (1 month prior)
  note("expertRFTrain: gathering past 1-month return...", now, lvl=2)
  p.ret1 <- getReturns(now-1, rdb = rdb, cum = F)
  
  # Financial ratios
  note("expertRFTrain: gathering financial ratios...", now, lvl=2)
  # Financial ratios (1 month prior)
  fin.ratios <- fr[fr[,dxn]==(now-1),]
  fin.ratios[,dxn] <- NULL
  
  # Assembling the panel
  m1 <- merge(p.rets, p.ret1, by=idn)
  m1 <- m1[complete.cases(m1),]
  m2 <- merge(m1, fin.ratios, by=idn)
  m2 <- m2[complete.cases(m2),]
  panel <- merge(m2, y.var, by=idn)
  panel <- panel[complete.cases(panel),]
  names(panel) <- NULL
  panel <- as.matrix(panel)
  
  # Training set
  tr.y <- rank(panel[,dim(panel)[2]])/length(panel[,dim(panel)[2]]) # Percentile rank
  tr.x <- panel[,2:(dim(panel)[2]-1)]
  
  note("expertRFTrain: the raw training set has", dim(tr.x)[1], "rows.", lvl=2)
  
  # (Optionally) trim away outliers
  # core <- (tr.y<=quantile(tr.y,0.99)) & (tr.y>=quantile(tr.y,0.01))
  # tr.y <- tr.y[core]
  # tr.x <- tr.x[core,]
  
  note("expertRFTrain: the trimmed training set has", dim(tr.x)[1], "rows.", lvl=2)
  
  tm()
  # print(dim(tr.x))
  # Train random forest model
  rf.mdl <- randomForest(tr.x, tr.y, ntree=P_RFNTR)
  note("expertRFTrain: randomForest training took", tm(F), "secs.", lvl=2)
  note("expertRFTrain: randomForest OOB correlation", cor(predict(rf.mdl), tr.y), lvl=2)
  
  # Train OLS model
  tm()
  ls.mdl <- lm(tr.y~as.matrix(tr.x))
  note("expertRFTrain: OLS training took", tm(F), "secs.", lvl=2)
  
  # Strip models to reduce storage requirement
  rf.mdl <- strip(rf.mdl, keep = "predict")
  ls.mdl <- strip(ls.mdl, keep = "predict")
  
  # Output
  out <- NULL
  out <- list()
  out[[1]] <- rf.mdl
  out[[2]] <- ls.mdl
  
  note("expertRFTrain: returning 2 models: randomForest + OLS.", lvl=2)
  
  # Record the time
  note("expertRFPredict: runtime =", (proc.time() - pt)[1], lvl=2)
  
  return(out)
}

# # Debug code
# models <- expertRFTrain2(900, v=T)

