library(randomForest)


expertRFPredict2 <- function(now, exp.wts = NULL, rdb = NULL, fdb = NULL, v = F) {
  # Predicts the returns of all experts + ensemble predictors.
  #
  # Args:
  #   now: predicted period (using data up to now-1)
  #   exp.wts: expert weights that can be used to compute the expert-weighted portfolio returns.
  #   rdb: data frame containing return data (default = use global default)
  #   fdb: data frame containing financial ratio data (default = use global default)
  #   v: verbose mode if true
  #
  # Returns:
  #   Table containing predicted returns of all experts plus portfolios using
  #   mean/expert-weighted/tweaked expert-weighted predictions
  
  # Keep track of time
  pt <- proc.time()
  
  # Get return data set
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  if(is.null(fdb)) fdb <- get(P_FRDB, globalenv())
  
  # Past return (12 months including now)
  note("expertRFPredict: gathering past 12-month return...", now, lvl=2)
  pdex <- rev(now-(1:12))
  p.rets <- getReturns(pdex, rdb = rdb, cum = T, gm = T)
  
  # Past return (1 month prior)
  note("expertRFTrain: gathering past 1-month return...", now, lvl=2)
  p.ret1 <- getReturns(now-1, rdb = rdb, cum = F)
  
  # Financial ratios (1 month prior)
  note("expertRFTrain: gathering financial ratios...", now, lvl=2)
  fin.ratios <- fr[fr[,P_DXN]==(now-1),]
  fin.ratios[,P_DXN] <- NULL
  
  # Assemble the x panel
  m1 <- merge(p.rets, p.ret1, by=P_IDN)
  m1 <- m1[complete.cases(m1),]
  m2 <- merge(m1, fin.ratios, by=P_IDN)
  m2 <- m2[complete.cases(m2),]
  panel <- m2
  names(panel) <- NULL
  panel <- as.matrix(panel)
  
  # Produce the test set
  ts.x <- panel[,2:(dim(panel)[2])]
  note("expertRFPredict: the test set has", dim(ts.x)[1], "rows.", lvl=2)
  
  # Create the table to hold prediction results
  preds <- as.data.frame(panel[,1])
  names(preds) <- P_IDN
  
  
  # Let the experts predict ----
  
  # Loop over all experts
  tm()
  counter <- 0 # Count of experts that have predicted
  for(e in expdb)
  {
    if(class(e)=="randomForest") {
      counter <- counter + 1
      preds <- cbind(preds, predict(e, ts.x))
      names(preds)[dim(preds)[2]] <- counter
    } else if (class(e)=="lm") {
      counter <- counter + 1
      preds <- cbind(preds, as.matrix(ts.x) %*% e$coefficients[-1] + e$coefficients[1])
      names(preds)[dim(preds)[2]] <- counter
    }
  }
  note("expertRFTrain: expert prediction took", tm(F),"secs.",lvl=2)
  
  
  # Add extra portfolios ----
  
  # Add the average of predicted values, removing highest or lowest value (like figure skating)
  if(dim(preds)[2]>5) pred.avg <- (rowSums(preds)-preds[,1]-apply(preds[,-1],1,min)-apply(preds[,-1],1,max))/counter
  else pred.avg <- (rowSums(preds)-preds[,1])/counter
  preds[,"avg"] <- pred.avg
  
  # Add the weighted average of expert predicted values (if expert weights are supplied.)
  if(!is.null(exp.wts)) {
    preds[,"exp"] <- as.matrix(preds[,2:(1+length(exp.wts))]) %*% exp.wts
  }
  else {
    preds[,"exp"] <- pred.avg # If no expert weights are available, use the simple average 
  }
  
  # Show how much of the average predicted return can be explained by an OLS regression of the inputs
  # This is purely diagnostic
  diag.reg <- summary(lm(pred.avg~ts.x))
  note("expertRFPredict: Linear coefs of inputs:", round(diag.reg$coef[,1],2),lvl=2)
  note("expertRFPredict: Adj. R^2 of inputs:", round(diag.reg$adj.r.squared,2),lvl=2)
  
  # Note the number of portfolios that will be formed
  note("expertRFPredict: forming", dim(preds)[2]-1,"portfolios...", lvl=2)
  
  
  # Compute portfolio returns ----

  # Get capitalization
  preds <- merge(preds, rdb[rdb[,P_DXN]==(now-1),c(P_IDN, P_CPN)], by=P_IDN, all.x=T)
  # Remove entries with NA capitalization
  preds <- preds[!is.na(preds[,P_CPN]),]
  # Take out caps from the table
  caps <- preds[,P_CPN]
  preds[,P_CPN] <- NULL
  
  # Get long-short value-weighted portfolio weights by ranking stocks on the predicted values
  weights <- pfRank(preds, pct = P_PFPCT, sep = F, caps = caps, ew = F)
  
  # Gather performance information on individual stocks and use it to tweak the weights
  indperf <- trackStocksGetPerf() # Get individual stock performance report
  if(!is.null(indperf)) {
    indperf <- merge(weights[,c(P_IDN, "avg")], indperf, by=P_IDN, all.x=T)
    # Significance of the deviation from the average model predicted value
    indperf[,"mv"] <- indperf[,"m"] / sqrt(indperf[,"v"] / indperf[,"n"])
    # Only use this measure if there have been at least 12 months since the first performance data was recorded
    if (max(indperf[,"n"], na.rm=T)>=12) {
      medperf <- median(indperf[,"mv"], na.rm = T)
      # Only apply the measure to stocks with more than 6 observations.
      indperf[indperf[,"n"]<6 | is.infinite(indperf[,"mv"]) | is.na(indperf[,"mv"]), "mv"] <- medperf
      # Tweaking based on the ranking of the significance measure
      bias <- rank(indperf[,"mv"])/length(indperf[,"mv"])
      # Tweak weights
      weights[, "expc"] <- tweakWeights(indperf[,"avg"], bias)
      # Note the changes
      note("expertRFPredict: weights have been tweaked: max change:", max(weights[,"expc"]-weights[,"avg"]),
           "min change:", min(weights[,"expc"]-weights[,"avg"]), "range before:",
           max(weights[,"avg"])-min(weights[,"avg"]), "range after:",  max(weights[,"expc"])-min(weights[,"expc"]),lvl=2)
    } else {
      # if there are too few records for everyone, do not tweak
      weights[, "expc"] <- weights[, "avg"]
    }
    
  } else {
    # if there are no individual performance records, do not tweak
    weights[, "expc"] <- weights[, "avg"]
  }
  
  
  # Compute portfolio returns based on all the weights
  rets <- pfReturns(weights, now, rdb = rdb)
  note("expertRFPredict: computed portfolio returns for time", now, lvl=2)
  
  # Add the second king of expert average return (average of individual-expert-based L-S portfolios)
  rets[,"avg2"] = (rowSums(rets[,1:(1+counter)])-rets[,1])/counter
  
  
  # Record individual asset performance ----
  
  # Track assets by their average deviation of actual quantile from predicted quantile
  f.ret <- getReturns(now, rdb = rdb, cum = F)
  f.ret[,"qtl.actl"] <- rank(f.ret[,2])/length(f.ret[,2])
  qtl <- merge(preds[,c(P_IDN,"avg")], f.ret[,c(P_IDN,"qtl.actl")], by=P_IDN)
  names(qtl) <- c(P_IDN, "qtl.pred", "qtl.actl")
  qtl[,"perf"] <- -abs(qtl[,"qtl.actl"]-qtl[,"qtl.pred"])
  qtl[,"pret"] <- rets[1, "expc"]
  trackStocks(qtl[,c(P_IDN, "perf", "pret")])
  note("expertRFPredict:", dim(qtl)[1], "stocks' performance has been tracked.", lvl=2)
  
  
  # Note the run time ----
  note("expertRFPredict: runtime =", (proc.time() - pt)[1], lvl=2)
  
  return(rets)
  
}

# # Debug code
# slice <- c2[c2$dex>=1400 & c2$dex<=1416,c(P_DXN, P_IDN, P_RTN, P_CPN)]
# tm()
# rets <- expertRFPredict(now, rdb = slice)
# print(tm(F))
