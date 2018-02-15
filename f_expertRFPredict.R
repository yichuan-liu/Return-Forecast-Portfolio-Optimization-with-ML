library(randomForest)

expertRFPredict <- function(now, exp.wts = NULL, rdb = NULL, fdb = NULL, v = F) {
  # Predicts the returns of all experts.
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
  #   median/mean/expert-weighted predictions
  
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  if(is.null(fdb)) fdb <- get(P_FRDB, globalenv())
  
  expdb <- get(P_EXPDB, globalenv())
  
  # Gather data used to predict ----
  panel <- NULL
  for (lkbk in 0) {
    # List of date indices for past returns
    pdex <- rev(now-lkbk-(1:12))
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
  
  # Test set
  ts.x <- panel[,2:(dim(panel)[2])]
  
  # Table to hold the result
  preds <- as.data.frame(panel[,1])
  names(preds) <- P_IDN
  
  # Predict ----
  
  # Loop over all experts
  counter <- 0
  for(e in expdb)
  {
    counter <- counter + 1
    if(class(e)=="randomForest") {
      preds <- cbind(preds, predict(e, ts.x))
      names(preds)[dim(preds)[2]] <- counter
    } else if (class(e)=="lm") {
      preds <- cbind(preds, as.matrix(ts.x) %*% e$coefficients[-1])
      names(preds)[dim(preds)[2]] <- counter
    }
  }
  
  # Add extra portfolios ----
  
  # Add the median of predicted values
  pred_med <- apply(preds[,-1],1,median)
  # Add the average of predicted values, taking out highest or lowest value
  if(dim(preds)[2]>5) pred_avg <- apply(preds[,-1],1,sum)-apply(preds[,-1],1,min)-apply(preds[,-1],1,max)
  else pred_avg <- apply(preds[,-1],1,sum)
  # Add the weighted average of predicted values (if expert weights are supplied.)
  if(!is.null(exp.wts)) pred_exp <- as.matrix(preds[,-1]) %*% exp.wts
  
  preds[,"med"] <- pred_med
  preds[,"avg"] <- pred_avg
  if(!is.null(exp.wts)) preds[,"exp"] <- pred_exp
  else preds[,"exp"] <- pred_avg
  
  # print(names(preds))
  
  # Get portfolio returns ----
  
  # Get capitalization
  preds <- merge(preds, rdb[rdb[,P_DXN]==(now-1),c(P_IDN, P_CPN)], by=P_IDN, all.x=T)
  # Remove entries with NA capitalization
  preds <- preds[!is.na(preds[,P_CPN]),]
  # Take out caps from the table
  caps <- preds[,P_CPN]
  preds[,P_CPN] <- NULL
  
  # Get portfolio weights based on the rankings
  weights <- pfRank(preds, pct = 0.3, sep = F, caps = caps, ew = F)
  
  # Get portfolio returns
  rets <- pfReturns(weights, now, rdb = rdb)
  
  return(rets)
  
}

# slice <- c2[c2$dex>=1400 & c2$dex<=1416,c(P_DXN, P_IDN, P_RTN, P_CPN)]
# tm()
# rets <- expertRFPredict(now, rdb = slice)
# print(tm(F))
