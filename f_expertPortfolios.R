
rowVars <- function(x) {
  # Computes the variance of each row
  #
  # Args:
  #   x: a data frame or matrix consisting of numerical data
  #
  # Returns:
  #   Vector of row variances
  rowSums((x - rowMeans(x))^2)/(dim(x)[2] - 1)
}

linearWeights <- function(sv) {
  # Assigns simple linear weights based on a sorting variable
  #
  # Args:
  #   sv: vector containing the sorting variable
  #
  # Returns:
  #   Vector containing linear weights
  
  rks <- rank(sv, na.last = "keep")
  rks <- rks - median(rks, na.rm = T)
  rks[is.na(rks)] <- 0
  
  lwts <- (rks * (rks>0))/sum(rks * (rks>0)) - (rks * (rks<0))/sum(rks * (rks<0))
  
  # # check weights
  # sw.l <- sum(lwts * (lwts>0))
  # sw.s <- sum(lwts * (lwts<0)) * (-1)
  # if (abs(sw.l-1)>1e-5 || abs(sw.s-1)>1e-5 ) {
  #   print("ERROR IN linearWeights: invalid portfolio weights.")
  # }
  
  lwts
}

sectorNeuralWeights <- function(df, svn, gpn=P_SCN, min.cnt=10) {
  # Assigns sector neutral linear weights
  #
  # Args:
  #   df: data frame containing IDs, sector IDs and ranking variable
  #   svn: sorting/ranking variable name (weights are based on this column)
  #   gpn: grouping/sector variable name
  #   min.cnt: minimum group size below which the entire group is assigned zero weight.
  #
  # Returns:
  #   Vector containing sector neutral weights
  
  # Original data order
  df$index <- 1:dim(df)[1]
  
  # Group by gpn
  df <- df[order(df[, gpn]),]
  
  # Rank and compute weights by group
  df$rnk <- unlist(by(df, df[,gpn], 
                      function(x) {
                        grp.cnt <- sum(complete.cases(x))
                        if (grp.cnt<min.cnt) {
                          rep(0, dim(x)[1])
                        } else {
                          rk <- rank(x[,svn], ties.method = "random", na.last = "keep")/grp.cnt
                          net.rk <- rk - median(rk, na.rm = T)
                          net.rk <- net.rk * (net.rk > 0) / sum( net.rk * (net.rk > 0), na.rm=T) - 
                                    net.rk * (net.rk < 0) / sum( net.rk * (net.rk < 0), na.rm=T)
                          net.rk * sqrt(grp.cnt)
                        }
                      }
  ))
  
  # Revert to orignal row order
  df <- df[order(df$index),]
  
  # Fill in NAs with 0 weights
  df$rnk[is.na(df$rnk)] <- 0
  
  # If all the weights are zero, return
  if (sum(abs(df$rnk)) <= 1e-5) {
    return(df$rnk)
  }
  
  # Scale long/short positions so their respective weights sum up to 1.
  df$rnk <- (df$rnk * (df$rnk > 0)) / sum(df$rnk * (df$rnk > 0)) -
            (df$rnk * (df$rnk < 0)) / sum(df$rnk * (df$rnk < 0))
  
  # # check weights
  # sw.l <- sum(df$rnk * (df$rnk > 0))
  # sw.s <- sum(df$rnk * (df$rnk < 0)) * (-1)
  # if (abs(sw.l-1)>1e-5 || abs(sw.s-1)>1e-5 ) {
  #   print("ERROR IN sectorNeuralWeights: invalid portfolio weights.")
  # }
  
  return(df$rnk)
}


expertPortfolios <- function(now, preds, exp.wts = NULL, rdb = NULL,
                             dxn = P_DXN, idn = P_IDN, rtn = P_RTN, cpn = P_CPN, scn= P_SCN) {
  # Forms portfolios based on expert predictions
  #
  # Args:
  #   now: return period (portfolios are formed in now-1)
  #   preds: expert predictions (col1 = IDs, col2-K = expert predictions)
  #   exp.wts: weights for experts based on their past performance
  #   rdb: data frame containing return data (default = use global default)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for IDS (default = P_IDN)
  #   rtn: column name for returns (default = P_RTN)
  #   cpn: column name for market cap (default = P_CPN)
  #   scn: column name for sector IDs (default = P_SCN)
  #
  # Returns:
  #   Data frame containing predicted returns of each experts
  
  # Prep work ----
  
  # Keep track of time
  st <- Sys.time()
  
  # Get return data set
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  
  # Number of experts
  exp.cnt <- dim(preds)[2] - 1 # Column 1 contains IDs
  
  # Note the number of experts
  note("expertPortfolios: working with", exp.cnt,"experts...", lvl = 2)
  
  # Eliminate assets that do not have capitalization data at time (now - 1)
  # In CRSP, missing cap means the stock has been delisted during the current month
  preds <- merge(preds, rdb[rdb[, dxn] == (now - 1), c(idn, cpn, scn)], by = idn, all.x = T)
  # Sort by ID again (just in case merge messed up the order)
  preds <- preds[order(preds[, idn]), ]
  # Remove entries with NA capitalization
  preds <- preds[!is.na(preds[, cpn]),]
  # Save the caps because they will be needed later
  caps <- preds[, cpn]
  # Remove the cap column
  preds[, cpn] <- NULL
  
  
  # Additional Sorting Variables ----
  
  # Add the average of predicted values, removing highest or lowest value (like figure skating)
  if(dim(preds)[2] > 5) {
    pred.avg <- (rowSums(preds) - preds[, 1] - apply(preds[, -1], 1, min) - apply(preds[, -1], 1, max)) / exp.cnt
  } else {
    pred.avg <- (rowSums(preds)-preds[, 1]) / exp.cnt
  }
  preds[, "avg"] <- pred.avg
  
  # Add the weighted average of expert-predicted values (if expert weights are supplied.)
  if(!is.null(exp.wts)) {
    preds[, "exp"] <- as.matrix(preds[, 2:(1 + length(exp.wts))]) %*% exp.wts
  } else {
    preds[, "exp"] <- pred.avg # If no expert weights are available, use the simple average 
  }
  
  
  # Portfolio Weights ----
  
  # Get long-short value-weighted portfolio weights by ranking stocks on the predicted values
  weights <- pfRank(preds[, setdiff(names(preds), "scn")], pct = P_PFPCT, sep = F, caps = caps, ew = F)
  weights <- weights[order(weights[, idn]), ]
  
  # Add sector-neutral linear weights on mean/variance ratios
  avg.sec <- preds[, c(idn, scn, "exp")]
  avg.sec[, "exp"] <- avg.sec[, "exp"] / rowVars(preds[,2:(exp.cnt+1)])
  weights[, "expl0"] <- sectorNeuralWeights(avg.sec[,c(idn, scn, "exp")], svn = "exp")
  
  # Use gradient descent to improve sector-neutral linear weights
  if (exp.cnt >= 24) {
    # Use mean & cov of expert predictions as proxies for asset mean & cov
    var.cov <- cov(t(as.matrix(preds[, 2:(exp.cnt + 1)])))
    exp.ret <- rowMeans(preds[, 2:(exp.cnt + 1)])
    wts.twk <- tweakWeightsGD(weights[, "expl0"], exp.ret, var.cov, npass = 20)
    # Re-standardize long/short portfolio weights
    wts.twk <- (wts.twk * (wts.twk > 0)) / sum(wts.twk * (wts.twk > 0)) - 
               (wts.twk * (wts.twk < 0)) / sum(wts.twk * (wts.twk < 0))
    weights[, "expl"] <- wts.twk
  } else {
    weights[, "expl"] <- weights[, "expl0"]
  }
  
  # Tweak weights based on disagreement
  if (exp.cnt >= 6) {
    # Expert dispersion
    exp.disp <- 1 / rowVars(preds[, 2:(exp.cnt + 1)])
    # Tweak based on dispersion rank
    bias.dis <- (rank(exp.disp) / length(exp.disp))
    # Tweak weights
    weights[, "expd"] <- tweakWeights(weights[, "expl"], bias.dis)
  } else {
    weights[, "expd"] <- weights[, "expl"]
    bias.dis <- weights[,"expl"] * 0.0 + 1.0
  }
  
  # Gather performance information on individual stocks and use it to tweak the weights
  indperf <- trackStocksGetPerf() # Get individual stock performance report
  if(!is.null(indperf)) {
    indperf <- merge(weights[,c(idn, "expl")], indperf, by=idn, all.x=T)
    indperf <- indperf[order(indperf[, idn]), ]
    # Significance of the deviation from the average model predicted value
    indperf[,"mv"] <- indperf[,"m"] / sqrt(indperf[,"v"] / indperf[,"n"])
    # Only use this measure if there have been at least 12 months since the first performance data was recorded
    if (max(indperf[, "n"], na.rm=T) >= 12) {
      medperf <- median(indperf[, "mv"], na.rm = T)
      # Only apply the measure to stocks with more than 6 observations.
      indperf[indperf[, "n"]<6 | is.infinite(indperf[, "mv"]) | is.na(indperf[, "mv"]), "mv"] <- medperf
      # Tweaking based on the ranking of the significance measure
      bias <- rank(indperf[, "mv"]) / length(indperf[, "mv"])
      # Tweak weights
      weights[, "expc"] <- tweakWeights(indperf[, "expl"], bias)
      # Note the changes
      note("expertPortfolios: weights have been tweaked: max change:", max(weights[, "expc"] - weights[, "expl"]),
           "min change:", min(weights[, "expc"] - weights[, "expl"]), "range before:",
           max(weights[, "expl"]) - min(weights[, "expl"]), "range after:",  
           max(weights[, "expc"]) - min(weights[, "expc"]), lvl = 2)
    } else {
      # if there are too few records for everyone, do not tweak
      weights[, "expc"] <- weights[, "expl"]
    }
    
  } else {
    # if there are no individual performance records, do not tweak
    weights[, "expc"] <- weights[, "expl"]
  }
  
  # Tweak weights again with expert disgreement ranking
  weights[, "expcd"] <- tweakWeights(weights[, "expc"], bias.dis)
  
  # L-S Portfolio Returns ----
  
  # Compute portfolio returns based on all the weights
  rets <- pfReturns(weights, now, rdb = rdb)
  note("expertPortfolios: computed portfolio returns for time", now, lvl = 2)
  
  # Add the second kind of expert average return (average of individual-expert-based L-S portfolios)
  rets[, "avg2"] = (rowSums(rets[, 1:(1 + exp.cnt)]) - rets[, 1]) / exp.cnt
  
  
  # Record individual asset performance ----
  # Track assets by their average deviation of actual quantile from predicted quantile
  
  # Actual return quantiles
  f.ret <- getReturns(now, rdb = rdb, cum = F)
  f.ret[, "qtl.actl"] <- rank(f.ret[, 2], ties.method = "average") / length(f.ret[, 2])
  # Predicted return quantiles
  pred.ret <- preds[, c(idn, "exp")]
  pred.ret[, "exp"] <- rank(pred.ret[, "exp"], ties.method = "average") / length(pred.ret[, 2])
  # Merge tables
  qtl <- merge(pred.ret, f.ret[,c(idn,"qtl.actl")], by = idn)
  names(qtl) <- c(idn, "qtl.pred", "qtl.actl")
  # Compute absolute deviation
  qtl[, "perf"] <- -abs(qtl[,"qtl.actl"] - qtl[,"qtl.pred"])
  # Take expert-weigthed returns
  qtl[, "pret"] <- rets[1, "exp"]
  # Store performance data
  trackStocks(qtl[,c(idn, "perf", "pret")])
  note("expertPortfolios:", dim(qtl)[1], "stocks' performance has been tracked.", lvl = 2)
  
  
  # Note the run time ----
  note("expertPortfolios: runtime =", as.numeric(Sys.time() - st), lvl = 2)
  
  return(rets)
}