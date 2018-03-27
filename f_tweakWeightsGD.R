tweakWeightsGD <- function(wts, exp.ret, var.cov, npass = 1) {
  # Tweaks portfolio weights in the direction of the portfolio Sharpe ratio gradient
  #
  # Args:
  #   wts (Nx1 vector): weights of the portfolio
  #   exp.ret (Nx1 vector): expected return vector
  #   var.cov (NxN matrix): variance-covariance matrix
  #   npass: number of times gradient descent is performed
  #
  # Returns:
  #   The tweaked weights (Nx1)
  
  # Type of portfolio (zero-investment or regular)
  if (abs(sum(wts))<1e-4) {
    zi = T
  } else if (abs(sum(wts)-1)<1e-4) {
    zi = F
  } else {
    print("ERROR in tweakWeights: sum of weights must be either 0 (zero-investment) or 1 (regular).")
    return()
  }
  
  # Save the original weights, just in case
  wts.orignal <- wts
  
  # Keep track of time
  st <- Sys.time()
  
  # Repeat the gradient descent procedure
  for (pass in 1:npass) {
    
    # Max weight transfer = average magnitude of weights / 2
    max.delta <- mean(abs(wts)) / 10
    # Portfolio expected return
    er <- sum(wts*exp.ret)
    # Portfolio standard deviation
    sr <- as.numeric ( sqrt(t(wts) %*% var.cov %*% wts) )
    # The Sharpe ratio gradient
    grad <- ( sr * exp.ret - (er/sr) * (var.cov %*% wts) ) / sr^2
    
    # Rank assets based on the gradient
    g <- rank(grad) / length(grad)
    # Bottom half transfer weights to the top half
    g <- g - median(g)
    g <- g * 2
    # If g is ranked close to the middle (middle 20%), set transfer to zero
    g[abs(g) < 0.2] <- 0
    wts <- wts + g * max.delta
    
    sharpe <- sum(exp.ret * wts) / sqrt( t(wts) %*% var.cov %*% wts)
    # print(paste(now, pass, "sd", sd(grad), "max", max(grad), "min", min(grad), "sr", sharpe))

    # obj <- function(x) {
    #   if (t(wts + g * x) %*% var.cov %*% (wts + g * x) <= 0) {
    #     -1e5
    #   } else {
    #     sum(exp.ret, wts + g * x) / sqrt(t(wts + g * x) %*% var.cov %*% (wts + g * x))
    #   }
    # }
    # # print(sum(exp.ret, wts) / sqrt(t(wts) %*% var.cov %*% (wts)))
    # # print(obj(0))
    # # print(obj(-max.delta))
    # # print(obj(max.delta))
    # 
    # # Compute the optimal amount of transfer
    # s0 <- sum(exp.ret, wts) / sqrt(t(wts) %*% var.cov %*% (wts))
    # opt <- optimize(obj, c(-max.delta, max.delta), maximum = T)
    # 
    # # print(opt$maximum)
    # # print(opt$objective)
    # # print(s0)
    # if (opt$objective > s0) {
    #   wts <- wts + opt$maximum * g
    #   # print(sum(wts))
    # }
    
  }
  
  # Return the tweaked weights
  wts
}
