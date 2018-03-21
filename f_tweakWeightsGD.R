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
    
    # Average magnitude of weights
    wts.avg <- mean(abs(wts))
    # Portfolio expected return
    er <- sum(wts*exp.ret)
    # Portfolio standard deviation
    sr <- as.numeric ( sqrt(t(wts) %*% var.cov %*% wts) )
    # The Sharpe ratio gradient
    grad <- ( sr * exp.ret - (er/sr) * (var.cov %*% wts) ) / sr^2
    # Rank assets based on the gradient
    d <- rank(grad) / length(grad)
    # Bottom half transfer weights to the top half
    d <- d - median(d)
    # Max amount of transfer = 10% of the average magnitude of the current weights
    d <- d * 2 * (wts.avg/10)
    # Make the transfers
    wts <- wts + d
  }
  
  # Return the tweaked weights
  wts
}
