tweakWeights = function(wts, bias) {
  # Tweaks portfolio weights by nudging them in the direction of the bias
  #
  # Args:
  #   wts (Nx1): weights of the portfolio
  #   bias (Nx1): the directions to nudge the weights; b>1 => magnify; 0<b<1 => shrink; b=0 => omit
  #
  # Returns:
  #   The tweaked weights (Nx1)
  
  # Bias cannot be negative
  if (any(bias<0)) {
    print("ERROR in tweakWeights: bias cannot be negative.")
    return()
  }
  
  # Type of portfolio (zero-investment or regular)
  if (abs(sum(wts))<1e-4) {
    zi = T
  } else if (abs(sum(wts)-1)<1e-4) {
    zi = F
  } else {
    print("ERROR in tweakWeights: sum of weights must be either 0 (zero-investment) or 1 (regular).")
    return()
  }
  
  # Zero-investment
  if (zi) {
    wts.p <- wts * (wts>0)
    wts.n <- wts * (wts<0)
    if (abs(sum(wts.p)-1)>1e-4 || abs(sum(wts.n)+1)>1e-4) {
      print("ERROR in tweakWeights: sum of weights in long or short position do not sum to 1.")
      return()
    }
    if(sum(wts.p * bias)<=0 || sum(wts.n * bias)>=0) {
      print("ERROR in tweakWeights: bias causes nonpositive sum of weights.")
      return()
    }
    wts.p <- (wts.p * bias)/sum(wts.p * bias)
    wts.n <- -(wts.n * bias)/sum(wts.n * bias)
    wts.new <- wts.p + wts.n
  } else {
    # Regular portfolio
    wts.new <- (wts * bias)/sum(wts * bias)
  }
  
  # Return the new weights
  return(wts.new)
}

# print(tweakWeights(c(rep(.25,4),rep(-.25,4)),c(2,3,1,0,1,1,2,0)))
# print(tweakWeights(c(rep(.125,8)),c(2,3,1,0,1,1,2,0)))