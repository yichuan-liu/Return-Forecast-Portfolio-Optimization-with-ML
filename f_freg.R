freg <- function(y, f, v=T, dxn = P_DXN)
{
  # Performs the standard factor regression for one or multiple portfolios
  #
  # Args:
  #   y: portfolio returns with columns = DXN, pf1, pf2, ...
  #   f: list of ids (NULL = use all available)
  #   v: print the results in console if true
  #   dxn: column name for dex (default = P_DXN)
  #   
  #
  # Returns:
  #   Regression estimates
  
  # Last column for y
  ycol <- dim(y)[2]
  
  # Merge in factor returns
  y <- merge(y, f, by=dxn)
  
  # Last column for factors
  fcol <- dim(y)[2]
  
  # Performs factor regressions
  X <- as.matrix(y[,(ycol+1):fcol])
  for (ci in 2:ycol) {
    results <- summary(lm(y[,ci]~X))
    if(v) print(results)
  }
  
}

freg(z_umd, ff[,c(P_DXN,"mktrf","smb","hml","umd")])