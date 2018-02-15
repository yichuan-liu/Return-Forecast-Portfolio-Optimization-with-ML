umd <- function(now, fp=1:11, hp=1, rdb=NULL, idn=P_IDN, cpn=P_CPN, prn=P_PRN) {
  # Computes momentum (UMD) return for a given period
  #
  # Args:
  #   now: time of portfolio formation
  #   fp: formation periods (looking back from now, i.e., now - fp)
  #   hp: holding periods (looking forward from now, i.e., now + hp)
  #   rdb: return data set (use global default if NULL)
  #   idn: column name for IDs
  #   cpn: column name for caps
  #   prn: column name for prices
  #
  # Returns:
  #   Table containing UMD returns for the specified period
  #
  
  # Get the default return data set if nothing is supplied
  if(is.null(rdb)) rdb <- get(P_RETDB, globalenv())
  
  # Get the past time indexes
  pdex <- rev(now - fp)
  # Get the cumulative past returns
  cum.ret <- getReturns(pdex, rdb = rdb, cum = T, gm = T)
  
  # Remove stocks with (1) price less than $1 (2) no cap
  # Critical: caps and prices are taken at time now
  m1 <- merge(cum.ret, rdb[rdb$dex==now,c(idn, cpn, prn)], by=idn, all.x=T)
  m2 <- m1[!is.na(m1$cap) & m1$prc>=1, c(idn, "cret", cpn)]
  
  # Get portfolio weights
  m2[,"rank"] <- rank(m2[,"cret"])
  m3 <- rankToWeights(m2, name="umd")
  # print(colMeans(m3[,-1]))
  # print(head(m3))
  
  # Compute long-short portfolio returns
  weights <- m3[,c(idn,"umd.ew","umd.vw")]
  rets <- pfReturns(weights, now+hp, rdb = rdb)
  
  return(rets)
  
}

# weights <- umd(500)
umd.rets <- umd(500)
print(umd.rets)
# head(weights)
rm(umd.rets)