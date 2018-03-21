library(tidyr)

getReturns <- function(dex, cum=T, ids=NULL, rdb=NULL, dxn=P_DXN, idn=P_IDN, rtn=P_RTN, gm=T)
{
  # Forms a panel of returns for the given ids/periods; optionally computes the cumulative returns
  #
  # Args:
  #   dex: list of time periods
  #   ids: list of ids (NULL = use all available)
  #   rdb: data frame containing return data (default = NULL: use global default)
  #   dxn: column name for dex (default = P_DXN)
  #   idn: column name for ids (default = P_IDN)
  #   rtn: column name for returns (default = P_RTN)
  #   gm: geometric sum if true; arithmetic sum otherwise
  #   parms: list of parameters (TODO)
  #
  # Returns:
  #   Table containing ids and cumulative returns
  
  # Keep track of time
  pt <- proc.time()
  
  # Get global default return data set if none is specified
  if(is.null(rdb)) rdb = get(P_RETDB, globalenv())
  
  # Order dex and ids
  dex <- dex[order(dex)]
  if(!is.null(ids)) ids <- ids[order(ids)]
  
  # Pre-slice the data
  slc <- rdb[rdb[,dxn]>=min(dex) & rdb[,dxn]<=max(dex),c(dxn, idn, rtn)]
  rm(rdb)
  
  # Transpose return data from long to wide
  # out <- reshape(slc, idvar = idn, timevar = dxn, direction = "wide")
  out <- spread(slc, dxn, rtn)
  
  # If cumulative return is requested
  if(cum && dim(out)[2]>2) {
    if(!gm) {
      out[,"cret"] <- rowSums(out[,-1]) # Arithmetic sum
    } else {
      out[,"cret"] <- pmax( apply(out[,-1]+1, 1, prod), 0) - 1 # Geometric sum
    }
    out <- out[!is.na(out[,"cret"]),c(idn, "cret")]
    return(out)
  }
  
  # Record the time
  note("getReturns: runtime =", (proc.time() - pt)[1], lvl=4)
  
  return(out)
  
}

# pt <- proc.time()
# cr <- getReturns(868:879, cum=F, gm=T)
# print((proc.time()-pt)[1])
# pt <- proc.time()
# cr2 <- getReturns2(868:879, cum=F, gm=T)
# print((proc.time()-pt)[1])
# 
# print(all(cr==cr2, na.rm = T))