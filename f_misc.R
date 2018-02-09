# MISCELLANEOUS HELPER FUNCTIONS:
# - clean: cleans up the global environment
# - clear: clears console
# - clip: copies table to clipboard
# - fromDex: converts dex to (year,month)
# - inc: increments variable
# - l: =length()
# - missing: counts % of NA observations 
# - purify: removes groups with missing observations
# - se: computes standard error
# - slicer: subset data frame
# - slicerDT: subset data table
# - toDex: converts (year,month) to dex


clear <- function() {
  # Clears console
  cat("\014")
}

clean <- function() {
  # Clean up the global environment, saving only the following:
  # - All functions
  # - All parameters with prefix P_
  # - All tracked results with prefix z_
  # - All essential data sets: P_RETDB, P_FFDB, P_FRDB
  keep = c(P_RETDB, P_FFDB, P_FRDB)
  delete = setdiff(ls(pos = ".GlobalEnv"), c(lsf.str(pos = ".GlobalEnv"), keep, 
                                             ls(pos = ".GlobalEnv", pattern="z_"),
                                             ls(pos = ".GlobalEnv", pattern="P_")))
  rm(list=delete, pos = ".GlobalEnv")
}

missing <- function(x) {
  # Computes the percentage of value that is missing
  #
  # Args:
  #   x: input vector
  #
  # Returns:
  #   Percentage of x that is missing (NA) or NAN
  mean(is.na(x) | is.nan(x))
}

l <- function(x) {
  # Computes vector length (same as length but quicker)
  #
  # Args:
  #   x: input vector
  #
  # Returns:
  #   Length of x
  length(x)
}

toDex <- function(years, months) {
  # Converts years and months to a single index "dex" = (year-1900)*12+month
  #
  # Args:
  #   years: vector of years
  #   months: vector of months
  #
  # Returns:
  #   vector of dex
  (years-1900)*12+months
}

fromDex <- function(dex) {
  # Converts dex to years and months
  #
  # Args:
  #   dex: vector of dex
  #
  # Returns:
  #   Table containing years and months
  years = (dex-1) %/% 12 + 1900
  months = dex - (years-1900)*12
  cbind(years, months)
}

clip <- function(x,row.names=FALSE,col.names=FALSE,...) {
  # Copies to clipboard
  #
  # Args:
  #   x: Variable / table / vector to be copied to clipboard
  #   row.names: include row names if true
  #   col.names: include column names if true
  #
  # Returns:
  #   None.
  write.table(x,"clipboard-16384",sep="\t",quote=FALSE, row.names=row.names,col.names=col.names,...)
}

se <- function(x) {
  # Estimates the simple standard error of a sample
  #
  # Args:
  #   x: sample = vector of values
  #
  # Returns:
  #   Standard error estimate.
  mean(x)/sd(x)*sqrt(length(x)-1)
}

inc <- function(x, v=1) {
  # Increments x by 1 (or v) in place
  #
  # Args:
  #   x: variable to be incremented
  #   v: increment amount (default = 1)
  #
  # Returns:
  #   None.
  eval.parent(substitute(x <- x+v))
}

slicerDT <- function(dt,dexs=NULL,ids=NULL,cols=NULL,dexn=P_DXN,idn=P_IDN) {
  # Take a subsample of a data table given list of ids and time periods
  #
  # Args:
  #   dt: data table containing all observations
  #   dexs: list of time periods (NULL = use all periods)
  #   ids: list of id's to include (NULL = use all ids)
  #   dexn: name of the column containing time index (default = dex)
  #   idn:  name of the column containing id's (default = permno)
  #
  # Returns:
  #   The requested subsample
  if(is.null(dexs) && is.null(ids))
  {
    slice <- dt
  } else if (is.null(dexs)) {
    slice <- dt[get(idn) %in% ids,]
  } else if (is.null(ids)) {
    slice <- dt[get(dexn) %in% dexs,]
  } else {
    slice <- dt[(get(dexn) %in% dexs) & (get(idn) %in% ids),]
  }
  return(slice)
}

slicer <- function(df,dexs=NULL,ids=NULL,dexn=P_DXN,idn=P_IDN) {
  # Take a subsample of a data frame given list of ids and time periods
  #
  # Args:
  #   df: data frame containing all observations
  #   dexs: list of time periods (NULL = use all periods)
  #   ids: list of id's to include (NULL = use all ids)
  #   dexn: name of the column containing time index (default = dex)
  #   idn:  name of the column containing id's (default = permno)
  #
  # Returns:
  #   The requested subsample
  if(is.null(dexs) && is.null(ids))
  {
    slice <- df
  } else if (is.null(dexs)) {
    slice <- df[(df[,idn] %in% ids),]
  } else if (is.null(ids)) {
    slice <- df[(df[,dexn] %in% dexs),]
  } else {
    slice <- df[(df[,dexn] %in% dexs) & (df[,idn] %in% ids),]
  }
  return(slice)
}

purify <- function(x,id=P_IDN,var=P_RTN,len=60) {
  # Remove observations for all id's with less than a specific number of non-NA observations (rows)
  #
  # Args:
  #   x: the data set
  #   id: the name of the column containing the id (default = permno)
  #   var: the name of the column containing the variable for the purpose of counting (default = retrf)
  #   len: the required length below which all entries with the id will be cut (default = 60)
  #
  # Returns:
  #   The cut table where all ids have at least len number of non-NA observations
  ct = aggregate(x,by=list(x[,id]),FUN=function(z) length(which(!is.na(z))))
  elg = ct[ct[,var]==len,1]
  return( x[x[,id] %in% elg,] )
}
