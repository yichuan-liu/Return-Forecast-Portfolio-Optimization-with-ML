track <- function(dbn, x, r=T, prefix="z_") {
  # Keeps record of certain output information x in a permanent variable
  #
  # Args:
  #   dbn: name of the database where x is kept
  #   x: append x to the data base
  #   r: append as new row if true; column if false
  #   prefix: prefix on the database name (default = z_)
  #
  # Returns:
  #   None
  #
  
  # Append prefix to database name
  dbn <- paste0(prefix,dbn)
  
  # Create variable if dbn does not exist
  if(!exists(dbn)) assign(dbn, NULL, envir = globalenv())
  
  
  if(r) {
    assign(dbn, rbind.data.frame(get(dbn, globalenv()), x), envir = globalenv())
  } else {
    assign(dbn, cbind.data.frame(get(dbn, globalenv()), x), envir = globalenv())
  }
  
}

resetTracker <- function(dbn, prefix="z_")
{
  # Reset a tracking database to NULL
  #
  # Args:
  #   dbn: name of the database
  #   prefix: prefix on the database name (default = z_)
  #
  # Returns:
  #   None
  #
  dbn <- paste0(prefix,dbn)
  if(!exists(dbn)) return()
  assign(dbn, NULL, envir = globalenv())
}

# track("test", t(c(1,5,23)))
# track("test", t(c(5,2,3)))
# track("test", t(c(19,5,3)))
# resetTracker("test")
# track("test", t(c(5,2,3)))
# track("test", t(c(19,5,3)))
