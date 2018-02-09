tm <- function(s=T)
{
  # Starts/stops timer
  #
  # Args:
  #   s: start timer if true; stop otherwise
  #   v: print the results in console if true
  #
  # Returns:
  #   Elapsed time if s=F (stops)
  
  if(s) {
    timer <<- proc.time()
    return()
  }
  
  elapsed <- as.numeric((proc.time()-timer)[1])
  return(elapsed)
}

# tm()
# print(tm(F))