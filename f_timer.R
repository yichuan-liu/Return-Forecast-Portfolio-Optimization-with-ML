tm <- function(s = T)
{
  # Starts/stops timer
  #
  # Args:
  #   s: start timer if true; stop otherwise
  #
  # Returns:
  #   Elapsed time if s = F (stops)
  
  if(s) {
    timer <<- Sys.time()
    return()
  }
  
  elapsed <- as.numeric(Sys.time() - timer)
  return(elapsed)
}

# tm()
# print(tm(F))