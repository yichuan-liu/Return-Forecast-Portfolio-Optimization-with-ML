note <- function(..., lvl=0) {
  # Adds note to the log file (log.txt in the current directory)
  #
  # Args:
  #   ...: things to note
  #   
  # Returns:
  #   None
  
  logfile <- P_LOGFILE
  if (lvl>0) {
    prefix <- strrep(" ",(lvl*2-1))
    cat(prefix, ..., "\n", file = logfile, append = T)
  } else {
    cat(..., "\n", file = logfile, append = T)
  }
}

noteBanner <- function(bntxt) {
  # Write a big banner to the log file signifying the beginning of a section
  #
  # Args:
  #   bntxt: banner text
  #   
  # Returns:
  #   None
  
  logfile <- P_LOGFILE
  bw <- 50
  ml <- (bw - 2 - nchar(bntxt)) %/% 2
  mr <- bw - 2 - nchar(bntxt) - ml
  cat("\n", strrep("=",bw), "\n", strrep("=",ml), " ", bntxt, " ", strrep("=",mr), "\n", 
      strrep("=",bw), "\n\n", file = logfile, sep="", append = T)
}

noteReset <- function() {
  # Rename the logfile for permanent storage
  
  logfile <- P_LOGFILE
  logfile.new <- paste0("log", format(Sys.time(), '%m-%d-%H%M%S'), ".txt")
  if(file.exists(logfile)) {
    file.rename(logfile, logfile.new)
  }
}

# noteBanner("1980 / 12")
# note("hello", "goodbye", "dkfjskldf\n")
# note("hello", "goodbye", "dkfjskldf\n", lvl=1)
# note("hello", "goodbye", "dkfjskldf\n", lvl=2)
# note("hello", "goodbye", "dkfjskldf\n", lvl=3)
# note("hello", "goodbye", "dkfjskldf\n", lvl=4)
# noteReset()
