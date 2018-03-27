expertAdd <- function(now, models) {
  # Adds one or more experts to expdb and expnf in the global environment
  #
  # Args:
  #   now: the time index now (used to set birthdates)
  #   models: list of models to add to the database
  #
  # Returns:
  #   None
  
  # Compute the average of existing weights, next available expert ID
  # and next available position in the expert database
  if(exists("expdb", globalenv()) && exists("expnf", globalenv())) {
    avg.wt <- 1 # mean(expnf[,"w"])
    nxt.id <- max(expnf[,"eid"])+1
    nxt.loc <- length(expdb)+1
  } else {
    expdb <<- list()
    expnf <<- NULL
    avg.wt <- 1
    nxt.id <- 1
    nxt.loc <- 1
  }
  
  # Number of experts to add
  exp.cnt <- length(models)
  
  # new info table
  new.info <- NULL
  
  # Add experts
  for(e in 1:exp.cnt) {
    note("expertAdd: adding expert (# =", nxt.id, "t =", now, "w0 =", avg.wt, "m =", class(models[[e]]), ")", lvl=1)
    new.info <- rbind(new.info, c(nxt.id, e, now, nxt.loc, avg.wt, 0, avg.wt) )
    expdb[nxt.loc] <<- models[e]
    nxt.loc <- nxt.loc + 1
    nxt.id <- nxt.id + 1
  }
  
  # Append new information
  new.info <- as.data.frame(new.info)
  names(new.info) <- c("eid", "type", "bdex", "loc", "iwt", "perf", "w")
  expnf <<- rbind(expnf, new.info)
  
}

# expertReset()
# models <- expertRFTrain(500)
# expertAdd(500, models)
# models <- expertRFTrain(501)
# expertAdd(501, models)