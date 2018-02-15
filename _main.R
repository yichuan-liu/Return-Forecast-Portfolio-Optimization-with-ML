# Main Program

# Clean up the global environment
clean()

# General Parameters ----
verbose <- T

# Loop Parameters ----

# Return data set
rdb <- get(P_RETDB, globalenv())

# Lead time before the first portfolio period, in months
# Min/max dex of the loop
min.dex = 70*12+1 # min(rdb$dex)
max.dex = max(rdb$dex)
timeline = min.dex:max.dex

# Reset trackers
resetTracker("umd")
resetTracker("perf")

# Reset expert database
expertReset()


# Main Loop ----
for (now in timeline) {
  # Start loop time counter
  ptm<-proc.time()
  
  # Show year/month
  ym = fromDex(now)
  if(verbose) cat(ym[1],"/",ym[2],"\n")
  
  # Cache a smaller slice of returnsfor faster runtime
  ret.slc <- rdb[(rdb[,P_DXN]>=now-12) & (rdb[,P_DXN]<=now),]
  
  # Manage existing experts
  if(verbose) cat("  Managing existing experts...\n")
  expertManage(now, rdb = ret.slc)
  
  # Train and add new experts if it's the end of each year
  if(now>=min.dex+12 && now %% 12 == 0) {
    cat("  It's December! Training new experts...\n")
    new.models <- expertRFTrain(now, rdb = ret.slc, v=F)
    # Add new experts
    expertAdd(now, new.models)
    cat("  - There are",dim(z_expnf)[1], "experts alive now.\n")
  }
  
  
  # Construct momentum portfolio and find returns
  if (now>=min.dex+13) {
    # cat("  Momentum portfolio... ")
    # Get momentum returns (formation period = now - 1)
    umd.rets <- umd(now-1, rdb = ret.slc)
    # Save returns
    track("umd", umd.rets)
    # cat("done.\n")
  }
  
  # Show loop time
  if (verbose) cat(" ",(proc.time()-ptm)[1],"sec\n")
}

# Clean up
rm(rdb)