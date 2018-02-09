# Main loop

# General Parameters ----
verbose <- T

# Loop Parameters ----

# Return data set
rdb <- get(P_RETDB, globalenv())

# Lead time before the first portfolio period, in months
# Min/max dex of the loop
min.dex = min(rdb$dex)
max.dex = max(rdb$dex)
timeline = min.dex:max.dex

# Reset trackers
resetTracker("umd")


# Main Loop ----
for (now in timeline[1:200]) {
  # Start loop time counter
  ptm<-proc.time()
  
  # Show year/month
  ym = fromDex(now)
  if(verbose) cat(ym[1],"/",ym[2],"\n")
  
  # Construct momentum portfolio and find returns
  if (now>=min.dex+13) {
    cat("  Momentum portfolio... ")
    # Pre-slice return data for faster runtime
    ret.slc <- rdb[(rdb[,P_DXN]>=now-12) & (rdb[,P_DXN]<=now),]
    # Get momentum returns
    umd.rets <- umd(now-1, rdb = ret.slc)
    # Save returns
    track("umd", umd.rets)
    cat("done.\n")
  }
  
  # Show loop time
  if (verbose) cat(" ",(proc.time()-ptm)[1],"sec\n")
}

# Clean up
rm(rdb)