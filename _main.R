# Main Program


# Initialization ----
clean()

# Program controls
do.umd <- F # produces momentum portfolio returns simultaneously if true
verbose <- T # shows progress in console if true

# Sources
source("_global_params.R") # Global parameters
source("f_misc.R") # Miscellaneous helper functions
source("f_expertAdd.R") # Function that adds new experts to the database
source("f_expertManage2.R") # Function that manages existing experts
source("f_expertRFTrain2.R") # Function that trains new experts
source("f_expertRFPredict2.R") # Function that predicts returns and form portfolios
source("f_experts_misc.R") # Miscellaneous helper functions relating to experts
source("f_getReturns.R") # Functions that get returns from the return data set
source("f_pfRank.R") # Function that ranks stocks and assigns portfolio weights
source("f_pfReturns.R") # Function that computes portfolio returns
source("f_tweakWeights.R") # Function that tweaks portfolio weights
source("f_track.R") # Functions that track outputs over time
source("f_umd.R") # Function that computes momentum-like portfolio returns
source("f_trackStocks.R") # Functions that track performance of expert prediction on individual stocks
source("f_note.R") # Function that writes debug information to the log file
source("f_timer.R") # FUnction that time code segments

# UMD return tracker
if (do.umd) resetTracker("umd")

# L-S portfolio return tracker
resetTracker("perf")

# Reset experts
expertReset()

# Reset individual stock records
trackStocksReset()


# For Loop Parameters ----

# Min/max time index of the loop
min.dex = min(fr[, P_DXN])+1 # First period in which a portfolio return is computed.
max.dex = max(fr$dex)+1 # Last period in which a portfolio return is computed.
timeline = min.dex:max.dex


# Main Loop ----
for (now in timeline) {
  # Start loop time counter
  ptm<-proc.time()
  
  # Current year and month
  ym = fromDex(now)
  # Show year/month in console
  if(verbose) cat(ym[1], "/", ym[2], " ")
  # Make note of the current period in the log file
  noteBanner(paste(ym[1], "/" ,ym[2], "(", now, ")"))
  
  # Cache a smaller slice of returns for faster runtime
  ret.slc <- c2[(c2[,P_DXN]>=now-12) & (c2[,P_DXN]<=now),]
  
  # Manage existing experts
  note("main: managing existing experts...")
  expertManage2(now, rdb = ret.slc)
  
  # Train and add new experts based on birth frequency P_EXPBR
  if(now>=min.dex+11 && now %% P_EXPBR == 0) {
    note("main: training new experts...")
    # Train new experts
    new.models <- expertRFTrain2(now, rdb = ret.slc, v=F)
    # Add new experts
    note("main: adding", length(new.models), "experts...")
    expertAdd(now, new.models)
  }
  
  # Construct momentum portfolio and find returns
  # Note: the portfolio is formed one period before now; the returns are from the period now.
  if (do.umd && now>=min.dex+12) {
    # Get momentum returns (formation period = now - 1)
    umd.rets <- umd(now-1, rdb = ret.slc)
    # Save returns
    track("umd", umd.rets)
  }
  
  # Show loop time
  if (verbose) cat((proc.time()-ptm)[1],"sec\n")
}

# Save and reset the log file
noteReset()

# Save the expert database and information table
z_expdb <- expdb
z_expnf <- expnf

# Clean up
clean()