# Defines the global parameters
# 
# Run this file FIRST, before all others
#


# Remove all parameters to reset them
rm(list=ls(pos = ".GlobalEnv", pattern="P_"), pos = ".GlobalEnv")

# Column name for IDs
P_IDN <- "permno"
lockBinding("P_IDN", globalenv())

# Column name for time
P_DXN <- "dex"
lockBinding("P_DXN", globalenv())

# Column name for capitalization
P_CPN <- "cap"
lockBinding("P_CPN", globalenv())

# Column name for returns
P_RTN <- "retrf"
lockBinding("P_RTN", globalenv())

# Column name for price
P_PRN <- "prc"
lockBinding("P_PRN", globalenv())

# Column name for max return
P_MXN <- "hi"
lockBinding("P_MXN", globalenv())

# Column name for min return
P_MNN <- "lo"
lockBinding("P_MNN", globalenv())

# Column name for SIC code
P_SCN <- "sic"
lockBinding("P_SCN", globalenv())

# Column name for rank
P_RKN <- "rank"
lockBinding("P_RKN", globalenv())

# Name for the return data set as data frame
P_RETDB <- "c2"
lockBinding("P_RETDB", globalenv())

# Name for the return data set as data table
P_RETDT <- "c2t"
lockBinding("P_RETDT", globalenv())

# Name for the FF data set
P_FFDB <- "ff"
lockBinding("P_FFDB", globalenv())

# Name for the financial ratio data set
P_FRDB <- "fr"
lockBinding("P_FRDB", globalenv())

# Name for the second financial ratio data set
P_FRDB2 <- "qq"
lockBinding("P_FRDB2", globalenv())

# Path of the log file
P_LOGFILE <- "log.txt"
lockBinding("P_LOGFILE", globalenv())

# Performance decay parameter (not locked)
# LAMBDA = weight on current performance (return); (1-LAMBDA) = weight for past performance
P_LAMBDA <- 0.97

# Expert Lifespan in time periods (not locked)
P_EXPLF <- 120

# Expert birth rate (one set of experts every x time periods)
P_EXPBR <- 1

# NUMBER OF TREES PER RANDOM FOREST (FIRST PASS)
P_RFNTR <- 500

# NUMBER OF TREES PER RANDOM FOREST (SECOND PASS)
P_RFNTR2 <- 500

# "TYPICAL" PERCENT OF ASSETS CHOSEN INTO L-S PORTFOLIOS
P_PFPCT <- 0.3 # Long 30% & Short 30%
