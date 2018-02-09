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