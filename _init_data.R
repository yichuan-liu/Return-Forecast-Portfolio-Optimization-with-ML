# Reads in and pre-processes the data
#
# This only needs to be run once.
# It is a time-consuming process so uncomment and run ONE SECTION AT A TIME.
#
# Required inputs:
# - c0.csv: CRSP monthly stock return data containing the following columns:
#           - "date": date in YYYYMMDD format (change the code below if the date is in another format)
#           - "PERMNO": stock identifiers
#           - "PRC": stock prices
#           - "SHROUT": shares outstanding
#           - "RET": monthly returns
#           - "DLRET": delisting returns
# - ff.csv: monthly FF3+UMD return data containing the following columns:
#           - "dateoff": date in YYYYMMDD format (change the code below if the date is in another format)
#           - "rf": monthly risk-free rates
#           - "mktrf": monthly market-minus-risk-free-rate returns
#           - "smb": size factor returns
#           - "hml": value factor returns
#           - "umd": momentum factor returns
# - fr: monthly financial/accounting ratios containing the following columns:
#           - "public_date": date in YYYYMMDD format (change the code below if the date is in another format)
#           - "permno": stock identifiers
#           - ... financial ratios (change the code below to select desired ratios)


# Set/reset all global parameters
source("_global_params.R")

# Data Set Paths ----
crsp.path <- "c0.csv"
compustat.path <- "a0.csv"
ff.path <- "ff0.csv"
fin.ratio.path <- "fr0.csv"

# Load back already processed data sets ----
if(!exists("ff")) load("ff.RData")
if(!exists("c2")) load("c2.RData")
if(!exists("fr")) load("fr.RData")

# # Read and process the FF data set ----
# ff0 <- read.csv(ff.path, header=T, stringsAsFactors=F)
# ff0$year <- floor(ff0$dateff/10000)
# ff0$month <- floor((ff0$date-ff0$year*10000)/100.0)
# ff0[,P_DXN] <- toDex(ff0$year, ff0$month)
# ff <- ff0[, c(P_DXN, "year", "month", "rf", "mktrf", "smb", "hml", "umd")]
# rm(ff0) # clean-up
# save(ff, file="ff.RData")

# # Read and process CRSP data set ----
# c0 <- read.csv(crsp.path, header=T, stringsAsFactors=F) # EXTREMELY TIME-CONSUMING!!!
# # Process the ID variable: default = PERMNO
# c1 <- as.data.frame(as.numeric(as.character(c0[,"PERMNO"])))
# names(c1) <- P_IDN
# # Capitalization
# c1[,P_CPN] <- round( ifelse(c0$SHROUT>0,abs(c0$PRC)*c0$SHROUT,NA) )
# # Price
# c1[,P_PRN] <- abs(c0$PRC)
# # Date
# c1$date <- as.numeric(as.character(c0$date))
# # Year
# c1$year <- floor(c1$date/10000)
# # Month
# c1$month <- floor((c1$date-c1$year*10000)/100.0)
# # Dex (Year-Month combined index)
# c1[,P_DXN] <- toDex(c1$year, c1$month)
# # Returns
# c1$ret0 <- as.numeric(as.character(c0$RET))
# # Delisting returns
# c1$dlret <- as.numeric(as.character(c0$DLRET))
# # Fill in delisting returns, if return is na
# c1$ret <- ifelse( !is.na(c1$ret0), c1$ret0, c1$dlret )
# # 2-Digit SIC Code
# c1[,P_SCN] <- as.numeric(c0$SICCD) %/% 100
# c1[is.na(c1[,P_SCN]), P_SCN] <- (-1)
# # Max/MIN Return
# c1[, P_MXN] <- c0$ASKHI
# c1[, P_MNN] <- c0$BIDLO
# c1.last <- c1[, c(P_DXN, P_IDN, P_PRN)]
# c1.last[, P_DXN] <- c1.last[, P_DXN] + 1
# names(c1.last) <- c(P_DXN, P_IDN, "prc.l")
# c1 <- merge(c1, c1.last, by=c(P_DXN, P_IDN), all.x = T)
# c1[, P_MXN] <- pmax(abs(c1[, P_MXN])/abs(c1[, "prc.l"]), 0) - 1
# c1[, P_MNN] <- pmax(abs(c1[, P_MNN])/abs(c1[, "prc.l"]), 0) - 1

# # Merge with FF data to add in the "risk-free" rate
# c2 <- merge(c1, ff[,c(P_DXN, "rf")], by=P_DXN, all.x=T)
# # Set risk-free rate to zero for a few months in the beginning when it is not available.
# c2$rf[is.na(c2$rf)] <- 0
# # Set excess return as the default return variable
# c2[,P_RTN] <- c2$ret - c2$rf
# # Remove observations with NA returns and reorder the columns
# c2 <- c2[!is.na(c2[,P_RTN]), c(P_DXN, P_IDN, P_RTN, P_CPN, P_PRN, P_MXN, P_MNN, P_SCN)]
# 
# # Save processed return data ----
# c2 <- c2[order(c2[,P_DXN],c2[,P_IDN]),]
# rownames(c2) <- NULL
# save(c2,file="c2.RData")
# rm(c0, c1, c1.last) # clean-up

# # Read and process financial ratio data ----
# fr0 <- read.csv(fin.ratio.path, header=T, stringsAsFactors=F) # EXTREMELY TIME-CONSUMING!!!
# fr1 <- fr0[,c('permno',
#               'public_date',
#               'CAPEI',
#               'bm',
#               'evm',
#               'pe_exi',
#               'pe_inc',
#               'ps',
#               'pcf',
#               'npm',
#               'opmbd',
#               'gpm',
#               'ptpm',
#               'cfm',
#               'roa',
#               'roe',
#               'roce',
#               'aftret_eq',
#               'equity_invcap',
#               'capital_ratio',
#               'int_debt',
#               'cash_lt',
#               'invt_act',
#               'rect_act',
#               'debt_at',
#               'short_debt',
#               'lt_debt',
#               'cash_debt',
#               'lt_ppent',
#               'intcov',
#               'cash_ratio',
#               'quick_ratio',
#               'curr_ratio',
#               'cash_conversion',
#               'inv_turn',
#               'at_turn',
#               'rect_turn',
#               'pay_turn',
#               'sale_invcap',
#               'rd_sale',
#               'adv_sale',
#               'staff_sale',
#               'accrual')]
fr1 <- fr0
fr1$pe_op_dil <- NULL
fr1$PEG_trailing <- NULL
fr1$divyield <- NULL
fr1$PEG_1yrforward <- NULL
fr1$PEG_ltgforward <- NULL
fr1$adate <- NULL
fr1$qdate <- NULL
# ratio.names <- names(fr1)[3:dim(fr1)[2]]
# rm(fr0) # clean-up
# fr1$year <- floor(fr1$public_date/10000)
# fr1$month <- floor((fr1$public_date-fr1$year*10000)/100.0)
# fr1[,P_DXN] <- toDex(fr1$year, fr1$month)
# fr <- cbind(fr1[,c(P_DXN,P_IDN)], fr1[,ratio.names])
# rm(fr1) # clean-up
# save(fr,file="fr.RData")
