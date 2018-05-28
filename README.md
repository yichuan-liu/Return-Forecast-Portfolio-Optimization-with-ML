# Return Forecast & Portfolio Optimization with Machine Learning Models

Abstract: We forecast monthly US stock returns using generations of random forest models and perform portfolio optimization using a gradient descent technique. The model inputs include only past returns and common accounting ratios, and only the largest 1,000 US stocks are used in portfolio formation. The algorithm produces long-short portfolios that perform up to five times better, in terms of Sharpe ratio and information ratio, than similar portfolios based on traditional methods. 

Note: the code contained in this folder serves as a demonstration of the algorithm; it is not the up-to-date version. Please contact me if you are interested in the latest version. Thank you.

Current portfolio performance:
(all numbers are annualized L-S portfolio Sharpe ratios, 1975-2015)
- RF ensemble + H30%-L30% weights:	0.70
- RF ensemble + linear sector-neutral weights: 1.63
- RF ensemble + M-V optimal weights w/ gradient-ascent-style adjustments: 3.03
- RF ensemble + M-V optimal weights w/ gradient-ascent-style adjustments + further refinement: 3.11

Model Setup:
- X: past stock returns (CRSP) and financial/accounting ratios (COMPUSTAT)
- Y: one-month-forward stock returns (CRSP)
- Model: generations of monthly cross-sectional random forest models
- Portfolio weights: M-V optimal weights assuming identical correlations + gradient ascent adjustments

Data Requirement:
- CRSP monthly return data set: date, PERMNO, PRC, SHROUT, RET, DLRET
- FF return data set: dateff, rf, mktrf, smb, hml, umd
- WRDS financial ratio data set: permno, public_date, ... (any number of financial/accounting ratios)

Package Requirement:
- ranger (randomForest but much faster)
- tidyr

Instructions:
- Run the data initialization script (_init.R) to load and format data sets. Change parameters (incl. paths) as needed.
- Run the main program (_main.R).
- Function definitions are in f_functionname.R files.
