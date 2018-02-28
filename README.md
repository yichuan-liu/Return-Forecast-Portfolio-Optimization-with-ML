# "Smart" Portfolios

Predicting monthly stock returns with a performance-weighted ensemble of random forest models
+ Additional tweaks to weights based on prediction history of individual stocks

The eventual goal is to create an unsupervised program that can discover signals, optimize portfolio weights, evaluate performance and make real-time adjustments. 

Data Requirement:
- CRSP monthly return data set: date, PERMNO, PRC, SHROUT, RET, DLRET
- FF return data set: dateff, rf, mktrf, smb, hml, umd
- WRDS financial ratio data set: permno, public_date, ... (any number of financial/accounting ratios)

Instructions:
- Run the data initialization script (_init.R) to load and format data sets. Change parameters (incl. paths) as needed.
- Run the main program (_main.R).
- Function definitions are in f_functionname.R files.

Current model performance (1970-2015)
                       OLS-based portfolio     "Smart" portfolio     "Smarter" portfolio
Mean (mo.)	                0.0074                    0.0099                  0.0126
Std Dev (mo.)	              0.0355                    0.0393                  0.0385
Sharpe Ratio (mo.)	        0.2080                    0.2514                  0.3274
Sharpe Ratio (an.)	        0.7205                    0.8709                  1.1342
