# Return Forecast & Portfolio Formation using Machine Learning Techniques

Predicting monthly stock returns with a performance-weighted ensemble of random forest models
+ Gradient descent style adjustments to portfolio weight
+ Additional tweaks to weights based on prediction history of individual stocks

The resulting long-short portfolio vastly outperforms portfolios based on traditional linear models.

(The eventual goal is to create an unsupervised program that can discover signals, optimize portfolio weights, evaluate performance and make real-time adjustments.)

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

Current model performance, with portfolios containing only the largest 1,000 US stocks:

(all numbers are annualized L-S portfolio Sharpe ratios, 1972-2015)
- RF model ensemble + H30%-L30% weights:	0.58
- RF model ensemble + linear sector-neutral weights: 1.63
- RF model ensemble + linear sector-neutral weights + gradient-descent-style adjustments: 2.94
