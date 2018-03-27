# Return Forecast & Portfolio Formation using Machine Learning Techniques

Main Features:
- X: past stock returns (CRSP) and financial/accounting ratios (COMPUSTAT)
- Y: one-month-forward stock returns (CRSP)
- Model: a performance-weighted ensemble of random forest models
- Portfolio weights: M-V optimal weights assuming identical correlations + gradient descent adjustments
- Output: tradable monthly long-short portfolio from 1975 to 2015.
- Performance: the long-short portfolio vastly outperforms portfolios based on traditional linear models and weighting schemes.

Goals:
- To show that machine-learning models outperform linear predictive models on the same inputs. (Check)
- To show that machine-learning models can effeciently identify useful predictors from a pool of potential predictors. (Check)
- To show that linear and sector-neutral weighting schemes can be matched and improved upon with primitive goal-seeking behavior. (Check)
- To show that algorithm can generate useful predictors (with trial & error & learning) from a pool of raw data. (In progress)
- To create an unsupervised & self-learning program that can discover signals, optimize portfolio weights and evaluate performance. (Ultimately)

Current model performance, with monthly portfolios containing ONLY the largest 1,000 US stocks:

(all numbers are annualized L-S portfolio Sharpe ratios, 1975-2015)
- RF ensemble + H30%-L30% weights:	0.58
- RF ensemble + linear sector-neutral weights: 1.63
- RF ensemble + M-V optimal weights w/ gradient-descent-style adjustments: 3.15

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
