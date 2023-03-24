The "ht_cleaned_covariates.csv" file contains economic factors (covariates) that may be related to sex trafficking conviction.

The "human_trafficking_analysis.R" is the R code used to analyze the dataset mentioned above. The analysis is mainly implemented using the R package "tscount" to fit the log-linear INGARCH model to the data. Each covariate is included into the model one by one with time lags of up to 5. For significant ones, useful figures, such as estimated mean process and pit histograms, are drawn.
