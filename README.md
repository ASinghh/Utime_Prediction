# Comparision of Regression methods for modelling Video Transcoding Time
The goal of this project is to predict video transcoding time using linear models. We compare OLS Regression with Ridge and Lasso Regression methods.

## Data
The dataset was obtianed from the University of California Irvine's Machine Learning Repoistory. Link: https://archive.ics.uci.edu/ml/datasets/Online+Video+Characteristics+and+Transcoding+Time+Dataset

## Modelling Software
R

### Packages Used
* glmnet
* leaps

## Hardware
* i7 Quadcore Processor
* 16 GBs RAM
 


# Results
We Observed the following results from the modelling exercise

## Test RMSE
* OLS     22.6 284
* Lasso   8.7747
* Ridge   9.2546


# Conclusion
 For our use-case, we found that Lasso Regression performs the best.
 For detailed project report, please read <strong>Project_Report.pdf</strong>.



