# Final_Project_479  

## Task  
use kernel trick to solve the customized problems  

1. SVM Regression (prediction of future price) 
2. SVM Classification(prediction of future direction)

## Data: Dow Jones Index Stock Sets  

the data link: <https://archive.ics.uci.edu/ml/datasets/Dow+Jones+Index>

**Relevant Papers**  
Brown, M. S., Pelosi, M. & Dirska, H. (2013). Dynamic-radius Species-conserving Genetic Algorithm for 
the Financial Forecasting of Dow Jones Index Stocks. Machine Learning and Data Mining in Pattern 
Recognition, 7988, 27-41.

## what each script is about 

1. `svmRegression_auto.R` is the svm regression method in which the inputs are only the lagged value of the response variable. 
2. `svmRegression_mixed.R` is the svm regression method in which the inputs includes other lagged highly correlated stock prices rather than just lagged response variable.
3. `classification_auto.R` is the svm classification method in which the response value of a factor variable whose level takes -1 or 1 to symbolize the direction of stock price change. Likewise, the inputs are only the lagged value of the response variable.   
4. `classification_mixed.R` is the svm classification method in which the response value of a factor variable whose level takes -1 or 1 to symbolize the direction of stock price change. Likewise, the inputs includes other lagged highly correlated stock prices rather than just lagged response variable.
5. The `tune.R` is still being worked on. It is supposed to tune the parameters of svm kernel funcions.

## How to use 

1. Make sure you have installed Studio.  
2. Double click the `Final_Project.Rproj`  
3. `source("svmRegression_mixed.R")`or other R scripts. ![](./doc/sample.png)  
4. or you can run the script in command line `➜  Final_Project_479 git:(master) Rscript classification_auto.R` ![](./doc/commandline.png)
