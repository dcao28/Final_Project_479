# Final_Project_479  

## Task  
use kernel trick to solve the customized problems  

1. SVM Regression (prediction of future price) 
2. SVM Classification(prediction of future direction)
3. KPCA + SVM (apply KPCA to reconstruct the inputs.)

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
6. The `Gaussionkpca+svmR.R` refers to the combined method in which I use KPCA to reconstruct the **mixed** inputs which will be put into svm regression. The reason why I did not consider the **auto** inputs is because from the results of above script, the mixed inputs give out a better performance.  

## How to use 

1. Make sure you have installed Studio.  
2. Double click the `Final_Project.Rproj`  
3. `source("svmRegression_mixed.R")`or other R scripts. ![](./doc/sample.png)  
4. or you can run the script in command line `➜  Final_Project_479 git:(master) Rscript classification_auto.R` ![](./doc/commandline.png)

## How to interpret the results

1. it is easy to understand what "auto" or "mixed" mean, if you have read the above section. 
2. "static" means I only use the insample data to train the svm and predicted the future prices by the same svm.  
3. "rolling" means I use the latest several(the rolling window number) data to train the svm everytime I predict the one step forward future price.  
4. "outsample" means the result is in the outsample(test data) period.
5. Additionally, you can tell whether it is about regression or classification by the name of the R scripts.  
6. "PE ratio" refers to the prediction error ratio which only applies to the classification problem.  
7. "MSE" refers to the mean squared errors which only applies to the regression problem.




