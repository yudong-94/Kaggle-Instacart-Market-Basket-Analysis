# Kaggle-Instacart-Market-Basket-Analysis
individual effort for the Instacart Market Basket Analysis Competition on Kaggle

link to the competition: https://www.kaggle.com/c/instacart-market-basket-analysis

## Summary
This is the first time I have worked on a Kaggle competition individually, and this is my second Kaggle competition. BTW, this is also the first time I have used xgboost package in R.

The objective of this competition is to predict the products that a certaion user will reorder in his/her next order based on his/her order history and order pattern. Therefore, I defined it as a classification problem - predict whether a product that has been ordered by a user in the past will be reorderd in his/her next order or not. The classification problem nature is defined as following:

**Label**: Whether reordered or not in the most recent order;  
**Training data**: Each (user, product) is a seperate row, with different features of this pair, and the reorder label;  
**Test data**: Again, each row is a (user, product) pair, and the users in the test set do not appear in the training set;  
**Features**: In the end, I have 57 features. The features list could be found here: [Features List](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/Features%20list.csv).

## Documentation

### 1st Part: EDA and Feature Engineering
This part includes an initial EDA file [EDA.rmd](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/EDA.Rmd), 3 phases of feature engineerings - [phase1](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/feature%20engineering.R), [phase2](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/feature%20engineering%202.R), [phase3](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/feature%20engineering%203.R). Each one of them relies on the result from the previous one.

### 2nd Part: Modeling (xgboost) & Prediction
To tune parameters and select features, I have tried several ways. At first, I used a single validation set and tune based on F1 score (please refer to [modeling.R](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/modeling.R)).

Later, to make the test result more reliable, I used Cross-validation method in the xgboost function, and used auc score as the tuning objective (at that time I didn't know I could define my own evaluation metrics) (please refer to [modeling2.R](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/modeling2.R)).

However, this CV schema is far from reliable. Since I was training on (user, product) pairs, one user could appear in both training fold and validation fold during CV. However, in real test set, none of the users appeared in traning set. This will definitely make the CV error underestimated. Therefore, I implemented a new CV schema which made sure that all of the users in validation fold didn't appear in training fold. You could find the new schema here - [new cv.R](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/new%20cv.R).

The [predict.R](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/predict.R) make prediction on test set, and transforms the prediction result to the submission format (one user per line). However, I know these codes are far from optimization. Currently I am using large loop. I should have wrote them using data.table...

### 3rd Part: Optimization
This part includes feature selection codes and F1 optimization codes.

The [feature selection.R](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/feature%20selection.R) file was used to select features based on [previous feature importance](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/feature%20importance3.tiff), correlation among features, and CV results. It also includes some codes for tunning parameters of xgboost.

Since this competition used F1 as the evaluation metric, the [F1 optimization.R](https://github.com/yudong-94/Kaggle-Instacart-Market-Basket-Analysis/blob/master/F1%20optimization.R) file tries to optimize the F1 score of predictions. This script credits to https://www.kaggle.com/lukeeee/fast-r-port-of-faron-s-f1-maximization.

