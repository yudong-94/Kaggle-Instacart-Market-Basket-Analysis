# Kaggle Instacart Market Basket Analysis
individual effort for the Instacart Market Basket Analysis Competition on Kaggle

link to the competition: https://www.kaggle.com/c/instacart-market-basket-analysis

Final achievement on this competition: *to be added*

## Summary
This is the first time I have worked on a Kaggle competition individually, and this is my second Kaggle competition. BTW, this is also the first time I have used xgboost package in R.

The objective of this competition is to predict the products that a certaion user will reorder in his/her next order based on his/her order history and order pattern. Therefore, I defined it as a classification problem - predict whether a product that has been ordered by a user in the past will be reorderd in his/her next order or not. The classification problem nature is defined as following:

**Label**: Whether reordered or not in the most recent order;  
**Training data**: Each unique (user, product) pair is a seperate row, with different features of this pair, and the reorder label;  
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

## Takeaways
1. Feature engineering is the most important part for this competition. My LB jumped from 0.32 (with only ~10 features) to 0.38 simply because of adding more features. Besides, some of the top influential features could lead to overfitting (lower CV in the end), so it's important to be careful with them (I dropped them).
2. Tunning parameters is important for sure, but does not help as much as feature engineering in this case. Also, it's a pain to do hyper-parameter tunning with my local machine of 8G RAM...
3. How to transform probability prediction to binary prediction is a tough task. Using a single fixed threshold is not a good idea in this case (highest LB I have achieved with a fixed threshold is 0.382). Ideas in discussion forum include training a seperate classifier for threshold, using Bayesian therom to calculate different thresholds, implementing F1 optimization algorithm (since this competition used F1 as the evaluation metric), etc.
4. What could be further improved to achieve a higher F1 score - adding more features (adding features involving the information of 'None' prediction; adding features involving text information of the product names; etc.); better feature selection schema; ensembling different models (in discussion forum, some people said LightGBM gives better performance than XGboost in this competition).
