#############################################################
# credit to:
# https://www.kaggle.com/lukeeee/fast-r-port-of-faron-s-f1-maximization
# Author: Faron, Lukasz Grad
#
# Quite fast implementation of Faron's expected F1 maximization using Rcpp and R
library(inline)
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")

# Input: p: item reorder probabilities (sorted), p_none: none probability (0 if not specified)
# Output: matrix[2][n + 1] out: out[0][j] - F1 score with top j products and None
#                               out[1][j] - F1 score with top j products
cppFunction(
    'NumericMatrix get_expectations(NumericVector p, double p_none) {
    // Assuming p is sorted, p_none == 0 if not specified
    int n = p.size();
    NumericMatrix expectations = NumericMatrix(2, n + 1);
    double DP_C[n + 2][n + 1];
    std::fill(DP_C[0], DP_C[0] + (n + 2) * (n + 1), 0);
    if (p_none == 0.0) {
    p_none = std::accumulate(p.begin(), p.end(), 1.0, [](double &a, double &b) {return a * (1.0 - b);});
    }
    DP_C[0][0] = 1.0;
    for (int j = 1; j < n; ++j)
    DP_C[0][j] = (1.0 - p[j - 1]) * DP_C[0][j - 1];
    for (int i = 1; i < n + 1; ++i) {
    DP_C[i][i] = DP_C[i - 1][i - 1] * p[i - 1];
    for (int j = i + 1; j < n + 1; ++j)
    DP_C[i][j] = p[j - 1] * DP_C[i - 1][j - 1] + (1.0 - p[j - 1]) * DP_C[i][j - 1];
    }
    double DP_S[2 * n + 1];
    double DP_SNone[2 * n + 1];
    for (int i = 1; i < (2 * n + 1); ++i) {
    DP_S[i] = 1.0 / (1.0 * i);
    DP_SNone[i] = 1.0 / (1.0 * i + 1);
    }
    for (int k = n; k >= 0; --k) {
    double f1 = 0.0;
    double f1None = 0.0;
    for (int k1 = 0; k1 < (n + 1); ++k1) {
    f1 += 2 * k1 * DP_C[k1][k] * DP_S[k + k1];
    f1None += 2 * k1 * DP_C[k1][k] * DP_SNone[k + k1];
    }
    for (int i = 1; i < (2 * k - 1); ++i) {
    DP_S[i] = (1 - p[k - 1]) * DP_S[i] + p[k - 1] * DP_S[i + 1];
    DP_SNone[i] = (1 - p[k - 1]) * DP_SNone[i] + p[k - 1] * DP_SNone[i + 1];
    }
    expectations(0, k) = f1None + 2 * p_none / (2.0 + k);
    expectations(1, k) = f1;
    }
    return expectations;
    }'
)

# Input: ps - item reorder probabilities, prods - item ids
# Output: reordered items string (as required in submission)
exact_F1_max_none <- function(ps, prods) {
    prods <- as.character(prods)
    perm <- order(ps, decreasing = T)
    ps <- ps[perm]
    prods <- prods[perm]
    expectations <-  get_expectations(ps, 0.0)
    max_idx <-  which.max(expectations)
    add_none <- max_idx %% 2 == 1
    size <- as.integer(max(0, max_idx - 1) / 2)
    if (size == 0) {
        return("None")
    }
    else {
        if (add_none)
            return(paste(c(prods[1:size], "None"), collapse = " "))
        else 
            return(paste(prods[1:size], collapse = " "))
    }
}

# How to use it with dplyr:
#
# submission <- data %>%
#        group_by(order_id) %>%
#        summarise(products = exact_F1_max_none(reordered_prob, product_id))

# Quick example
exact_F1_max_none(c(0.5, 0.9, 0.8, 0.1, 0.2, 0.3), c(129832, 1024, 32, 432, 1421, 1032))

#############################################################
# training model with all features (excluding overfitting ones)
library(xgboost)
library(dplyr)
library(ggplot2)

load("~/Desktop/Instacart/train+test.RData")
all_data$X = NULL
all_data$avg_order_size = NULL
all_data$product_bought_count = NULL
all_data$reoder_user_percent = NULL

train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")

train_x = train[,-c(1:4,7:9)]
train_x = as.matrix(train_x)
train_y = as.vector(train[,7])
train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = test[,-c(1:4,7:9)]
test_x = as.matrix(test_x)


params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bylevel=0.4)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 600,
                     verbose = 1,
                     save_name = "xgboost.model")

pred <- predict(xg_model, test_x)
prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
colnames(prediction) = c('order_id', 'product_id', 'pred')

submission <- prediction %>%
       group_by(order_id) %>%
       summarise(products = exact_F1_max_none(pred, product_id))

write.csv(submission, "submission_F1_op.csv")

# 0.3922646

# exclude those useless features
all_data$peak_hour = NULL  #
all_data$dow_reorder_freq = NULL  #
all_data$order_number = NULL
all_data$reorder_rank_d = NULL
all_data$recency = NULL
all_data$highest_reorder_dow = NULL  # 
all_data$favorite_dow = NULL
all_data$hod_reorder_freq = NULL  #
all_data$favorite_hod = NULL  #
all_data$unique_user_count_dept = NULL
all_data$aisle_id = NULL  #
all_data$department_id = NULL
all_data$dept_bought_from = NULL
all_data$from_prod_favorite_hod = NULL
all_data$on_prod_favorite_dow = NULL  #
all_data$prod_favorite_dow = NULL  #
all_data$prod_favorite_hod = NULL  #
all_data$from_favorite_hod = NULL  #
all_data$highest_reorder_hod = NULL #

train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")

train_x = train[,-c(1,2,5:7)]
train_x = as.matrix(train_x)
train_y = as.vector(train[,5])
train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = test[,-c(1,2,5:7)]
test_x = as.matrix(test_x)


params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bylevel=0.4)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 600,
                     verbose = 1,
                     save_name = "xgboost.model")

pred <- predict(xg_model, test_x)
prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
colnames(prediction) = c('order_id', 'product_id', 'pred')

submission <- prediction %>%
    group_by(order_id) %>%
    summarise(products = exact_F1_max_none(pred, product_id))

write.csv(submission, "submission_F1_op.csv")

# 0.3921143