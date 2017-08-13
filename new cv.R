# new CV schema - try not to split the same user_id

library(xgboost)
library(dplyr)

load("~/Desktop/Instacart/train+test.RData")
all_data$X = NULL

all_data$peak_hour = NULL
all_data$dow_reorder_freq = NULL
all_data$order_number_log = NULL
all_data$reorder_rank_d = NULL
all_data$recency = NULL
all_data$highest_reorder_dow = NULL
all_data$favorite_dow = NULL
all_data$hod_reorder_freq = NULL
all_data$favorite_hod = NULL
all_data$unique_user_count_dept = NULL
all_data$aisle_id = NULL
all_data$department_id = NULL
all_data$dept_bought_from = NULL

# split the data
train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")

train_x = train[,-c(1,2,5,6,7)]
train_x = as.matrix(train_x)
train_y = as.vector(train$reordered)

# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

# create folds according to modular by 5 
# make sure records with same user_id are in the same fold
fold_num = train$user_id %% 5
fold_1 = (1:nrow(train))[fold_num == 1]
fold_2 = (1:nrow(train))[fold_num == 2]
fold_3 = (1:nrow(train))[fold_num == 3]
fold_4 = (1:nrow(train))[fold_num == 4]
fold_5 = (1:nrow(train))[fold_num == 0]
folds = list(fold_1, fold_2, fold_3, fold_4, fold_5)
rm(fold_1, fold_2, fold_3, fold_4, fold_5)
rm(fold_num)

# cv

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 300, showsd = T, 
                 folds = folds, print_every_n = 1, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

# get rid of the new features???
train_x = train[,-c(1,2,5,6,7, 31:36)]
train_x = as.matrix(train_x)
train_y = as.vector(train$reordered)

# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_tunned = xgb.train(params = params,
                            data = train_x,
                            nrounds = 350,
                            verbose = 1,
                            save_name = "xgboost_350.model")

# threshold 0.155 (0.1570) - 0.3804737
# threshold 0.16 (0.1517) - 0.3808912
# threshold 0.162 (0.1496) - 0.3810056
# threshold 0.165 (0.1466) - 0.3808448
# threshold 0.17 (0.1418) - 0.3801066

# add only the unique_user_count ones??
train_x = train[,-c(1,2,5,6,7, 31,32,35,36)]
train_x = as.matrix(train_x)
train_y = as.vector(train$reordered)

# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_tunned = xgb.train(params = params,
                            data = train_x,
                            nrounds = 350,
                            verbose = 1,
                            save_name = "xgboost_350.model")


# threshold 0.153 (0.1596) - 0.3811778
# threshold 0.155 (0.1573) - 0.3811974
# threshold 0.16 (0.1519) - 0.3810886
# threshold 0.165 (0.1468) - 0.3809445

# # add only the days_since_xxx ones??
# train_x = train[,-c(1,2,5,6,7, 31,32,33,34)]
# train_x = as.matrix(train_x)
# train_y = as.vector(train$reordered)
# 
# # transform to sparse matrix
# train_x = xgb.DMatrix(data = train_x, label = train_y)
# 
# params <- list(booster = "gbtree", objective = "binary:logistic", 
#                eta=0.15, gamma=0, max_depth=6, 
#                min_child_weight=3, subsample=0.85, 
#                colsample_bytree=1)
# 
# xg_model_tunned2 = xgb.train(params = params,
#                             data = train_x,
#                             nrounds = 350,
#                             verbose = 1,
#                             save_name = "xgboost_350_2.model")
# 
# # 0.165 - 0.3802668


# try with lower learning rate and more rounds
train_x = train[,-c(1,2,5,6,7, 31,32,35,36)]
train_x = as.matrix(train_x)
train_y = as.vector(train$reordered)

# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

params <- list(booster = "gbtree", objective = "2binary:logistic", 
               eta=0.05, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 500, showsd = T, 
                 folds = folds, print_every_n = 1, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)
