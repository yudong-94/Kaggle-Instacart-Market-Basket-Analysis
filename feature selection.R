library(xgboost)
library(dplyr)
library(ggplot2)

load("~/Desktop/Instacart/train+test.RData")
all_data$X = NULL

# #check the relationship between the label and all the features
# all_data_num = all_data[,-c(1,2,7,8,9)]
# all_data_num = scale(all_data_num)
# correlation = cor(x = all_data_num, y = all_data$reordered)
# correlation = abs(correlation)

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

#rm(all_data_num, correlation)

### 45 features left

library(caret)

# split the data
train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")

fold_num = train$user_id %% 5
fold_1 = (1:nrow(train))[fold_num == 1]
fold_2 = (1:nrow(train))[fold_num == 2]
fold_3 = (1:nrow(train))[fold_num == 3]
fold_4 = (1:nrow(train))[fold_num == 4]
fold_5 = (1:nrow(train))[fold_num == 0]
folds = list(fold_1, fold_2, fold_3, fold_4, fold_5)
rm(fold_1, fold_2, fold_3, fold_4, fold_5)
rm(fold_num)

# try to calculate mean F1
library(data.table)
library(MLmetrics)

# trial:
prediction <- data.frame(label = as.factor(train[folds[[5]],5]), user_id = train[folds[[5]],2], pred)
prediction$pred_label = as.factor(ifelse(prediction$pred >= 0.15, 1, 0))
prediction = data.table(prediction)
f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
f1_mean[is.nan(f1_mean$f1),2] = 0
mean(f1_mean$f1)

# loop over thresholds:
mean_f1 = data.frame(threshold = numeric(0), mean_f1 = numeric(0))
for (threshold in seq(0.16, 0.17, 0.002)) {
    prediction <- data.frame(label = as.factor(train[folds[[5]],5]), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(threshold, mean(f1_mean$f1))
}

mean_f1[which.max(mean_f1$mean_f1), 1] 
# 0.166

# tune parameters

#subsample - 0.5 to 0.9
#colsample_bylevel - 0.4 to 1


# use fold 5 as the validation set
train_x = train[-folds[[5]],-c(1,2,5,6,7)]
train_x = as.matrix(train_x)
train_y = as.vector(train[-folds[[5]],5])

train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = train[folds[[5]],-c(1,2,5,6,7)]
test_x = as.matrix(test_x)
test_y = as.vector(train[folds[[5]],5])

mean_f1 = data.frame(level = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

# turn colsample_bylevel
for (level in seq(0.3, 0.5, 0.2)) {
    params <- list(booster = "gbtree", objective = "binary:logistic", 
                   eta=0.1, gamma=0, max_depth=6, 
                   min_child_weight=3, subsample=0.8, 
                   colsample_bylevel=level)
    
    xg_model = xgb.train(params = params,
                         data = train_x,
                         nrounds = 500,
                         verbose = 1,
                         save_name = "xgboost.model")
    
    pred <- predict(xg_model, test_x)
    
    for (threshold in seq(0.15, 0.18, 0.0025)) {
        prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
        prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
        prediction = data.table(prediction)
        f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
        f1_mean[is.nan(f1_mean$f1),2] = 0
        mean_f1[nrow(mean_f1)+1, ] = c(level, threshold, mean(f1_mean$f1))
    }
}

write.csv(mean_f1, file = "f1_colsample_by_level.csv")

# best colsample_by_level = 0.4 (threshold = 0.1675)
# train the model
train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")

train_x = train[,-c(1,2,5,6,7)]
train_x = as.matrix(train_x)
train_y = as.vector(train[,5])

train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = test[,-c(1,2,5,6,7)]
test_x = as.matrix(test_x)
test_y = as.vector(test[,5])

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.8, 
               colsample_bylevel=0.4)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 500,
                     verbose = 1,
                     save_name = "xgboost.model")

# 0.3758787


# remove one of the influential features (maybe leading to overfitting)
# suspecious features - 
## relative_days_since_last_purch (20)
## current_inv_avg_inv (29)
## percent_days_since_last_purch (22)
## avg_order_size (23)
## product_bought_count (30)


feature_lst = c(20,29,22,23,30)

mean_f1 = data.frame(removed_feature = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

for (feature in feature_lst) {
    train_x = train[-folds[[5]],-c(1,2,5,6,7, feature)]
    train_x = as.matrix(train_x)
    train_y = as.vector(train[-folds[[5]],5])
    
    train_x = xgb.DMatrix(data = train_x, label = train_y)
    
    test_x = train[folds[[5]],-c(1,2,5,6,7, feature)]
    test_x = as.matrix(test_x)
    test_y = as.vector(train[folds[[5]],5])
    
    params <- list(booster = "gbtree", objective = "binary:logistic", 
                   eta=0.1, gamma=0, max_depth=6, 
                   min_child_weight=3, subsample=0.8, 
                   colsample_bylevel=0.4)
    
    xg_model = xgb.train(params = params,
                         data = train_x,
                         nrounds = 500,
                         verbose = 1,
                         save_name = "xgboost.model")
    
    pred <- predict(xg_model, test_x)
    
    for (threshold in seq(0.15, 0.18, 0.005)) {
        prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
        prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
        prediction = data.table(prediction)
        f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
        f1_mean[is.nan(f1_mean$f1),2] = 0
        mean_f1[nrow(mean_f1)+1, ] = c(feature, threshold, mean(f1_mean$f1))
    }
}


write.csv(mean_f1, file = "f1_remove_influencing_feature.csv")

# 23 and 30 are highly suspecious
# avg_order_size & product_bought_count

## exclude both:

train_x = train[-folds[[5]],-c(1,2,5,6,7, 23,30)]
train_x = as.matrix(train_x)
train_y = as.vector(train[-folds[[5]],5])

train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = train[folds[[5]],-c(1,2,5,6,7, 23,30)]
test_x = as.matrix(test_x)
test_y = as.vector(train[folds[[5]],5])

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.8, 
               colsample_bylevel=0.4)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 500,
                     verbose = 1,
                     save_name = "xgboost.model")

pred <- predict(xg_model, test_x)

for (threshold in seq(0.165, 0.18, 0.005)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    print(paste(threshold, mean(f1_mean$f1)))
}

## mean_F1: 0.3775 (0.175)

# try to exclude some other influential features
## bought_last_3_order (43)
## count (8)
## days_since_last_purch (19)
## bought_last_1_order (42)
## days_since_prior_order (9)
## reorder_user_percent (37)
## bought_last_1_month (44)
## avg_reorder_inv (28)
## on_highest_reorder_hod (27)

feature_lst = c(43,8,19,42,9,37,44,28,27)

mean_f1 = data.frame(removed_feature = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

for (feature in feature_lst) {
    train_x = train[-folds[[5]],-c(1,2,5,6,7,23,30, feature)]
    train_x = as.matrix(train_x)
    train_y = as.vector(train[-folds[[5]],5])
    
    train_x = xgb.DMatrix(data = train_x, label = train_y)
    
    test_x = train[folds[[5]],-c(1,2,5,6,7,23,30, feature)]
    test_x = as.matrix(test_x)
    test_y = as.vector(train[folds[[5]],5])
    
    params <- list(booster = "gbtree", objective = "binary:logistic", 
                   eta=0.1, gamma=0, max_depth=6, 
                   min_child_weight=3, subsample=0.8, 
                   colsample_bylevel=0.4)
    
    xg_model = xgb.train(params = params,
                         data = train_x,
                         nrounds = 500,
                         verbose = 1,
                         save_name = "xgboost.model")
    
    pred <- predict(xg_model, test_x)
    
    for (threshold in seq(0.16, 0.19, 0.005)) {
        prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
        prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
        prediction = data.table(prediction)
        f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
        f1_mean[is.nan(f1_mean$f1),2] = 0
        mean_f1[nrow(mean_f1)+1, ] = c(feature, threshold, mean(f1_mean$f1))
    }
    write.csv(mean_f1, file = "f1_remove_influencing_feature2.csv")
}

# baseline: 0.3775
# only feature to remove: 37 (reorder_user_percent)

# tune the nrounds
train_x = train[-folds[[5]],-c(1,2,5,6,7,23,30,37)]
train_x = as.matrix(train_x)
train_y = as.vector(train[-folds[[5]],5])

train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = train[folds[[5]],-c(1,2,5,6,7,23,30,37)]
test_x = as.matrix(test_x)
test_y = as.vector(train[folds[[5]],5])

mean_f1 = data.frame(rounds = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.8, 
               colsample_bylevel=0.4)

xg_model_400 = xgb.train(params = params,
                     data = train_x,
                     nrounds = 400,
                     verbose = 1,
                     save_name = "xgboost_400.model")

pred <- predict(xg_model_400, test_x)

for (threshold in seq(0.15, 0.18, 0.005)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(400, threshold, mean(f1_mean$f1))
    write.csv(mean_f1, file = "f1_nrounds.csv")
}

xg_model_500 = xgb.train(params = params,
                         data = train_x,
                         nrounds = 100,
                         verbose = 1,
                         xgb_model = xg_model_400,
                         save_name = "xgboost_500.model")

pred <- predict(xg_model_500, test_x)

for (threshold in seq(0.15, 0.18, 0.005)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(500, threshold, mean(f1_mean$f1))
    write.csv(mean_f1, file = "f1_nrounds.csv")
}

xg_model_550 = xgb.train(params = params,
                         data = train_x,
                         nrounds = 50,
                         verbose = 1,
                         xgb_model = xg_model_500,
                         save_name = "xgboost_550.model")

pred <- predict(xg_model_550, test_x)

for (threshold in seq(0.15, 0.18, 0.005)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(550, threshold, mean(f1_mean$f1))
    write.csv(mean_f1, file = "f1_nrounds.csv")
}

xg_model_600 = xgb.train(params = params,
                         data = train_x,
                         nrounds = 50,
                         verbose = 1,
                         xgb_model = xg_model_550,
                         save_name = "xgboost_600.model")

pred <- predict(xg_model_600, test_x)

for (threshold in seq(0.15, 0.18, 0.005)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(600, threshold, mean(f1_mean$f1))
    write.csv(mean_f1, file = "f1_nrounds.csv")
}

xg_model_650 = xgb.train(params = params,
                         data = train_x,
                         nrounds = 50,
                         verbose = 1,
                         xgb_model = xg_model_600,
                         save_name = "xgboost_650.model")

pred <- predict(xg_model_650, test_x)

for (threshold in seq(0.15, 0.18, 0.005)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(650, threshold, mean(f1_mean$f1))
    write.csv(mean_f1, file = "f1_nrounds.csv")
}

# 600 rounds

# tune subsample

mean_f1 = data.frame(subsample = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

for (sub in seq(0.55, 1, 0.15)) {
    params <- list(booster = "gbtree", objective = "binary:logistic", 
                   eta=0.1, gamma=0, max_depth=6, 
                   min_child_weight=3, subsample=sub, 
                   colsample_bylevel=0.4)
    
    xg_model = xgb.train(params = params,
                         data = train_x,
                         nrounds = 600,
                         verbose = 1,
                         save_name = "xgboost.model")
    
    pred <- predict(xg_model, test_x)
    
    for (threshold in seq(0.155, 0.185, 0.005)) {
        prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
        prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
        prediction = data.table(prediction)
        f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
        f1_mean[is.nan(f1_mean$f1),2] = 0
        mean_f1[nrow(mean_f1)+1, ] = c(sub, threshold, mean(f1_mean$f1))
        write.csv(mean_f1, file = "f1_subsample.csv")
    }
}

# subsample = 0.85

# tune min_child_weight
mean_f1 = data.frame(min_child_weight = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

for (weight in seq(3, 5, 1)) {
    params <- list(booster = "gbtree", objective = "binary:logistic", 
                   eta=0.1, gamma=0, max_depth=6, 
                   min_child_weight=weight, subsample=0.85, 
                   colsample_bylevel=0.4)
    
    xg_model = xgb.train(params = params,
                         data = train_x,
                         nrounds = 600,
                         verbose = 1,
                         save_name = "xgboost.model")
    
    pred <- predict(xg_model, test_x)
    
    for (threshold in seq(0.155, 0.185, 0.005)) {
        prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
        prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
        prediction = data.table(prediction)
        f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
        f1_mean[is.nan(f1_mean$f1),2] = 0
        mean_f1[nrow(mean_f1)+1, ] = c(weight, threshold, mean(f1_mean$f1))
        write.csv(mean_f1, file = "f1_weight.csv")
    }
}

# min_child_weight = 3

## find the best threshold for CV
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

mean_f1 = data.frame(threshold = numeric(0), 
                     threshold = numeric(0), 
                     mean_f1 = numeric(0))

for (threshold in seq(0.155, 0.19, 0.0025)) {
    prediction <- data.frame(label = as.factor(test_y), user_id = train[folds[[5]],2], pred)
    prediction$pred_label = as.factor(ifelse(prediction$pred >= threshold, 1, 0))
    prediction = data.table(prediction)
    f1_mean = prediction[, .(f1 = F1_Score(label, pred_label, positive = "1")), by = user_id]
    f1_mean[is.nan(f1_mean$f1),2] = 0
    mean_f1[nrow(mean_f1)+1, ] = c(threshold, threshold, mean(f1_mean$f1))
    write.csv(mean_f1, file = "f1_threshold.csv")
}

# best threshold - 0.175 ~ 0.185

## train the model

## further exclude those intermediary features, 
## such as sumdays(21), avg_interval (18)

train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")

train_x = train[,-c(1,2,5,6,7,23,30,37,18,21)]
train_x = as.matrix(train_x)
train_y = as.vector(train[,5])

train_x = xgb.DMatrix(data = train_x, label = train_y)

test_x = test[,-c(1,2,5,6,7,23,30,37,18,21)]
test_x = as.matrix(test_x)
test_y = as.vector(test[,5])

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bylevel=0.4)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 600,
                     verbose = 1,
                     save_name = "xgboost.model")
