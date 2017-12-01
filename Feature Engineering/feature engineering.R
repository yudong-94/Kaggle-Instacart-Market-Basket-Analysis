# starting from the prior_count2 from the eda.r
library(dplyr)
colnames(prior_count2)
prior_count2[is.na(prior_count2$reordered),]$reordered = 0

# code three categorical variables - order_dow & peak_hour & recency
prior_count2$order_dow = as.numeric(as.character(prior_count2$order_dow))
prior_count2$peak_hour = ifelse(prior_count2$peak_hour == 'not', 0, 1)
prior_count2$recency = ifelse(prior_count2$recency == 'within_1week', 2,
                              ifelse(prior_count2$recency == "same day", 1, 0))

# add logged order_number
prior_count2$order_number_log = log(prior_count2$order_number)

# add dept and aisle ranks
dept_merged = merge(merged, products, 
                    by.x = "product_id", by.y = "product_id")

dept_reorder = dept_merged %>%
    group_by(department_id) %>%
    summarise(percentage_reorder_d = sum(reordered) / n())

dept_reorder$reorder_rank_d = 
    nrow(dept_reorder) - rank(dept_reorder$percentage_reorder_d)

aisle_reorder = dept_merged %>%
    group_by(aisle_id) %>%
    summarise(percentage_reorder_a = sum(reordered) / n())

aisle_reorder$reorder_rank_a = nrow(aisle_reorder) - rank(aisle_reorder$percentage_reorder_a)

prior_count2 = merge(prior_count2, dept_reorder,
                     by.x = "department_id", 
                     by.y = "department_id")

prior_count2 = merge(prior_count2, aisle_reorder,
                     by.x = "aisle_id", 
                     by.y = "aisle_id")

# add avg_add_order
add_reorder = merged %>%
    group_by(product_id) %>%
    summarise(avg_add_order = mean(add_to_cart_order))

prior_count2 = merge(prior_count2, add_reorder,
                     by.x = "product_id", 
                     by.y = "product_id")

# add avg_add_order_relative
order_size = merged %>%
    group_by(order_id) %>%
    summarise(order_size = max(add_to_cart_order))
    
order_merged = merge(merged, order_size, 
                     by.x = "order_id", by.y = "order_id")

order_merged$relative_add_order = order_merged$add_to_cart_order / order_merged$order_size

add_reorder_relative = order_merged %>%
    group_by(product_id) %>%
    summarise(avg_add_order_relative = mean(relative_add_order))
    
prior_count2 = merge(prior_count2, add_reorder_relative,
                     by.x = "product_id", 
                     by.y = "product_id")

# add avg_interval
interval = orders %>%
    filter(!is.na(days_since_prior_order)) %>%
    group_by(user_id) %>%
    summarise(avg_interval = mean(days_since_prior_order))

prior_count2 = merge(prior_count2, interval,
                     by.x = "user_id", 
                     by.y = "user_id")

# days since last purchase
# filter out last seen order id
# go to 'orders' to find later orders for this user, add up days

uid = 1
pid = 38928
previous_orders = filter(merged, user_id == uid, product_id == pid, eval_set == "prior")
oid = previous_orders[which.max(previous_orders$order_number), "order_number"]
days = orders_copy[orders_copy$order_id == uid, "days_to_last"]

prior_count2$days_since_last_purch = 999

orders_copy = orders
orders_copy[is.na(orders_copy$days_since_prior_order),]$days_since_prior_order = 0

orders_sumdays = orders_copy %>%
    filter(!is.na(days_since_prior_order)) %>%
    group_by(user_id) %>%
    summarise(sumdays = sum(days_since_prior_order))

orders_copy = merge(orders_copy, orders_sumdays,
                    by.x = "user_id", by.y = "user_id")

orders_copy = orders_copy %>%
    group_by(user_id) %>%
    mutate(days_to_last = sumdays - cumsum(days_since_prior_order))

all_seen = merged %>%
    filter(eval_set == "prior") %>%
    group_by(user_id, product_id) %>%
    arrange(order_number) %>%
    slice(n()) %>%
    ungroup()

all_seen = merge(all_seen, orders_copy,
                 by.x = "order_id", by.y = "order_id")

all_seen = all_seen[,c(2,8,18)]
all_seen$user_id = all_seen$user_id.x
all_seen$user_id.x = NULL

prior_count2 = merge(prior_count2, all_seen,
                     by.x = c("user_id", "product_id"),
                     by.y = c("user_id", "product_id"),
                     all.x = TRUE)

prior_count2$days_since_last_purch = prior_count2$days_to_last
prior_count2$days_to_last = NULL

# adjust column sequences
prior_count2 = prior_count2[,c(6,8,1:2,7,3:4,5,9:25)]

# relative days since last purch:
## days since last purch / avg interval of the user
prior_count2$relative_days_since_last_purch = 
    prior_count2$days_since_last_purch /
    prior_count2$avg_interval

## days since last purch / sum purch days of the user
prior_count2 = merge(prior_count2, orders_sumdays,
                     by.x = "user_id", 
                     by.y = "user_id")

prior_count2$percent_days_since_last_purch = 
    prior_count2$days_since_last_purch /
    prior_count2$sumdays

# average order size
order_size = merged %>%
    group_by(user_id, order_id) %>%
    summarise(order_size = max(add_to_cart_order))

avg_order_size = order_size %>%
    group_by(user_id) %>%
    summarise(avg_order_size = mean(order_size))

prior_count2 = merge(prior_count2, avg_order_size,
                     by.x = "user_id", 
                     by.y = "user_id")

# dow reorder frequency
dow_freq = merged %>%
    group_by(order_dow) %>%
    summarise(dow_reorder_freq = sum(reordered) / n())

prior_count2 = merge(prior_count2, dow_freq,
                     by.x = "order_dow", 
                     by.y = "order_dow")

# hour of day reorder frequency
hod_freq = merged %>%
    group_by(order_hour_of_day) %>%
    summarise(hod_reorder_freq = sum(reordered) / n())

prior_count2 = merge(prior_count2, hod_freq,
                     by.x = "order_hour_of_day", 
                     by.y = "order_hour_of_day")

# most frequent dow by user
dow_freq = merged %>%
    group_by(user_id, order_dow) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    dplyr::slice(1)

dow_freq$favorite_dow = dow_freq$order_dow
dow_freq$order_dow = NULL
dow_freq$count = NULL

prior_count2 = merge(prior_count2, dow_freq,
                     by.x = "user_id", 
                     by.y = "user_id")

prior_count2$on_favorite_dow = 
    ifelse(prior_count2$order_dow == prior_count2$favorite_dow,
           1, 0)

# most frequent hour of day by user
hod_freq = merged %>%
    group_by(user_id, order_hour_of_day) %>%
    summarise(count = n()) %>%
    arrange(-count) %>%
    dplyr::slice(1)

hod_freq$favorite_hod = hod_freq$order_hour_of_day
hod_freq$order_hour_of_day = NULL
hod_freq$count = NULL

prior_count2 = merge(prior_count2, hod_freq,
                     by.x = "user_id", 
                     by.y = "user_id")

prior_count2$on_favorite_hod = 
    ifelse(prior_count2$order_hour_of_day == prior_count2$favorite_hod,
           1, 0)

# dow with highest reorder rate by user
dow_freq = merged %>%
    group_by(user_id, order_dow) %>%
    summarise(reorder_rate = sum(reordered)/n()) %>%
    arrange(-reorder_rate) %>%
    dplyr::slice(1)

dow_freq$highest_reorder_dow = dow_freq$order_dow
dow_freq$order_dow = NULL
dow_freq$reorder_rate = NULL

prior_count2 = merge(prior_count2, dow_freq,
                     by.x = "user_id", 
                     by.y = "user_id")

prior_count2$on_highest_reorder_dow = 
    ifelse(prior_count2$order_dow == prior_count2$highest_reorder_dow,
           1, 0)

# hod with highest reorder rate by user
hod_freq = merged %>%
    group_by(user_id, order_hour_of_day) %>%
    summarise(reorder_rate = sum(reordered)/n()) %>%
    arrange(-reorder_rate) %>%
    dplyr::slice(1)

hod_freq$highest_reorder_hod = hod_freq$order_hour_of_day
hod_freq$order_hour_of_day = NULL
hod_freq$reorder_rate = NULL

prior_count2 = merge(prior_count2, hod_freq,
                     by.x = "user_id", 
                     by.y = "user_id")

prior_count2$on_highest_reorder_hod = 
    ifelse(prior_count2$order_hour_of_day == prior_count2$highest_reorder_hod,
           1, 0)

# average reorder interval
orders_copy = orders
orders_copy[is.na(orders_copy$days_since_prior_order),]$days_since_prior_order = 0

orders_sumdays = orders_copy %>%
    filter(!is.na(days_since_prior_order)) %>%
    group_by(user_id) %>%
    mutate(sumdays = cumsum(days_since_prior_order)) %>%
    ungroup()
orders_sumdays = select(orders_sumdays, order_id, sumdays)

orders_sumdays = merge(orders_sumdays, merged,
                       by.x = "order_id",
                       by.y = "order_id")

avg_reorder_inv = orders_sumdays %>%
    filter(eval_set == "prior") %>%
    group_by(user_id, product_id) %>%
    summarise(avg_reorder_inv = (max(sumdays) - min(sumdays))/n())

avg_reorder_inv[avg_reorder_inv$avg_reorder_inv == 0,]$avg_reorder_inv = 999

prior_count2 = merge(prior_count2, avg_reorder_inv,
                     by.x = c("user_id", "product_id"),
                     by.y = c("user_id", "product_id"),
                     all.x = TRUE)

# days since last purchase / average purchase interval
prior_count2$current_inv_avg_inv = 
    prior_count2$days_since_last_purch /
    prior_count2$avg_reorder_inv

write.csv(prior_count2, "train+test.csv")


############################### modeling
library(xgboost)
library(dplyr)

# shuffle the data
# setwd("~/Desktop/Instacart")
# load("~/Desktop/Instacart/train.rdata")
train_shuffled <- train[sample(nrow(train)),]
train_shuffled$X = NULL

# initialize data
## exclude label column, eval_test, user_id, order_id
train_x = train_shuffled[,-c(1,5,6,7)]
train_x = as.matrix(train_x)
train_y = as.vector(train_shuffled$reordered)

# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

# modeling trial
params_lst <- list(booster = "gbtree", objective = "binary:logistic", 
                     eta=0.1, gamma=10, max_depth=15)

xg_model = xgb.train(params = params_lst,
                     data = train_x,
                     nrounds = 100,
                     verbose = 1,
                     save_name = "xgboost.model")

fitted_values <- predict(xg_model, train_x)
predicted_values = ifelse(fitted_values >= 0.35, 1, 0)
sum(predicted_values == train_y) / length(predicted_values)

# cross validation to find the best nround
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 250, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F)

which.min(xgbcv$evaluation_log$test_error_mean)
# best nround: just use 250, cv-error is 0.089037

# train a default model
xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 250,
                     verbose = 1,
                     save_name = "xgboost_default.model")
Sys.time()

# test on testset
# refer to predict.R
# threshold - 0.23: 0.368486

# do a grid search on other parameters
#create learner
library(mlr)
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(booster = "gbtree", 
                     objective="binary:logistic", 
                     eval_metric="auc", nrounds=250L, eta=0.1)
#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 5L,upper = 12L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
#search strategy
ctrl <- makeTuneControlRandom(maxit = 10L)
#create task
train_task_data = train_shuffled[,-c(1,6,7)]
traintask <- makeClassifTask(data = train_task_data,target = "reordered")
#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores() -1)

#parameter tuning
Sys.time() #21:22
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)
Sys.time()

#training time is too long - manual tunning:

## min-child-weight = 1
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xgbcv_1 <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 200, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

## min-child-weight = 2
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=2, subsample=1, 
               colsample_bytree=1)

xgbcv_2 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 200, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

## min-child-weight = 3
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=1, 
               colsample_bytree=1)

xgbcv_3 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 200, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

xg_model_3 = xgb.train(params = params,
                     data = train_x,
                     nrounds = 230,
                     verbose = 2,
                     save_name = "xgboost_3.model")

# threshold 0.22 - 0.3714863
# threshold 0.21 - 0.3736636
# threshold 0.20 - 0.3756004
# threshold 0.19 - 0.3773406

## min-child-weight = 4
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=4, subsample=1, 
               colsample_bytree=1)

xgbcv_4 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 200, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

## min-child-weight = 3.5
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3.5, subsample=1, 
               colsample_bytree=1)

xgbcv_35 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 200, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

## min-child-weight = 5
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=5, subsample=1, 
               colsample_bytree=1)

xgbcv_5 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 250, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

## another trial
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=5, subsample=1, 
               colsample_bytree=1)

xgbcv_5 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 250, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

# decision: min_child_weight = 3, nrounds = 250

# colsample_bytree = 0.5 - each tree, only half of the features used

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=1, 
               colsample_bytree=0.5)

xgbcv_50 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 250, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

# colsample_bytree = 0.9

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=1, 
               colsample_bytree=0.9)

xgbcv_90 <- xgb.cv( params = params, 
                   data = train_x, 
                   nrounds = 250, nfold = 5, showsd = T, 
                   stratified = T, print_every_n = 5, 
                   early_stop_round = 20, maximize = F,
                   metrics = 'auc', verbose = T)

# decision: colsample = 1
# subsample = 0.75

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.75, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                    data = train_x, 
                    nrounds = 250, nfold = 5, showsd = T, 
                    stratified = T, print_every_n = 5, 
                    early_stop_round = 20, maximize = F,
                    metrics = 'auc', verbose = T)

# subsample = 0.6

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.6, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 250, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

# subsample = 0.9

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.9, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 250, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

# subsample = 0.85

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 250, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

# train the model

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_tunned = xgb.train(params = params,
                       data = train_x,
                       nrounds = 250,
                       verbose = 2,
                       save_name = "xgboost_tunned.model")

# threshold 0.175 (0.136933584811346) - 0.3804137
# threshold 0.18 - 0.3798336
# threshold 0.19 - 0.3783399


params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=1, 
               colsample_bytree=1)

xg_model_tunned2 = xgb.train(params = params,
                            data = train_x,
                            nrounds = 250,
                            verbose = 1,
                            save_name = "xgboost_tunned.model")

# threshold 0.18 - 0.3790964
# threshold 0.19 - 0.3773541

## add scale_pos_weight = 9
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1, scale_pos_weight = 9)

xg_model = xgb.train(params = params,
                     data = train_x,
                     nrounds = 250,
                     verbose = 1,
                     save_name = "xgboost_scaled.model")

# threshold 0.62 (reorder rate 0.1586) - 0.3810030
# threshold 0.625 (reorder rate 0.1556) - 0.3810163  ***
# threshold 0.63 (reorder rate 0.153) - 0.3809279
# threshold 0.65 (reorder rate 0.141) - 0.3802018
# threshold 0.7 (reorder rate 0.114) - 0.3756110

# test scale_pos_weight = 9, and add more rounds
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1, scale_pos_weight = 9)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 300, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

# no scale, but more rounds, and add more rounds
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = train_x, 
                 nrounds = 350, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F,
                 metrics = 'auc', verbose = T)

## still not overfitting, testing auc is increasing very slowly

# train the model with 350 rounds

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_tunned = xgb.train(params = params,
                            data = train_x,
                            nrounds = 350,
                            verbose = 1,
                            save_name = "xgboost_350.model")

# threshold 0.17 (0.1415) - 0.3804679
# threshold 0.165 (0.1463) - 0.3807197
# threshold 0.16 - (0.1515) - 0.3809370
# threshold 0.155 - (0.1568) - 0.3811694
# threshold 0.157 - 0.3812255 ***
# threshold 0.15 - (0.1624) - 0.3805235

importance_table = xgb.importance(feature_names = colnames(train_shuffled[,-c(1,5,6,7)]), model = xg_model_tunned)
xgb.plot.importance(importance_table)


