### 3rd phase feature engineering

load("~/Desktop/Instacart/train+test.RData")
# load("~/Desktop/Instacart/all_prior.rdata")


library(dplyr)

# product level

## The percentage of users who have bought this, reordered in the future
prod_reorder = all_prior %>%
    filter(reordered == 1) %>%
    group_by(product_id) %>%
    summarise(unique_reorder_user_count_product = length(unique(user_id)))

all_data = merge(all_data, prod_reorder,
                 by.x = "product_id",
                 by.y = "product_id")

all_data$reoder_user_percent = all_data$unique_reorder_user_count_product / 
                                all_data$unique_user_count_product

## avg order size of the orders containing this product
order_size = all_prior %>%
    group_by(order_id) %>%
    summarise(order_size = n())

all_prior = merge(all_prior, order_size,
                 by.x = "order_id",
                 by.y = "order_id")

prod_order_size = all_prior %>%
    group_by(product_id) %>%
    summarise(avg_order_size_product = mean(order_size))

all_data = merge(all_data, prod_order_size,
                 by.x = "product_id",
                 by.y = "product_id")


## the number this product has appeared in the dataset
prod_count = all_prior %>%
    group_by(product_id) %>%
    summarise(prod_count = n())

all_data = merge(all_data, prod_count,
                 by.x = "product_id",
                 by.y = "product_id")

all_data$avg_prod_order_times = all_data$prod_count / all_data$unique_user_count_product

## favoriate purchase dow of the product
prod_dow = all_prior %>%
    group_by(product_id, order_dow) %>%
    summarise(product_dow_count = n()) %>%
    arrange(-product_dow_count) %>%
    slice(1)

prod_dow$prod_favorite_dow = prod_dow$order_dow
prod_dow$order_dow = NULL
prod_dow$product_dow_count = NULL

all_data = merge(all_data, prod_dow,
                 by.x = "product_id",
                 by.y = "product_id")

all_data$on_prod_favorite_dow = ifelse(all_data$order_dow == all_data$prod_favorite_dow,
                                        1, 0)

## favoriate purchase hod of the product
prod_hod = all_prior %>%
    group_by(product_id, order_hour_of_day) %>%
    summarise(product_hod_count = n()) %>%
    arrange(-product_hod_count) %>%
    slice(1)

prod_hod$prod_favorite_hod = prod_hod$order_hour_of_day
prod_hod$order_hour_of_day = NULL
prod_hod$product_hod_count = NULL

all_data = merge(all_data, prod_hod,
                 by.x = "product_id",
                 by.y = "product_id")

all_data$from_prod_favorite_hod =
    ifelse(abs(all_data$order_hour_of_day - all_data$prod_favorite_hod) > 12,
            24 - abs(all_data$order_hour_of_day - all_data$prod_favorite_hod),
            abs(all_data$order_hour_of_day - all_data$prod_favorite_hod))

# user level

all_data$from_favorite_hod =
    ifelse(abs(all_data$order_hour_of_day - all_data$favorite_hod) > 12,
           24 - abs(all_data$order_hour_of_day - all_data$favorite_hod),
           abs(all_data$order_hour_of_day - all_data$favorite_hod))

all_data$from_highest_reorder_hod =
    ifelse(abs(all_data$order_hour_of_day - all_data$highest_reorder_hod) > 12,
           24 - abs(all_data$order_hour_of_day - all_data$highest_reorder_hod),
           abs(all_data$order_hour_of_day - all_data$highest_reorder_hod))

# user, product level

## whether ordered this product in last month
orders = read.csv('orders.csv')

orders[is.na(orders$days_since_prior_order),]$days_since_prior_order = 0

orders_sumdays = orders %>%
    group_by(user_id) %>%
    mutate(sumdays = cumsum(days_since_prior_order)) %>%
    ungroup()

orders_sumdays = select(orders_sumdays, order_id, user_id, sumdays)

all_prior = merge(all_prior, orders_sumdays,
                  by.x = c("user_id", "order_id"),
                  by.y = c("user_id", "order_id"))

max_day = orders_sumdays %>%
    group_by(user_id) %>%
    summarise(max_day = max(sumdays))

all_prior = merge(all_prior, max_day,
                  by.x = "user_id",
                  by.y = "user_id")

all_prior$past_days = all_prior$max_day - all_prior$sumdays

prod_purchased = all_prior %>%
    filter(past_days <= 30) %>%
    group_by(product_id, user_id) %>%
    summarise(bought_last_1_month = 1)

all_data = merge(all_data, prod_purchased,
                 by.x = c("product_id", "user_id"),
                 by.y = c("product_id", "user_id"),
                 all.x = TRUE)

all_data[is.na(all_data$bought_last_1_month),]$bought_last_1_month = 0


## whether ordered this product in last 2 months

prod_purchased = all_prior %>%
    filter(past_days <= 60) %>%
    group_by(product_id, user_id) %>%
    summarise(bought_last_2_month = 1)

all_data = merge(all_data, prod_purchased,
                 by.x = c("product_id", "user_id"),
                 by.y = c("product_id", "user_id"),
                 all.x = TRUE)

all_data[is.na(all_data$bought_last_2_month),]$bought_last_2_month = 0


## whether ordered this product in last 1 order
user_order_num = all_prior %>%
    group_by(user_id) %>%
    summarise(total_order_num = max(order_number))

all_prior = merge(all_prior, user_order_num,
                  by.x = "user_id",
                  by.y = "user_id")

all_prior$past_order_count = all_prior$total_order_num - all_prior$order_number

prod_purchased = all_prior %>%
    filter(past_order_count == 0) %>%
    group_by(product_id, user_id) %>%
    summarise(bought_last_1_order = 1)

all_data = merge(all_data, prod_purchased,
                 by.x = c("product_id", "user_id"),
                 by.y = c("product_id", "user_id"),
                 all.x = TRUE)

all_data[is.na(all_data$bought_last_1_order),]$bought_last_1_order = 0


## whether ordered this product in last 3 orders

prod_purchased = all_prior %>%
    filter(past_order_count < 3) %>%
    group_by(product_id, user_id) %>%
    summarise(bought_last_3_order = 1)

all_data = merge(all_data, prod_purchased,
                 by.x = c("product_id", "user_id"),
                 by.y = c("product_id", "user_id"),
                 all.x = TRUE)

all_data[is.na(all_data$bought_last_3_order),]$bought_last_3_order = 0

save(all_data, file = 'train+test.RData')


### cv
library(xgboost)

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
all_data$days_since_last_purch_dept = NULL
all_data$days_since_last_purch_aisle = NULL

all_data$from_prod_favorite_hod = NULL
all_data$on_prod_favorite_dow = NULL
all_data$prod_favorite_dow = NULL
all_data$prod_favorite_hod = NULL
all_data$from_favorite_hod = NULL


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

# modeling

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_new = xgb.train(params = params,
                            data = train_x,
                            nrounds = 350,
                            verbose = 1,
                            save_name = "xgboost_new.model")

# prediction
test_x = test[,-c(1,2,5,6,7)]
test_x = as.matrix(test_x)

Sys.time()
pred <- predict(xg_model_new, test_x)
print("Predicting finished")
Sys.time()

# try different thresholds

## 0.18
prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.16, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

colnames(prediction) = c("order_id", "product_id", "pred", "reorder")

# extract those predcited as will not reorder anything
pred_count = prediction %>%
    group_by(order_id) %>%
    summarise(count_pred = sum(reorder))

predictions_null = filter(pred_count, count_pred == 0)
predictions_null$products = "None"
predictions_null$count_pred = NULL

# extract those reordered something
predictions_reorderd = filter(prediction, reorder == 1)
order_id_lst = unique(predictions_reorderd$order_id)
submission = data.frame("order_id" = NA, "products" = NA)
for (oid in order_id_lst) {
    pid = predictions_reorderd[predictions_reorderd$order_id == oid, "product_id"]
    submission = rbind(submission, c(oid,paste(pid, collapse = ' ')))
}

submission = submission[-1,]
submission$order_id = as.numeric(submission$order_id)
submission = rbind(submission, predictions_null)
write.csv(submission, "submission_16_2.csv")
Sys.time()

# 0.18 (0.1335) - 0.3740646
# 0.17 (0.1423)
# 0.165 (0.1470) - 0.3758335
# 0.16 (0.1521)
# 0.155 (0.1573) - 0.3756082


importance_table = xgb.importance(feature_names = colnames(train[,-c(1,2,5,6,7)]), model = xg_model_new)
xgb.plot.importance(importance_table)