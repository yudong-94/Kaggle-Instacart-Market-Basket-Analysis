############# 2nd phase feature engineering (starting from train+test.csv)
all_data = read.csv("train+test.csv")

all_prior = merge(merged, products,
                  by.x = 'product_id', 
                  by.y = 'product_id') %>%
    filter(eval_set == 'prior')

# number of differnt products/dept/aisles bought by a customer
all_prior$user_id = as.character(all_prior$user_id)

user_unique = all_prior %>%
    group_by(user_id) %>%
    summarise(product_bought_count = length(unique(product_id)),
              dept_bought_from = length(unique(department_id)),
              aisle_bought_from = length(unique(aisle_id)))

all_data = merge(all_data, user_unique,
                 by.x = "user_id",
                 by.y = "user_id")

# number of different users bought this product/dept/aisles
prod_unique = all_prior %>%
    group_by(product_id) %>%
    summarise(unique_user_count_product = length(unique(user_id)))

all_data = merge(all_data, prod_unique,
                 by.x = "product_id",
                 by.y = "product_id")

dept_unique = all_prior %>%
    group_by(department_id) %>%
    summarise(unique_user_count_dept = length(unique(user_id)))

all_data = merge(all_data, dept_unique,
                 by.x = "department_id",
                 by.y = "department_id")

aisle_unique = all_prior %>%
    group_by(aisle_id) %>%
    summarise(unique_user_count_aisle = length(unique(user_id)))

all_data = merge(all_data, aisle_unique,
                 by.x = "aisle_id",
                 by.y = "aisle_id")

# days since last purchase in a certain department
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

all_seen = all_prior %>%
    group_by(user_id, department_id) %>%
    arrange(order_number) %>%
    slice(n()) %>%
    ungroup()

all_seen = merge(all_seen, orders_copy,
                 by.x = "order_id", by.y = "order_id")

all_seen = all_seen[,c(3,14,22)]
all_seen$user_id = all_seen$user_id.x
all_seen$user_id.x = NULL

all_data = merge(all_data, all_seen,
                 by.x = c("user_id", "department_id"),
                 by.y = c("user_id", "department_id"),
                 all.x = TRUE)

all_data$days_since_last_purch_dept = all_data$days_to_last
all_data$days_to_last = NULL

# days since last purchase in a certain aisle
all_seen = all_prior %>%
    group_by(user_id, aisle_id) %>%
    arrange(order_number) %>%
    slice(n()) %>%
    ungroup()

all_seen = merge(all_seen, orders_copy,
                 by.x = "order_id", by.y = "order_id")

all_seen = all_seen[,c(3,13,22)]
all_seen$user_id = all_seen$user_id.x
all_seen$user_id.x = NULL

all_data = merge(all_data, all_seen,
                 by.x = c("user_id", "aisle_id"),
                 by.y = c("user_id", "aisle_id"),
                 all.x = TRUE)

all_data$days_since_last_purch_aisle = all_data$days_to_last
all_data$days_to_last = NULL


# split to training and test
train = filter(all_data, eval_set == "train")
test = filter(all_data, eval_set == "test")
save(train, file = 'train.RData')
save(test, file = 'test.RData')
save(all_data, file = 'train+test.RData')


#### modeling after feature engineering phase2
library(xgboost)
library(dplyr)

load("~/Desktop/Instacart/train.RData")
load("~/Desktop/Instacart/test.RData")

train_shuffled <- train[sample(nrow(train)),]
train_shuffled$X = NULL
# rm(train)
train_x = train_shuffled[,-c(1,7,8,9)]
train_x = as.matrix(train_x)
train_y = as.vector(train_shuffled$reordered)

# transform to sparse matrix
train_x = xgb.DMatrix(data = train_x, label = train_y)

# same parameters setting
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

# much better!!!

# train the model (may need more rounds)
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_new = xgb.train(params = params,
                         data = train_x,
                         nrounds = 400,
                         verbose = 1,
                         print_every_n = 5,
                         save_name = "xgboost_new.model")

# 0.16 (0.1524) - 0.3755781
# 0.165 (0.1474) - 0.3761360
# 0.18 (0.1339) - 0.3742663
# 0.19 (0.1259) - 0.3728131

### trained with too many rounds???

# adjust the rounds number

params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.15, gamma=0, max_depth=6, 
               min_child_weight=3, subsample=0.85, 
               colsample_bytree=1)

xg_model_new = xgb.train(params = params,
                         data = train_x,
                         nrounds = 300,
                         verbose = 1,
                         print_every_n = 5,
                         save_name = "xgboost_new.model")

# 0.155 (0.1575) - 0.3754749
# 0.165 (0.1472) - 0.3761469