## use CV in xgboost package based on AUC score

library(xgboost)

all_train = read.csv("train.csv")
# shuffle the data
all_train <- all_train[sample(nrow(all_train)),]

# initialize data
all_train_x = all_train[,c(4,8:19)]
all_train_x$order_dow = as.numeric(as.character(all_train$order_dow))
all_train_x = as.matrix(all_train_x)
all_train_y = as.vector(all_train$reordered)

# transform to sparse matrix
all_train_x = xgb.DMatrix(data = all_train_x, label = all_train_y)

# cross validation to find the best nround
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.1, gamma=0, max_depth=6, 
               min_child_weight=1, subsample=1, 
               colsample_bytree=1)

xgbcv <- xgb.cv( params = params, 
                 data = all_train_x, 
                 nrounds = 150, nfold = 5, showsd = T, 
                 stratified = T, print_every_n = 5, 
                 early_stop_round = 20, maximize = F)

which.min(xgbcv$evaluation_log$test_error_mean)

# best nround: 137

# train a default model
xg_model = xgb.train(params = params,
          data = all_train_x,
          nrounds = 137,
          verbose = 1,
          save_name = "xgboost_default.model")

# test on testset
# refer to predict.R

# do a grid search on other parameters
#create learner
library(mlr)
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list(booster = "gbtree", 
                     objective="binary:logistic", 
                     eval_metric="auc", nrounds=137L, eta=0.1)
#set parameter space
params <- makeParamSet(makeIntegerParam("max_depth",lower = 5L,upper = 15L),
                       makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
                       makeNumericParam("subsample",lower = 0.5,upper = 1),
                       makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))
#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
#search strategy
ctrl <- makeTuneControlRandom(maxit = 20L)
#create task
train_task_data = train
train_task_data$reordered = as.factor(train_task_data$reordered)
train_task_data$order_dow = as.numeric(as.character(train_task_data$order_dow))
train_task_data = train_task_data[,c(3:4,7:18)]
traintask <- makeClassifTask(data = train_task_data,target = "reordered")
#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores() - 1)

#parameter tuning
mytune <- tuneParams(learner = lrn, 
                     task = traintask, 
                     resampling = rdesc, 
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)


#Result: max_depth=6; min_child_weight=3.12; subsample=0.691; colsample_bytree=0.905 : acc.test.mean=0.907

# modeling using the tunned result

# train a default model

params_tuned <- list(booster = "gbtree", objective = "binary:logistic", 
                eta=0.1, gamma=0, max_depth=6, 
                min_child_weight=3.12, subsample=0.691, 
                colsample_bytree=0.905)

xg_model = xgb.train(params = params_tuned,
                     data = all_train_x,
                     nrounds = 137,
                     verbose = 1,
                     save_name = "xgboost_tunned.model")

# get predictions for training set
fitted_values <- predict(xg_model, all_train_x)
fitted <- data.frame(label = all_train$reordered, xgboost = fitted_values)
write.csv(fitted, "Predictions_Train.csv")

# test on testset
pred <- predict(xg_model, test_x)

prediction <- cbind(test, pred)
prediction = select(prediction, order_id, product_id, pred)
prediction$reorder = ifelse(prediction$pred >= 0.2, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

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
write.csv(submission, "submission.csv")
