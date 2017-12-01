## use a single validation set and tune based on F1

library(xgboost)
library(caret)

train = read.csv("train.csv")
test = read.csv("test.csv")

# split into training and validate set
set.seed(42)
train_no = sample(1:nrow(train), round(nrow(train)*0.7))
validate = train[-train_no,]
train = train[train_no,]

train_x = train[,c(3,7:18)]
train_x$order_dow = as.numeric(as.character(train_x$order_dow))
train_x = as.matrix(train_x)
train_y = as.vector(train$reordered)


xg_model = xgboost(data = train_x, label = train_y, 
                   max.depth = 15, nround = 100, 
                   eta = 0.1, gamma = 10, min_child_weight = 1,
                   verbose = 1,
                   objective = "binary:logistic")


# test on the validate set
validate_x = validate[,c(3,7:18)]
validate_x$order_dow = as.numeric(as.character(validate_x$order_dow))
validate_x = as.matrix(validate_x)
validate_y = as.vector(validate$reordered)

validate_func <- function(model, threshold) {
    pred_vali <- predict(model, validate_x)
    print("Predicting finished")
    
    prediction <- cbind(validate, pred_vali)
    prediction$reorder = ifelse(prediction$pred_vali >= threshold, 1, 0)
    print(paste("percentage of reorder is", sum(prediction$reorder) / nrow(prediction)))
    print(paste("validation error is", mean(prediction$reorder != validate$reordered)))
    pred_factor = as.factor(prediction$reorder)
    label_factor = as.factor(validate$reordered)
    precision = posPredValue(pred_factor, label_factor)
    recall = sensitivity(pred_factor, label_factor)
    F1 = (2 * precision * recall) / (precision + recall)
    print(paste("F1 score is", F1))
}

validate_func(xg_model, 0.25)
validate_func(xg_model, 0.2)

# view feature importance
importance_matrix <- xgb.importance(model = xg_model)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


