### pakage predicting and outputing
library(dplyr)


# load("~/Desktop/Instacart/test.RData")


# test_x = test[,-c(1,2,5,6,7)]
# test_x = as.matrix(test_x)

Sys.time()
pred <- predict(xg_model, test_x)
print("Predicting finished")
Sys.time()


# 0.165

prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.165, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

# current best percent with highest score: (0.1568, 0.1624)

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
write.csv(submission, "submission_165.csv")
Sys.time()

# 0.17
prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.17, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

# current best percent with highest score: (0.1568, 0.1624)

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
write.csv(submission, "submission_17.csv")
Sys.time()

# 0.175

prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.175, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

# current best percent with highest score: (0.1568, 0.1624)

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
write.csv(submission, "submission_175.csv")
Sys.time()

# 0.18

prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.18, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

# current best percent with highest score: (0.1568, 0.1624)

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
write.csv(submission, "submission_18.csv")
Sys.time()

# 0.185

prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.185, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

# current best percent with highest score: (0.1568, 0.1624)

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
write.csv(submission, "submission_185.csv")
Sys.time()

# 0.173

prediction <- data.frame(cbind(test$order_id, test$product_id, pred))
prediction$reorder = ifelse(prediction$pred >= 0.173, 1, 0)
print(paste("percentage of reorder is", 
            sum(prediction$reorder) / nrow(prediction)))

# current best percent with highest score: (0.1568, 0.1624)

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
write.csv(submission, "submission_173.csv")
Sys.time()

