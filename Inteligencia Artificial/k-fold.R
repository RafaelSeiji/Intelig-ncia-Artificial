library(caret)
#number = k
train_control <- trainControl(method="cv", number=10)
model <- train(factor(target)~., data=dataset2, trControl=train_control, method = "knn")
print(model)



