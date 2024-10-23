####read data####
dat = read.table("D:\\nagisa\\NAGISA\\學校\\碩班\\課程\\統計學習\\期末.csv",header = T,sep=",")
# dat = read.table("D:\\nagisa\\NAGISA\\學校\\碩班\\統計方法\\SL2022_test.csv",header = T,sep=",")
dat$Country = as.numeric(dat$Country=='Taiwan')
dat$Country = as.factor(dat$Country)
dat.org = dat
dat = dat[,-c(1)] #刪掉編號

# da = dat[,-c(3)] #刪掉全是0的第二行
# da1 = dat[,-c(3)]
# ###############################################################################
library(caret)
trainIndex <- createDataPartition(dat$Country, p = 0.7, list = FALSE, times = 1)
training <- dat[trainIndex,]
validating <- dat[-trainIndex,]

# ################################################################################
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)

##################################random forest##############################
# Train the model
model.rf <- train(Country~., data = training, method = "rf",family = "binomial",trControl = train.control)
#
print(model.rf)
summary(model.rf)
plot(model.rf)
library(randomForest)
m = randomForest(Country~., data = training, mtry = 3)
print(m)
summary(m)
plot(m)
# Summarize the results
y = predict(model.rf, newdata = validating)
y = predict(m, newdata = validating)
confusionMatrix(as.factor(y), as.factor(validating$Country))


#####################################KNN###################################
model.knn = train(Country~., data = training, method = "knn",trControl = train.control)
# Summarize the results
print(model.knn)
# mm = knn(training[,-1],validating[,-1],training[,1],k=9)
y = predict(model.knn, newdata = validating)
confusionMatrix(as.factor(y), as.factor(validating$Country))
plot(model.knn)

# ##################################SVM###########################################
# model.svm = train(Country~., data = training, method = "svmRadial",trControl = train.control)
# # Summarize the results
# print(model.svm)
# y = predict(model.svm, newdata = validating)
# confusionMatrix(as.factor(y), as.factor(validating$Country))
# plot(model.svm)

# #####################################LDA######################################
# # Train the model
# model.LDA <- train(Country~., data = training, method = "lda",trControl = train.control,)
# # Summarize the results
# print(model.LDA)
# y = as.numeric(model$finalModel$fitted.values>0.5)
# confusionMatrix(as.factor(y), as.factor(dat$Country))

#####################################Logistic###################################
# Train the model
model.L <- train(Country~., data = training, method = "glm",family = "binomial",trControl = train.control)
# Summarize the results
print(model.L)
y = predict(model.L, newdata = validating)
confusionMatrix(as.factor(y), as.factor(validating$Country))
