#### library ####
library(caret) 
library(RColorBrewer)
library(class)
library(readr)
library(caret)
library(e1071)

#### read data ####
train <-  read.csv("D:\\nagisa\\NAGISA\\求職\\作品資料\\Digit Recognizer\\mnist_train.csv")
test <-  read.csv("D:\\nagisa\\NAGISA\\求職\\作品資料\\Digit Recognizer\\mnist_test.csv")
####檢查data 大小####
dim(train)
dim(test)
str(train)
str(test)

####作為類別(把column 1 弄成 factor)####
train[,1]<- as.factor(train[,1])
test[,1]<- as.factor(test[,1])

####檢查資料型態####
head(sapply(train[1,],class))
head(sapply(test[1,],class))

####將多餘的特徵值移除####
train_orig <- train
test_orig <- test

nzv.data <- nearZeroVar(train, saveMetrics = TRUE)
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]
train <- train[,!names(train) %in% drop.cols]
test <- test[,!names(test) %in% drop.cols]

#### print 圖####
BNW <- c("white", "black")
CUSTOM_BNW <- colorRampPalette(colors = BNW)

# train
par(mfrow = c(4, 3), pty = "s", mar = c(1, 1, 1, 1), xaxt = "n", yaxt = "n")
images_digits_0_9 <- array(dim = c(10, 28 * 28))
for (digit in 0:9) {
  images_digits_0_9[digit + 1, ] <- apply(train_orig[train_orig[, 1] == digit, -1], 2, sum)
  images_digits_0_9[digit + 1, ] <- images_digits_0_9[digit + 1, ]/max(images_digits_0_9[digit + 1, ]) * 255
  z <- array(images_digits_0_9[digit + 1, ], dim = c(28, 28))
  z <- z[, 28:1]
  image(1:28, 1:28, z, main = digit, col = CUSTOM_BNW(255))
}

# test
par(mfrow = c(4, 3), pty = "s", mar = c(1, 1, 1, 1), xaxt = "n", yaxt = "n")
images_digits_0_9 <- array(dim = c(10, 28 * 28))
for (digit in 0:9) {
  images_digits_0_9[digit + 1, ] <- apply(test_orig[test_orig[, 1] == digit, -1], 2, sum)
  images_digits_0_9[digit + 1, ] <- images_digits_0_9[digit + 1, ]/max(images_digits_0_9[digit + 1, ]) * 255
  z <- array(images_digits_0_9[digit + 1, ], dim = c(28, 28))
  z <- z[, 28:1]
  image(1:28, 1:28, z, main = digit, col = CUSTOM_BNW(256))
}

########篩選較少資料########
set.seed(123)
trainIndex <- createDataPartition(train$label, p = 0.1, list = FALSE, times = 1)
training <- train[trainIndex,]
validating <- train[-trainIndex,]
# allindices <- c(1:60000)
# vali0_index <- allindices[! allindices %in% trainIndex]
validIndex <- createDataPartition(validating$label, p = 0.01, list = FALSE, times = 1)
validating <- validating[validIndex,]
# original_validindex <- vali0_index[validIndex]

set.seed(123)
# cl <- makeCluster(3)
# registerDoParallel(cl)
tc <- trainControl(method = "cv", number = 5, verboseIter = F, allowParallel = T)


#################################### LDA #####################################
# Train the model
model.LDA <- train(label~., data = training, method = "lda",trControl = tc)

# 驗證
# model.LDA.V <- as.numeric(predict(model.LDA, newdata = validating))-1
# confusionMatrix(as.factor(model.LDA.V), validating$label)

#test
model.LDA.t <- as.numeric(predict(model.LDA, newdata = test))-1
confusionMatrix(as.factor(model.LDA.t), test$label)

#################################### KNN ##################################
model.knn = train(label~., data =training, method = "knn",trControl = tc)

# 驗證
# model.knn.V <- as.numeric(predict(model.knn, newdata = validating))-1
# confusionMatrix(as.factor(model.knn.V), validating$label)

#test
model.knn.t <- as.numeric(predict(model.knn, newdata = test))-1
confusionMatrix(as.factor(model.knn.t), test$label)

################################# random forst #############################
model.rf <- train(label~., data = training, method = "rf",trControl = tc)

# 驗證
# model.rf.V <- as.numeric(predict(model.rf, newdata = validating))-1
# confusionMatrix(as.factor(model.rf.V), validating$label)

#test
model.rf.t <- as.numeric(predict(model.rf, newdata = test))-1
confusionMatrix(as.factor(model.rf.t), test$label)

#################################### SVM ##################################
model.SVM = train(label~., data =training, method = "svmRadial",trControl = tc)

# 驗證
# model.SVM.V <- as.numeric(predict(model.SVM, newdata = validating))-1
# confusionMatrix(as.factor(model.SVM.V), validating$label)

#test
model.SVM.t <- as.numeric(predict(model.SVM, newdata = test))-1
confusionMatrix(as.factor(model.SVM.t), test$label)
