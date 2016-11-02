## KNN, LDA, QDA and Logistic Regression for classifications

##
train <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/train_block.csv")
table(train$block)
test <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/test_block.csv")
table(test$block)


## LDA
train <- train[, -1]
test <- test[, -1]
model <- lda(block ~ ., prior = c(25, 293, 109, 103, 78)/608, data = train)
predict(model,test)$class
table(test$block,predict(model,test)$class)
write.csv(table(test$block,predict(model,test)$class),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/lda_test.csv")

table(train$block,predict(model,train)$class)
write.csv(table(train$block,predict(model,train)$class),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/lda_train_prediction.csv")

test_for_plot <- as.matrix(test[,-1:-2])
LD_for_plot <- model$scaling[-1,c(1:2)]

projection_matrix <- test_for_plot%*%LD_for_plot
label <- test$block
label.list <- as.numeric(test$block)
projection_matrix <- data.frame(projection_matrix,label)
plot(projection_matrix[,1],projection_matrix[,2],pch=label.list,xlab="LD1",ylab="LD2", main= "Projecting test data onto the first two discriminant directions")
legend(-0.5, 4, pch=c(1:5), legend = c("graphic","h_line","picture","text","v_line"))


## code for QDA is fine
train.qda <- train[,c(-1:-2)]
test.qda <- test[,c(-1:-2)]
block <- train$block
model2 <- qda(train.qda, block, data= train.qda)
predict(model2, test.qda)$class
table(test$block,predict(model2,test.qda)$class)
write.csv(table(test$block,predict(model2,test.qda)$class),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/qda_test_prediction.csv")

table(train$block,predict(model2,train.qda)$class)
write.csv(table(train$block,predict(model2,train.qda)$class),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/qda_train_prediction.csv")


### Code for Logistic Regression
##I refer to this page http://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/
library(nnet)
model3 <- multinom(block~height+length+area+eccen+pblock+psmooth+meantr+totalb+totals+trans, data=train)
predict(model3)
table(train$block,predict(model3))
write.csv(table(train$block,predict(model3)), "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/logreg_train.csv")

# Function to predict multinomial logit choice model outcomes
# model = nnet class multinomial model
# newdata = data frame containing new values to predict
predictMNL <- function(model, newdata) {
 
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
 
    # Draw random values
    vals <- runif(nrow(newdata))
 
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
 
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
 
    # Return the values
    return(ids)
  }
}
#This function can now be used to predict the outcomes for some new data.
# newdata is the test data, and function is model3

y2 <- predictMNL(model3,test)

df2 <- cbind(test,y=y2)
write.csv(df2, "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/logreg_test_prediction.csv")

##then I added one column in the csv file to denote the prediciton outcome based on model3. The column is called predicted

predicted <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/logreg_test_prediction.csv")
table(predicted$predicted, predicted$block)
write.csv(table(predicted$predicted, predicted$block),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/logreg_test_table.csv")

## k-NN classifier
index_train_1 <- sample(1:nrow(train), size = 76)
train_1 <- train[index_train_1,]
dim(train_1)
left <- train[-index_train_1,]
dim(left)

index_train_2 <- sample(1:nrow(left), size = 76)
train_2 <- left[index_train_2,]
dim(train_2)
left <- left[-index_train_2,]
dim(left)

index_train_3 <- sample(1:nrow(left),size=76)
train_3 <- left[index_train_3,]
dim(train_3)
left <- left[-index_train_3,]
dim(left)

index_train_4 <- sample(1:nrow(left),size=76)
train_4 <- left[index_train_4,]
dim(train_4)
left <- left[-index_train_4,]
dim(left)

index_train_5 <- sample(1:nrow(left),size=76)
train_5 <- left[index_train_5,]
dim(train_5)
left <- left[-index_train_5,]
dim(left)

index_train_6 <- sample(1:nrow(left),size=76)
train_6 <- left[index_train_6,]
dim(train_6)
left <- left[-index_train_6,]
dim(left)

index_train_7 <- sample(1:nrow(left),size=76)
train_7 <- left[index_train_7,]
dim(train_7)
left <- left[-index_train_7,]
dim(left)

cv <- left

labels <- train_1[,2]
knn(train_1,cv,labels,k=c(3:10))


###KNN classifer
train <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/train_block.csv")
table(train$block)
test <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/test_block.csv")
table(test$block)

### CV
block <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/blocks.csv")
cl <- block$block
predict.cv <- c(rep(NA, 676*50))
dim(predict.cv) <- c(676,1,50)

for (i in 1:50) {
	for (j in 1:nrow(block)) {
		predict.cv[j,1,i] <- knn.cv(block[,2:11],cl,k=i)[j]
	}
}

cl <- as.numeric(cl)


cv.error <- rep(NA, 50)
for (i in 1:50) {
	cv.error[i] <- sum(diag(table(cl,cl))-diag(table(predict.cv[,,i],cl)))/676
}


table (cl, predict.cv[,2])

cl$training <- train$block
cl$training <- as.numeric(cl$training)
predict.training <- matrix(NA, nrow(train), 50)
for (i in 1:50) {
	for (j in 1:nrow(train)) {
		predict.training[j,i] <- knn(train[,3:12], test = train[,3:12], cl=cl$training, k=i)[j]
	}
}

write.csv(predict.training,"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/predict_training.csv")


table(cl$training,predict.training)

predict.test <- matrix(NA, nrow(test),50) 
for (i in 1:50) {
	for (j in 1:nrow(test)) {
		predict.test[j,i] <- knn(train[,3:12], test = train[,3:12], cl=cl$training, k=i)[j]
	}
}

write.csv(predict.test,"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/predict_test.csv")

write.csv(predict.cv,"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/knn_cv.csv")


## Training error
train <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/train_block.csv")
table(train$block)

test <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW3/test_block.csv")
table(test$block)
