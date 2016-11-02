
data <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/blocks.csv")

#transform selected variables and combine into a new dataset
log_data <-log(data[,2:11])
newdata <- data.frame(data[,1],log_data[,1:2],log_data[,4],data[,6:7],log_data[,7],log_data[,10])
colnames(newdata) <- c("block","log height","log length","log eccen", "pblock","psmooth","log meantr","log trans")
head(newdata)

## select 90% of data as training data and the rest as test data
train = sample(1:nrow(newdata),608)
train.data <- subset(data[train,])
write.csv(train.data, "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/train_data.csv")
test.data <-newdata[-train,]
write.csv(test.data, "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/test_data.csv")

train.data <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/train_data.csv")
test.data <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/test_data.csv")

## use rpart package to build a classification tree
library(rpart)
## grow tree
fit <- rpart(block~., data=train.data)
printcp(fit, digits=3) ## display results
##Classification tree:
##rpart(formula = block ~ ., data = train.data)

##Variables actually used in tree construction:
##[1] log eccen  log height log length log meantr log trans  pblock    

##Root node error: 314/608 = 0.51645

##n= 608 

##        CP        nsplit rel error  xerror     xstd
##1 0.238854      0   1.00000 1.00000 0.039243
##2 0.216561      1   0.76115 0.80892 0.038729
##3 0.197452      2   0.54459 0.55414 0.035493
##4 0.079618      3   0.34713 0.40446 0.031922
##5 0.060510      4   0.26752 0.32803 0.029457
##6 0.035032      5   0.20701 0.30892 0.028755
##7 0.017516      6   0.17197 0.24522 0.026116
##8 0.010000      8   0.13694 0.21656 0.024750

write.table(printcp(fit, digits=3),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/error_cp_classification_tree.csv", sep=",")

plotcp(fit) #visualize cross-validation results

summary(fit) # detailed summary of splits

##
tree.pred=predict(fit,test.data,type="class")
table(tree.pred, test.data$block)

write.table(table(tree.pred, test.data$block), "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/pred_block.csv",sep=",")

## 

plot(fit, uniform=TRUE, 
   main="Classification Tree for block data")
text(fit, use.n=TRUE, all=TRUE, cex=.5)

## beautify the tree
post(fit, file = "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/Ctree_blockdata.pdf", 
   title = "Classification Tree for block data")


## prune the tree 
pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
   main="Pruned Classification Tree for block data")
text(pfit, use.n=TRUE, all=TRUE, cex=.5)
post(pfit, file = "/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/Pruned_Ctree_blockdata.pdf", 
   title = "Pruned Classification Tree for block data")


## Random Forest
library(randomForest)
train.data <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/train_data.csv")
test.data <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW4/test_data.csv")

fit = randomForest(block~., data = train.data, mtry=3, ntree=1000, importance = T)
importance(fit)
varImpPlot(fit)
print(fit)

##Call:
##randomForest(formula = block ~ ., data = train.data, mtry = 3,      ntree = 1000, importance = T) 
               Type of random forest: classification
                     Number of trees: 1000
##No. of variables tried at each split: 3

##        OOB estimate of  error rate: 0.49%
##Confusion matrix:
##        graphic h_line picture text v_line class.error
##graphic      25      0       0    0      0  0.00000000
##h_line        0    294       0    0      0  0.00000000
##picture       0      0     111    0      0  0.00000000
##text          0      1       0  100      0  0.00990099
##v_line        0      1       1    0     75  0.02597403


tree.pred=predict(fit,test.data,type="class")
table(tree.pred, test.data$block)
plot(fit)


## Support Vector Classifier
library(e1071)
svmfit = svm(block~., data=train.data, kernel = "linear", scale=FALSE, cost=10)
pred.svm <- predict(svmfit, train.data, type="class")
table(pred.svm, train.data$block)
## training error is 0% 
pred.svm <- predict(svmfit, test.data, type="class")
table(pred.svm, test.data$block)


##Call:
##svm(formula = block ~ ., data = train.data, kernel = "linear", cost = 10, scale = FALSE)


##Parameters:
##   SVM-Type:  C-classification 
## SVM-Kernel:  linear 
##      cost:  10 
##      gamma:  0.1 

##Number of Support Vectors:  100

## ( 27 30 27 4 12 )


##Number of Classes:  5 

##Levels: 
## graphic h_line picture text v_line
svmfit = svm(block~., data=train.data, kernel = "linear", scale=FALSE, cost=0.01)
##Call:
##svm(formula = block ~ ., data = train.data, kernel = "linear", cost = 0.01, scale = FALSE)


##Parameters:
##   SVM-Type:  C-classification 
## SVM-Kernel:  linear 
##       cost:  0.01 
##      gamma:  0.1 

##Number of Support Vectors:  128

#3 ( 37 30 37 4 20 )


##Number of Classes:  5 

##Levels: 
## graphic h_line picture text v_line

## training error
pred.svm <- predict(svmfit, train.data, type="class")
table(pred.svm, train.data$block)
## training error is 0% 
pred.svm <- predict(svmfit, test.data, type="class")
table(pred.svm, test.data$block)
## test error is 0%


##another linear kernel
svmfit = svm(block~., data=train.data, kernel = "linear", scale=FALSE, cost=100)
summary(svmfit)
## training error =0%; test error = 0%

## Support Vector Machine
svmfit = svm(block~., data=train.data, kernel = "radial", gamma=1, cost=1)
summary(svmfit)
##Call:
##svm(formula = block ~ ., data = train.data, kernel = "radial", gamma = 1, cost = 1)
##Parameters:
##SVM-Type:  C-classification 
##SVM-Kernel:  radial 
##   cost:  1 
##  gamma:  1 
##Number of Support Vectors:  327
## ( 62 128 66 25 46 )
##Number of Classes:  5 
##Levels: 
##graphic h_line picture text v_line

pred.svm <- predict(svmfit, train.data, type="class")
table(pred.svm, train.data$block)
## training error is 0.33% 
pred.svm <- predict(svmfit, test.data, type="class")
table(pred.svm, test.data$block)
## test error is 2.94%


svmfit = svm(block~., data=train.data, kernel = "radial", gamma=1, cost=0.01)
summary(svmfit)
##Call:
##svm(formula = block ~ ., data = train.data, kernel = "radial", gamma = 1, cost = 0.01)
##Parameters:
##SVM-Type:  C-classification 
##SVM-Kernel:  radial 
## cost:  0.01 
## gamma:  1 
##Number of Support Vectors:  505
 ##( 99 193 110 26 77 )
##Number of Classes:  5 
##Levels: 
##graphic h_line picture text v_line
pred.svm <- predict(svmfit, train.data, type="class")
table(pred.svm, train.data$block)
##training error is 51.64%; predict everything as h_line
##pred.svm  graphic h_line picture text v_line
##graphic       0      0       0    0      0
##h_line       25    294     111  101     77
##picture       0      0       0    0      0
##text          0      0       0    0      0
##v_line        0      0       0    0      0
pred.svm <- predict(svmfit, test.data, type="class")
table(pred.svm, test.data$block)
##test error is 52.94$; predict everything as h_line
