data <- read.table("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/crabs.txt",sep="\t")
write.csv(data,"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/crabs.csv")

##delete the first row in the saved csv file

data <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/crabs.csv",sep=",")

data <- data[,4:8]
summary(data)

write.csv(summary(data),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/summary_data.csv")


##Q(1)
plot(data)

par(mfrow=c(3,2))
hist(data[,1],xlab="Frontal Lobe Size (mm)",main="Histgram of Frontal Lobe Size")
hist(data[,2],xlab="Rear Width (mm)",main="Histgram of Rear Width")
hist(data[,3],xlab="Carapace Length(mm)", main="Carapace Length")
hist(data[,4],xlab="Carapace Width (mm)", main="Carapace Width")
hist(data[,5],clab="Body Depth (mm)",main="Body Depth")

##PCA
##Q(3)
prcomp_5variables <- prcomp(data,scale=TRUE) 
summary(prcomp_5variables)
summary(prcomp_5variables)$importance


##scoreplot based on the first two PCs
crab.scores.cor = predict(prcomp_5variables)
plot(crab.scores.cor[,1],crab.scores.cor[,2],main="First two principal components from correlation",xlab="PC1",ylab="PC2")
text(crab.scores.cor[,1],crab.scores.cor[,2],labels=1:length(data[,1]),cex=0.7,col="red")


##(3) ## screen diagram based on the correlation matrix
screeplot(prcomp(data,col=T),main="Scree diagram based on the correlation matrix")

#producing biplot
biplot(prcomp(data,cor=FALSE))
title("Biplot based on the covariance matrix")

##(6) boostrap

bs_data <- matrix(nrow=5000,ncol=1)
for (i in 1:5000) {
	sample_data <- sample(nrow(data),size=nrow(data),replace=T)
	newdata <- data[sample_data,]
	newpca <- prcomp(newdata[,],center=T,scale=T)
	bs_data[i,] <- summary(newpca)$importance[3,2]
}

hist(bs_data,xlab="variance explained by the first 2 PCs",main="Bootstrapping the confidence interval for the precentage of variance")
mean_bs_data <- mean(bs_data)
stdev_bs_data <- sqrt(var(bs_data))
samplesize_bs_data <- 5000
error <- qnorm(0.975)*stdev_bs_data/sqrt(samplesize_bs_data)
left <- mean_bs_data - error
right <- mean_bs_data + error
abline(v=left,col="red",lty=2)
abline(v=right,col="red",lty=2)
abline(v=mean_bs_data,col="red")
legend("topleft",names(c("95% CI","mean"),cex=0.8,lty=1:2))

