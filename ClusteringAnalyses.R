crabs <- read.csv("/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW5/crabs.txt",sep="\t")
##Prepare Data
data <- crabs[,3:7] 
##Partitioning

##Determine number of clusters
wss <- (nrow(data)-1)*sum(apply(data,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(data, centers=i)$withinss)
plot(1:15, wss, type="b",xlab="Number of clusters",ylab="within groups sum of squares")

## K-Means Cluster Analysis
fit <-kmeans(data, 4, nstart=10) ## 4 cluster solution
#get cluster means
aggregate(data, by=list(fit$cluster),FUN=mean)

## compare results of K-means cluster with pre-defined categories
head(crabs)
groups <- rep(0,nrow(crabs))

for (i in 1:nrow(crabs)) {
	if (crabs[i,1] == 1 & crabs[i,2] == 1) {
		groups[i] <- "BM"
	}
	if (crabs[i,1] == 1 & crabs[i,2] == 2) {
		groups[i] <- "BF"
	}
	if (crabs[i,1] == 2 & crabs[i,2] == 1) {
		groups[i] <- "OM"
	}
	if (crabs[i,1] == 2 & crabs[i,2] ==2) {
		groups[i] <- "OF"
	}
	}

compare <- data.frame(groups, fit$cluster)

## The silhouette plot of k-means with function kmeans
library(cluster)
diss_crabs <- daisy(data)
sil.km = silhouette(fit$cluster, diss_crabs)
plot(sil.km, main="Silhouette plot from K-means - function kmeans")

##Projection onto the first two PCs
clusplot(data, fit$cluster, color=T, shade=F, labels=1, lines=0,main="projection onto the first two PCs, kmeans")

## PAM
library(cluster)
pam.crabs = pam(data, k=4, diss=F)
##The silhouette plot
sil.pam = silhouette(pam.crabs$cluster, diss_crabs)
plot(sil.pam, main="Silhouette plot from K-means - function pam")

## project onto the first two PCs
clusplot(data, pam.crabs$cluster, color=T, shade=F, labels=1, lines=0, main="projection onto the first two PCs, crabs data, function pam")

## Hierarchical clustering
library(cluster)
crabs.single = agnes(data, diss=F, method="single")
##silhouette plot
diss_crabs = daisy(data)
sil.single = silhouette(cutree(crabs.single, k=4), diss_crabs)
plot(sil.single,main="Hierarchical Clustering, single")

##complete
crabs.complete = agnes(data, diss=F, method="complete")
sil.complete = silhouette(cutree(crabs.complete, k=4), diss_crabs)
plot(sil.complete, main="Hierarchical Clustering, complete")

#average
crabs.average = agnes(data, diss=F, method="average")
sil.average = silhouette(cutree(crabs.average, k=4), diss_crabs)
plot(sil.average, main="Heirarchical Clustering, average")

#ward
crabs.ward = agnes(data, diss=F, method="ward")
sil.ward = silhouette(cutree(crabs.average, k=4), diss_crabs)
plot(sil.ward, main="Hierarchical Clustering, ward")

par(mfrow=c(2,2))
plot(sil.single,main="Hierarchical Clustering, single")
plot(sil.complete, main="Hierarchical Clustering, complete")
plot(sil.average, main="Heirarchical Clustering, average")
plot(sil.ward, main="Hierarchical Clustering, ward")

plot(crabs.single, which.plots=2)

##
d <- dist(data, method="euclidean")
ffit <- hclust(d, method="ward")
plot(ffit) # display dendogram of complete method
gpals <- cutree(ffit, k=4) # cut tree into 4 clusters
# draw dendogram with red borders around the 4 clusters 
rect.hclust(ffit, k=4, border="red")

BM <- subset(groups,groups=="BM")
BF <- subset(groups,groups=="BF")
OM <- subset(groups,groups=="OM")
OF <- subset(groups,groups=="OF")

table(BM, gpals[1:50])
table(BF, gpals[51:100])
table(OM, gpals[101:150])
table(OF, gpals[151:214])

##model-based clustering
library(mclust)
crabs.mclust = Mclust(data)
round(crabs.mclust$BIC,0)
write.csv(round(crabs.mclust$BIC,0),"/Users/achimnyswallow/Documents/Courses/Stats 503/Homework/HW5/Mclust_BIC.csv" )
library(mclust)
fit <- Mclust(data)
plot(fit) # plot results 
summary(fit) # display the best model
