View(wine)
dim(wine) # used to get or set the dimension of the specified
mydata<-wine[,-1]
View(mydata)
attach(mydata)
hist(Alcohol)
hist(Malic)
hist(Ash)
hist(Alcalinity)
hist(Magnesium)
hist(Phenols)
hist(Flavanoids)
hist(Nonflavanoids)
hist(Proanthocyanins)
hist(Color)
hist(Hue)
hist(Dilution)
hist(Proline)
boxplot(Alcohol)
boxplot(Malic)
boxplot(Ash)
boxplot(Alcalinity)
boxplot(Magnesium)
boxplot(Phenols)
boxplot(Flavanoids)
boxplot(Nonflavanoids)
boxplot(Proanthocyanins)
boxplot(Color)
boxplot(Hue)
boxplot(Dilution)
boxplot(Proline)
cor(mydata)
pcaObj<-princomp(mydata, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
loadings(pcaObj)
plot(pcaObj)
pcaObj$scores
pcaObj$scores[,1:3]
mydata1<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata1)
clus_data<-mydata1[,14:16]
clus_data
dist1<-dist(clus_data,method = "euclidean")
install.packages("factoextra")
library(factoextra)
fit1<-hclust(dist1,method="complete")
fviz_nbclust(clus_data, FUNcluster = hcut, method = c("silhouette"), 
             diss = NULL, k.max = 25, nboot = 100,
             verbose = interactive(), barfill = "steelblue",
             barcolor = "steelblue", linecolor = "steelblue",
             print.summary = TRUE)
plot(fit1, hang=-1)
groups<-cutree(fit1,3)
rect.hclust(fit1, k=3, border="red")
wine2 <- as.matrix(groups)
winegroup <- cbind(mydata1,wine2)
View(winegroup)

#original data clustering
mydata2<-scale(mydata)
dist<-dist(mydata2,method = "euclidean")
install.packages("factoextra")
library(factoextra)
fit1<-hclust(dist,method="complete")
fviz_nbclust(mydata2, FUNcluster = hcut, method = c("silhouette"), 
             diss = NULL, k.max = 25, nboot = 100,
             verbose = interactive(), barfill = "steelblue",
             barcolor = "steelblue", linecolor = "steelblue",
             print.summary = TRUE)
plot(fit1, hang=-1)
groups<-cutree(fit1,3)
rect.hclust(fit1, k=3, border="red")
wine2 <- as.matrix(groups)
winegroup <- cbind(mydata2,wine2)
View(winegroup)


#Kmeans
?kmeans()
fit <- kmeans(mydata1,3) # 3 cluster solution
str(fit)
fit$centers
fit$cluster
final2<- data.frame(clus_data, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
final4 <- cbind(mydata1,final3)
View(final4)

#orignal data

?kmeans()
fit <- kmeans(mydata2,3) # 3 cluster solution
str(fit)
fit$centers
fit$cluster
final2<- data.frame(mydata2, fit$cluster) # append cluster membership
View(final2)
final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
final4 <- cbind(mydata1,final3)
View(final4)
