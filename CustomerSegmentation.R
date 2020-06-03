library(dplyr)
data<-read.csv("D:/Ivy/Files/projects/clustering/customer-segmentation-dataset/Mall_Customers.csv")
#Hierarchical Clustering
View(data)
dim(data)
glimpse(data)
summary(data)
sum(is.na(data))
dist<-dist(data,method="euclidean")
dendogram<-hclust(dist, method = "ward.D")
plot(dendogram, main = paste("Dendogram of hierarchy"), xlab = "Customers", ylab = "Euclideandist")

#Model fitting
hc_mod<-hclust(dist, method = "ward.D")
y_hc=cutree(hc_mod,5)
library(cluster)
clusplot(data,y_hc, shade = T, color = T, labels = 2, plotchar = F, span = T, main = "Customer Segmentation", xlab = "Annual Income", ylab = 'Spending Score')

table(y_hc)

#K-means Clustering
sd(data$Age)

#Gender Distribution
prop.table(table(data$Age))
var<-table(data$Age)
barplot(as.matrix(var), main="Gender Comparison", ylab = "Count", xlab = "Gender", legend.text = rownames(var))

hist(data$Age, col = "blue", main = "Count of Age", xlab = "Age Class", ylab = "frequency", labels = T)
boxplot(data$Age)
colnames(data)<-c('ID','Gender','Age','AnnualIncome','SpendingScore')
hist(data$AnnualIncome, main = "Count of Age", xlab = "Age Class", ylab = "frequency", labels = T)
boxplot(data$AnnualIncome)

library("purrr")
set.seed(999)
#Elbow method
iss<- function(k){
  kmeans(data[,3:5], k, iter.max = 10, nstart=100, algorithm = 'Lloyd')$tot.withiniss
}
k.values=1:10
iss_values<-map_dbl(k.values, iss)
#Error: Result 1 must be a single double, not NULL of length 0(for later check)

#Average Silhouette
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
k_mod2<-kmeans(data[,3:5],2,iter.max = 100, nstart = 50, algorithm = "Lloyd")
dist<-dist(data[,3:5],method="euclidean")
plot(silhouette(k_mod2$cluster, dist))
k_mod3<-kmeans(data[,3:5],3,iter.max = 100, nstart = 50, algorithm = "Lloyd")
#dist<-dist(data[,3:5],method="euclidean")
plot(silhouette(k_mod3$cluster, dist))
#'ll be doing till k=10
#'find the optimal number of cluster
fviz_nbclust(data[,3:5],kmeans,method = "silhouette")

#Gap statistic(preferred): helps to formalize elbow and sillouhette
fviz_nbclust(data[,3:5],kmeans,method = "gap_stat")

#Visualize using first two principle components
pcClust<-prcomp(data[,3:5], scale= F)
summary(pcClust)
pcClust$rotation[,1:2]

#Segmentation using ggplot2
mod.optimal<-kmeans(data[,3:5],6,iter.max = 100, nstart = 50, algorithm = "Lloyd")
ggplot(data, aes(x = SpendingScore, y= Age))+geom_point(stat="identity",col= as.factor(mod.optimal$cluster))
#sacle_color_discrete()/ggtitle()







