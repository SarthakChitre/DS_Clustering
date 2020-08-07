#Airlines Hierarichal Clustering
library(readxl)
library(openxlsx)
input=read.xlsx(file.choose(),sheet=2)
View(input)
my_data=input[,c(-1)]
View(my_data)

normalized_data=scale(my_data)  #To normalize the data
d=dist(normalized_data,method="euclidean") #To find distance between points
d
fit=hclust(d,method="complete") # hierarchy clustering
plot(fit)
plot(fit,hang=-1) #common base
groups<-cutree(fit,k=5) #5 clusters
rect.hclust(fit,k=5,border="red") # red border

membership<-as.matrix(groups) #grouping 

final<-data.frame(my_data,membership)
final1<-final[,c(ncol(final),1:(ncol(final)-1))] # Move  the grouping in 1st column 
View(final1)





##Airlines K-means Clustering##
library(readxl)
library(openxlsx)
input=read.xlsx(file.choose(),sheet=2)
View(input)
my_data=input[,c(-1)]
View(my_data)
normalize=scale(my_data)
View(normalize)
km=kmeans(normalize,5) #k means
str(km)
km$withinss

install.packages("animation")
library(animation)
km <- kmeans.ani(normalize, 12)

#elbow curve & k ~ sqrt(n/2) to decide the k value
wss = (nrow(normalize)-1)*sum(apply(normalize, 2, var))		 # Determine number of clusters by scree-plot 
for (i in 2:15) wss[i] = sum(kmeans(normalize, centers=i)$withinss) #12 0r 15
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")



## k clustering alternative for large dataset

install.packages("cluster")
library(cluster)
xcl <- clara(normalize,3, sample = 1000)  
clusplot(xcl)
xpm <- pam(normalize, 3)
clusplot(xpm)


#
