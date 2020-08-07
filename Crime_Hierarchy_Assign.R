##Crime Data
CD=read.csv(file.choose())
View(CD)
CD1=CD[,c(-1)]
View(CD1)

normalized_data=scale(CD1)
d=dist(normalized_data,method="euclidean")
fit=hclust(d,method="complete")

plot(fit)
plot(fit,hang=-1)
groups=cutree(fit,k=4)
rect.hclust(fit,k=4,border = "red")

crime_degree=as.matrix(groups)
final=data.frame(CD,crime_degree)
View(final)
final1<-final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)
