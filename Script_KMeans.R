# -------------------------------------------- K MEANS


# Import de données
don <- read.csv("donclassif.txt", sep = ";")

# K means
km <- kmeans(don,centers=4)
names(km)
plot(don,col=km$cluster)
points(km$centers,cex=3,pch=16,col=1:4)
legend("topright",legend=paste("Classe ",1:4),col=1:4,pch=16)

km$tot.withinss
km$betweenss
km$withinss
km$size
km$cluster
km$centers
km$iter

tmp_within <- rep(NA, 30)
for (i in 1:30){
  km <- kmeans(don,centers=i, nstart = 10)
  tmp_within[i] <- km$tot.withinss
}
plot(tmp_within)

# ------------------------------------------ CAH

cah <- hclust(dist(don, method = "euclidian"), method="ward.D")
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=4)
plot(don,col=gpcah)

cah <- hclust(dist(don, method = "manhattan"), method="complete")
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=6)
plot(don,col=gpcah)

cah <- hclust(dist(don, method = "euclidian"), method="single")
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=20)
plot(don,col=gpcah)
