# -------------------------------------------- K MEANS

library(dbscan)

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

cah <- hclust(dist(don, method = "manhattan"), method="complete")  # complete : groupes relativement homogènes
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=10)
plot(don,col=gpcah)

cah <- hclust(dist(don, method = "euclidian"), method="single") # beaucoup de groupes dont des très petits
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=40)
plot(don,col=gpcah)

cah <- hclust(dist(don, method = "maximum"), method="median")
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=9)
plot(don,col=gpcah)

cah <- hclust(dist(don, method = "euclidian"), method="complete")
plot(as.dendrogram(cah))
plot(sort(cah$height,dec=T)[1:50],type="h")
gpcah <- cutree(cah, k=14)
plot(don,col=gpcah)

# ---------------------------------------- DBSCAN
par(mfrow = c(1, 1))

library(dbscan)
kNNdistplot(don, k=3)
db <- dbscan::dbscan(don, eps = 0.25, minPts = 4) # le groupe 0 est le groupe poubelle, il n'apparait pas sur le graph
plot(don, col = db$cluster)
table(db$cluster)
sum(table(db$cluster))

library(fpc)
db2 <- fpc::dbscan(don, eps = 0.2, MinPts = 4)
plot(don, col = db2$cluster)

# ---------------------------------------- Modèles de mélange

library(mclust)

melange <- Mclust(don,1:12)
summary(melange)
names(summary(melange))
melange$bic
melange$modelName
plot(don, col=summary(melange)$classification)
melange$BIC

plot(melange)

# --------------------------------------- ACP

library(FactoMineR)
data("decathlon")

res.pca=PCA(decathlon,quanti.sup=11:12,quali.sup=13)
names(res.pca)
barplot(res.pca$eig[,1], names=paste("Dim",1:nrow(res.pca$eig)), main="inertie expliquée")
names(res.pca$ind)
plot(res.pca,choix="ind",habillage=13,cex=.7)

res.pca$var$contrib
