# ------------------- Machine learning 2 - Variable quali

library(glmnet)
library(rpart)
library(randomForest)


# ----- Import des données
don <- read.csv("artere.txt", sep = " ", stringsAsFactors = TRUE)
head(don)
sapply(don, class)

colnames(don)[which(colnames(don) == "chd")] <- "Y"
don$Y <- as.factor(don$Y)
don$agrp <- as.factor(don$agrp)
sapply(don, class)


# ----- Random forest 
mod <- randomForest(Y ~ ., don)
mod

varImpPlot(mod)


