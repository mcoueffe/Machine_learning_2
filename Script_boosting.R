# ------ Boosting

library(gbm)
library(xgboost)

# ----- Import des données
don <- read.csv("ozone.txt", sep = ";", stringsAsFactors = TRUE)
head(don)
sapply(don, class)

don <- don[,-1]
colnames(don)[1] <- "Y"

# ----- Modèle
# Gaussian pour régression, adaboost ou Bernouilli pour classification
mod <- gbm(formula = Y ~ ., distribution = "gaussian", data = don)

best.iter <- gbm.perf(mod, method = "OOB")
print(best.iter)

mod <- gbm(formula = Y ~ ., distribution = "gaussian", data = don,  
           cv.folds = 100)

best.iter <- gbm.perf(mod, method = "OOB")
print(best.iter)

mod1 <- gbm(formula = Y ~ ., distribution = "gaussian", data = don, n.trees = 100)
mod2 <- gbm(formula = Y ~ ., distribution = "gaussian", data = don, n.trees = 23)

plot(don$Y, type="l")
lines(predict(mod1, don), col ="red")
lines(predict(mod2, don), col ="blue")

summary(mod1, n.trees = 1)          # using first tree
summary(mod1, n.trees = best.iter)  # using estimated best number of trees

# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(mod1, i.tree = 1))
print(pretty.gbm.tree(mod1, i.tree = mod1$n.trees))
