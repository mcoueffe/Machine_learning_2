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
           cv.folds = 10)

gbm.perf(mod)
best.iter <- gbm.perf(mod, method = "cv")
print(best.iter)

mod1 <- gbm(formula = Y ~ ., distribution = "gaussian", data = don, n.trees = 100)
mod2 <- gbm(formula = Y ~ ., distribution = "gaussian", data = don, n.trees = 23)

plot(don$Y, type="l")
lines(predict(mod1, don), col ="red")
lines(predict(mod1, don), col ="green", n.trees = 23)
lines(predict(mod2, don), col ="blue")

gbm.perf(mod1)

summary(mod1, n.trees = 1)          # using first tree
summary(mod1, n.trees = best.iter)  # using estimated best number of trees

# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(mod1, i.tree = 1))
print(pretty.gbm.tree(mod1, i.tree = mod1$n.trees))

# ------------------------------------------- XGBOOST ----------------------

XX <- model.matrix(Y~.,data=don)[,-1]
YY <- don$Y

bst <- xgboost(data = XX, label = YY,
               max_depth = 1, eta = 0.1, nthread = 2, nrounds = 100)
pred <- predict(bst, agaricus.test$data)

dtrain <- xgb.DMatrix(data = XX, label = YY)

params <- list(objective = "reg:squarederror",
               max_depth = 1, eta = 0.1, eval_metric = "rmse")

cv.results <- xgb.cv(params = params,
                     data = dtrain,
                     nfold = 10,
                     nrounds = 100,
                     early_stopping_rounds = 10)

titi = cv.results$evaluation_log
plot(titi$iter, titi$train_rmse_mean)
points(titi$iter, titi$test_rmse_mean, col = "red")

best_nround = cv.results$best_iteration

final_model <- xgb.train(data = dtrain, nrounds = best_nround)
