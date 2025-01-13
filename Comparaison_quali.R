# ------------------- Comparaison - Variable quali

library(glmnet)
library(rpart)
library(randomForest)
library(gbm)
library(bestglm)
library(xgboost)

# ----- Import des données
data(SAheart)
don <- SAheart
head(don)
sapply(don, class)

colnames(don)[which(colnames(don) == "chd")] <- "Y"
don$Y <- as.numeric(don$Y)
sapply(don, class)


# ------- Validation croisée
dim(don)
nb=10
set.seed(1234)
blocs <- sample(rep(1:nb,length=nrow(don)))
RES <- data.frame(Y=don$Y)
### en matrices pour glmnet
#library(caret)
#dmy <- dummyVars(Y~ ., don = don)
#XX <- predict(dmy,don)
XX <- model.matrix(Y~.,data=don)[,-1]
YY <- don$Y
for(ii in 1:nb){
  print(ii)
  donA = don[blocs!=ii,]
  donT = don[blocs==ii,]
  XXA = XX[blocs!=ii,]
  XXT = XX[blocs==ii,]
  YYA = YY[blocs!=ii]
  ###############
  mod1 <- glm(Y~.,data=donA,family="binomial")
  RES[blocs==ii,"glob"] <- predict(mod1,donT,type="response")
  mod2 <- step(mod1,trace=0)
  RES[blocs==ii,"AIC"] <- predict(mod2,donT,type="response")
  mod3 <- step(mod1,trace=0,k=log(nrow(donA)))
  RES[blocs==ii,"BIC"] <- predict(mod3,donT,type="response")
  ###arbre
  arbre <- rpart(factor(Y)~.,data=donA)
  RES[blocs==ii,"arbre"] <- predict(arbre,donT,type="prob")[,2]
  ###foret
  foret <- randomForest(factor(Y)~.,data=donA)
  RES[blocs==ii,"foret"] <- predict(foret,donT,type="prob")[,2]
  ###ridge
  tmp <- cv.glmnet(XXA,YYA,alpha=0,family="binomial")
  RES[blocs==ii,"ridgemin"] <- predict(tmp,XXT,s="lambda.min",type="response")
  RES[blocs==ii,"ridge1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
  ###lasso
  tmp <- cv.glmnet(XXA,YYA,alpha=1,family="binomial")
  RES[blocs==ii,"lassomin"] <- predict(tmp,XXT,s="lambda.min",type="response")
  RES[blocs==ii,"lasso1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
  ###elastic
  tmp <- cv.glmnet(XXA,YYA,alpha=.5,family="binomial")
  RES[blocs==ii,"elasmin"] <- predict(tmp,XXT,s="lambda.min",type="response")
  RES[blocs==ii,"elas1se"] <- predict(tmp,XXT,s="lambda.1se",type="response")
  ###gradient boosting
  tmp <- gbm(Y~.,data=donA, distribution = "bernoulli", cv.folds = 10, n.minobsinnode = 5)
  best.iter <- gbm.perf(tmp, method = "cv")
  RES[blocs==ii,"boosting"] <- predict(tmp,donT,type="response")
  tmp.opt <- gbm(Y~.,data=donA, distribution = "bernoulli", n.trees = best.iter, n.minobsinnode = 5)
  RES[blocs==ii,"boosting_2etapes"] <- predict(tmp.opt,donT,type="response")
  ###xgboost
  dtrain <- xgb.DMatrix(data = XXA, label = YYA)
  dtest <- xgb.DMatrix(data = XXT)
  params <- list(objective = "binary:logistic", max_depth = 1, eta = 0.1, eval_metric = "error")
  cv.results <- xgb.cv(params = params, data = dtrain,nfold = 10,nrounds = 100,early_stopping_rounds = 10)
  best_nround = cv.results$best_iteration
  final_model <- xgb.train(data = dtrain, nrounds = best_nround)
  RES[blocs==ii,"xgboost"] <- predict(final_model,dtest,type="response")
}


# ********* Analyse des résultats
library(pROC)

rocglm <- roc(RES$Y, RES$glob)
plot(rocglm)

rocglm <- roc(Y ~ glob, RES)
plot(rocglm)
coords(rocglm) # spécificité et sensibilité en fonction des seuils

coords(rocglm, x = 0.5)
coords(rocglm, x = 0.5, ret = c("threshold","tn")) # true negatives
coords(rocglm, x = 0.5, ret = c("threshold","accuracy", "sensitivity", "specifity"))
coords(rocglm, x = "best")

roctout <- roc(Y ~ ., RES)
sapply(roctout, coords, x=0.5, ret = c("threshold","accuracy", "sensitivity", "specifity"))
sapply(roctout, coords, x="best", ret = c("threshold","accuracy", "sensitivity", "specifity"))


###choix du seuil "naturel" celui qui conserve
###la proportion de à/1 initiale
n0 = sum(don$Y==0)
n0
seuilnat <- seq(2:ncol(RES))*0
for(ii in 1:length(seuilnat)){
  seuilnat[ii] <- mean(sort(RES[,ii+1])[n0:(n0+1)])
}
seuilnat
#### on calcule maintenant pour chaque seuil les accuracy...
res <- NULL
for(ii in 1:length(seuilnat)){
  tmp =coords(roctout[[ii]],x=seuilnat[ii],
              ret=c("threshold","accuracy","sensitivity","specificity"))
  res = rbind(res,tmp)
}
rownames(res) <- colnames(RES)[-1]
res
res$med <- res$sensitivity+res$specificity
res

