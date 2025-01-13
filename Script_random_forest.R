# ------------------- Machine learning 2 - Variable quanti

library(glmnet)
library(rpart)
library(randomForest)

# ----- Import des données
don <- read.csv("ozone.txt", sep = ";", stringsAsFactors = TRUE)
head(don)
sapply(don, class)

don <- don[,-1]
colnames(don)[1] <- "Y"


# ----- Random forest 
mod <- randomForest(Y ~ ., don)
mod

mod2 <- randomForest(Y ~ ., don, ntree = 100, mtry = 2)
mod2

mod3 <- randomForest(Y ~ ., don, ntree = 100, mtry = 11)
mod3

varImpPlot(mod)
varImpPlot(mod2)

plot(don$Y, type = "l")
lines(mod$predicted, col = "red")
lines(mod2$predicted, col = "blue")
lines(mod3$predicted, col = "green")

plot(mod$mse)
lines(mod2$mse, col = "blue")
lines(mod3$mse, col = "green")

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
  mod1 <- glm(Y~.,data=donA)
  RES[blocs==ii,"glob"] <- predict(mod1,donT)
  mod2 <- step(mod1,trace=0)
  RES[blocs==ii,"AIC"] <- predict(mod2,donT)
  mod3 <- step(mod1,trace=0,k=log(nrow(donA)))
  RES[blocs==ii,"BIC"] <- predict(mod3,donT)
  ###arbre
  arbre <- rpart(Y~.,data=donA)
  RES[blocs==ii,"arbre"] <- predict(arbre,donT)
  ###random forest
  foret <- randomForest(Y~.,data=donA)
  RES[blocs==ii,"foret"] <- predict(foret,donT)
  ###ridge
  tmp <- cv.glmnet(XXA,YYA,alpha=0)
  RES[blocs==ii,"ridgemin"] <- predict(tmp,XXT,s="lambda.min")
  RES[blocs==ii,"ridge1se"] <- predict(tmp,XXT,s="lambda.1se")
  ###lasso
  tmp <- cv.glmnet(XXA,YYA,alpha=1)
  RES[blocs==ii,"lassomin"] <- predict(tmp,XXT,s="lambda.min")
  RES[blocs==ii,"lasso1se"] <- predict(tmp,XXT,s="lambda.1se")
  ###elastic
  tmp <- cv.glmnet(XXA,YYA,alpha=.5)
  RES[blocs==ii,"elasmin"] <- predict(tmp,XXT,s="lambda.min")
  RES[blocs==ii,"elas1se"] <- predict(tmp,XXT,s="lambda.1se")
}

RES[1:4,]
erreur <- function(X,Y){mean((X-Y)^2)}
round(apply(RES,2,erreur,Y=RES[,1]),2)

