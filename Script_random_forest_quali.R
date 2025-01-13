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
  ###arbre
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
}

RES[1:4,]
erreur <- function(X,Y){mean((X-Y)^2)}
round(apply(RES,2,erreur,Y=RES[,1]),2)

