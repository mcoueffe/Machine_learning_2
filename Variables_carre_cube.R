###nettoyage
don <- read.table("DONNEES/ozone.txt",header=T,
                  sep=";",row.names = 1,stringsAsFactors = T)
summary(don)
don$Y = don$O3
don$O3 = NULL
dim(don)
saveRDS(don,"don.RDS")
###########
typevar = sapply(don,class)
XX <- don[,typevar!="factor"] %>% select(-Y)
Xca = XX^2
Xcu = XX^3
colnames(Xca) <- paste(colnames(XX),"car",sep="")
colnames(Xcu) <- paste(colnames(XX),"cub",sep="")
donP <- cbind(don,Xca,Xcu)
dim(donP)
saveRDS(donP,"donP.RDS")


###nettoyage
don <- read.table("DONNEES/donapp.csv",header=T,sep=",")
summary(don)
don$Nacelle_angle = NULL
saveRDS(don,"don.RDS")
typevar = sapply(don,class)
library(tidyverse)
XX <- don[,typevar!="factor"] %>% select(-Y)
Xca = XX^2
Xcu = XX^3
colnames(Xca) <- paste(colnames(XX),"car",sep="")
colnames(Xcu) <- paste(colnames(XX),"cub",sep="")
donP <- cbind(don,Xca,Xcu)
dim(donP)
saveRDS(donP,"donP.RDS")

tmp <- model.matrix(Y~.^2,data=don)[,-1]
dim(tmp)
donI <- data.frame(Y=don$Y,tmp)
saveRDS(donI,"donI.RDS")
