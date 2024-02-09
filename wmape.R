library(glmnet)
library(tidyverse)
library(bayesm)
library(arules)
library(Matrix)
library(data.table)

olsw <- elasticity3
lassow <- elastlasso
# elastlasso elasthbm2

tlogprecio=log(tablaprecio[,2:ncol(tablaprecio)])
tlogprecio=cbind(tablaprecio[,1],tlogprecio)
colnames(tlogprecio)[1] <- "WEEK_END_DATE"
elasticity3=cbind(prod2[i,1],t(model1$coefficients),summary(model1)$r.squared,sigma(model1),mean(wmape$ape),sum(demand$UNITS))
i=1
demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
datos=datos[,-1]
model1=lm(log(UNITS)~.,data=datos)
wmape <- data.frame(pred=exp(model1$fitted.values),real=as.vector.data.frame(demand$UNITS))
plot(wmape$pred,wmape$real)
wmape$dif <- abs(wmape$pred-wmape$real)
wmape$ape <- wmape$dif/wmape$real
#,error=(demand$units - exp(predict(model1))),model1$residuals)
exp(model1$residuals)/demand$UNITS
#summary(model1)
str(demand$UNITS)
str(predict(model1))
str(exp(model1$residuals))
predict(model1)


elasticity3[i,]=cbind(prod2[i,1],t(model1$coefficients),summary(model1)$r.squared,sigma(model1))



for (i in 1:nrow(prod2) ) {
  demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
  datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
  datos=datos[,-1]
  model1=lm(log(UNITS)~.,data=datos)
  wmape <- data.frame(pred=exp(model1$fitted.values),real=as.vector.data.frame(demand$UNITS))
  wmape$dif <- abs(wmape$pred-wmape$real)
  wmape$ape <- wmape$dif/wmape$real
  elasticity3[i,]=cbind(prod2[i,1],t(model1$coefficients),summary(model1)$r.squared,sigma(model1),mean(wmape$ape),sum(demand$UNITS))
  #elasticity3[i,]=cbind(prod2[i,1],t(model1$coefficients),summary(model1)$r.squared,sigma(model1))
  }
elasticity3[is.na(elasticity3)] <- 0

elasticity3$wei <- elasticity3$`sum(demand$UNITS)`/sum(elasticity3$`sum(demand$UNITS)`)

#Weighted MAPE OLS
sum(elasticity3$`mean(wmape$ape)`*elasticity3$wei)

#Weighted MAPE OLS
mean(elasticity3$`mean(wmape$ape)`)

tablafinal=as.matrix.data.frame(matrix(nrow=3,ncol=2))
colnames(tablafinal)=c("mape","wmape")
rownames(tablafinal)=c("OLS","HB","LASSO")

tablafinal[1,]=c(mean(elasticity3$`mean(wmape$ape)`),sum(elasticity3$`mean(wmape$ape)`*elasticity3$wei))
#write.xlsx(elasticity3, "crosselast3.xls")

#LASSO
i=1
demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
datos=datos[,-1]
datos$UNITS=log(datos$UNITS)
colnames(datos)[1]="logUNITS"
model1=glmnet(as.matrix(datos[,-1]),as.matrix(datos[,1]),alpha=1)
#plot(model1,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(as.matrix(datos[,-1]),as.matrix(datos[,1]))
#plot(cv.lasso)
modelcoeffs <- coef(cv.lasso, s = "lambda.min")
modelcoeffs2= data.frame(name = modelcoeffs@Dimnames[[1]][modelcoeffs@i + 1], coefficient = modelcoeffs@x)
mergame= merge(x = listvars, y = modelcoeffs2, by = "name", all.x = TRUE, all.y=TRUE)
rownames(mergame) <- mergame[,1]
wmape <- data.frame(exp(predict(model1,as.matrix(datos[,-1]),s=cv.lasso$lambda.min)),real=as.vector.data.frame(demand$UNITS))
#plot(wmape$s1,wmape$real)
wmape$dif <- abs(wmape$s1-wmape$real)
wmape$ape <- wmape$dif/wmape$real
elastlasso[i,]=cbind(prod2[i,1],t(mergame[,2]),mean(wmape$ape),sum(demand$UNITS))


wmape


set.seed(1234)
library("glmnet")
rm(elastlasso)
#elastlasso=elasticity3
elastlasso=as.data.frame(as.matrix(cbind(prod2[1,1],tmergame)),stringsAsFactors = FALSE)
elastlasso$ape <- mean(wmape$ape)
elastlasso$demand <- sum(demand$UNITS)
prod=as.data.frame(prod2[,1])
colnames(prod)[1]="name"
listvars=rbind.data.frame(prod,"DISPLAY","FEATURE","TPR_ONLY")
for (i in 1:nrow(prod2) ) {
  demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
  datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
  datos=datos[,-1]
  datos$UNITS=log(datos$UNITS)
  colnames(datos)[1]="logUNITS"
  model1=glmnet(as.matrix(datos[,-1]),as.matrix(datos[,1]),alpha=1)
  #plot(model1,xvar="lambda",label=TRUE)
  cv.lasso=cv.glmnet(as.matrix(datos[,-1]),as.matrix(datos[,1]))
  #plot(cv.lasso)
  modelcoeffs <- coef(cv.lasso, s = "lambda.min")
  modelcoeffs2= data.frame(name = modelcoeffs@Dimnames[[1]][modelcoeffs@i + 1], coefficient = modelcoeffs@x)
  mergame= merge(x = listvars, y = modelcoeffs2, by = "name", all.x = TRUE, all.y=TRUE)
  rownames(mergame) <- mergame[,1]
  wmape <- data.frame(exp(predict(model1,as.matrix(datos[,-1]),s=cv.lasso$lambda.min)),real=as.vector.data.frame(demand$UNITS))
  #plot(wmape$s1,wmape$real)
  wmape$dif <- abs(wmape$s1-wmape$real)
  wmape$ape <- wmape$dif/wmape$real
  elastlasso[i,]=cbind(prod2[i,1],t(mergame[,2]),mean(wmape$ape),sum(demand$UNITS))
  
}
elastlasso[is.na(elastlasso)] <- 0

elastlasso$wei <- elastlasso$demand/sum(elastlasso$demand)

#Weighted MAPE LASSO
sum(elastlasso$ape*elastlasso$wei)

#Weighted MAPE LASSO
mean(elastlasso$ape)

tablafinal[3,]=c(mean(elastlasso$ape),sum(elastlasso$ape*elastlasso$wei))

############ HBM MAPES

#betahbm2
#f_demandhbm2
function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  c=as.matrix(betahbm2)
  #c=as.matrix(betatemp)
  #c[is.na(c)] <- 0
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}

#hbmwmape=cbind(prod2[i,1],mean(wmape$ape),sum(demand$UNITS))
wmape=data.frame(prod=prod2[i,1],mape=mean(demand$UNITS),demand=sum(demand$UNITS))
for (i in 1:nrow(prod2) ) {
  demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
  datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
  datos=datos[,-1]
  datos$UNITS=log(datos$UNITS)
  colnames(datos)[1]="logUNITS"
  b=as.matrix(cbind(rep(1,nrow(datos)),datos[,-1]))
  c=as.matrix(betahbm2[i,])
  predict <- exp(b%*%t(c))
  colnames(predict)[1] <- "predict"
  hbmwmape <- data.frame(predict,real=as.vector.data.frame(demand$UNITS))
  hbmwmape$dif <- abs(hbmwmape$predict-hbmwmape$real)
  hbmwmape$ape <- hbmwmape$dif/hbmwmape$real
  #plot(wmape$s1,wmape$real)
  wmape[i,] <- c(prod2[i,1],mean(hbmwmape$ape),sum(hbmwmape$real))
  #elastlasso[i,]=cbind(prod2[i,1],t(mergame[,2]),mean(wmape$ape),sum(demand$UNITS))
  }

wmape$wei <- wmape$demand/sum(wmape$demand)

#Weighted MAPE LASSO
sum(wmape$mape*wmape$wei)

#Weighted MAPE LASSO

tablafinal[2,] <- c(mean(wmape$mape),sum(wmape$mape*wmape$wei))

