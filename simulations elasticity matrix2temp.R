library("ggplot2")
library("GGally")
rm(list=ls())
nweeks=1200
nprod=4
nruns=2000
elast=matrix(c(-2.93,0.15,0.18,0.17,0.38,-1.84,0.03,-0.01,0.89,0.32,-3.72,0.36,1.24,0.92,-0.13,-3.82), nrow=nprod, ncol=nprod)
elast=t(elast)
runelast<-elast
elastsd=matrix(c(1,0.75,0.65,0.39,0.22,0.96,0.3,0.22,0.43,0.98,1.22,0.42,0.76,0.9,0.77,0.85), nrow=nprod, ncol=nprod)

Prices=matrix(0,nrow=nweeks, ncol=nprod)
for (i in 1:nprod ) {
  Prices[,i]=round(rnorm(nweeks, mean=30*i, sd=3*i))
  if (elast[i,i] > 0) elast[i,i] <- elast[i,i]*-1
  }
alpha=c(10.65,9.64,9.29,8.69)
#alpha=matrix(rnorm(nweeks*nprod,mean=15,sd=3), nrow=nweeks, ncol=nprod)
Sales=exp(matrix(rnorm(nweeks*nprod,mean=,sd=3), nrow=nweeks, ncol=nprod)+log(Prices)%*%elast)
#demanda=cbind(sales,Prices)
#library(gdata)
#concat_data <- cbindX(sales, Prices)
colnames(Prices)=c("Price1", "Price2", "Price3", "Price4")
colnames(Sales)=c("Sales1", "Sales2", "Sales3", "sales4")
#Estim_elast=solve(t(Prices)%*%Prices)%*%(t(Prices)%*%Sales)
#aux=solve(t(Prices)%*%Prices)
for (i in 1:nprod ) {
  aux2=paste("fitmodel",i,sep="_")
  assign(aux2,lm(Sales[,i]~Prices))

}

Precio=c(10,10,10,10)
Pricemin=matrix(0,nruns,nprod)
colnames(Pricemin)=c("Price1", "Price2", "Price3","Price4")
Pricemin2<-Pricemin
#Precio=Prices[nweeks,]
f_demand <- function(Precio) {
  demand <- -sum(Precio[1]*exp(alpha[1] +sum(log(Precio)%*%runelast)),Precio[2]*exp(alpha[2] +sum(log(Precio)%*%runelast)),Precio[3]*exp(alpha[3] +sum(log(Precio)%*%runelast)),Precio[4]*exp(alpha[4] +sum(log(Precio)%*%runelast)))
  return(demand)
}

for (k in 1:nruns ) {

#creando instancia de matriz
for (i in 1:nprod ) {
for (j in 1:nprod)  {
  runelast[i,j]=rnorm(1, mean=elast[i,j], sd=elastsd[i,j])
}
}
#caso 1 con rest laxas
  Priceopt<-optim(Precio,f_demand, method="SANN", control=list(maxit=100))
  Priceopt<-optim(Priceopt$par,f_demand, lower=Precio*0.01, upper=Precio*4,method="L-BFGS-B")
  Pricemin[k,c(1:4)]<-Priceopt$par
  #Pricemin[k,5]= -Priceopt$value
  # Precio=c(10,10,10,10)
  #caso 2 con rest mas apretadas 
  Priceopt2<-optim(Precio,f_demand, method="SANN", control=list(maxit=100))
  Priceopt<-optim(Priceopt2$par,f_demand, lower=Precio*0.5, upper=Precio*1.5,method="L-BFGS-B")
 # Priceopt2<-optim(Precio,f_demand, lower=Precio*0.8, upper=Precio*1.2,method="L-BFGS-B")
  Pricemin2[k,c(1:4)]<-Priceopt$par
  #Pricemin2[k,5]= -Priceopt$value 
  
}

Pricemin2[40,]

ggpairs(data.frame(Pricemin), title="Opt prices with 0.01-4X constraints",upper = list(continuous = "density", combo = "box"))
ggpairs(data.frame(Pricemin2), title="Opt prices with +-50% constraints",upper = list(continuous = "density", combo = "box"))

#ggpairs(iris, aes(colour = Species, alpha = 0.4))

################### optim example
#
# f_demandtest <- function(p) {
#   demand <- -30-40*p+2*p^2
#   return(demand)
# }
# pmin<-optim(0,f_demandtest, method="Brent",lower = -10000, upper = 10000)
# #pmin<- optimize(f_demand, c(0,10000), tol=0.001)
# pmin
#
# ##############


