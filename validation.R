##regla promedio profit
profit
rulesprofit=data.table(profit$prom, profit$historeal,(profit$historeal>mean(profit$historeal))*1)
colnames(rulesprofit)=c("prom","profit","dummyprofit")
#min(rulesprofit$profit)

rulesprofit
#quantile(rulesprofit$profit, 0.00)
rprofit=matrix(0,nrow=5,ncol=6)
colnames(rprofit)=c("cut","pi>c","pi<c","%pi>piprom","lift pi>c","c_prom")
j=1
for (i in c(0.05,0.25,0.5,0.75,0.95) ) {
  rprofit[j,1]=i
  rprofit[j,2]=rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i)),mean(profit)]
  rprofit[j,3]=rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i)),mean(profit)]
  rprofit[j,4]=rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i)),mean(dummyprofit)]
  rprofit[j,5]=rprofit[j,2]/mean(rulesprofit$profit)
  rprofit[j,6]=as.numeric(quantile(rulesprofit$prom,i))
  # rprofit[j,5]=nrow(rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i))])/156
  # rprofit[j,6]=nrow(rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i))])/156
  j=j+1
}
rprofit2=matrix(0,nrow=5,ncol=6)
colnames(rprofit2)=c("cut","pi<c","pi>c","%pi<piprom","lift pi<c","c_prom")
j=1
for (i in c(0.05,0.25,0.5,0.75,0.95) ) {
  rprofit2[j,1]=i
  rprofit2[j,2]=rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i)),mean(profit)]
  rprofit2[j,3]=rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i)),mean(profit)]
  rprofit2[j,4]=rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i)),mean(dummyprofit)]
  rprofit2[j,5]=rprofit2[j,2]/mean(rulesprofit$profit)
  rprofit2[j,6]=as.numeric(quantile(rulesprofit$prom,i))
  # rprofit[j,5]=nrow(rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i))])/156
  # rprofit[j,6]=nrow(rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i))])/156
  j=j+1
}


#### OPTIMIZACION CON PROMEDIOS
#prom=cbind.data.frame(prom/15, dt[,1],dt[,2]/15) CHEQUEO dt solo ve precios aparecidos.
nprod=nrow(prod2)
uiprom=matrix(nrow=2,ncol=nprod)
uiprom=rbind(c(rep(1,nprod)),c(rep(-1,nprod)))
uiprom
#ciprom=as.numeric(-percdt)
prom
ciprofit=c(rprofit[4,6]*nprod,-max(prom[,2])*nprod)
#rehacer cortando por soporte solo la que da positivo
#ciprofit2=c(as.numeric(quantile(prom[,2], 0.05)*nprod),as.numeric(-quantile(prom[,2], 0.95)*nprod))
ciprofit2=rprofit[4,6]*nprod
ciprofit/nprod
rprofit
rprofit2
eps=-1e-6
rhs=uiprom%*%t(tablaprecio[154,2:16])
rhs
rhsresult=data.frame(rhs,ciprofit,rhs>ciprofit)
rhsresult
ciprofit[1]
ciprofit*nprod
uiprom
### ols prom todo
p_profit <- constrOptim(t(tablaprecio[154,2:16]), f_demand3, NULL, ui=uiprom, ci=ciprofit, control=list(fnscale=-1))
p_profit$val
mean(p_profit$par)

### ols prom solo 1 restriccion
p_profit2 <- constrOptim(t(tablaprecio[154,2:16]), f_demand3, NULL, ui=uiprom[1,], ci=ciprofit[1], control=list(fnscale=-1))
p_profit2$val
mean(p_profit2$par)

### hbm prom todo
p_profit <- constrOptim(t(tablaprecio[154,2:16]), f_demandhbm, NULL, ui=uiprom, ci=ciprofit, control=list(fnscale=-1))
p_profit$val
mean(p_profit$par)

### hbm prom solo 1 restr
p_profit2 <- constrOptim(t(tablaprecio[154,2:16]), f_demandhbm, NULL, ui=uiprom[1,], ci=ciprofit[1], control=list(fnscale=-1))
p_profit2$val
mean(p_profit2$par)

### lasso prom todo
p_profit <- constrOptim(t(tablaprecio[154,2:16]), f_demandlasso2, NULL, ui=uiprom, ci=ciprofit, control=list(fnscale=-1))
p_profit$val
mean(p_profit$par)

### lasso prom rango 5%-95%
p_profit2 <- constrOptim(t(tablaprecio[154,2:16]), f_demandlasso2, NULL, ui=uiprom[1,], ci=ciprofit[1], control=list(fnscale=-1))
p_profit2$val
mean(p_profit2$par)


### profit precios single

profitprod=data.frame(tablaprecio, profit= profit$historeal,dummy=(profit$historeal>mean(profit$historeal))*1)
#profitprod[,k]>0.09,
mean(profitprod[profitprod[,k]>0.09,"profit"])

profitpar_all=profitpar
profitpar=profitpar_all

# Split into training and testing
N=nrow(profitprod)
N_train <- N*0.8
N_test <- N*0.2
train_ind <- sample(c(1:N), size = N_train, replace = FALSE)
profit_train <- profitprod[train_ind,]
profit_test <- profitprod[-train_ind,]
mean(profit_train$profit)
sd(profit_train$profit)
mean(profit_test$profit)
sd(profit_test$profit)

k=4
j=1
temp3=nprod+1
rprofitpar=data.table(matrix(0,nrow=5*nprod,ncol=20))
colnames(rprofitpar)=c("prod","cut","prfi>c","prfi<c","conf","sup_r_prf","lift pi>c","liftpi<c","c_prom","Sup_rule","Sup_invrule","sup_inv_r_prf","CONFINV","SUP_Prf","LIFT2","LIFT2INV","Pr_train","Pr_test","Pr_train-1","Pr_test-1")
for (k in 2:temp3) {
    temp=as.matrix(profitprod[,k])
for (i in c(0.05,0.25,0.5,0.75,0.95) ) {
  temp2=as.numeric(quantile(temp,i))
  rprofitpar[j,1]=as.numeric(prod2[k-1,1])
  rprofitpar[j,2]=i
  rprofitpar[j,3]=mean(profitprod[profitprod[,k]>temp2,"profit"])
  rprofitpar[j,4]=mean(profitprod[profitprod[,k]<=temp2,"profit"])
  rprofitpar[j,5]=mean(profitprod[profitprod[,k]>temp2,"dummy"]) #conf
  rprofitpar[j,6]=sum(profitprod[profitprod[,k]>temp2,"dummy"]) #support rule and profit
  rprofitpar[j,7]=rprofitpar[j,3]/mean(rulesprofit$profit)
  rprofitpar[j,8]=rprofitpar[j,4]/mean(rulesprofit$profit)
  rprofitpar[j,9]=temp2
  rprofitpar[j,10]=nrow(profitprod[profitprod[,k]>temp2,]) #support pricing rule
  rprofitpar[j,11]=nrow(profitprod[profitprod[,k]<=temp2,])
  rprofitpar[j,12]=sum(profitprod[profitprod[,k]<=temp2,"dummy"]) # sup profit y no rule
  #rprofitpar[j,12]=rprofitpar[j,5]/rprofitpar[j,9] #conf
  rprofitpar[j,13]=mean(profitprod[profitprod[,k]<=temp2,"dummy"])#confinv
  rprofitpar[j,14]=sum(profitprod[,"dummy"])/nrow(profitprod) #suppi
  rprofitpar[j,15]=rprofitpar[j,5]/rprofitpar[j,14] #LIFT
  rprofitpar[j,16]=rprofitpar[j,13]/rprofitpar[j,14] #LIFT2
  rprofitpar[j,17]=mean(profit_train[profit_train[,k]>temp2,"profit"]) #profit train
  rprofitpar[j,18]=mean(profit_test[profit_test[,k]>temp2,"profit"]) #profit test
  rprofitpar[j,19]=mean(profit_train[profit_train[,k]<=temp2,"profit"]) #profit regla inversa train
  rprofitpar[j,20]=mean(profit_test[profit_test[,k]<=temp2,"profit"]) #profit regla inversa test
  
  # rprofit[j,5]=nrow(rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i))])/156
  # rprofit[j,6]=nrow(rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i))])/156
  j=j+1
 }
}
rprofitpar2=rprofitpar[order(-`lift pi>c`),]
data.table(rprofitpar2)
prodlist=unique(rprofitpar$prod)

selrules=rprofitpar[1,]
for (i in prodlist) {
temp=rprofitpar[prod==i&cut<0.8,]
temp2=max(temp$`lift pi>c`,na.rm=TRUE)
#temp3=temp[`lift pi>c`==temp2,]
temp3= temp[temp$`lift pi>c`==temp2,]
selrules=rbind(selrules,temp3[1,])
}
selrules=selrules[-1,]

selrules2=rprofitpar[1,]
for (i in prodlist) {
  temp=rprofitpar[prod==i&cut>0.2,]
  temp2=max(temp$`liftpi<c`,na.rm=TRUE)
  #temp3=temp[`lift pi>c`==temp2,]
  temp3= temp[temp$`liftpi<c`==temp2,]
  selrules2=rbind(selrules2,temp3[1,])
}
selrules2=selrules2[-1,]


uiprodprofit=cbind.data.frame(diag(1,15,15), cut=selrules$c_prom, lift=selrules$`lift pi>c`)
uiprodprofit=uiprodprofit[uiprodprofit$lift>1.03,]
uiprodprofit2=cbind.data.frame(diag(-1,15,15), cut=-selrules2$c_prom, lift=selrules2$`liftpi<c`)
uiprodprofit2=uiprodprofit2[uiprodprofit2$lift>1.001,]
uiprodprofit2

dim(uiprodprofit)
dim(uiprodprofit2)
uiprodprofit=rbind(uiprodprofit,uiprodprofit2)
uiprodprofit=uiprodprofit[,-ncol(uiprodprofit)] #borro el lift
uiprodprofit=as.matrix(uiprodprofit)
ciprodprofit=uiprodprofit[,ncol(uiprodprofit)]+eps
uiprodprofit=uiprodprofit[,-ncol(uiprodprofit)]

ciprodprofit
### optimizaciones con productos separados

eps=-1e-6
rhs=uiprodprofit%*%t(tablaprecio[156,2:16])
rhs
rhsresult=data.frame(rhs,ciprodprofit,rhs>ciprodprofit)
rhsresult

### ols prod
p_prod <- constrOptim(t(tablaprecio[156,2:16]), f_demand3, NULL, ui=uiprodprofit, ci=ciprodprofit, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)


### hbm prod todo
p_hbm <- constrOptim(t(tablaprecio[156,2:16]), f_demandhbm, NULL, ui=uiprodprofit, ci=ciprodprofit, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)


### lasso prod todo
p_lasso <- constrOptim(t(tablaprecio[156,2:16]), f_demandlasso, NULL, ui=uiprodprofit, ci=ciprodprofit, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

### lasso 2
p_lasso <- constrOptim(t(tablaprecio[155,2:16]), f_demandlasso2, NULL, ui=uiprodprofit, ci=ciprodprofit, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
nprod

p_lasso$par
uiprodprofit
selrules2


### optimizaciones con pares de productos

profitpar=cbind.data.frame(difprecios, profit= profit$historeal,dummy=(profit$historeal>mean(profit$historeal))*1)
#profitprod[,k]>0.09,
#mean(profitprod[profitprod[,k]>0.09,"profit"])

# Split into training and testing
N=nrow(profitpar)
N_train <- N*0.8
N_test <- N*0.2
train_ind2 <- sample(c(1:N), size = N_train, replace = FALSE)
profit2_train <- profitpar[train_ind2,]
profit2_test <- profitpar[-train_ind2,]

j=1
k=1
profitpar2=data.frame(matrix(0,nrow=5*ncol(difprecios),ncol=20))
colnames(profitpar2)=c("prod","cut","prfi>c","prfi<c","conf","sup_r_prf","lift pi>c","liftpi<c","c_prom","Sup_rule","Sup_invrule","sup_inv_r_prf","CONFINV","SUP_Prf","LIFT2","LIFT2INV","Pr_train","Pr_test","Pr_train-1","Pr_test-1")
for (k in 1:ncol(difprecios)) {
  temp=as.matrix(profitpar[,k])
  for (i in c(0.05,0.25,0.5,0.75,0.95) ) {
    temp2=as.numeric(quantile(temp,i))
    profitpar2[j,1]=colnames(difprecios[k])
    profitpar2[j,2]=i
    profitpar2[j,3]=mean(profitpar[profitpar[,k]>temp2,"profit"])
    profitpar2[j,4]=mean(profitpar[profitpar[,k]<=temp2,"profit"])
    profitpar2[j,5]=mean(profitpar[profitpar[,k]>temp2,"dummy"]) #conf
    profitpar2[j,6]=sum(profitpar[profitpar[,k]>temp2,"dummy"]) #support rule and profit
    profitpar2[j,7]=profitpar2[j,3]/mean(rulesprofit$profit)
    profitpar2[j,8]=profitpar2[j,4]/mean(rulesprofit$profit)
    profitpar2[j,9]=temp2
    profitpar2[j,10]=nrow(profitpar[profitpar[,k]>temp2,]) #support pricing rule
    profitpar2[j,11]=nrow(profitpar[profitpar[,k]<=temp2,])
    profitpar2[j,12]=sum(profitpar[profitpar[,k]<=temp2,"dummy"]) # sup profit y no rule
    #profitpar2[j,12]=profitpar2[j,5]/profitpar2[j,9] #conf
    profitpar2[j,13]=mean(profitpar[profitpar[,k]<=temp2,"dummy"])#confinv
    profitpar2[j,14]=sum(profitpar[,"dummy"])/nrow(profitpar) #suppi
    profitpar2[j,15]=profitpar2[j,5]/profitpar2[j,14] #LIFT
    profitpar2[j,16]=profitpar2[j,13]/profitpar2[j,14] #LIFT2
    profitpar2[j,17]=mean(profit2_train[profit2_train[,k]>temp2,"profit"]) #profit train
    profitpar2[j,18]=mean(profit2_test[profit2_test[,k]>temp2,"profit"]) #profit test
    profitpar2[j,19]=mean(profit2_train[profit2_train[,k]<=temp2,"profit"]) #profit regla inversa train
    profitpar2[j,20]=mean(profit2_test[profit2_test[,k]<=temp2,"profit"]) #profit regla inversa test
    # rprofit[j,5]=nrow(rulesprofit[rulesprofit$prom>as.numeric(quantile(rulesprofit$prom,i))])/156
    # rprofit[j,6]=nrow(rulesprofit[rulesprofit$prom<as.numeric(quantile(rulesprofit$prom,i))])/156
    j=j+1
  }
}
str(profitpar2)
#profitpar2[profitpar2$cut<0.8,]


#profitpar2=as.data.frame.table(profitpar2)
selpar=profitpar2[1,]
parlist=unique(profitpar2$prod)
for (i in parlist) {
  temp=profitpar2[profitpar2$prod==i&profitpar2$cut<0.6,]
  temp2=max(temp$`lift pi>c`,na.rm=TRUE)
  #temp3=temp[`lift pi>c`==temp2,]
  temp3= temp[temp$`lift pi>c`==temp2,]
  selpar=rbind(selpar,temp3[1,])
}

selpar=selpar[complete.cases(selpar),]


selpar2=profitpar2[1,]
for (i in parlist) {
  temp=profitpar2[profitpar2$prod==i&profitpar2$cut>0.4,]
  temp2=max(temp$`liftpi<c`,na.rm=TRUE)
  #temp3=temp[`lift pi>c`==temp2,]
  temp3= temp[temp$`liftpi<c`==temp2,]
  selpar2=rbind(selpar2,temp3[1,])
}
selpar2=selpar2[-1,]
nrow(selpar2)
selpar2=selpar2[complete.cases(selpar2),]
nrow(selpar2)


# library(xlsx)
# write.xlsx(selpar,file="selrules1.xlsx")
# write.xlsx(selpar2,file="selrules2.xlsx")



cor_prices=cor(tablaprecio[,-1])

temp=do.call(rbind.data.frame, strsplit(selpar2$prod1,' - '))
colnames(temp)=c("p1","p2")

selpar2=cbind.data.frame(temp,selpar2)

temp=do.call(rbind.data.frame, strsplit(selpar$prod1,' - '))
colnames(temp)=c("p1","p2")
selpar=cbind.data.frame(temp,selpar)
nprod
uiprofitpar=matrix(0,nrow(selpar),ncol=nprod)
colnames(uiprofitpar)=prodlist
#prodlist2=cbind(prodlist,seq=seq(1:15))
for (j in 1:nrow(uiprofitpar))  {
  for (k in 1:nprod)  {
    temp1=selpar[j,1]
    temp2=selpar[j,2]
    if(temp1==prodlist[k] ) {uiprofitpar[j,k]=1 }
    if(temp2==prodlist[k] ) {uiprofitpar[j,k]=-1 }
  }  
} 
uiprofitpar2=matrix(0,nrow(selpar2),ncol=nprod)
colnames(uiprofitpar2)=prodlist
for (j in 1:nrow(uiprofitpar2))  {
  for (k in 1:nprod)  {
    temp1=selpar2[j,1]
    temp2=selpar2[j,2]
    if(temp1==prodlist[k] ) {uiprofitpar2[j,k]=-1 }
    if(temp2==prodlist[k] ) {uiprofitpar2[j,k]=1 }
  }  
} 
temp1=cbind.data.frame(uiprofitpar,selpar[,c("lift pi>c","c_prom")])
temp2=cbind.data.frame(uiprofitpar2,selpar2$`liftpi<c`,-selpar2$c_prom)
dim(temp1)
dim(temp2)
uiprofitpartotal=rbind.data.frame(temp1[temp1$`lift pi>c`>1.02,],temp2[temp2$`liftpi<c`>1.02,])
uiprofitpartotal=uiprofitpartotal[,-16]

uiprofitparfinal=as.matrix(uiprofitpartotal[,1:15])

ciprofitparfinal=uiprofitpartotal[,16]+eps
str(uiprofitparfinal)
### optimizaciones con 

eps=-1e-4
rhs=uiprofitparfinal%*%t(tablaprecio[150,2:16])
rhs
#rhsresult=data.frame(uiprofitparfinal, rhs,ciprofitparfinal,rhs>ciprofitparfinal)
rhsresult=data.frame(rhs,ciprofitparfinal,comp=rhs>ciprofitparfinal)
rhsresult
#precioini=tablaprecio[155,2:16]
temp=cbind.data.frame(i,sum(rhsresult$X150.1))
temp
check=rbind.data.frame(check,temp)
temp=1
i=1
check=rbind(check,temp)
nweeks=156
check=c(0,0)
for (k in 1:nweeks)  {
  rhs=uiprofitparfinal%*%t(tablaprecio[i,2:16])
  #rhs
  #rhsresult=data.frame(uiprofitparfinal, rhs,ciprofitparfinal,rhs>ciprofitparfinal)
  rhsresult=data.frame(rhs,rhs>ciprofitparfinal)
  colnames(rhsresult)=c("rhs","check")
  temp=cbind.data.frame(i,sum(rhsresult$check))
  check=rbind(check,temp)
  i=i+1
}
check
precioini=t(tablaprecio[142,2:16])
rhs=uiprofitparfinal%*%precioini
#rhs
#rhsresult=data.frame(uiprofitparfinal, rhs,ciprofitparfinal,rhs>ciprofitparfinal)
rhsresult=data.frame(rhs,rhs>ciprofitparfinal)
colnames(rhsresult)=c("rhs","check")
rhsresult
rhsresult[check==FALSE,]
temp=cbind.data.frame(i,sum(rhsresult$check))


# precioini[,8]=precioini[,8]+0.17
# precioini[,3]=precioini[,3]+0.01
# dim(uiprofitparfinal2)
# ciprofitparfinal2[84]=ciprofitparfinal2[84]-0.0007
# 
# ciprofit
# rhs=uiprofitparfinal2%*%t(precioini)
# rhs
# rhsresult=data.frame(uiprofitparfinal2, rhs,ciprofitparfinal2,rhs>ciprofitparfinal2)
# rhsresult[rhsresult$X155.1==FALSE,] 
# write.csv(rhsresult[rhsresult$X155.1==FALSE,] , file = "rhsresult.csv")
# 
# ### programming feasible solutions
# #aux=diag(1,nrow=nrow(uiprofitparfinal),ncol=nrow(uiprofitparfinal))
# aux=rep(1,nrow(uiprofitparfinal))
# b=diag(1,nrow=length(aux),ncol=length(aux))
# str(b)
# str(aux)
# sum(aux)
# sum(solini)
# sum()
# sum(solini[-(1:15)])
# dim(solini)
# solini
# f_sumaux <- function(solini) {
#   demand <- (sum(solini[-(1:15)]))
#   return(demand)
# }
# uiaux=cbind(uiprofitparfinal,b)
# uiaux=rbind(uiaux,-uiaux)
# dim(uiaux)
# dim(b)
# dim(uiprofitparfinal)
# length(ciprofitparfinal)
# temp=matrix(0,nrow=nrow(uiprofitparfinal),ncol=ncol(uiprofitparfinal))
# temp=cbind(temp,b)
# uiaux=rbind(uiaux,temp)
# dim(uiaux)
# ciaux=as.matrix(data.frame(ciprofitparfinal))
# temp=as.matrix(rep(-0.2,nrow(uiprofitparfinal)))
# ciaux=as.vector(rbind(ciaux,-ciaux,temp))
# dim(temp)
# dim(uiaux)
# length(ciaux)
# str(ciprofitparfinal)
# str(ciprofit)
# eps
# solini=rbind(as.matrix(rep(0,nprod)),as.matrix(data.frame(ciprofitparfinal)))
# dim(solini)
# 
# 
# rhs=uiprofitparfinal%*%t(tablaprecio[155,2:16])
# rhs
# rhsresult=data.frame(uiprofitparfinal, rhs,ciprofitparfinal,rhs>ciprofitparfinal)
# rhsresult
# 
# dim(solini)
# rhs=uiaux%*%solini
# rhs
# rhsresult=data.frame(rhs,ciaux,rhs>ciaux+eps)
# rhsresult
# min(ciaux)
# ### phase 1
# source("myconstrOptim.r")
# p_factible <- constrOptim(solini, f_sumaux, NULL, ui=uiaux, ci=ciaux+eps)
# #p_factible <- constrOptim(t(solini), f_sumaux, NULL, ui=uiaux, ci=ciaux)
# solini
# p_factible$par - solini
# p_factible$val[1:15]
# p_factible$val
# mean(p_factible$par)
# 
# 
# #p2=optim(solini, f_sumaux, NULL, method="L-BFGS-B",)
# p2$par
# 

### soluciones para pares profit
uiprofitparfinal
ciprofitparfinal

### ols prod
p_prod <- constrOptim(precioini, f_demand3, NULL, ui=uiprofitparfinal, ci=ciprofitparfinal, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)


### hbm prod todo
p_hbm <- constrOptim(precioini, f_demandhbm, NULL, ui=uiprofitparfinal, ci=ciprofitparfinal, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par


### lasso prod todo
p_lasso <- constrOptim(precioini, f_demandlasso, NULL, ui=uiprofitparfinal, ci=ciprofitparfinal, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(precioini, f_demandlasso2, NULL, ui=uiprofitparfinal, ci=ciprofitparfinal, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
dim(uiprofitparfinal)

## pegando set de restricciones de productos al 5% y 95%
# #uiprod
# #ciprod3

uiprofitparfinal2=rbind(uiprofitparfinal,uiprod)
ciprofitparfinal2=rbind(as.matrix(ciprofitparfinal),as.matrix(ciprod3))
dim(uiprofitparfinal)
ciprofitparfinal2
dim(uiprofitparfinal2)
precioini=t(precioini)
### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uiprofitparfinal2, ci=ciprofitparfinal2, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)


### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uiprofitparfinal2, ci=ciprofitparfinal2, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par


### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uiprofitparfinal2, ci=ciprofitparfinal2, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uiprofitparfinal2, ci=ciprofitparfinal2, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

p_lasso$par
nprod

p_lasso$par
uiprodprofit
selrules2

##### creando restricciones total todas profit mas caja 5-95 de precios indiv. uiprofitparfinal2 + uiprom[1] ciprofit2 
#uiprodprofit ciprodprofit
dim(uiprodprofit)
uitotal=rbind(uiprofitparfinal2,uiprom[1],uiprodprofit)
citotal=rbind(ciprofitparfinal2,as.matrix(ciprofit2),as.matrix(ciprodprofit))
dim(citotal)
dim(uitotal)

rhs=uitotal%*%t(precioini)
rhs
rhsresult=data.frame(uitotal,rhs,citotal,rhs>citotal+eps)
rhsresult[rhsresult$X142.1==FALSE,]
colnames(rhsresult)=c("rhs","check")

rhsresult[rhsresult$X142.1==FALSE,]
dim(precioini)
precioini[1,7]=precioini[1,7]+0.005
precioini[1,13]=precioini[1,13]-0.02
#ciprofit2=ciprofit2-0.01

### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par


### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

### 3ER CORRIDA.AGREGAR REGLAS 5-95 DE PARES Y PROMEDIO
### uiprom ciprom2
### uiparf ciparf2

dim(uiparf)
dim(uitotal)

uitotal2=rbind(uitotal,uiprom,uiparf)
citotal2=rbind(citotal,as.matrix(ciprom2),as.matrix(ciparf2))
dim(uitotal2)
dim(citotal2)

rhs=uitotal2%*%t(precioini)
rhs
rhsresult=data.frame(uitotal2, rhs,citotal2,check=rhs>citotal2+eps)
rhsresult=rhsresult[rhsresult$X155.1==FALSE,]


### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uitotal2, ci=citotal2, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par


### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uitotal, ci=citotal, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

### scenario 3. todas las reglas de orden juntas.
## uiprom ciprom2
## uiprod ciprod3
### uiparf ciparf2

uiesc3=rbind(uiprom,uiprod,uiparf)
ciesc3=rbind(as.matrix(ciprom2),as.matrix(ciprod3),as.matrix(ciparf2))

rhs=uiesc3%*%t(tablaprecio[1,2:16])
rhs
rhsresult=data.frame(uiesc3, rhs,ciesc3,check=rhs>ciesc3+eps)
rhsresult=rhsresult[rhsresult$X142.1==FALSE,]
rhsresult
dim(rhsresult)

### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uiesc3, ci=ciesc3, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(tablaprecio[1,2:16]), f_demandhbm, NULL, ui=uiesc3, ci=ciesc3, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(t(tablaprecio[1,2:16]), f_demandlasso, NULL, ui=uiesc3, ci=ciesc3, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(tablaprecio[1,2:16]), f_demandlasso2, NULL, ui=uiesc3, ci=ciesc3, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

### scenario 3B. todas las reglas de orden juntas pero de rango completo.
## uiprom ciprom
## uiprod ciprod2
### uiparf ciparf2

uiesc3=rbind(uiprom,uiprod,uiparf)
ciesc3b=rbind(as.matrix(ciprom),as.matrix(ciprod2),as.matrix(ciparf))

rhs=uiesc3%*%t(tablaprecio[1,2:16])
rhs
rhsresult=data.frame(uiesc3, rhs,ciesc3,check=rhs>ciesc3+eps)
rhsresult=rhsresult[rhsresult$X142.1==FALSE,]
rhsresult
dim(rhsresult)

### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uiesc3, ci=ciesc3b, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uiesc3, ci=ciesc3b, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uiesc3, ci=ciesc3b, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uiesc3, ci=ciesc3b, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

dim(uiesc3)

### scenario 4. todas las reglas de orden juntas. + profit individual prices
## uiprom ciprom2
## uiprod ciprod3
### uiparf ciparf2
### uiprodprofit ciprodprofit

uiesc4=rbind(uiesc3,uiprodprofit)
ciesc4=rbind(as.matrix(ciesc3),as.matrix(ciprodprofit))
dim(uiesc4)
demo=f_optim(uiesc4,ciesc4,niter)

tabusearch=matrix(0,nrow(tablaprecio),2)
for (i in 1:nrow(tablaprecio)){

rhs=uiesc4%*%t(tablaprecio[i,2:16])
rhs
rhsresult=data.frame(uiesc4, rhs,ciesc4,check=rhs>ciesc4)
rhsresult
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
tabusearch[i,]=c(i,nrow(rhsresult))

}
tabusearch

rhs=uiesc4%*%precio2
rhs
rhsresult=data.frame(uiesc4, rhs,ciesc4,check=rhs>ciesc4)
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
rhsresult

eps=-1e-3
ciesc4=ciesc4+eps

precio2=t(tablaprecio[99,2:16])
dim(precio2)
precio2[7,1]=precio2[7,1]+0.013

dim(uiesc4)
### ols prod
p_prod <- constrOptim(precio2, f_demand3, NULL, ui=uiesc4, ci=ciesc4, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(precio2, f_demandhbm, NULL, ui=uiesc4, ci=ciesc4, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(precio2, f_demandlasso, NULL, ui=uiesc4, ci=ciesc4, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(precio2, f_demandlasso2, NULL, ui=uiesc4, ci=ciesc4, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

### scenario 4b. todas las reglas de orden rango juntas. + profit individual prices
## uiprom ciprom2
## uiprod ciprod3
### uiparf ciparf2
### uiprodprofit ciprodprofit

uiesc4=rbind(uiesc3,uiprodprofit)
ciesc4b=rbind(as.matrix(ciesc3b),as.matrix(ciprodprofit))

demo=f_optim(uiesc4,ciesc4b,niter)

tabusearch=matrix(0,nrow(tablaprecio),2)
for (i in 1:nrow(tablaprecio)){
  
  rhs=uiesc4%*%t(tablaprecio[i,2:16])
  rhs
  rhsresult=data.frame(uiesc4, rhs,ciesc4b,check=rhs>ciesc4b)
  rhsresult
  colnames(rhsresult)[18]="check"
  rhsresult=rhsresult[rhsresult$check==FALSE,]
  tabusearch[i,]=c(i,nrow(rhsresult))
  
}
tabusearch

rhs=uiesc4%*%precio2
rhs
rhsresult=data.frame(uiesc4, rhs,ciesc4,check=rhs>ciesc4)
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
rhsresult

eps=-1e-3
ciesc4=ciesc4+eps

precio2=t(tablaprecio[99,2:16])
dim(precio2)
precio2[7,1]=precio2[7,1]+0.013

dim(uiesc4)
### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uiesc4, ci=ciesc4b, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uiesc4, ci=ciesc4b, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uiesc4, ci=ciesc4b, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uiesc4, ci=ciesc4b, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)



### scenario 5. esc 4 + profit par + profit prom
## uiesc4 ciesc4
## ui=uiprom[1,], ci=ciprofit[1]
### uiprofitparfinal ciprofitparfinal


uiesc5=rbind(uiesc4,uiprom[1,],uiprofitparfinal)
ciesc5=rbind(as.matrix(ciesc4),as.matrix(ciprofit[1]),as.matrix(ciprofitparfinal))
dim(uiesc5)
demo=f_optim(uiesc5,ciesc5,niter*2)

tabusearch=matrix(0,nrow(tablaprecio),2)
for (i in 1:nrow(tablaprecio)){
  
  rhs=uiesc5%*%t(tablaprecio[i,2:16])
  rhs
  rhsresult=data.frame(uiesc5, rhs,ciesc5,check=rhs>ciesc5)
  rhsresult
  colnames(rhsresult)[18]="check"
  rhsresult=rhsresult[rhsresult$check==FALSE,]
  tabusearch[i,]=c(i,nrow(rhsresult))
  
}
tabusearch

rhs=uiesc5%*%t(tablaprecio[154,2:16])
rhs
rhsresult=data.frame(uiesc5, rhs,ciesc5,check=rhs>ciesc5)
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
rhsresult

rhs=uiesc5%*%precio3
rhs
rhsresult=data.frame(uiesc5, rhs,ciesc5,check=rhs>ciesc5)
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
rhsresult

eps=-1e-3
ciesc4=ciesc4+eps

precio3=t(tablaprecio[154,2:16])
precio3=precio3+0.00001
dim(precio3)
precio3[9,1]=precio3[9,1]+0.01
precio3=precio3+0.001

dim(ciesc5)
ciesc5[2,1]=ciesc5[2,1]-0.01
dim(uiesc4)
### ols prod
p_prod <- constrOptim(precio3, f_demand3, NULL, ui=uiesc5, ci=ciesc5, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(p_prod$par, f_demandhbm, NULL, ui=uiesc5, ci=ciesc5, control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(precio3, f_demandlasso, NULL, ui=uiesc5, ci=ciesc5, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(precio3, f_demandlasso2, NULL, ui=uiesc5, ci=ciesc5, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

dim(uiesc5)


### scenario 5b. esc 4b + profit par + profit prom
## uiesc4 ciesc4
## ui=uiprom[1,], ci=ciprofit[1]
### uiprofitparfinal ciprofitparfinal


uiesc5=rbind(uiesc4,uiprom[1,],uiprofitparfinal)
ciesc5b=rbind(as.matrix(ciesc4b),as.matrix(ciprofit[1]),as.matrix(ciprofitparfinal))
dim(uiesc5)
demo=f_optim(uiesc5,ciesc5b,niter)

tabusearch=matrix(0,nrow(tablaprecio),2)
for (i in 1:nrow(tablaprecio)){
  
  rhs=uiesc5%*%t(tablaprecio[i,2:16])
  rhs
  rhsresult=data.frame(uiesc5, rhs,ciesc5,check=rhs>ciesc5)
  rhsresult
  colnames(rhsresult)[18]="check"
  rhsresult=rhsresult[rhsresult$check==FALSE,]
  tabusearch[i,]=c(i,nrow(rhsresult))
  
}
tabusearch

rhs=uiesc5%*%t(tablaprecio[154,2:16])
rhs
rhsresult=data.frame(uiesc5, rhs,ciesc5,check=rhs>ciesc5)
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
rhsresult

rhs=uiesc5%*%precio3
rhs
rhsresult=data.frame(uiesc5, rhs,ciesc5,check=rhs>ciesc5)
colnames(rhsresult)[18]="check"
rhsresult=rhsresult[rhsresult$check==FALSE,]
rhsresult

eps=-1e-3
ciesc4=ciesc4+eps

precio3=t(tablaprecio[154,2:16])
precio3=precio3+0.00001
dim(precio3)
precio3[9,1]=precio3[9,1]+0.01
precio3=precio3+0.001

dim(ciesc5)
ciesc5[2,1]=ciesc5[2,1]-0.01
dim(uiesc4)
### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uiesc5, ci=ciesc5b, control=list(fnscale=-1))
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uiesc5, ci=ciesc5b,control=list(fnscale=-1))
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uiesc5, ci=ciesc5b, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uiesc5, ci=ciesc5b, control=list(fnscale=-1))
p_lasso$val
mean(p_lasso$par)

### scenario 5c. Minimizing PRofit
## uiesc4 ciesc4
## ui=uiprom[1,], ci=ciprofit[1]
### uiprofitparfinal ciprofitparfinal

### ols prod
p_prod <- constrOptim(t(precioini), f_demand3, NULL, ui=uiesc5, ci=ciesc5b)
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(precioini), f_demandhbm, NULL, ui=uiesc5, ci=ciesc5b)
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(t(precioini), f_demandlasso, NULL, ui=uiesc5, ci=ciesc5b)
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(precioini), f_demandlasso2, NULL, ui=uiesc5, ci=ciesc5b)
p_lasso$val
mean(p_lasso$par)


### scenario 3c. Minimizing PRofit all order rules
## uiprom ciprom2
## uiprod ciprod3
### uiparf ciparf2

uiesc3=rbind(uiprom,uiprod,uiparf)
ciesc3=rbind(as.matrix(ciprom2),as.matrix(ciprod3),as.matrix(ciparf2))

rhs=uiesc3%*%t(tablaprecio[1,2:16])
rhs
rhsresult=data.frame(uiesc3, rhs,ciesc3,check=rhs>ciesc3+eps)
rhsresult=rhsresult[rhsresult$X142.1==FALSE,]
rhsresult
dim(rhsresult)

### ols prod
p_prod <- constrOptim(t(tablaprecio[1,2:16]), f_demand3, NULL, ui=uiesc3, ci=ciesc3)
p_prod$val
mean(p_prod$par)

### hbm prod todo
p_hbm <- constrOptim(t(tablaprecio[1,2:16]), f_demandhbm, NULL, ui=uiesc3, ci=ciesc3)
p_hbm$val
mean(p_hbm$par)
p_hbm$par

### lasso prod todo
p_lasso <- constrOptim(t(tablaprecio[1,2:16]), f_demandlasso, NULL, ui=uiesc3, ci=ciesc3)
p_lasso$val
mean(p_lasso$par)
p_lasso$par
### lasso 2
p_lasso <- constrOptim(t(tablaprecio[1,2:16]), f_demandlasso2, NULL, ui=uiesc3, ci=ciesc3)
p_lasso$val
mean(p_lasso$par)

