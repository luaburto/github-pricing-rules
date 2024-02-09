rm(list=ls(all=TRUE)) 
library(readxl)
library(sqldf)
library("RSQLite", lib.loc="~/R/win-library/3.4")
library(lubridate)
library(data.table)
library("ggplot2")
library(arules)
library("arulesViz")
library("xlsx")
library("limSolve", lib.loc="~/R/win-library/3.5")

load("C:/Users/luabu/Dropbox/2018 - Pricing Rules/datasets/dataset.RData")

prod <- read_excel("C:/Users/luabu/Dropbox/2018 - Pricing Rules/datasets/dunnhumby - Breakfast at the Frat.xlsx",sheet = "dh Products Lookup")

#### equipo ISCI
setwd("C:/Users/luabu_000/Dropbox (Consumer Analytics)/2018 - Pricing Rules/datasets")




trx <- read_excel("C:/Users/luabu_000/Dropbox (Consumer Analytics)/2018 - Pricing Rules/datasets/dunnhumby - Breakfast at the Frat.xlsx", 
                  sheet = "dh Transaction Data", col_types = c("date", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "numeric", "numeric"))
prod <- read_excel("C:/Users/luabu_000/Dropbox (Consumer Analytics)/2018 - Pricing Rules/datasets/dunnhumby - Breakfast at the Frat.xlsx", 
                  sheet = "dh Products Lookup")

#### equipo NOTEBOOK LUABURTO
load("C:/Users/luabu/Dropbox/2018 - Pricing Rules/datasets/dataset.RData")
setwd("C:/Users/luabu/Dropbox/2018 - Pricing Rules/datasets")
trx <- read_excel("C:/Users/luabu/Dropbox/2018 - Pricing Rules/datasets/dunnhumby - Breakfast at the Frat.xlsx", 
                   sheet = "dh Transaction Data", col_types = c("date", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric", "numeric", 
                                                                "numeric", "numeric"))
prod <- read_excel("C:/Users/luabu/Dropbox/2018 - Pricing Rules/datasets/dunnhumby - Breakfast at the Frat.xlsx", 
                    sheet = "dh Products Lookup")
#########

#calculo de precio normalizado por formato
tablon <- merge(prod, trx, by = c("UPC"))
#head(tablon)
tablon$PRECIONORM=tablon$PRICE/tablon$SIZE
subcategories <- sqldf("select count(distinct UPC) , SUB_CATEGORY from tablon group by SUB_CATEGORY")
subcategories
categories <- sqldf("select count(distinct UPC) , CATEGORY from tablon group by CATEGORY")
categories
sizes <- sqldf("select DISTINCT size from tablon")
sizes
grupy <- sqldf("select avg(price) as avgprice, count(price) , stdev(price), size from tablon where CATEGORY='BAG SNACKS' group by size order by size desc")
grupy
story <- sqldf("select avg(price) as avgprice, count(price) AS REG , stdev(price), STORE_NUM from tablon where CATEGORY='BAG SNACKS' group by STORE_NUM order by REG desc")
story
#store.sel=story[story[,2==max(story$REG)],]$REG
store.sel=sqldf(sprintf("select STORE_NUM from story where REG = '%s'",max(story$REG) ))


store=sqldf("select DISTINCT STORE_NUM from tablon ORDER BY STORE_NUM")
store=unique(tablon$STORE_NUM)

totalweeks=sqldf("select count(distinct WEEK_END_DATE) as totalweeks, UPC from tablon group by UPC")


#elijo solo tuplas que tengan igual fabricante o igual categoria
proddoble=merge(prod,prod,by=NULL)
proddoble2<-subset(proddoble,UPC.x!=UPC.y)
proddoble3<-subset(proddoble2,MANUFACTURER.x==MANUFACTURER.y | CATEGORY.x==CATEGORY.y)
proddoble4<-subset(proddoble3,SUB_CATEGORY.x=='PRETZELS')
final=proddoble4[c("UPC.x","UPC.y","DESCRIPTION.x","DESCRIPTION.y","MANUFACTURER.x","MANUFACTURER.y","CATEGORY.x","CATEGORY.y")]

#FILTRO DE TRANSACCIONAL Y PROD SOLO PARA LA CATEGORIA ELEGIDA DE BAG SNACKS Y STORE=STORE.SEL
#tablon3=sqldf(sprintf("select * from tablon where CATEGORY='BAG SNACKS' AND STORE_NUM='%s'",store.sel))
tablon3=sqldf(sprintf("select * from tablon"))

prod2=subset(prod,SUB_CATEGORY=='PRETZELS')
#rm(ejemplo, proddoble,proddoble2, proddoble3, tablon, trx, join )
timemax=nrow(sqldf("select DISTINCT WEEK_END_DATE from tablon3 ORDER BY WEEK_END_DATE"))
timex=sqldf("select DISTINCT WEEK_END_DATE from tablon3 ORDER BY WEEK_END_DATE")
#timey=as.Date.POSIXct(tablon3[,3])

df <- data.frame(Date = timex)
#as.Date(df$Date, "%Y/%m/%d %H:%M:%S")
df$WEEK_END_DATE
df$WEEK_END_DATE<-gsub("/","-",df$WEEK_END_DATE)
parse_date_time(df$WEEK_END_DATE, orders="ymd HMS")
df$WEEK_END_DATE

tablon$WEEK_END_DATE<-as.character(tablon$WEEK_END_DATE)
tablon3$WEEK_END_DATE<-as.character(tablon3$WEEK_END_DATE)
tablon$WEEK_END_DATE<-as.character(tablon$WEEK_END_DATE)

# 
# tablonx[vary,]<-c(prod2[i,1],prod2[j,1],as.Date((as.POSIXct(timex[t,1]))),0,0)
# tablonx[vary,3]<-as.Date((timex[t,1]),origin = '1970-01-01')
# typeof(timex[t,1])
# tablonx[vary,]
# tablonx[vary,]
# timex[t,1]
i=1
j=1
t=1
vary=1
tablony=matrix(c(0, 0, 0, 0, 0),nrow=nrow(prod2)*nrow(prod2)*timemax, ncol=5)
tablonx=as.data.frame(tablony, col_types = c("numeric", "numeric","chr", "numeric", "numeric"))

# tablonx[vary,4]=sqldf(paste0("select PRECIONORM from tablon where UPC=",tablonx[vary,1]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE=",tablonx[vary,3]))
# sqldf(paste0("select MAX(PRECIONORM) from tablon where UPC=",tablonx[vary,1]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tablonx[vary,3],"'"))
# sqldf(paste0("select MAX(PRECIONORM) from tablon where UPC=",1111009477," AND STORE_NUM=",2277))
# sqldf(paste0("select MAX(PRECIONORM) from tablon where UPC=",tablon[vary,1]," AND STORE_NUM=",2277," AND WEEK_END_DATE='",2009-01-14,"'"))
# sqldf(paste0("select MAX(PRECIONORM) from tablon where UPC=",tablon[vary,1]," AND STORE_NUM=",2277))
# sqldf(paste0("select MAX(PRECIONORM) from tablon3 where WEEK_END_DATE=",str))

str='2009-01-13 21:00:00'
#sqldf(paste0("select MAX(PRECIONORM) from tablon3 where UPC=",tablon[vary,1]," AND STORE_NUM=",2277," AND WEEK_END_DATE='",str,"'"))

clon=as.data.table(tablon3)

as.Date(str)
sql <- sprintf("select * from tablon3 where WEEK_END_DATE = %d", as.Date(str))
sqldf(sql)
str(tablon3)
#sqldf("select * from tablon3 where WEEK_END_DATE = '2009-01-13 21:00:00'",method = 'name__class')
tablon$WEEK_END_DATE<-as.character(tablon$WEEK_END_DATE)
as.character(timex)
timex
for (i in 1:nrow(prod2) ) {
  for (j in 1:nrow(prod2) ) {
    for (t in 1:nrow(df )) {
    tablonx[vary,]<-c(as.numeric(prod2[i,1]),as.numeric(prod2[j,1]),df$WEEK_END_DATE[t],0,0)
    tablonx[vary,4]=sqldf(paste0("select MAX(PRECIONORM) from tablon3 where UPC=",tablonx[vary,1]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tablonx[vary,3],"'"))
    tablonx[vary,5]=sqldf(paste0("select MAX(PRECIONORM) from tablon3 where UPC=",tablonx[vary,2]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tablonx[vary,3],"'"))
    vary<-vary+1
    print(vary)
}
}
}
save.image("C:/Users/luabu_000/Dropbox (Consumer Analytics)/2018 - Pricing Rules/datasets/dataset.RData")
save.image("C:/Users/Asus/Dropbox (Consumer Analytics)/2018 - Pricing Rules/datasets/dataset.RData")
tabla10=as.data.table(matrix(nrow=timemax,ncol=nrow(final)+2))
tienda=2277
sku=1111009477
tabla10[,1]=timex
tabla10[,2]=sqldf("select sum(SPEND) from tablon3 where SUB_CATEGORY='PRETZELS' AND STORE_NUM=2277 group by WEEK_END_DATE")
for (i in 1:nrow(tabla10) ) {
  for (j in 3:ncol(tabla10) ) {
#    price1=sqldf(paste0("select PRECIONORM from tablon3 where UPC=",final[j-2,1]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tabla10[i,1],"'"))
#    price2=sqldf(paste0("select PRECIONORM from tablon3 where UPC=",final[j-2,2]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tabla10[i,1],"'"))
    
    price1=clon[UPC==final[j-2,1] & STORE_NUM==tienda & WEEK_END_DATE==as.character(tabla10[i,1]),PRECIONORM]
    price2=clon[UPC==final[j-2,2] & STORE_NUM==tienda & WEEK_END_DATE==as.character(tabla10[i,1]),PRECIONORM]
        print(i)
    print(j)
    print(price1)
    print(price2)
    if (length(price1)+length(price2)>1){
    if(price1>price2) tabla10[i,j] <- TRUE else tabla10[i,j]= FALSE
    }
    }
  }

names(tabla10)[1]<-"WEEK_END_DATE"
names(tabla10)[2]<-"SPEND"

for (i in 3:ncol(tabla10) ) {
  names(tabla10)[i] <- paste0(final[i-2,1]," > ",final[i-2,2])
}

#reporte sin reglas por semana
totalNA=sum(is.na(tabla10))
num_NAcol=as.data.table(colSums(is.na(tabla10)))
num_NArow =as.data.table(cbind(timex,tabla10[, rowSums(is.na(tabla10))]))


# clon[paste0("UPC==1111009477 & STORE_NUM==",store.sel)]
# paste0("clon[UPC==1111009477 & STORE_NUM==",store.sel,"]")
# clon[UPC==1111009477 & STORE_NUM==2277]
# clon[UPC==sku & STORE_NUM==tienda, PRECIONORM]
# clon[UPC==final[1,1], max(PRECIONORM)]
# str(store.sel)
# str(sku)
# 
# clon[paste0("UPC==1111009477 & STORE_NUM==",tienda)]
# i=1
# j=3
# price1=clon[UPC==final[13-2,1] & STORE_NUM==tienda & WEEK_END_DATE==as.character(tabla10[7,1]),PRECIONORM]
# price2=clon[UPC==final[18-2,1] & STORE_NUM==tienda & WEEK_END_DATE==as.character(tabla10[7,1]),PRECIONORM]

analytics=tabla10[,-1]
analytics$SPEND=scale(analytics$SPEND)
attach(analytics)
model=rpart(SPEND~.,data=analytics,control = rpart.control(minsplit = 40))
rpart.plot(model)
library(RColorBrewer)
library(rattle)
library(rpart)
library(rpart.plot)
fancyRpartPlot(model, cex = 0.5)

spend_avg=mean(analytics$SPEND)
analytics2=analytics
#analytics2[SPEND>spend_avg,1]$spendavg=1
#analytics[,,mean(SPEND)]$spendavg

#DUMMY PARA VENTA MAYOR QUE EL PROMEDIO
for (i in 1:nrow(analytics2) ) {
  if(analytics2[i,SPEND] > spend_avg) analytics2[i,'spenddummy']=TRUE else
    analytics2[i,'spenddummy']=FALSE
}
analytics3=analytics2[,-1]
str2=''
############# AQUI QUEDE CON LAS REGLAS
### con nombres largos
for (i in 1:nrow(analytics3) ) {
str2[i]=''  
#str2[i]=paste("semana",i,",")
for (j in 1:ncol(analytics3) ) {
  if(is.na(analytics3[i,j,with=FALSE])==FALSE)
  if(analytics3[i,j,with=FALSE]==TRUE) str2[i]<-paste(str2[i]," ",names(analytics3)[j]," , ")
}
}
#analytics5=as.data.frame(str2)
### con nombres cortos

for (i in 1:nrow(analytics3) ) {
  str2[i]=''  
  #str2[i]=paste("semana",i,",")
  for (j in 1:ncol(analytics3) ) {
    if(analytics3[i,j,with=FALSE]==TRUE) str2[i]<-paste(str2[i]," regla ",j," , ")
  }
}
test=as.data.frame(str2)
write.csv(str2, file='dump2', row.names = FALSE)

analytics4=read.transactions("dump2", format="basket", sep=",",cols=NULL)
#DE ALGUNA FORMA HAY QUE BORRAR LAS COMILLAS DEL DUMP
summary(analytics4)
itemFrequencyPlot(analytics4,topN=180,type="absolute")
#data("Adult")
#rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
rules=apriori(analytics4,parameter=list(supp=0.2, conf=0.2,maxlen=2),appearance = list (default="lhs",rhs="spenddummy"), control = list (verbose=F))
rules2=apriori(analytics4,parameter=list(supp=0.1, conf=0.1,maxlen=2),appearance = list (default="lhs",rhs="spenddummy"), control = list (verbose=F))
rules3=apriori(analytics4,parameter=list(supp=0.1, conf=0.1,maxlen=2))

summary(rules3)
summary(rules2)
inspect(head(sort(rules3, by ="support"),124))
inspect(head(sort(rules3, by ="support"),30))
plot(rules, method="graph")
plot(rules3)

# data <- paste(
#   "# this is some test data", 
#   "item1, item2", 
#   "item1", 
#   "item2, item3", 
#   sep="\n")
# cat(data)
# write(data, file = "demo_basket")
# tr <- read.transactions("demo_basket", format = "basket", sep=",", skip = 1)
# rules=apriori(tr,parameter=list(supp=0.1, conf=0.1))
# 
# inspect(tr)
pricelast=as.data.frame(cbind(prod2[1,1],demand[nrow(demand),2]))
### HAY QUE CORRER DE NUEVOS LAS ELASTICIDADES. 

# elasticity=cbind(prod2[1,1],model1$coefficients[2],model1$coefficients[1],summary(model1)$r.squared)
# 
# for (i in 1:nrow(prod2) ) {
# demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0,list(UNITS,PRECIONORM,FEATURE,DISPLAY,TPR_ONLY)]
# model1=lm(log(UNITS)~log(PRECIONORM)+FEATURE+DISPLAY+TPR_ONLY,data=demand)
# elasticity[i,]=cbind(prod2[i,1],model1$coefficients[2],model1$coefficients[1],summary(model1)$r.squared)
# pricelast[i,]=cbind(prod2[i,1],demand[nrow(demand),2])
# }
# i=10
# demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0,list(UNITS,PRECIONORM,FEATURE,DISPLAY,TPR_ONLY)]
# model1=lm(log(UNITS)~log(PRECIONORM)+FEATURE+DISPLAY+TPR_ONLY,data=demand)
# summary(model1)

#attach(demand)
#names(clon)[1]<-UPC
#names(clon)[2]<-"DESCRIPTION"
rm(sales.exp)
sales.exp =as.data.frame(pricelast[,2]*(exp(elasticity[,3] + elasticity[,2]*log(pricelast[,2]))))
sales.exp
sumsales=sum(as.vector(pricelast[,2]*(exp(elasticity[,3] + elasticity[,2]*log(pricelast[,2])))))



# f_demand <- function(Precio) {
#   demand <- sum(as.vector(Precio*(exp(elasticity[,3] + elasticity[,2]*log(Precio)))))
#   return(demand)
# }
f_demand(pricelast[,2])
preciopt=pricelast[,2]
preciopt
pricelast[,2]
ui=matrix(rep(c(1, -1,0), times = c(1,1,13)), nrow=1)
# preciopt<- constrOptim(pricelast[,2], f_demand, NULL, matrix(rep(c(1, -1,0), times = c(1,1,13)), nrow=1), ci=c(0,0),
#             control=list(fnscale=-1))
#preciopt<- Optim(pricelast[,2], f_demand, NULL, NULL, NULL, control=list(fnscale=-1))
Priceoptnorest<-optim(pricelast[,2],f_demand, method="SANN", control=list(maxit=20000,fnscale=-1))
Priceoptnorest
mean(Priceoptnorest$par)
f_demand(Priceoptnorest$par)
Priceoptlim<-optim(pricelast[,2],f_demand, lower=pricelast[,2]*0.6, upper=pricelast[,2]*1.4,method="L-BFGS-B",control=list(maxit=100,fnscale=-1))
print(Priceoptlim$par==pricelast[,2]*0.6)
print(Priceoptlim$par==pricelast[,2]*1.4)
Priceoptimlim
Priceoptlim<-optim(pricelast[,2],f_demand, lower=0.01, upper=2,method="L-BFGS-B",control=list(maxit=1000,fnscale=-1))
Priceoptlim
print(Priceoptlim$par==0.01)

unit.exp =as.data.frame(exp(elasticity[,3] + elasticity[,2]*log(pricelast[,2])))
unit.exp
sales.exp =as.data.frame(pricelast[,2]*(exp(elasticity[,3] + elasticity[,2]*log(pricelast[,2]))))
sum(sales.exp)

### volviendo a market basket
itemFrequencyPlot(analytics4,topN=180,type="absolute")
#data("Adult")
#rules <- apriori(Adult, parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
length(analytics4)
items(analytics4)
image(analytics4)
as(analytics4[1:5], "list")
itemFrequencyPlot(analytics4, support = 0.4, cex.names=0.5)
reglas=apriori(analytics4,parameter=list(supp=0.1, conf=0.1,maxlen=2))
profit <- subset(reglas, subset = rhs %in% "spenddummy" & support>0.4 & lift>1)
orden <- apriori(analytics4,parameter=list(supp=0.98, conf=0.1,maxlen=1))
inspect(profit)
inspect(orden)
#demo=as(profit, "data.frame")

profitdf = data.frame(
  lhs = labels(lhs(profit)),
  rhs = labels(rhs(profit)), 
  profit@quality)

ordendf = data.frame(
  lhs = labels(lhs(orden)),
  rhs = labels(rhs(orden)), 
  orden@quality)

restric1=data.frame(prod2[,1])
restric2=as.data.frame(t(restric1))
colnames(restric2)<-restric2
str(restric2)

strsplit(profit[1,1],">")

for (i in 1:nrow(profitdf) ) {
  temp= as.character(profitdf[i,1])
  temp=gsub("}","",temp)
  temp=gsub("[{]","",temp)
  temp=gsub(" ","",temp)
  temp2=strsplit(temp,">")
  for (j in 1:nrow(prod2) ) {
    if(temp2[[1]][1] == as.character(prod2[j,1])) restric2[i,j]=1 else
      if(temp2[[1]][2] == as.character(prod2[j,1])) restric2[i,j]=-1 else
        restric2[i,j]=0
}
}
ci=c(rep(0,ncol(ui)))
uibase=as.matrix(restric2)
ui=as.matrix(uibase[c(1:2),])
ui
str(ui)
ncol(ui)
ci=c(rep(0,nrow(ui)))
str(ci)
pricelast[,2]=memory
pricelast[,2]=rep(0,nrow(pricelast))

#----

#source("myconstrOptim.r")
nsim = 1
mineps = 1e-6*mean(pricelast[,2])
maxeps = 1e-6*mean(pricelast[,2])
eps = seq(mineps, maxeps, length.out=nsim)
#eps=-0.01*mean(pricelast[,2])
#eps=1e-6
preciopt.par = array(NA, dim=c(nsim,nrow(prod2)))
preciopt.val = array(NA, dim=nsim)
for (i in 1:nsim){
  myeps = eps[i] 
  myconst = 1:nrow(uibase)#myconst = sample(1:nrow(uibase), min(nrow(uibase),100))
  myuibase = uibase[myconst,]
  myci = c(rep(0-myeps,nrow(myuibase)))
  
  #print(myuibase %*% pricelast[,2] >= myci)
  preciopt = constrOptim(pricelast[,2], f_demand, NULL, ui=myuibase, ci=myci, control=list(fnscale=-1))
  precio_profit2 = constrOptim(pricelast[,2], f_demand, NULL, ui=myuibase, ci=myci, method= "SANN", control=list(maxit=1000,fnscale=-1))
  preciopt.val[i] = preciopt$value
  preciopt.par[i,] = preciopt$par
}
f_demand(pricelast[,2])
mean(pricelast[,2])
preciopt.val
mean(preciopt.par)
eps
mean(preciopt.par)
plot(x=eps, preciopt.val)
#preciopt
#preciopt$value
#---
log(preciopt$par)
ladoizq=uibase%*%pricelast[,2]
print(ladoizq>=c(rep(0,nrow(uibase))))
# for (j in 1:nrow(uibase) ) {
#   if(uibase[j,]*pricelast[,2]) restric2[i,j]=1 else
#     if(temp2[[1]][2] == as.character(prod2[j,1])) restric2[i,j]=-1 else
#       restric2[i,j]=0
# }
#lambda=c(price)
#rep(1,nrow(restric2))
#buscando puntos factibles
# sum(lambda[,2])
# lambda=c(pricelast[,2],rep(1,nrow(restric2)))
# lambda[(nrow(pricelast)+1):length(lambda)]
# 
# rm(test3)
# uibaseneg=uibase*-1
# test=rbind(uibase,uibaseneg)
# testizq=do.call(rbind, replicate(2, test, simplify=FALSE))
# test2=matrix(1,nrow(uibase),nrow(uibase))
# test2neg=test2*-1
# test3=rbind(test2,test2neg)
# test4=test3*-1
# dim(test3)
# testder=test=rbind(test3,test4)
# ui2=cbind(testizq,testder)
# dim(ui2)
# lambda=matrix(0.001,ncol(ui2),1)
# function2 <- function(lambda) {
#   demand <- sum(lambda[(nrow(pricelast)+1):length(lambda)])
#   return(demand)
# }
# lambda=rbind(rep(0,nrow(pricelast)))
# pricefact<-constrOptim(lambda, function2, NULL, ui=ui2, ci=c(rep(eps,nrow(ui2))))
# pricefact$par
# #pricefact<-constrOptim(lambda, sum(lambda[(nrow(pricelast)+1):length(lambda)]), NULL, ui=ui2, ci=c(rep(eps,nrow(ui2))))
# sum(lambda)


restric_orden2=as.data.frame(t(restric1))
colnames(restric_orden2)<-restric_orden2
str(restric2)

for (i in 1:nrow(ordendf) ) {
  temp= as.character(ordendf[i,2])
  temp=gsub("}","",temp)
  temp=gsub("[{]","",temp)
  temp=gsub(" ","",temp)
  temp2=strsplit(temp,">")
  for (j in 1:nrow(prod2) ) {
    if(temp2[[1]][1] == as.character(prod2[j,1])) restric_orden2[i,j]=1 else
      if(temp2[[1]][2] == as.character(prod2[j,1])) restric_orden2[i,j]=-1 else
        restric_orden2[i,j]=0
  }
}
as.matrix(restric_orden2)
eps=-1e-8
myconst = sample(1:nrow(restric_orden2), min(nrow(restric_orden2),100))
myuibase = restric_orden2[myconst,]
myci = c(rep(0+eps,nrow(myuibase)))
myuibase=as.matrix(myuibase)
#print(myuibase %*% rep(0, ncol(myuibase)) >= myci)
#preciopt<- constrOptim(rep(0, ncol(myuibase)), f_demand, NULL, ui=myuibase, ci=myci, control=list(fnscale=-1))

print(myuibase %*% pricelast[,2] >= myci)
precioptorden<- constrOptim(pricelast[,2], f_demand, NULL, ui=myuibase, ci=myci, control=list(fnscale=-1))
precioptorden$value
mean(precioptorden$par)
print(myuibase %*% preciopt$par == myci)

### implementacion matriz de rangos
Pmin=pricelast[,2]*0.6
Pmax=pricelast[,2]*1.4
str(Pmin)
cirango=c(cbind(Pmin,-Pmax))
cirango
rango= diag(nrow(prod2))
uirango=rbind(rango,-rango)
uirango
preciorango2<- constrOptim(pricelast[,2], f_demand, NULL, ui=uirango, ci=cirango, control=list(fnscale=-1))
preciorango2
mean(preciorango2$par)
print(uirango %*% preciorango2$par == cirango)
#### arreglando matriz de elasticidad
elas2= elasticity
elas2[1,2]=-elasticity[1,2]
elas2[2,2]=-elasticity[2,2]
elas2[3,2]=-elasticity[3,2]
elas2[1,3]=mean(elas2[4:15,3])
elas2[2,3]=mean(elas2[4:15,3])
elas2[3,3]=mean(elas2[4:15,3])
elas2

#### funciones
f_demand <- function(Precio) {
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(elasticity[,3] + elasticity[,2]*log(Precio))))
  return(demand)
}

f_demandSANN <- function(logPrecio) {
  #  print(logPrecio)
  #  print(log(exp(logPrecio)))
  demand <- sum((exp(logPrecio)-(pricelast[,2]/1.3))*(exp(elasticity[,3] + elasticity[,2]*log(exp(logPrecio)))))
  return(demand)
}
f_demandSANN2 <- function(logPrecio) {
  #  print(logPrecio)
  #  print(log(exp(logPrecio)))
  demand <- sum((exp(logPrecio)-(pricelast[,2]/1.3))*(exp(elas2[,3] + elas2[,2]*log(exp(logPrecio)))))
  return(demand)
}

f_demand2 <- function(Precio) {
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(elas2[,3] + elas2[,2]*log(Precio))))
  return(demand)
}


f_venta <- function(Precio) {
  demand <- (Precio-(pricelast[,2]/1.3))*(exp(elasticity[,3] + elasticity[,2]*log(Precio)))
  return(demand)
}
f_venta2 <- function(Precio) {
  demand <- (Precio-(pricelast[,2]/1.3))*(exp(elas2[,3] + elas2[,2]*log(Precio)))
  return(demand)
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

f_demandSANN3 <- function(logPrecio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(logPrecio)))
  c=as.matrix(elasticity3[,2:20])
  demand <- (exp(logPrecio)-(pricelast[,2]/1.3))*(exp(c%*%b))
#  browser()
  return(sum(demand))
}

f_SANNhbm <- function(logPrecio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(logPrecio)))
  c=as.matrix(betahbm)
  #c[is.na(c)] <- 0
  demand <- sum((exp(logPrecio)-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}

f_demand3 <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  c=as.matrix(elasticity3[,2:20])
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}

f_demand4 <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  #c=as.matrix(elasticity3[,2:20])
  c=as.matrix(runelast[,2:20])
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}

f_venta3 <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  c=as.matrix(elasticity3[,2:20])
  demand <-(Precio-(pricelast[,2]/1.3))*(exp(c%*%b))
  return(demand)
}
f_demandhbm <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  c=as.matrix(betahbm)
  #c[is.na(c)] <- 0
  demand <- sum((Precio-(pricelast[,2]/1.3))*(exp(c%*%b)))
  return(demand)
}

f_ventahbm <- function(Precio) {
  b=as.vector(cbind(t(c(1,0,0,0)),t(log(Precio))))
  c=as.matrix(betahbm)
  #c[is.na(c)] <- 0
  demand <- (Precio-(pricelast[,2]/1.3))*(exp(c%*%b))
  return(demand)
}

f_optim <- function(ui,ci,niter) {
  message=FALSE
  warning=FALSE
  suppressMessages(library(limSolve))
  aux=diag(15)
  aux2=rbind(ui,aux)
  aux3=rbind(as.matrix(ci), as.matrix(rep(0,15)))
  dim(aux2)
  precio1=xsample(G=aux2, H=aux3)
  precio2=precio1$X[nrow(precio1$X),]
  
  precio=0
  pi=0
  iter=0
  for(i in 1:niter) {
    precio2=precio1$X[nrow(precio1$X)-i,]
    p_prod <- constrOptim(precio2, f_demand3, NULL, ui=ui, ci=ci, control=list(fnscale=-1))
    print(c(pi,p_prod$value, mean(precio2),i,mean(precio)))
    if (p_prod$value> pi) {
      pi= p_prod$value
      iter=i
      precio=p_prod$par
    } 
  }
  print(c(pi,iter, mean(precio)))
  result=list(pi=pi,iter=iter,price=precio)
  return(result)
}

f_optim2 <- function(ui,ci,niter) {
  message=FALSE
  warning=FALSE
  suppressMessages(library(limSolve))
  aux=diag(15)
  aux2=rbind(ui,aux)
  aux3=rbind(as.matrix(ci), as.matrix(rep(0,15)))
  dim(aux2)
  precio1=xsample(G=aux2, H=aux3)
  precio2=precio1$X[nrow(precio1$X),]
  precio=0
  pi=0
  iter=0
  for(i in 1:niter) {
    precio2=precio1$X[nrow(precio1$X)-i,]
    t=try(constrOptim(precio2, f_demand4, NULL, ui=ui, ci=ci, control=list(fnscale=-1)),silent=T)
    if("try-error" %in% class(t)) print("+INF") else {
      p_prod <- constrOptim(precio2, f_demand4, NULL, ui=ui, ci=ci, control=list(fnscale=-1))
    print(c(pi,p_prod$value, mean(precio2),i,mean(precio)))
    if (p_prod$value> pi) {
      pi= p_prod$value
      iter=i
      precio=p_prod$par
    } 
    } 
  }
  print(c(pi,iter, mean(precio)))
  result=list(pi=pi,iter=iter,price=precio)
  return(result)
}


