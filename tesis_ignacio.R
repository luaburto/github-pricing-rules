tlogprecio=log(tablaprecio[,2:ncol(tablaprecio)])
tlogprecio=cbind(tablaprecio[,1],tlogprecio)
colnames(tlogprecio)[1] <- "WEEK_END_DATE"

i=3


demand=clon[UPC==as.numeric(prod2[i,1])&PRECIONORM!=0&STORE_NUM==tienda,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
datos=merge(demand,tlogprecio,by="WEEK_END_DATE")
datos=datos[,-1]
datos$UNITS <- log(datos$UNITS)
View(tablaprecio)


model1=lm(UNITS~.,data=datos)
prod2


tablaprecio=as.data.frame(matrix(nrow=timemax,ncol=nrow(prod2)+1))
tablatemp=as.data.table(matrix(nrow=timemax,ncol=nrow(prod2)+1))

tienda=2277
tablaprecio[,1]=timex
tablaprecio
for (i in 1:nrow(tablaprecio) ) {
  for (j in 2:ncol(tablaprecio) ) {
    #    price1=sqldf(paste0("select PRECIONORM from tablon3 where UPC=",final[j-2,1]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tabla10[i,1],"'"))
    #    price2=sqldf(paste0("select PRECIONORM from tablon3 where UPC=",final[j-2,2]," AND STORE_NUM=",store.sel," AND WEEK_END_DATE='",tabla10[i,1],"'"))
    tablatemp=clon[UPC==final[j-1,1] & STORE_NUM==tienda,list(UPC, WEEK_END_DATE, PRECIONORM)]
    if(length(tablatemp[WEEK_END_DATE==as.character(timex[i,1]),PRECIONORM])>0)
      tablaprecio[i,j]=tablatemp[WEEK_END_DATE==as.character(timex[i,1]),PRECIONORM]
    else
      tablaprecio[i,j]=tablaprecio[i-1,j]  
  }
}

for (i in 2:ncol(tablaprecio) ) {
  names(tablaprecio)[i] <- paste0(prod2[i-1,1])
}
