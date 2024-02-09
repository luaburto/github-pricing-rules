##Paquetes necesarios para correr la aplicación

install.packages("dplyr")
install.packages("shiny")
install.packages("plotly")
install.packages("ggplot2")
install.packages("GGally")
install.packages("scales")
install.packages("lubridate")
install.packages("gsubfn")
install.packages("latticeExtra")
install.packages("shinyWidgets")
install.packages("magrittr")
install.packages("qwraps2")
install.packages("rlang")
install.packages("tidyverse")
install.packages("data.table")
library("data.table")
library("tidyverse")
library("rlang")
library("shinyWidgets")
library("latticeExtra")
library("scales")
library("gsubfn")
library("lubridate")
library("ggplot2")
library("GGally")
library("dplyr")
library("shiny")
library("plotly")
library("magrittr")
library("qwraps2")
###



###Cálculo de [matrix_aux_3], junta el SKU con el UPC del producto.
nth<-paste(tablon3$UPC,sep = ",",tablon3$DESCRIPTION)
matrix_aux2<-matrix(nth)
matrix_aux3<-cbind(matrix_aux2,tablon3)
names(matrix_aux3)[1]="SKU_PROD"

### [dataset_m], ordena la data que se va a usar en la aplicación, junto con llevar el tipo de fecha a un mismo estilo.

dataset_m<-matrix_aux3[matrix_aux3$SUB_CATEGORY=="MOUTHWASHES (ANTISEPTIC)"  & matrix_aux3$STORE_NUM==2277,]
dataset_m$WEEK_END_DATE<-as.character(dataset_m$WEEK_END_DATE)
dataset_m$WEEK_END_DATE<-as.Date(dataset_m$WEEK_END_DATE,format="%Y-%m-%d")
dataset_m=as.data.frame(dataset_m)
dataset_m<-dataset_m[order(dataset_m$WEEK_END_DATE),]


#### Tablaprecio_alf

prod2_alf=prod[prod$SUB_CATEGORY=='MOUTHWASHES (ANTISEPTIC)',]

clon_alf=clon_alf=as.data.table(tablon3)

timemax_alf=nrow(sqldf("select DISTINCT WEEK_END_DATE from tablon3 ORDER BY WEEK_END_DATE"))
timex_alf=sqldf("select DISTINCT WEEK_END_DATE from tablon3 ORDER BY WEEK_END_DATE")

tablaprecio_alf=as.data.frame(matrix(nrow=timemax_alf,ncol=nrow(prod2_alf)+1))
tablatemp_alf=as.data.table(matrix(nrow=timemax_alf,ncol=nrow(prod2_alf)+1))

tienda_alf=2277
tablaprecio_alf[,1]=timex_alf

for (i in 1:nrow(tablaprecio_alf) ) {
  for (j in 2:ncol(tablaprecio_alf) ) {
    
    tablatemp_alf=clon_alf[UPC==as.character(prod2_alf[j-1,1]) & STORE_NUM==tienda_alf,list(UPC, WEEK_END_DATE, PRECIONORM)]
    if(length(tablatemp_alf[WEEK_END_DATE==as.character(timex[i,1]),PRECIONORM])>0)
      tablaprecio_alf[i,j]=tablatemp_alf[WEEK_END_DATE==as.character(timex[i,1]),PRECIONORM]
    else
      tablaprecio_alf[i,j]=tablaprecio_alf[i-1,j]  
  }
}

for (i in 2:ncol(tablaprecio_alf) ) {
  names(tablaprecio_alf)[i] <- paste0(prod2_alf[i-1,1])
}

colnames(tablaprecio_alf)[1]="WEEK_END_DATE"

###Cálculo de elasticidades y funciones varias que determinan la tabla profit para mouthwashes

###ELASTICITY3

tlogprecio_alf=log(tablaprecio_alf[,2:ncol(tablaprecio_alf)])
tlogprecio_alf=cbind(tablaprecio_alf[,1],tlogprecio_alf)
colnames(tlogprecio_alf)[1] <- "WEEK_END_DATE"
tlogprecio_alf$WEEK_END_DATE<-as.character(tlogprecio_alf$WEEK_END_DATE)
elasticity3_alf=cbind(prod2_alf[i,1],t(model1_alf$coefficients),summary(model1_alf)$r.squared,sigma(model1_alf))
tlogprecio_alf$WEEK_END_DATE<-as.character(tlogprecio_alf$WEEK_END_DATE)
tlogprecio_alf$WEEK_END_DATE<-as.Date(tlogprecio_alf$WEEK_END_DATE,format="%Y-%m-%d")
clon_alf$WEEK_END_DATE<-as.character(clon_alf$WEEK_END_DATE)
clon_alf$WEEK_END_DATE<-as.Date(clon_alf$WEEK_END_DATE,format="%Y-%m-%d")
tablaprecio_alf$WEEK_END_DATE<-as.character(tablaprecio_alf$WEEK_END_DATE)
tablaprecio_alf$WEEK_END_DATE<-as.Date(tablaprecio_alf$WEEK_END_DATE,format="%Y-%m-%d")

for (i in 1:nrow(prod2_alf) ) {
  demand_alf=clon_alf[UPC==as.numeric(prod2_alf[i,1])&PRECIONORM!=0&STORE_NUM==tienda_alf,list(WEEK_END_DATE,UNITS,FEATURE,DISPLAY,TPR_ONLY)]
  datos_alf=merge(demand_alf,tlogprecio_alf,by="WEEK_END_DATE")
  datos_alf=datos_alf[,-1]
  model1_alf=lm(log(UNITS)~.,data=datos_alf)
  elasticity3_alf[i,]=cbind(prod2_alf[i,1],t(model1_alf$coefficients),summary(model1_alf)$r.squared,sigma(model1_alf))
}
elasticity3_alf[is.na(elasticity3_alf)] <- 0

###[pricelast_alf]
tabla_aux_alf=as.data.frame(tablaprecio_alf[156,2:9])
prod_aux_alf=prod2_alf[,1]
pricelast_alf=cbind(prod_aux_alf,t(tabla_aux_alf))

###

###[prom_alf]

prom_alf=cbind.data.frame(tablaprecio_alf[,1],rowSums(tablaprecio_alf[,2:9])/8)
colnames(prom_alf)=c("WEEK_END_DATE","prom")
prom_alf=as.data.frame(prom_alf)

###


f_demand3_alf<-function(Precio_Alf) {   ###ERROR
  b_alf=as.numeric(as.vector(cbind(t(c(1,0,0,0)),t(log(Precio_Alf)))))
  #c_alf=as.matrix(elasticity3_alf[,2:13])
  #c[is.na(c)] <- 0
  demand_alf <- sum((Precio_Alf-(pricelast_alf[,2]/1.3))*(exp(as.matrix(elasticity3_alf[,2:13])%*%b_alf)))
  return(demand_alf)
}


## [tabladda_alf]

tabladda_alf=as.data.frame(matrix(nrow=timemax_alf,ncol=nrow(prod2_alf)+1))

tienda_alf=2277
tabladda_alf[,1]=timex_alf
for (i in 1:timemax_alf ) {
  for (j in 2:(nrow(prod2_alf)+1)) {
    
    tablatemp_alf=clon_alf[UPC==as.numeric(prod2_alf[j-1,1]) & STORE_NUM==tienda_alf,list(UPC, UNITS, WEEK_END_DATE, PRECIONORM)]
    if(length(tablatemp_alf[WEEK_END_DATE==as.character(timex[i,1]),UNITS])>0)
      tabladda_alf[i,j]=tablatemp_alf[WEEK_END_DATE==as.character(timex[i,1]),UNITS]
    else
      tabladda_alf[i,j]=0
    
  }
}

for (i in 2:ncol(tabladda_alf) ) {
  
  names(tabladda_alf)[i] <- paste0(prod2_alf[i-1,1])
}
names(tabladda_alf)[1]="WEEK_END_DATE"

### [profit_alf]


histols_alf=1
histols_alf[i]=f_demand3_alf(t(tablaprecio_alf[i,2:9]))
historeal_alf=histols_alf

for (i in 1:nrow(tablaprecio_alf) ) {
  
  histols_alf[i]=f_demand3_alf(t(tablaprecio_alf[i,2:9]))
  historeal_alf[i]=as.matrix(tablaprecio_alf[i,2:9]-(pricelast_alf[,2]/1.3))%*%t(as.matrix(tabladda_alf[i,2:9]))
}

profit_alf_mast=data.frame(prom_alf, historeal_alf, histols_alf)
###

###Código para calcular (uiprodprofit_alf) y (ciprodprofit_alf)

nprod_alf=8

profitprod_alf=data.frame(tablaprecio_alf, profit= profit_alf_mast$historeal_alf,dummy=(profit_alf_mast$historeal_alf>mean(profit_alf_mast$historeal_alf))*1)
temp3_alf=nprod_alf+1
rprofitpar_alf=data.table(matrix(0,nrow=5*nprod_alf,ncol=16))
rprofitpar_alf<-as.data.frame(rprofitpar_alf)
colnames(rprofitpar_alf)=c("prod","cut","pi>c","pi<c","SUP_p>c_y_util>u","%util>u_en_regla","liftant P>c","c_prom","SUP_pi>c","SUP_pi<c","SUP_pi<piprom","CONF","CONFINV","SUP_PI","LIFT2","LIFT2INV")
for (k in 2:temp3_alf) {
  temp_alf=as.matrix(profitprod_alf[,k])
  for (i in c(0.05,0.25,0.5,0.75,0.95) ) {
    temp2_alf=as.numeric(quantile(temp_alf,i))
    print(temp2_alf)
    print(as.numeric(profitprod_alf[profitprod_alf[,k],"profit"]))
    rprofitpar_alf[j,1]=as.numeric(prod2_alf[k-1,1])
    rprofitpar_alf[j,2]=i
    rprofitpar_alf[j,3]=mean(profitprod_alf[profitprod_alf[,k]>temp2_alf,"profit"])
    rprofitpar_alf[j,4]=mean(profitprod_alf[profitprod_alf[,k]<=temp2_alf,"profit"])
    rprofitpar_alf[j,5]=sum(profitprod_alf[profitprod_alf[,k]>temp2_alf,"dummy"])
    rprofitpar_alf[j,6]=rprofitpar_alf[j,3]/mean(rulesprofit_alf$profit)
    rprofitpar_alf[j,7]=rprofitpar_alf[j,4]/mean(rulesprofit_alf$profit)
    rprofitpar_alf[j,8]=temp2_alf
    rprofitpar_alf[j,9]=nrow(profitprod_alf[profitprod_alf[,k]>temp2_alf,])
    rprofitpar_alf[j,10]=nrow(profitprod_alf[profitprod_alf[,k]<=temp2_alf,])
    rprofitpar_alf[j,11]=sum(profitprod_alf[profitprod_alf[,k]<=temp2_alf,"dummy"])
    rprofitpar_alf[j,12]=rprofitpar_alf[j,5]/rprofitpar_alf[j,9] #conf
    rprofitpar_alf[j,13]=rprofitpar_alf[j,11]/rprofitpar_alf[j,10]#confinv
    rprofitpar_alf[j,14]=sum(profitprod_alf[,"dummy"])/nrow(profitprod_alf) #suppi
    rprofitpar_alf[j,15]=rprofitpar_alf[j,12]/rprofitpar_alf[j,14] #LIFT
    rprofitpar_alf[j,16]=rprofitpar_alf[j,13]/rprofitpar_alf[j,14] #LIFT2
    
    j=j+1
  }
}
cirprofitpar2_alf=rprofitpar_alf[order(-`liftant P>c`),]
datatable(rprofitpar2_alf)
prodlist_alf=unique(rprofitpar_alf$prod)

selrules_alf=rprofitpar_alf[1,]
for (i in prodlist_alf) {
  temp_alf=rprofitpar_alf[prod==i&cut<0.8,]
  temp2_alf=max(temp_alf$`liftant P>c`,na.rm=TRUE)
  #temp3=temp[`lift pi>c`==temp2,]
  temp3_alf= temp_alf[temp_alf$`liftant P>c`==temp2_alf,]
  selrules_alf=rbind(selrules_alf,temp3_alf[1,])
}
selrules_alf=selrules_alf[-1,]

selrules2_alf=rprofitpar_alf[1,]
for (i in prodlist_alf) {
  temp_alf=rprofitpar_alf[prod==i&cut>0.2,]
  temp2_alf=max(temp_alf$`liftant P>c`,na.rm=TRUE)
  #temp3=temp[`lift pi>c`==temp2,]
  temp3_alf= temp_alf[temp_alf$`liftant P>c`==temp2_alf,]
  selrules2_alf=rbind(selrules2_alf,temp3_alf[1,])
}
selrules2_alf=selrules2_alf[-1,]

uiprodprofit_alf=cbind.data.frame(diag(1,8,8), cut=(selrules_alf$c_prom), lift=selrules_alf$`liftant P>c`)
uiprodprofit_alf=uiprodprofit_alf[uiprodprofit_alf$lift>0.02,]
uiprodprofit2_alf=cbind.data.frame(diag(-1,8,8), cut=(-selrules2_alf$c_prom), lift=selrules2_alf$`liftant P>c`)
uiprodprofit2_alf=uiprodprofit2_alf[uiprodprofit2_alf$lift>0.01,]
uiprodprofit2

dim(uiprodprofit)
dim(uiprodprofit2)
uiprodprofit_alf=rbind(uiprodprofit_alf,uiprodprofit2_alf)
uiprodprofit_alf=uiprodprofit_alf[,-ncol(uiprodprofit_alf)] #borro el lift
uiprodprofit_alf
uiprodprofit_alf=as.matrix(uiprodprofit_alf)
ciprodprofit_alf=uiprodprofit_alf[,ncol(uiprodprofit_alf)]
uiprodprofit_alf=uiprodprofit_alf[,-ncol(uiprodprofit_alf)]


ciprodprofit_alf

###


## [tabshiny_mouth], tabla auxiliar que permite unir los datos (ui) con los datos (ci), para visualizar las reglas.

tabshiny_mouth=matrix(0,nrow = 16,ncol = 2)

tabprofshiny_mouth_final=cbind(uiprodprofit_alf,ciprodprofit_alf)
name_aux1=t(prod2_alf[,1])
colnames(tabprofshiny_mouth_final)=cbind(name_aux1,"rule_mouth")


for (i in 1:nrow(tabprofshiny_mouth_final)){
  
  for (j in 1:((ncol(tabprofshiny_mouth_final))-1)){
    
    if (tabprofshiny_mouth_final[i,j]==1 ){
      
      tabshiny_mouth[i,1]=as.matrix(prod2_alf[j,1])
      tabshiny_mouth[i,2]=as.matrix(tabprofshiny_mouth_final[j,9])
      
      
    }
    if (tabprofshiny_mouth_final[i,j]== -1 ){
      
      tabshiny_mouth[i,1]=as.matrix(prod2_alf[j,1])
      tabshiny_mouth[i,2]=as.matrix(tabprofshiny_mouth_final[j,9])*-1
      print(tabprofshiny_mouth_final[i,j])
      
      
    }
    
  }
}
colnames(tabshiny_mouth)=c("UPC","RULE")
tabshiny_mouth[(9:16),2]=tabshiny_mouth[(9:16),2]*-1
tabshiny_mouth<-as.data.frame(tabshiny_mouth)

###

### [profreal_aux2], formatea el estilo de fecha en profit para que se pueda visualizar en la aplicación.

profreal_aux2=profit_alf_mast
profreal_aux2$WEEK_END_DATE<-as.character(profreal_aux2$WEEK_END_DATE)
profreal_aux2$WEEK_END_DATE<-as.Date(profreal_aux2$WEEK_END_DATE,format="%Y-%m-%d")

###

###Cálculo [difprecios_alf]

difprecios_alf=data.frame(0,nrow=timemax_alf,ncol=nprod_alf**2)
for (i in 1:timemax_alf ) {
  for (j in 1:nprod_alf)  {
    for (k in 1:nprod_alf)  {
      difprecios_alf[i,nprod_alf*(j-1)+k]=tablaprecio_alf[i,k+1]-tablaprecio_alf[i,j+1]
    }  
  }
}
for (j in 1:nprod_alf)  {
  for (k in 1:nprod_alf)  {
    colnames(difprecios_alf)[nprod_alf*(j-1)+k]=paste(colnames(tablaprecio_alf)[k+1],"-",colnames(tablaprecio_alf)[j+1])
    print(j)
    print(k)
  }  
}


###




### [difprecios_mouth], junta las fechas en que ocurrieron las diferencias de precios para que se pueda visualizar en la aplicación.

difprecios_mouth=cbind(tablaprecio_alf[,1],difprecios_alf)
colnames(difprecios_mouth)[1]="WEEK_END_DATE"
difprecios_mouth$WEEK_END_DATE<-as.character(difprecios_mouth$WEEK_END_DATE)
difprecios_mouth$WEEK_END_DATE<-as.Date(difprecios_mouth$WEEK_END_DATE,format="%Y-%m-%d")
difprecios_mouth=as.data.frame(difprecios_mouth)

### [auxp] y [auxp2], junta las diferencias de precios y los distintos "UPC" como caracteres para que puedan ser visualizados como opciones en la aplicación.


auxm=as.vector(colnames(difprecios_alf))
auxm=as.data.frame(auxm)
colnames(auxm)[1]="NAMES"

auxm2=unique(dataset_m$SKU_PROD)
namessku_m=as.vector(auxm2)
auxm2=as.data.frame(auxm2)
colnames(auxm2)[1]="NAMES"

auxm3=rbind(auxm2,auxm)
####

### cbind [tablaprecio_alf] con [difprecios_alf]

precio_bindm=cbind(tablaprecio_alf,difprecios_mouth[,2:65])
colnames(precio_bindm)[1]="WEEK_END_DATE"
precio_bindm$WEEK_END_DATE<-as.character(precio_bindm$WEEK_END_DATE)
precio_bindm$WEEK_END_DATE<-as.Date(precio_bindm$WEEK_END_DATE,format="%Y-%m-%d")
precio_bindm=as.data.frame(precio_bindm)

colnames(precio_bindm)[2:9]=namessku_m


###


###

ui <- fluidPage(
  
  titlePanel("Pricing Environment for Mouthwashes"),
  
  
  
  sidebarPanel(
    
    selectInput(inputId = "product", label = "View", choices = auxm3$NAMES),
    
    selectInput(inputId = "contour1", label = "PROD [X]", choices = auxm3$NAMES),
    
    selectInput(inputId = "contour2", label = "PROD2 [Y]", choices = auxm3$NAMES),
    
    
    
    
    
    ##Seleccionar dates con date input
    
    dateRangeInput(
      
      inputId = "date",
      
      label = "Select the date range",
      
      start =min(dataset_m$WEEK_END_DATE),
      
      end = max(dataset_m$WEEK_END_DATE),
      
      min = min(dataset_m$WEEK_END_DATE),
      
      max = max(dataset_m$WEEK_END_DATE),
      
      format = "yyyy-mm-dd",
      
      separator = "-"
      
    ),
    
    tabsetPanel(
      id = 'dataset',
      tabPanel("PRICE", tableOutput("sum")),
      tabPanel("DEMAND", tableOutput("sum2")),
      tabPanel("PROFIT", tableOutput("sum3"))
    )
  ),
  
  mainPanel(
    plotOutput(outputId = "distPlot"),
    
    plotOutput(outputId = "distPlot2"),
    
    plotlyOutput(outputId = "distPlot3"),
    
    plotlyOutput(outputId = "distPlot4")
    
    
    
    
  )
  
  
  
)
server<-function(session,input,output){
  
  
  
  
  
  output$distPlot <- renderPlot({
    
    #Density Plot
    
    
    
    
    dataset_aux1=precio_bindm[between(precio_bindm$WEEK_END_DATE,min(input$date),max(input$date)),]
    histprice = precio_bindm[,input$product]
    
    Price_DifPrice <- histprice
    min_price<-min(histprice)
    mean_price<-mean(histprice)
    max_price<-max(histprice)
    
    ggplot(data = dataset_aux1, aes(x = Price_DifPrice)) +
      geom_density()+
      
      #Mínimo
      geom_vline(xintercept = min_price,
                 color = "blue",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        min_price,
                        min_price+ 1,
                        label = "Minimum",
                        color = "blue")+
      
      #Mediada
      geom_vline(xintercept = mean_price,
                 color = "darkred",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        mean_price,
                        mean_price+ 1,
                        label = "Mean",
                        color = "darkred")+
      #Máximo
      geom_vline(xintercept = max_price,
                 color = "#6b8e23",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        max_price,
                        max_price+ 1,
                        label = "Maximum",
                        color = "#6b8e23")
    
    
  })
  
  ###Primera tabla resumen
  
  output$sum <- renderTable({
    
    dataset_auxx = dataset_m[dataset_m$SKU_PROD==input$product, ]
    
    
    #Probando con la función between
    
    
    PRICE_DATA=dataset_auxx[between(dataset_auxx$WEEK_END_DATE,min(input$date),max(input$date)),]
    our_summary1 <-
      list("Precio" =
             list("min" = ~min(.data$PRECIONORM),
                  "max" = ~max(.data$PRECIONORM),
                  "mean" = ~mean(.data$PRECIONORM)))
    
    
    
    whole <- summary_table(PRICE_DATA, our_summary1)
    whole
    whole_2<-c("min","max","mean")
    names(whole_2)[1]<-"Measurent"
    whole_3<-cbind(whole_2,whole)
    names(whole_3)[names(whole_3) == "V1"] <- "Measurement"
    whole_3
    
  })
  
  
  ###Segunda tabla resumen
  
  output$sum2 <- renderTable({
    
    dataset_auxx = dataset_m[dataset_m$SKU_PROD==input$product, ]
    
    
    
    #Probando con la función between
    
    
    DEMAND_DATA=dataset_auxx[between(dataset_auxx$WEEK_END_DATE,min(input$date),max(input$date)),]
    our_summary1 <-
      list("Precio" =
             list("min" = ~min(.data$UNITS),
                  "max" = ~max(.data$UNITS),
                  "mean" = ~mean(.data$UNITS)))
    
    
    
    whole <- summary_table(DEMAND_DATA, our_summary1)
    whole
    whole_2<-c("min","max","mean")
    names(whole_2)[1]<-"Measurent"
    whole_3<-cbind(whole_2,whole)
    names(whole_3)[names(whole_3) == "V1"] <- "Measurement"
    whole_3
    
  })
  
  
  ###Tercera tabla resumen (PROFIT, USA UN DATAFRAME DISTINTO)
  output$sum3 <- renderTable({
    
    dataset_auxx = dataset_m[dataset_m$SKU_PROD==input$product, ]
    
    dataset_auxx=dataset_auxx[between(dataset_auxx$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    profit_graph=profreal_aux2
    
    PROFIT_DATA=profit_graph[profit_graph$WEEK_END_DATE%in%dataset_auxx$WEEK_END_DATE,]
    our_summary1 <-
      list("Precio" =
             list("min" = ~min(.data$historeal),
                  "max" = ~max(.data$historeal),
                  "mean" = ~mean(.data$historeal)))
    
    
    
    whole <- summary_table(PROFIT_DATA, our_summary1)
    whole
    whole_2<-c("min","max","mean")
    names(whole_2)[1]<-"Measurent"
    whole_3<-cbind(whole_2,whole)
    names(whole_3)[names(whole_3) == "V1"] <- "Measurement"
    whole_3
    
  })
  
  #Scatter PLot
  output$distPlot2 <- renderPlot({
    
    
    
    dataset_aux3 = dataset_m[dataset_m$SKU_PROD==input$product, ]
    
    ##Probando con la función between
    
    
    
    dataset_aux3=dataset_aux3[between(dataset_aux3$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    profit_graph=profreal_aux2
    
    profit_graph=profit_graph[profit_graph$WEEK_END_DATE%in%dataset_aux3$WEEK_END_DATE,]
    
    Profit <- profit_graph$historeal
    
    min_profit<-min(Profit)
    mean_profit<-mean(Profit)
    max_profit<-max(Profit)
    
    ###Precio_
    
    Price <- dataset_aux3$PRECIONORM #[1:nrow(profit_graph)]
    
    min_price<- min(Price)
    mean_price<- mean(Price)
    max_price<- max(Price)
    
    
    
    datax<-data.frame(Price,Profit)
    
    
    ##Defino la data para el display de las reglas
    prod_shiny=unique(dataset_aux3$UPC)
    tabshiny = tabshiny_mouth
    rules_aux=tabshiny[tabshiny$UPC==prod_shiny,]
    rules_aux=as.data.frame(rules_aux)
    rule1=rules_aux$RULE
    rule1=rule1[1]
    rule2=rule1[2]
    
    
    
    
    ggplot(datax, aes(x=Price, y=Profit)) +geom_point(shape=1) +
      geom_smooth(method=loess)+
      
      #Mínimo
      geom_hline(yintercept = min_profit,
                 color = "#63b8ff",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        min_price,
                        min_price+ 1,
                        label = "Minimum",
                        color = "#63b8ff")+
      
      #Mediada
      geom_hline(yintercept = mean_profit,
                 color = "#cd0000",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        mean_price,
                        mean_price+ 1,
                        label = "Mean",
                        color = "#cd0000")+
      #Máximo
      geom_hline(yintercept = max_profit,
                 color = "#9acd32",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        max_price,
                        max_price+ 1,
                        label = "Maximum",
                        color = "#9acd32")+
      ###Aquí comienza el display de reglas
      geom_vline(xintercept = rule1,
                 color = "black",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        rule1,
                        rule1+ 1,
                        label = "Rule1",
                        color = "black")+
      
      geom_vline(xintercept = rule2,
                 color = "black",
                 linetype = "dashed") +
      ggplot2::annotate("text",
                        rule2,
                        rule2+ 1,
                        label = "Rule2",
                        color = "black")
    
    
    
  })
  
  
  
  ## 3axis Plot
  output$distPlot3 <- renderPlotly({
    
    
    
    dataset_aux2 = dataset_m[dataset_m$SKU_PROD==input$product, ]
    
    
    
    ###Probar con la función between
    
    
    dataset_aux2=dataset_aux2[between(dataset_aux2$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    Price <- dataset_aux2$PRECIONORM
    min_price<- min(Price)
    mean_price<- mean(Price)
    max_price<- max(Price)
    
    Demand <- dataset_aux2$UNITS
    min_demand<- min(Demand)
    mean_demand<- mean(Demand)
    max_demand<- max(Demand)
    
    Weeks<-dataset_aux2$WEEK_END_DATE
    
    ###Versión Bar and Line Chart
    p <- plot_ly(dataset_aux2) %>%
      add_trace(x = ~Weeks, y = ~Demand, type = 'bar', name = 'Demand',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(Demand, 'UN')) %>%
      add_trace(x = ~Weeks, y = ~Price, type = 'scatter', mode = 'lines',
                name = 'Price', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(Price, '$')) %>%
      layout(title = 'Price v/s Demand on selected Weeks',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Demand[UN]', showgrid= FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Price [$]', showgrid = FALSE,zeroline = FALSE))
    
    
    
    
    p
    
    
    
  })
  
  
  ##Contour Graph
  
  
  
  output$distPlot4<-renderPlotly({
    
    
    
    dataset_aux6 =precio_bindm[between(precio_bindm$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    profit_graph=profreal_aux2
    
    profit_graph=profit_graph[profit_graph$WEEK_END_DATE%in%dataset_aux6$WEEK_END_DATE,]
    
    
    
    PRODX =as.numeric(dataset_aux6[,input$contour1])
    PRODY=as.numeric(dataset_aux6[,input$contour2])
    PROFIT=as.numeric(profit_graph$historeal)
    mat_aux2=cbind(PRODX,PRODY,PROFIT)
    mat_aux2=as.data.frame(mat_aux2)
    
    p <- plot_ly(
      mat_aux2,
      x=~PRODX,
      y=~PRODY,
      z =PROFIT,
      type = 'contour')%>%
      colorbar(title = "Profit")
    
    
    
    p
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
}

shinyApp(ui,server)
