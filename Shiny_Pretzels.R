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

### [dataset_p], ordena la data que se va a usar en la aplicación, junto con llevar el tipo de fecha a un mismo estilo.

dataset_p<-matrix_aux3[matrix_aux3$SUB_CATEGORY=="PRETZELS"& matrix_aux3$STORE_NUM==2277,]
dataset_p$WEEK_END_DATE<-as.character(dataset_p$WEEK_END_DATE)
dataset_p$WEEK_END_DATE<-as.Date(dataset_p$WEEK_END_DATE,format="%Y-%m-%d")
dataset_p=as.data.frame(dataset_p)
dataset_p<-dataset_p[order(dataset_p$WEEK_END_DATE),]


## [tabshiny_pretz], tabla auxiliar que permite unir los datos (ui) con los datos (ci), para visualizar las reglas.

tabshiny_pretz=matrix(0,nrow = 14,ncol = 2)
tabprofshiny_pretz_final=cbind(uiprodprofit,ciprodprofit)
name_aux=t(prod2[,1])
colnames(tabprofshiny_pretz_final)=cbind(name_aux,"rule_pretz")


for (i in 1:nrow(tabprofshiny_pretz_final)){
  
  for (j in 1:((ncol(tabprofshiny_pretz_final))-1)){
    
    if (tabprofshiny_pretz_final[i,j]==1 ){
      
      tabshiny_pretz[i,1]=as.matrix(prod2[j,1])
      tabshiny_pretz[i,2]=as.matrix(tabprofshiny_pretz_final[i,16])
      
      
    }
    if (tabprofshiny_pretz_final[i,j]== -1 ){
      
      tabshiny_pretz[i,1]=as.matrix(prod2[j,1])
      tabshiny_pretz[i,2]=as.matrix(tabprofshiny_pretz_final[j,16])*-1
      print(tabprofshiny_pretz_final[i,j])
      
      
    }
    
  }
}
colnames(tabshiny_pretz)=c("UPC","RULE")
tabshiny_pretz[10,2]=tabshiny_pretz[10,2]*-1
tabshiny_pretz<-as.data.frame(tabshiny_pretz)

###


### [profreal_aux], formatea el estilo de fecha en profit para que se pueda visualizar en la aplicación.

profreal_aux=profit
profreal_aux$WEEK_END_DATE<-as.character(profreal_aux$WEEK_END_DATE)
profreal_aux$WEEK_END_DATE<-as.Date(profreal_aux$WEEK_END_DATE,format="%Y-%m-%d")

###

### [difprecios_pretz], junta las fechas en que ocurrieron las diferencias de precios para que se pueda visualizar en la aplicación.

difprecios_pretz=cbind(tablaprecio[,1],difprecios)
colnames(difprecios_pretz)[1]="WEEK_END_DATE"
difprecios_pretz$WEEK_END_DATE<-as.character(difprecios_pretz$WEEK_END_DATE)
difprecios_pretz$WEEK_END_DATE<-as.Date(difprecios_pretz$WEEK_END_DATE,format="%Y-%m-%d")
difprecios_pretz=as.data.frame(difprecios_pretz)

###

### [auxp] y [auxp2], junta las diferencias de precios y los distintos "UPC" como caracteres para que puedan ser visualizados como opciones en la aplicación.


auxp=as.vector(colnames(difprecios))
auxp=as.data.frame(auxp)
colnames(auxp)[1]="NAMES"

auxp2=unique(dataset_p$SKU_PROD)
namessku=as.vector(auxp2)
auxp2=as.data.frame(auxp2)
colnames(auxp2)[1]="NAMES"

auxp3=rbind(auxp2,auxp)


### cbind [tablaprecio] con [difprecios]

precio_bindp=cbind(tablaprecio,difprecios_pretz[,2:226])
colnames(precio_bindp)[1]="WEEK_END_DATE"
precio_bindp$WEEK_END_DATE<-as.character(precio_bindp$WEEK_END_DATE)
precio_bindp$WEEK_END_DATE<-as.Date(precio_bindp$WEEK_END_DATE,format="%Y-%m-%d")
precio_bindp=as.data.frame(precio_bindp)

colnames(precio_bindp)[2:16]=namessku








###

ui <- fluidPage(
  
  titlePanel("Pricing Environment for Pretzels"),
  
  
  
  sidebarPanel(
    
    selectInput(inputId = "product", label = "View", choices = auxp3$NAMES),
    
    selectInput(inputId = "contour1", label = "PROD [X]", choices = auxp3$NAMES),
    
    selectInput(inputId = "contour2", label = "PROD2 [Y]", choices = auxp3$NAMES),
    
    
    
    
    
    ##Seleccionar dates con date input
    
    dateRangeInput(
      
      inputId = "date",
      
      label = "Select the date range",
      
      start =min(dataset_p$WEEK_END_DATE),
      
      end = max(dataset_p$WEEK_END_DATE),
      
      min = min(dataset_p$WEEK_END_DATE),
      
      max = max(dataset_p$WEEK_END_DATE),
      
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
    
    
    
    
    dataset_aux1=precio_bindp[between(precio_bindp$WEEK_END_DATE,min(input$date),max(input$date)),]
    histprice = precio_bindp[,input$product]
    
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
    
    dataset_auxx = dataset_p[dataset_p$SKU_PROD==input$product, ]
    
    
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
    
    dataset_auxx = dataset_p[dataset_p$SKU_PROD==input$product, ]
    
    
    
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
    
    dataset_auxx = dataset_p[dataset_p$SKU_PROD==input$product, ]
    
    dataset_auxx=dataset_auxx[between(dataset_auxx$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    profit_graph=profreal_aux
    
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
    
    
    
    dataset_aux3 = dataset_p[dataset_p$SKU_PROD==input$product, ]
    
    ##Probando con la función between
    
    
    
    dataset_aux3=dataset_aux3[between(dataset_aux3$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    profit_graph=profreal_aux
    
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
    tabshiny = tabshiny_pretz
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
    
    
    
    dataset_aux2 = dataset_p[dataset_p$SKU_PROD==input$product, ]
    
    
    
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
    
    
    
    dataset_aux6 =precio_bindp[between(precio_bindp$WEEK_END_DATE,min(input$date),max(input$date)),]
    
    profit_graph=profreal_aux
    
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




