# instalar paquete readr
# install.packages("readr")
# cargar paquete readr
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
library("readr")
library("readxl")
library(ggpubr)
####
semanal_com <- read_csv("C:/Users/Esteban/OneDrive/Escritorio/TID/semanal_com.csv", 
                        col_types = cols(fecha = col_date(format = "%Y-%m-%d")))
CL_ID_LOCAL <- read_excel("C:/Users/Esteban/OneDrive/Escritorio/TID/CL-ID_LOCAL.xlsx")
costos1 <- read_excel("C:/Users/Esteban/OneDrive/Escritorio/TID/bd_costos_20221114.xlsx")
costos2 <- read_excel("C:/Users/Esteban/OneDrive/Escritorio/TID/bd_costos_20221013.xlsx")
costos3 <- read_excel("C:/Users/Esteban/OneDrive/Escritorio/TID/bd_costos_20221114.xlsx")
inventario1 <- read_delim("C:/Users/Esteban/OneDrive/Escritorio/TID/bd_inventario_202204_202209_J0101020403 (1).csv", 
                          delim = "|", escape_double = FALSE, trim_ws = TRUE)
inventario2 <- read_delim("C:/Users/Esteban/OneDrive/Escritorio/TID/bd_inventario_202210_20221114_J0101060101_J0101020403.csv", 
                          delim = "|", escape_double = FALSE, trim_ws = TRUE)
inventario3 <- read_csv("C:/Users/Esteban/OneDrive/Escritorio/TID/CL-Input_Elasticidad_prueba-Input_Elasticidad_PGC-FLC_J0101020403_ATUN_parte1.csv")
inventario4 <- read_csv("C:/Users/Esteban/OneDrive/Escritorio/TID/CL-Input_Elasticidad_prueba-Input_Elasticidad_PGC-FLC_J0101020403_ATUN_parte2.csv")
inventario5 <- read_csv("C:/Users/Esteban/OneDrive/Escritorio/TID/CL-Input_Elasticidad_prueba-Input_Elasticidad_PGC-FLC_J0101020403_ATUN_parte3.csv")
inventario6 <- read_csv("C:/Users/Esteban/OneDrive/Escritorio/TID/CL-Input_Elasticidad_prueba-Input_Elasticidad_PGC-FLC_J0101020403_ATUN_parte4.csv")
fechas <- read_csv("C:/Users/Esteban/OneDrive/Escritorio/TID/fechas2.csv")
inventario1$...1 <- NULL
inventario2$...1 <- NULL
costos1$...1 <- NULL
costos3$...1 <- NULL
# costosdef <- rbind(costos1,costos2,costos3)
costosdef <- costos3 %>%
  filter(cod_subclase=="J0101020403")
semanal_def <- merge(semanal_com,CL_ID_LOCAL)
semanal_def <- merge(semanal_def,costosdef,by="cod_sku")
semanal_def$desc_subclase.y <- NULL
semanal_def$cod_subclase.x <- NULL
semanal_def$cod_subclase.y <- NULL
semanal_def$desc_sku.y <- NULL

atun_inicio <- semanal_def  %>%
  group_by(fecha,semana,cod_sku,id_localfisico) %>%
  summarise(semana,
            fecha = min(fecha),
            cod_sku,
            id_localfisico,
            precio_total,
            suma_tot = sum(suma_unidadesventa),
            precio_prom = mean(precio),
            venta_cat = sum(suma_montoventa),
            profit = sum(suma_montoventa-(precio_costo*suma_unidadesventa)),
            desc_sku.x,
            desc_local)

auxi <- unique(semanal_def$cod_sku)
namessku <- as.vector(auxi)
auxi <- as.data.frame(auxi)
colnames(auxi)[1]="Nombres"

auxi2 <- unique(semanal_def$desc_sku.x)
namessku <- as.vector(auxi2)
auxi2 <- as.data.frame(auxi2)
colnames(auxi2)[1]="Nombres"

auxi3 <- unique(semanal_def$desc_local)
namessku <- as.vector(auxi3)


auxi3 <- as.data.frame(auxi3)
colnames(auxi3)[1]="nombre_local"

auxi4 <- rbind(auxi2,auxi) 




ui <- fluidPage(

  titlePanel("Precio atun"),



  sidebarPanel(

    selectInput(inputId = "product", label = "View", choices = auxi4$Nombres),
    
    selectInput(inputId = "product_y", label = "View_2", choices = auxi4$Nombres, selected = 'TRIPACK ATUN LOMITO ACEITE TOTTUS 80GX3U' ),
    
    selectInput(inputId = "nombrelocal", label = "Nombre Local", choices = auxi3$nombre_local, selected = 'San Bernardo Plaza' ),





    ##Seleccionar dates con date input

    dateRangeInput(

      inputId = "date",

      label = "Select the date range",

      start =min(semanal_com$fecha),

      end = max(semanal_com$fecha),

      min = min(semanal_com$fecha),

      max = max(semanal_com$fecha),

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
     # 
     plotOutput(outputId = "distPlot2"),
     # 
     plotlyOutput(outputId = "distPlot3"),
     # 
     plotlyOutput(outputId = "distPlot4"),
     # 
     plotlyOutput(outputId = "distPlot5"),
     
     plotOutput(outputId = "distPlot6")




  )



)
server<-function(session,input,output){

  ###Primera tabla resumen
  output$sum <- renderTable({
    atun_3 <- atun_inicio  %>%
      filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    atun_3=data.table(atun_3,profit_bin = (atun_3$profit>mean(atun_3$profit))*1)
    
    precio_maximo <- max(atun_3$precio_prom)
    precio_minimo <- min(atun_3$precio_prom)
    precio_promedio <- mean(atun_3$precio_prom)
    df <- data.frame(A=c('Max', 'Min', 'Mean'),
                     Valor=c(precio_maximo, precio_minimo, precio_promedio))
    df

  })
  
  ###Segunda tabla resumen
  
  output$sum2 <- renderTable({
    
    atun_5 <- atun_inicio  %>%
    filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    demanda_maxima <- max(atun_5$suma_tot)
    demanda_minima <- min(atun_5$suma_tot)
    demanda_promedio <- mean(atun_5$suma_tot)
    df2 <- data.frame(A=c('Max', 'Min', 'Mean'),
                     Valor=c(demanda_maxima, demanda_minima, demanda_promedio))
    df2
    
  })
  
  
  ###Tercera tabla resumen (PROFIT, USA UN DATAFRAME DISTINTO)
  output$sum3 <- renderTable({
    
    atun_6 <- atun_inicio  %>%
    filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    profit_maximo <- max(atun_6$profit)
    profit_minimo <- min(atun_6$profit)
    profit_promedio <- mean(atun_6$profit)
    df3 <- data.frame(A=c('Max', 'Min', 'Mean'),
                     Valor=c(profit_maximo, profit_minimo, profit_promedio))
    df3
    
  })


  output$distPlot <- renderPlot({
    
    atun_7 <- atun_inicio  %>%
      filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    min_price<-min(atun_7$precio_prom)
    mean_price<-mean(atun_7$precio_prom)
    max_price<-max(atun_7$precio_prom)
    
    
    ggplot(data = atun_7, aes(x = precio_prom)) +
      geom_density()+ xlab(input$product) + ylab('Densidad') +
      geom_vline(xintercept = min_price,
                 color = "blue",
                 linetype = "dashed")+
        ggplot2::annotate("text",
                          min_price,
                          min_price - 2,
                          label = "Minimum",
                          color = "blue")+
      geom_vline(xintercept = mean_price,
                 color = "darkred",
                 linetype = "dashed")+
        ggplot2::annotate("text",
                          mean_price,
                          mean_price - 2,
                          label = "Mean",
                          color = "darkred")+
      geom_vline(xintercept = max_price,
                 color = "#6b8e23",
                 linetype = "dashed")+
        ggplot2::annotate("text",
                          max_price,
                          max_price - 2,
                          label = "Maximum",
                          color = "#6b8e23")})
    
  output$distPlot2 <- renderPlot({
    atun_8 <- atun_inicio  %>%
      filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    ggplot(atun_8, aes(x=log(precio_prom), y=log(suma_tot))) + geom_point()+geom_smooth(formula = y ~ x, method=lm)+xlab(paste('LOG PRECIO',input$product))+
      ylab(paste('LOG DEMANDA',input$product))+stat_regline_equation(label.y = 4, aes(label = ..eq.label..))})

    
    
  output$distPlot3 <- renderPlotly({ 
    atun_9 <- atun_inicio  %>%
      filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    atun_9 <- data.frame(fecha=atun_9$fecha, precio_prom=atun_9$precio_prom, suma_tot=atun_9$suma_tot)
    p <- plot_ly(atun_9) %>%
      add_trace(x = ~fecha, y = ~suma_tot, type = 'bar', name = 'Demand',
                marker = list(color = 'blue'),
                hoverinfo = "text",
                text = ~paste(Demand, 'UN')) %>%
      add_trace(x = ~fecha, y = ~precio_prom, type = 'scatter', mode = "lines",
                name = 'Price', yaxis = 'y2',
                line = list(color = 'red'),
                # hoverinfo = "text",
                text = ~paste(Price, '$')) %>%
      layout(title = 'Price v/s Demand on selected Weeks',
             xaxis = list(title = ""),
             yaxis = list(side = 'left', title = 'Demand[UN]', showgrid= FALSE, zeroline = FALSE),
             yaxis2 = list(side = 'right', overlaying = "y", title = 'Price [$]', showgrid = FALSE,zeroline = FALSE))
    
    
    
    
    p
    
    
    
    
    p})

  output$distPlot4<-renderPlotly({
    
    atun_10 <- atun_inicio  %>%
      filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    productos <- data.frame(sort(unique(semanal_def$desc_sku.x)))
    regla1 <- data.frame(sample(2100:2200,30))
    datarules <- cbind(productos,regla1)
    colnames (datarules) <- c ('productos','regla1')
    rule <- datarules%>%
      filter(productos==input$product)
    rule1=as.numeric(rule[2])
    
    ggplot(atun_10, aes(x=precio_total, y=profit)) +geom_point() +
      geom_smooth(method=loess)+ scale_y_continuous(labels = label_dollar())
    
    
  })
  
  output$distPlot5<-renderPlotly({
    
    
    atun12 <- semanal_def  %>%
      group_by(semana,desc_local) %>%
      summarise(profit = sum(suma_montoventa-(precio_costo*suma_unidadesventa)))
    
    atun13 <- semanal_def  %>%
      summarise(precio_total,semana,desc_sku.x,desc_local) %>%
      filter(desc_local==input$nombrelocal)
    atun14 <- merge(atun12,atun13,by=c("semana", "desc_local"))
    p <- plot_ly(
      atun14,
      x=~atun14[desc_sku.x == input$product,"precio_total"],
      y=~atun14[desc_sku.x == input$product_y,"precio_total"],
      z=~profit,
      type = 'contour'
      )%>%
      colorbar(title = "Profit")%>%
      layout(title = 'Profit', plot_bgcolor = "#e5ecf6", xaxis = list(title = paste('PRECIO TOTAL', input$product)),
             yaxis = list(title = paste('PRECIO TOTAL', input$product_y)))
    
    p
    
     })
  
  output$distPlot6 <- renderPlot({
    atun_11 <- atun_inicio  %>%
        filter(desc_sku.x == input$product, desc_local==input$nombrelocal)
    atun_11=data.table(atun_11,profit_bin = (atun_11$profit>mean(atun_11$profit))*1)
    atun_11$profit_bin<-  as.factor(atun_11$profit_bin)
    ggplot(atun_11, aes(x=precio_prom, fill=profit_bin)) + geom_density(alpha=0.4)
    
    
    })

}
shinyApp(ui,server)


