
  #dia 27 de diciembre



# Instalacion librerias:
#--------------------------------------------------------

setwd("C:/Users/usuario/Desktop/Practica Shiny")
Name<-read.csv("nombres.csv") # dataset de nombres, sino se puede generar aleatoriamente una letra (me parecia mas bonito asi)

library(shiny)
library(shinydashboard)
library(zoo)
library(random)
library(ggplot2)
library(DT)

#para el mapa:https://www.r-bloggers.com/calendar-charts-with-googlevis/  :

library(devtools)
library(googleVis)



##funciones que utilizare en shiny App
#-----------------------------------------------------

generatedata<-function(n){
  
  Birthday<-sample(seq(as.Date('2018/01/01'), as.Date('2018/12/31'), by="day"), n, replace=TRUE)
  muestra<-sample(nrow(Name),n)
  Name<-Name[muestra,1]
  Name2<-seq(1:n)
  data<-data.frame(Name, Birthday,Name2)
  return(data)
}


generatecoincidences<-function(matriz){
  
  cumple<-matriz[duplicated(matriz$Birthday),]
  cumple<-cumple$Birthday
  repetidos<-data.frame(Name="No existen coincidencias",Birthday="")
  repetidos2<-data.frame()
  if (length(cumple)!=0) {
    for (i in 1:length(cumple)){
      cumple2<-matriz[(matriz$Birthday==cumple[i]),]
      repetidos2<-rbind(repetidos2,cumple2)
    }
  repetidos<-repetidos2
  }
  colnames(repetidos) <- c("Name", "Same Birthday")
  return(repetidos)
}


  #Shiny App
#--------------

  ##UI

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title="Birthday Pradox"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Single", tabName="practicaB",icon=icon("cog", lib = "glyphicon")),
        menuItem("Multiple", tabName="practicaC",icon=icon("cog", lib = "glyphicon"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName="practicaB",
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("numero", "Population",min = 0, max = 365, value = 30, animate=T)
                    
                  ),
                  mainPanel(
                    tags$style("#texto {color: blue; font-size: 40px; font-style: italic;}"),
                    textOutput("texto"),
                    htmlOutput("grafica1")
                  )
                ),
                #tabsetPanel(
                  fluidRow(
                    column(5, "All", DT::dataTableOutput("all")),
                    column(7,"Coincidences",DT::dataTableOutput("coincidences"))
                  #)
                  
                )
                
        ),
        tabItem(tabName="practicaC",
                plotOutput("Grafica", height="500px")
        )
      )
    )
  )
)

  ##Server

server<-shinyServer(function(input, output){
  
  # Create the object with no values
  mydata_start <- generatedata(50) #nose poner renderValues, me da error
  #mydata_start<-renderValues(generatedata(input$numero))
  
  
  output$all<-DT::renderDataTable({
    DT::datatable(mydata_start[,c(1,2)], options = list(lengthChange = FALSE, class = 'cell-border', fixedHeader = TRUE))
  })
  
  output$coincidences<- DT::renderDataTable({
    coincidencias<-generatecoincidences(mydata_start)
    DT::datatable(coincidencias[,c(1,2)], options = list(lengthChange = FALSE, class = 'cell-border', fixedHeader = TRUE)) 
  })
  output$grafica1<-renderGvis({
    # al ejecutar el programa tarda unos segundos en salir
    # no deja que la segunda columna sea un nombre, tiene que ser un  numero
    gvisCalendar(data=mydata_start[,c(2,3)], datevar="Birthday", numvar="Name2",
                 options=list(
                   title="Birthday Calendar of the class",
                   height=200,
                   calendar="{yearLabel: { fontName: 'Calibri',
                   fontSize: 32, color: 'blue', bold: true},
                   cellSize: 10,
                   cellColor: { stroke: 'red', strokeOpacity: 0.2 },
                   focusedCellColor: {stroke:'blue'}}"))
    
    })
  
  
  output$texto<-renderText({
    if (nrow(generatecoincidences(mydata_start))!=1) {
    paste("There are", nrow(generatecoincidences(mydata_start)), "coincidences")}
    else{
      paste("There are 0 coincidences")}
  })
  
  
})  
  

####

shinyApp(ui = ui, server = server)

###
