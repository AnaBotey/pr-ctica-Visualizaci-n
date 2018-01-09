#dia 9 de Enero



# Instalacion librerias:
#--------------------------------------------------------

#setwd("C:/Users/usuario/Desktop/Practica Shiny/Practica")
setwd("C:/Users/usuario/Documents/GitHub/Birthday Paradox/pr-ctica-Visualizaci-n")
Name<-read.csv("nombres.csv") # dataset de nombres, sino se puede generar aleatoriamente una letra (me parecia mas bonito asi)

library(shiny)
library(shinydashboard)
library(zoo)
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

#===========================================================
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
#==============================================================

generategraphs<-function(bday,t){
  n<-nrow(bday)
  sim.mat = matrix(NA, nrow=n, ncol=2)
  for(i in 2:n){
    
    bday1=bday[seq(1,i),]
    bday.table = apply(bday1, 2, table)
    sim.2 = ifelse(unlist(lapply(bday.table, max) ) >=t, 1, 0)
    sim.mat[i,1] = i
    sim.mat[i,2] = sum(sim.2)/length(sim.2)
  }
  graph.sim = t( sim.mat[,2] )
  colnames(graph.sim) = sim.mat[,1]
  return(graph.sim)
}



#=========================================================
graphconsecutiveD<-function(bday){
  n<-nrow(bday)
  sim.mat = matrix(NA, nrow=n, ncol=2)
  for(i in 2:n){
    bday1=bday[seq(1,i),]
    bday.table=apply(bday1,2,unique)
    bday.table=apply(bday1,2,sort)
    bday.table1=apply(bday.table,2,function(x){c(0,diff(x))}) #primeras diferencias:cuento num 1
    bday.table1[bday.table1!=1]=0
    sim.2<-apply(bday.table1,2,max)
    sim.mat[i,1] = i
    sim.mat[i,2] = sum(sim.2)/ncol(bday1)
  }
  graph.sim = t( sim.mat[,2] )
  colnames(graph.sim) = sim.mat[,1]
  return(graph.sim)
}
#==============================================================
generateMuestraGraphs<-function(n.rep,n){
  
  doy = seq(from=1, to=365, by=1)
  for(i in 2:n){
    bday = replicate(n.rep, sample(doy, size=i, replace=T) )
  }
  return(bday)
}

#===============================================================

#Shiny App
#--------------

##UI

ui<-shinyUI(
  dashboardPage(
    dashboardHeader(title="Birthday Paradox"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Single", tabName="practicaB",icon=icon("cog", lib = "glyphicon")),
        menuItem("Multiple", tabName="practicaC",icon=icon("cog", lib = "glyphicon"))
      )
    ),
    dashboardBody(
      #includeCSS("www/bootstrap1.css"),
      tabItems(
        tabItem(tabName="practicaB",
                sidebarLayout(
                  sidebarPanel(style="font-style: Italic;",
                               sliderInput("numero", "Population",min = 0, max = 365, value = 30, animate=T)
                  ),
                  mainPanel(style="text-align: center;",
                            tags$style("#texto {color: blue; font-size: 40px; font-style: Italic;}"),
                            textOutput("texto"),
                            htmlOutput("grafica1")
                  )
                ),
                fluidRow(style="margin-left:6vw;",
                         column(5, align="center",DT::dataTableOutput("all")),
                         column(7, align="center", DT::dataTableOutput("coincidences"))
                         
                )
                
        ),
        tabItem(tabName="practicaC",
                sidebarPanel(style="font-style: Italic;",
                             numericInput("numSim", "Number of simulations", 100),
                             numericInput("numPeople", "Number of people", 100),
                             verbatimTextOutput("value"),
                             actionButton("Go", "Run")
                ),
                mainPanel(style="text-align: center;",
                          tags$style("#texto3 {color: blue; font-size: 40px; font-style: Italic;}"),
                          textOutput("texto3"),
                          plotOutput("plot")
                          
                ),
                fluidRow(style="margin-left:6vw;",
                         tabsetPanel(
                           tabPanel("Triple coincidence", plotOutput("grafica2", width = "70%", height = 350)),
                           tabPanel("Four coincidence", plotOutput("grafica3", width = "70%", height = 350)),
                           tabPanel("Consecutive Days", plotOutput("grafica4", width = "70%", height = 350))
                         )
                )
        )
      )
    )
  )
)

##Server

server<-shinyServer(function(input, output){
  
  
  
  mydata_start<-reactive(generatedata(input$numero))
  
  #Simple
  #------
  output$all<-DT::renderDataTable({
    DT::datatable(mydata_start()[,c(1,2)], options = list(lengthChange = FALSE, class = 'cell-border', fixedHeader = TRUE))
  })
  
  output$coincidences<- DT::renderDataTable({
    coincidencias<-generatecoincidences(mydata_start())
    DT::datatable(coincidencias[,c(1,2)], options=list(lengthChange = F,class = 'cell-border', fixedHeader = TRUE)) 
  })
  
  output$grafica1<-renderGvis({
    # al ejecutar el programa tarda unos segundos en salir
    # no deja que la segunda columna sea un nombre, tiene que ser un  numero
    gvisCalendar(data=mydata_start()[,c(2,3)], datevar="Birthday", numvar="Name2",
                 options=list(
                   title="Birthday Calendar of the class",
                   height=150,
                   calendar="{yearLabel: { fontName: 'Italic',
                   fontSize: 32, color: 'blue', bold: true},
                   cellSize: 10.5,
                   cellColor: { stroke: 'blue', strokeOpacity: 0.1 },
                   focusedCellColor: {stroke:'lightblue'}}"))
    
    
  })
  
  
  output$texto<-renderText({
    if (nrow(generatecoincidences(mydata_start()))!=1) {
      numfilas<-generatecoincidences(mydata_start())
      paste("There are", length(unique(numfilas[,2])), "coincidences", "\n")}
    else{
      paste("There are 0 coincidences",  "\n") }
  })
  
  # Multiple
  #---------
  
  # output$value <- renderText({ paste(input$numsimulations)})
  bday<-eventReactive(input$Go, {
    generateMuestraGraphs(input$numSim, input$numPeople)
  })
  
  output$plot<-renderPlot({
    graph.sim<-as.vector(generategraphs(bday(),2))
    vect<-sort(na.omit(graph.sim[graph.sim>=0.5]))
    a<-which (graph.sim==vect[1])
    ggplot(data.frame(graph.sim),aes(x = seq_along(graph.sim), y = graph.sim)) + geom_step() +
      ggtitle("Probability of 2 people have the same birthday")+
      xlab("Number of people")+ ylab("Probability")+
      geom_vline(xintercept = a, colour = "blue")+
      geom_hline(yintercept = 0.5,colour = "blue") +coord_cartesian(ylim = c(0, 1))+
      scale_y_continuous(breaks = c(0,1,0.5))+scale_x_continuous(breaks = c(0,input$numero,a))+
      theme(plot.title = element_text(color="black",size=18,hjust = 0.5),
            axis.text.y = element_text(color="blue",size=12,hjust=1),
            axis.text.x = element_text(color="darkred",size=12,hjust=1,vjust=1),
            axis.title.x = element_text(color="darkred", size=14),
            axis.title.y = element_text(size=14))+
      coord_cartesian(ylim = c(0, 1))
  }) 
  
  output$grafica2<-renderPlot({
    
    graph.sim<-as.vector(generategraphs(bday(),3))
    vect<-sort(na.omit(graph.sim[graph.sim>=0.5]))
    a<-which (graph.sim==vect[1])
    ggplot(data.frame(graph.sim),aes(x = seq_along(graph.sim), y = graph.sim)) + geom_step() +
      ggtitle("Probability of 2 people have the same birthday")+
      xlab("Number of people")+ ylab("Probability")+
      geom_vline(xintercept = a, colour = "blue")+
      geom_hline(yintercept = 0.5,colour = "blue") +coord_cartesian(ylim = c(0, 1))+
      scale_y_continuous(breaks = c(0,1,0.5))+scale_x_continuous(breaks = c(0,input$numero,a))+
      theme(plot.title = element_text(color="black",size=18,hjust = 0.5),
            axis.text.y = element_text(color="blue",size=12,hjust=1),
            axis.text.x = element_text(color="darkred",size=12,hjust=1,vjust=1),
            axis.title.x = element_text(color="darkred", size=14),
            axis.title.y = element_text(size=14))+
      coord_cartesian(ylim = c(0, 1))
    
  })
  
  output$grafica3<-renderPlot({
    
    graph.sim<-as.vector(generategraphs(bday(),4))
    vect<-sort(na.omit(graph.sim[graph.sim>=0.5]))
    a<-which (graph.sim==vect[1])
    ggplot(data.frame(graph.sim),aes(x = seq_along(graph.sim), y = graph.sim)) + geom_step() +
      ggtitle("Probability of 2 people have the same birthday")+
      xlab("Number of people")+ ylab("Probability")+
      geom_vline(xintercept = a, colour = "blue")+
      geom_hline(yintercept = 0.5,colour = "blue") +coord_cartesian(ylim = c(0, 1))+
      scale_y_continuous(breaks = c(0,1,0.5))+scale_x_continuous(breaks = c(0,input$numero,a))+
      theme(plot.title = element_text(color="black",size=18,hjust = 0.5),
            axis.text.y = element_text(color="blue",size=12,hjust=1),
            axis.text.x = element_text(color="darkred",size=12,hjust=1,vjust=1),
            axis.title.x = element_text(color="darkred", size=14),
            axis.title.y = element_text(size=14))+
      coord_cartesian(ylim = c(0, 1))
  })
  output$grafica4<-renderPlot({
    
    graph.sim<-as.vector(graphconsecutiveD(bday()))
    vect<-sort(na.omit(graph.sim[graph.sim>=0.5]))
    a<-which (graph.sim==vect[1])
    ggplot(data.frame(graph.sim),aes(x = seq_along(graph.sim), y = graph.sim)) + geom_step() +
      ggtitle("Probability of consecutive birthday")+
      xlab("Number of people")+ ylab("Probability")+
      geom_vline(xintercept = a, colour = "blue")+
      geom_hline(yintercept = 0.5,colour = "blue") +coord_cartesian(ylim = c(0, 1))+
      scale_y_continuous(breaks = c(0,1,0.5))+scale_x_continuous(breaks = c(0, input$numero,a))+
      theme(plot.title = element_text(color="black",size=18,hjust = 0.5),
            axis.text.y = element_text(color="blue",size=12,hjust=1),
            axis.text.x = element_text(color="darkred",size=12,hjust=1,vjust=1),
            axis.title.x = element_text(color="darkred", size=14),
            axis.title.y = element_text(size=14))+
      coord_cartesian(ylim = c(0, 1))
    
  }) 
  output$texto3<-renderText({
    paste("Run", input$numSim," simulations per", input$numPeople,"people")
  }) 
  
  
})  


####

shinyApp(ui = ui, server = server)

###
