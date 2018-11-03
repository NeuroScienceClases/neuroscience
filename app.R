#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(jsonlite)
library(curl)
base_series_mensuales <- read_excel("base series mensuales.xlsx")
# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Series del BCRP"),
   sidebarLayout(position = "left",
                 sidebarPanel(h3("Presentacion"),
                              h6("Se busca fomentar la manipulacion de datos con finalidad academica")),
                 mainPanel(
                        tabsetPanel(
                             
                             tabPanel("Busqueda del codigo", 
                           
                           DT::dataTableOutput("table")
                             ),
                           tabPanel("Grafico",
                                  fluidRow( 
                                    textInput("codigo1","codigo"),
                                    dateRangeInput('dateRange',label = "Pediode danalyse : ",format = "mm/yyyy",language="sp",start = Sys.Date(), end=Sys.Date(),startview = "year",separator = " - "),
                                    textOutput("SliderText"),
                                    textOutput("SliderText1"),
                                    actionButton("Graficar","serie de tiempo"),
                                    textOutput("url1"),
                                    plotOutput("distPlot", height = "600px")
                                    #,
                                    #textInput("codigo3","tipo")
                                            )
                                    )
                           
                           
                           
                           )
                 )
                 
                 
                 )
  
 # sidebarPanel(
    
 #   h2("Calculadora Microfinanzas")
 #   
 # ),

# mainPanel(
   
   #fluidRow(
   #tabsetPanel(
  # tabPanel("Series y Codigos",
   #         fluidRow(

 
  #)
 #  ),
 # tabPanel("graficos")

#  )
# ,position = c("left", "right"),fluid=FALSE)
#)
)


# Define server logic required to draw a histogram



server <- function(input, output) {
  library(jsonlite)
  library(curl)
  output$table <- DT::renderDataTable(DT::datatable({
    data <- base_series_mensuales }))
    
    Dates <- reactiveValues()
    observe({
      Dates$SelectedDates <- c(as.character(format(input$dateRange[1],format = "%Y-%m")),as.character(format(input$dateRange[2],format = "%Y-%m")))
      Dates$SelectedDates1 <- format(input$dateRange,format = "%d/%m/%Y")
    })
    
    output$SliderText <- renderText({Dates$SelectedDates})
    output$SliderText1 <- renderText({Dates$SelectedDates1})
 
    observe({
    
    x<-Dates$SelectedDates[1]
    y<-Dates$SelectedDates[2]   
    Dates$url <-paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api",input$codigo1,"json",x,y,sep="/")
    })
    observeEvent(input$Graficar,{ 

    #x<-Dates$SelectedDates[1]
   # y<-Dates$SelectedDates[2]   
   # Dates$url <-paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api",input$codigo1,"json",x,y,sep="/")
   
      
   
       Dates$firt_readlines <- readLines(Dates$url, warn = FALSE)

       Dates$test1<- fromJSON(Dates$firt_readlines)
   
       Dates$split <- matrix(unlist(strsplit(Dates$test1$periods$name, split = "[.]")),ncol=2,byrow=TRUE)
  
    
       Dates$cambio<-function(datos1){
      if (datos1=="Ene") {datos1="01"}
      else if (datos1=="Feb"){datos1="02"}
      else if (datos1=="Mar"){datos1="03"}
      else if (datos1=="Abr"){datos1="04"}
      else if (datos1=="May"){datos1="05"}
      else if (datos1=="Jun"){datos1="06"} 
      else if (datos1=="Jul"){datos1="07"} 
      else if (datos1=="Ago"){datos1="08"} 
      else if (datos1=="Sep"){datos1="09"} 
      else if (datos1=="Oct"){datos1="10"} 
      else if (datos1=="Nov"){datos1="11"} 
      else {datos1="12"}
      datos1
    }

    Dates$split[,1]<-unlist(lapply(Dates$split[,1],Dates$cambio))
    Dates$date<-c(paste("01 ", Dates$split[,1], Dates$split[,2]))
  
   lct <- Sys.getlocale("LC_TIME")
   Sys.setlocale("LC_TIME", "C")
   Sys.setlocale("LC_TIME", "C")
   Sys.setlocale("LC_TIME", lct)
   Dates$test1$periods$name<-as.Date(Dates$date,format="%d %m %Y")
   names(Dates$test1$periods)<-(Dates$test1$config$series$name)

   
   output$distPlot<-renderPlot({
     
     plot(Dates$test1$periods)
   
   })
    })
    output$SliderText1 <- renderText({Dates$url})
    }
#)

 
#}


shinyApp(ui = ui, server = server)


