## app.R ##
library(shiny)
library(shinydashboard)
library(rCharts)
library(rcdimple)

source("util.R")
suppressMessages(
  singleton(
    addResourcePath(
      get_lib("nvd3")$name
      ,get_lib("nvd3")$url
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(
    title = "Data Everywhere: Community Portal",
    titleWidth = 450
    ),
  dashboardSidebar(
    radioButtons("language", "Select Language", as.vector(language$language)),
    selectInput("variable", "Select Variable", as.vector(unique(as.character(hhresponse$variable))))
          ),
  dashboardBody(
    tags$head(get_assets_shiny(get_lib("nvd3"))[-3]),
    
    fluidRow(
      infoBox("About", textOutput("caption"), icon = icon("info-circle"),width = 12)
    ),
    fluidRow(
      box(showOutput("areachart",lib="nvd3",add_lib = F),
          width = 12,title = "Stacked Area Chart",status = "primary", solidHeader = TRUE)      
    )
    ,
    fluidRow(
      box(dimpleOutput("barchart",height = 250), width = 12,title = "Stacked Bar Chart",
          status = "primary", solidHeader = TRUE)
    )
    
  )
)

server <- function(input, output) { 
  
  observe({
    browser()
    if(!is.null(input$variable)){
#        browser()
      description <- input$language
      df <- getData(input$variable)
      
      output$areachart <-  renderChart2({    
        p2 <- nPlot(rate ~ Var1, 
                    group = description, 
                    data = df, 
                    type = 'stackedAreaChart',
                    height = 250,
                    width = 1000 )
        p2$yAxis( axisLabel = "Percent (%)", width = 60 )
        return(p2)
      })
      
      output$barchart <- renderDimple({
        d1 <- dimple(
          x = "Var1", 
          y = "rate", 
          groups = description, 
          data = df, 
          type = 'bar')
        
        d1 <- xAxis(d1, orderRule = "Var1" )
        d1 <- yAxis(d1, type = "addPctAxis")
        
        return(d1)
      })
      
      output$caption <- renderText({
        caption <- as.character(subset(hhvariables,hhvariables$variable == input$variable)[,description])
        return(caption)
      })
    }
  })
  
  
}

shinyApp(ui, server)
