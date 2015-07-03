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
    sidebarMenu(
      id = "tabs",
      menuItem("Households", tabName = "household", icon = icon("home")),
      menuItem("Individuals", tabName = "individual",icon = icon("group"))
    ),
    tags$br(),
    radioButtons("language", "Select Language", as.vector(language$language)),
    #selectInput("variable", "Select Variable", as.vector(unique(as.character(hhresponse$variable))))
    
    conditionalPanel(
      condition = "input.tabs == 'household'",
      uiOutput("choosevarhh")
    ),
    conditionalPanel(
      condition = "input.tabs == 'individual'",
      uiOutput("choosevarind")
    )
    
  ),
  dashboardBody(
    tags$head(get_assets_shiny(get_lib("nvd3"))[-3]),
    tabItems(
      tabItem(tabName = "household",
              fluidRow(
                infoBox("About", textOutput("caption"), icon = icon("info-circle"),width = 12)
              ),
              fluidRow(
                box(showOutput("areachart",lib="nvd3",add_lib = F),
                    width = 12,title = "Stacked Area Chart",status = "primary", solidHeader = TRUE)      
              ),
              fluidRow(
                box(dimpleOutput("barchart",height = 450), width = 12,title = "Stacked Bar Chart",
                    status = "primary", solidHeader = TRUE)
              )
      ),
      tabItem(tabName = "individual",
              fluidRow(
                infoBox("About", textOutput("indcaption"), icon = icon("info-circle"),width = 12)
              ),
              fluidRow(
                box(showOutput("indareachart",lib="nvd3",add_lib = F),
                    width = 12,title = "Stacked Area Chart",status = "primary", solidHeader = TRUE)      
              ),
              fluidRow(
                box(dimpleOutput("indbarchart",height = 450), width = 12,title = "Stacked Bar Chart",
                    status = "primary", solidHeader = TRUE)
              )
      )
    )
    
    
    
  )
)

server <- function(input, output) { 
  
  observe({
    if(input$tabs =="household"){
      
      output$choosevarhh <- renderUI({
        selectInput("hhvariable", "Select Variable", as.vector(unique(as.character(hhresponse$variable))))
      })
      
      observe({
        if(!is.null(input$hhvariable)){
          description <- input$language
          df <- getData(input$hhvariable,"household")
          
          output$areachart <-  renderChart2({    
            p2 <- nPlot(rate ~ Var1, 
                        group = description, 
                        data = df, 
                        type = 'stackedAreaChart',
                        height = 450,
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
            caption <- as.character(subset(hhvariables,hhvariables$variable == input$hhvariable)[,description])
            return(caption)
          })
        }
      })
    }else if(input$tabs =="individual"){
      output$choosevarind <- renderUI({
        selectInput("indvariable", "Select Variable", as.vector(unique(as.character(indresponse$variable))))
      })
      
      observe({
        if(!is.null(input$indvariable)){
          
          description <- input$language
          df <- getData(input$indvariable,"individual")
          
          output$indareachart <-  renderChart2({    
            p2 <- nPlot(rate ~ Var1, 
                        group = description, 
                        data = df, 
                        type = 'stackedAreaChart',
                        height = 450,
                        width = 1000 )
            p2$yAxis( axisLabel = "Percent (%)", width = 60 )
            return(p2)
          })
          
          output$indbarchart <- renderDimple({
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
          
          output$indcaption <- renderText({
            caption <- as.character(subset(indvariables,indvariables$variable == input$indvariable)[,description])
            return(caption)
          })
        }
      })
    }
    
    
  })
  
  
}

shinyApp(ui, server)
