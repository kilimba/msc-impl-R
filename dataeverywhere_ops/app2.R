## app.R ##
library(shiny)
library(shinydashboard)
library(googleVis)

#source("dataeverywhere_ops/householdsetup.R")
#source("dataeverywhere_ops/individualsetup.R")
rounds <- as.vector(unique(dsDSRoundA$Name))

ui <- dashboardPage(
  dashboardHeader(title = "Data Operations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Households", tabName = "typeA", icon = icon("home")),
      menuItem("Individuals", tabName = "typeB",icon = icon("group")),
      menuItem("Verbal Autopsy", tabName = "typeC",icon = icon("plus-square"))
      
    ),
    tags$br(),
    uiOutput("choose_round"),
    uiOutput("choose_week")
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(htmlOutput("hhdcgauge"), width = 4, title = "Household Data Collection",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("hhdegauge"), width = 4, title = "Household Data Entry",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("hhdagauge"), width = 4, title = "Household Document Archiving",
              status = "primary", solidHeader = TRUE, background = "black")
          
        ),
        fluidRow(
          box(htmlOutput("inddcgauge"), width = 4, title = "Individual Data Collection",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("inddegauge"), width = 4, title = "Individual Data Entry",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("inddagauge"), width = 4, title = "Individual Document Archiving",
              status = "primary", solidHeader = TRUE, background = "black")
          
        ),
        
        fluidRow(
          box(htmlOutput("vaddcgauge"), width = 4, title = "Verbal Autopsy Data Collection",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("vaddegauge"), width = 4, title = "Verbal Autopsy Data Entry",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("vaddagauge"), width = 4, title = "Verbal Autopsy Document Archiving",
              status = "primary", solidHeader = TRUE, background = "black")
          
        ),
        
        fluidRow(
          box(title = "Household Refusals", status = "primary", solidHeader = TRUE, background = "black"),
          box(title = "Individual Contact Rate & Refusals", status = "primary", solidHeader = TRUE, background = "black"
          )
        )
      )
      
    )
    )
)

server <- function(input, output) { 
  
  output$choose_round <- renderUI({
    selectInput("round", "Select Round", rounds, width="95%")
  })
  
  observe({    
    if(!is.null(input$round)){
      roundData <<- getRoundData(subset(dsDSRoundA, dsDSRoundA$Name==input$round)["ExtID"])
    }
    
  observe({
    
    if(!is.null(input$round)){
      contingencyTable <- getContingencyTable(roundData,"household")
      
      output$hhdcgauge <- renderGvis({
        percent <- round((getTotalField("household")/getTotalDocs("household"))*100)
        label <- "Field"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$hhdegauge <- renderGvis({
        #browser()
        percent <- round((getTotalCaptured("household")/getTotalDocs("household"))*100)
        label <- "Captured"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$hhdagauge <- renderGvis({
        #browser()
        percent <- round((getTotalArchived("household")/getTotalDocs("household"))*100)
        label <- "Archived"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      #################################################################
      ###                           INDIVIDUAL                      ###
      
      indData <- getIndividualRoundData(getCurrentRound())
      indContingencyTable <- getContingencyTable(indData,"individual")
      
      output$inddcgauge <- renderGvis({
        percent <- round((getTotalArchived("individual")/getTotalDocs("individual"))*100)
        label <- "Field"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$inddegauge <- renderGvis({
        percent <- round((getTotalCaptured("individual")/getTotalDocs("individual"))*100)
        label <- "Captured"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$inddagauge <- renderGvis({
        percent <- round((getTotalArchived("individual")/getTotalDocs("individual"))*100)
        label <- "Archived"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
    }
  })

  
    
  })
}

shinyApp(ui, server)