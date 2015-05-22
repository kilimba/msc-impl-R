library(shiny)
library(shinydashboard)
library(googleVis)
#source("dataeverywhere_ops/setup.R")
rounds <- as.vector(unique(dsDSRoundA$Name))

ui <- dashboardPage(
  dashboardHeader(title = "Data Operations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Households", tabName = "typeA", icon = icon("home")),
      menuItem("Individuals", tabName = "typeB",icon = icon("group")),
      menuItem("Verbal Autopsy", tabName = "typeC",icon = icon("plus-square"))
    ),
    tags$br(),
    uiOutput("choose_round")
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "typeA",
        fluidRow(
          box(tableOutput("table"), width = 12, title = "Document Statuses",
              status = "primary", solidHeader = TRUE, background = "black")
          
        ),
        fluidRow(
          box(title = "Expected Completion Date", status = "primary", solidHeader = TRUE, background = "black"),
          box(title = "Percent Archived", status = "primary", solidHeader = TRUE, background = "black",
            htmlOutput("gauge")
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
      round <- input$round
      
      output$table <- renderTable({
        getContingencyTable(getRoundData(subset(dsDSRoundA, dsDSRoundA$Name==input$round)["ExtID"]))
      })
      
      output$gauge <- renderGvis({
        percent <- round((getTotalArchived()/getTotalDocs())*100)
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
  
  
  
}

shinyApp(ui, server)