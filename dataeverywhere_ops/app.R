library(shiny)
library(shinydashboard)
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
          box(),
          box()
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
    cat(paste(!is.null(input$round),": ",str(input$round),"\n"))
    if(!is.null(input$round)){
      browser()
      round <- input$round
      output$table <- renderTable({
      getContingencyTable(getRoundData(subset(dsDSRoundA, dsDSRoundA$Name==input$round)["ExtID"]))
      })
    }
  })
  
}

shinyApp(ui, server)