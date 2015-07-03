## app.R ##
library(shiny)
library(shinydashboard)
library(googleVis)
library(rCharts)

source("dataeverywhere_ops/householdsetup.R")
source("dataeverywhere_ops/individualsetup.R")
source("dataeverywhere_ops/vasetup.R")

roundsA <- as.vector(unique(dsDSRoundA$Name))
roundsB <- as.vector(rev(sort(dsDSRoundB$ExtID)))
yearsOfDeath <- as.vector(dsYearOfDeath$YearOfDeath)
#vadata <<- getVAData()

getGaugeCaptured <- function(type){
  percent <- round((getTotalCaptured(type)/getTotalDocs(type))*100,digits = 2)
  label <- "Captured %"
  dat <- cbind(label,percent)
  df <- as.data.frame(dat, stringsAsFactors = FALSE)
  df$percent <- as.numeric(df$percent)
  gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                      greenTo=100, yellowFrom=30, yellowTo=70,
                                      redFrom=0, redTo=30))
  return(gauge)
}

getGaugeArchived <- function(type){
  percent <- round((getTotalArchived(type)/getTotalDocs(type))*100,digits = 2)
  label <- "Archived %"
  dat <- cbind(label,percent)
  df <- as.data.frame(dat, stringsAsFactors = FALSE)
  df$percent <- as.numeric(df$percent)
  gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                      greenTo=100, yellowFrom=30, yellowTo=70,
                                      redFrom=0, redTo=30))
  return(gauge)
}

getGaugeCollected <- function(type){
  percent <- round((getTotalField(type)/getTotalDocs(type))*100,digits = 2)
  label <- "Field %"
  dat <- cbind(label,percent)
  df <- as.data.frame(dat, stringsAsFactors = FALSE)
  df$percent <- as.numeric(df$percent)
  gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                      greenTo=100, yellowFrom=30, yellowTo=70,
                                      redFrom=0, redTo=30))
  return(gauge)
}

ui <- dashboardPage(
  dashboardHeader(title = "Data Operations"),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Households", tabName = "typeA", icon = icon("home")),
      menuItem("Individuals", tabName = "typeB",icon = icon("group")),
      menuItem("Verbal Autopsy", tabName = "typeC",icon = icon("plus-square"))
      
    ),
    tags$br(),
    #     uiOutput("choose_round"),
    conditionalPanel(
      condition = "input.tabs != 'dashboard'",
      uiOutput("choose_week")
    )
    
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
          box(htmlOutput("vadcgauge"), width = 4, title = "Verbal Autopsy Data Collection",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("vadegauge"), width = 4, title = "Verbal Autopsy Data Entry",
              status = "primary", solidHeader = TRUE, background = "black"),
          box(htmlOutput("vadagauge"), width = 4, title = "Verbal Autopsy Document Archiving",
              status = "primary", solidHeader = TRUE, background = "black")
          
        ),
        
        fluidRow(
          #box(title = "Household Refusals",width = 6, status = "primary", solidHeader = TRUE, background = "black"),
          box(showOutput("consentRate",lib="nvd3"), width = 12,title = "HIV Consent & Refusal Rate", status = "primary", solidHeader = TRUE
          )
        )
      )
      ,tabItem(tabName = "typeA",
               fluidRow(
                 box(uiOutput("choose_round_household"),title = "Select Round Week", width = 3,status = "primary", solidHeader = TRUE, background = "black"),
                 box(htmlOutput("hhdcgaugeweek"),width = 3, title = "HH Data Collection", status = "primary", solidHeader = TRUE, background = "black"),
                 box(htmlOutput("hhdegaugeweek"),width = 3, title = "HH Data Entry", status = "primary", solidHeader = TRUE, background = "black"),
                 box(htmlOutput("hhdagaugeweek"),width = 3, title = "HH Document Archiving", status = "primary", solidHeader = TRUE, background = "black")
                 
               ),  
               
               fluidRow(
                 box(tableOutput("tableHousehold"), title = "Household Document Statuses",width = 12, status = "primary", solidHeader = TRUE, background = "black")
                 
               )               
               
      ),
      tabItem(tabName = "typeB",
              fluidRow(
                box(uiOutput("choose_round_individual"),title = "Select Round Week", width = 3,status = "primary", solidHeader = TRUE, background = "black"),
                box(htmlOutput("inddcgaugeweek"),width = 3, title = "Individual Data Collection", status = "primary", solidHeader = TRUE, background = "black"),
                box(htmlOutput("inddegaugeweek"),width = 3, title = "Individual Data Entry", status = "primary", solidHeader = TRUE, background = "black"),
                box(htmlOutput("inddagaugeweek"),width = 3, title = "Individual Document Archiving", status = "primary", solidHeader = TRUE, background = "black")
                
              ),  
              
              fluidRow(
                box(tableOutput("tableIndividual"), title = "Individual Document Statuses",width = 12, status = "primary", solidHeader = TRUE, background = "black")
                
              )
      ),
      tabItem(tabName = "typeC",
              fluidRow(
                box(uiOutput("choose_year"),title = "Select Year", width = 3,status = "primary", solidHeader = TRUE, background = "black"),
                box(htmlOutput("vadcgaugeweek"),width = 3, title = "VA Data Collection", status = "primary", solidHeader = TRUE, background = "black"),
                box(htmlOutput("vadegaugeweek"),width = 3, title = "VA Data Entry", status = "primary", solidHeader = TRUE, background = "black"),
                box(htmlOutput("vadagaugeweek"),width = 3, title = "VA Document Archiving", status = "primary", solidHeader = TRUE, background = "black")
                
              ),       
              fluidRow(
                box(tableOutput("tableVA"), title = "Individual Document Statuses",width = 12, status = "primary", solidHeader = TRUE, background = "black")
                
              )      
              
      )
    )
  )
)

sharedValues <<- reactiveValues()

observe({
  
  invalidateLater(370000,NULL) # update once an hour  
  
  sharedValues$data <- tryCatch(
                    {
                      cat("started data update...\n")
                      #print(Sys.time(),"...\n")
                      sqlFetch(conn,"vacUnifiedReports")
                      
                    },
                    error = function(err) {   
                      print(paste("MY_ERROR:  ",err))
                      # Choose a return value in case of error
                      conn <- odbcReConnect(conn)
                      cat("re-established ODBC connection...\n")
                      sqlFetch(conn,"vacUnifiedReports")
                    })
                    cat("finished data update...\n")
})

server <- function(input, output) {
      
      observe({
        
      if(nrow(sharedValues$data) > 0){
        
        isolate({
          hhRoundData <- reactive({getRoundData(dsDSRoundA[1,]["ExtID"],sharedValues$data)})
          getContingencyTable(hhRoundData(),"household",period = "round")
          
          output$hhdcgauge <- renderGvis({
            hhRoundData()
            getGaugeCollected("household")
          }) 
          
          output$hhdegauge <- renderGvis({
            hhRoundData()
            getGaugeCaptured("household")
          }) 
          
          output$hhdagauge <- renderGvis({
            hhRoundData()
            getGaugeCaptured("household")
          })          
          
        })
        
        observe({
          if(input$tabs == "typeA"){
            cat("You are in household...\n")
          }else{
            cat("You are in ",input$tabs)
          }
        })
        
        
      }
      
  })

}


shinyApp(ui, server)