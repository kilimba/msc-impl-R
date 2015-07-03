## app.R ##
library(shiny)
library(shinydashboard)
library(curl)
library(RODBC)
library(rCharts)
library(googleVis)


source("dataeverywhere_ops/householdsetup.R")
source("dataeverywhere_ops/individualsetup.R")
source("dataeverywhere_ops/vasetup.R")

roundsA <- as.vector(unique(dsDSRoundA$Name))
roundsB <- as.vector(rev(sort(dsDSRoundB$ExtID)))
yearsOfDeath <- as.vector(dsYearOfDeath$YearOfDeath)

conn <<- odbcConnect("unifiedReports")

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

ui <- dashboardPage(
  dashboardHeader(title = "Data Operations"),
  dashboardSidebar(
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Households", tabName = "typeA", icon = icon("home")),
      menuItem("Individuals", tabName = "typeB",icon = icon("group")),
      menuItem("Verbal Autopsy", tabName = "typeC",icon = icon("plus-square")),
      menuItem("Charts", tabName = "charts", icon = icon("line-chart"))
      
    ),
    tags$br(),
    #     uiOutput("choose_round"),
    conditionalPanel(
      condition = "input.tabs != 'dashboard' & input.tabs != 'typeC' & input.tabs != 'charts'",
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
          infoBox("About", textOutput("caption"), icon = icon("info-circle"),width = 12)
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
              
      ),
      tabItem(tabName = "charts",
              fluidRow(
                box(showOutput("consentRate",lib="nvd3"), width = 12,title = "HIV Consent & Refusal Rate", 
                    status = "primary", solidHeader = TRUE)                
              ),
              fluidRow(
                box(showOutput("contactRate",lib="nvd3"), width = 12,title = "HIV Contact Rate", 
                    status = "primary", solidHeader = TRUE)                
              )
              , 
              fluidRow(
                box(showOutput("bsRefusalRate",lib="nvd3"), width = 12,title = "Bounded Structure Refusal Rate", 
                    status = "primary", solidHeader = TRUE)                
              )
      )
    )
  )
)

sharedValues <- reactiveValues()

dataSetupImpl <- function(){
  print(paste("actual update time:",Sys.time(),"\n"))
  cat("checking for updates...\n")
  
  sharedValues$data <- tryCatch({
    sqlFetch(conn,"vacUnifiedReports")
  },  error = function(e) {
    print(paste("SQL CONNECTION ERROR:  ",e))
    conn <<- odbcReConnect(conn)
    cat("re-established ODBC connection...\n")
    sqlFetch(conn,"vacUnifiedReports")
  })
    
  sharedValues$vacvaUnifiedReportDeaths <- getvacvaUnifiedReportDeaths(conn)
  # HOUSEHOLD DATA SETUP
  sharedValues$hhRoundData <- getRoundData(dsDSRoundA[1,]["ExtID"],sharedValues$data)
  getContingencyTable(sharedValues$hhRoundData,"household","round")
  # INDIVIDUAL DATA SETUP
  sharedValues$indRoundData <- getIndividualRoundData(getCurrentRound(),sharedValues$data)
  getContingencyTable(sharedValues$indRoundData,"individual","round")
  # VERBAL AUTOPSY DATA SETUP
  
  
  sharedValues$vadata <- getVAData(sharedValues$data,sharedValues$vacvaUnifiedReportDeaths)
  vaDataForYear <- subset(sharedValues$vadata,sharedValues$vadata$YearOfDeath == yearsOfDeath[1])
  getContingencyTable(vaDataForYear,"va","round")
  
  # HOUSEHOLD DATA GAUGES 
  sharedValues$hhGaugeCollected <- getGaugeCollected("household")
  sharedValues$hhGaugeCaptured <- getGaugeCaptured("household")
  sharedValues$hhGaugeArchived <- getGaugeArchived("household")
  # INDIVIDUAL DATA GAUGES 
  sharedValues$indGaugeCollected <- getGaugeCollected("individual")
  sharedValues$indGaugeCaptured <- getGaugeCaptured("individual")
  sharedValues$indGaugeArchived <- getGaugeArchived("individual")
  # VERBAL AUTOPSY DATA GAUGES
  sharedValues$vaGaugeCollected <- getGaugeCollected("va")
  sharedValues$vaGaugeCaptured <- getGaugeCaptured("va")
  sharedValues$vaGaugeArchived <- getGaugeArchived("va")
  
  # update lastUpdate time
  sharedValues$lastupdate <- Sys.time()
  cat("finished data updates...\n")
  
}

dataSetup <- function(){
  #browser()
  
  print(paste("attempted update time:",Sys.time(),"\n"))
  
  if(is.null(sharedValues$lastupdate)){
    dataSetupImpl()    
  }
  # check if last update is at least 60 min ago
  else if(abs(as.numeric(difftime(sharedValues$lastupdate,Sys.time()),units = "mins" )) > 60){
      
      diff <- abs(as.numeric(difftime(sharedValues$lastupdate,Sys.time()),units = "mins" ))
      cat(diff,"minutes since last update...\n")
      
      dataSetupImpl()     
    }
  else{
    diff <- abs(as.numeric(difftime(sharedValues$lastupdate,Sys.time()),units = "mins" ))
    cat("current time:",Sys.time(),"\n")
    cat("last update time:",sharedValues$lastupdate,"\n")
    cat(diff,"minutes since last update...\n")
    #stop("less than 60 minutes have elapsed...",error =NULL)
    cat("less than 60 minutes have elapsed...\n")
    return()
  }
  
  
}

observe({  
  invalidateLater(3600000,NULL)  # run again in one hour 
  dataSetup()  
})

server <- function(input, output, session) {
  
  
  observe({
    # If selected "dashboard" screen:
    if(input$tabs == "dashboard"){
      
      cat("main dashboard tab selected...\n")
      
      # household gauges
      output$hhdcgauge <- renderGvis({
        sharedValues$hhGaugeCollected
      }) 
      
      output$hhdegauge <- renderGvis({
        sharedValues$hhGaugeCaptured
      }) 
      
      output$hhdagauge <- renderGvis({
        sharedValues$hhGaugeArchived
      }) 
      ##############################
      # individual gauges
      output$inddcgauge <- renderGvis({
        sharedValues$indGaugeCollected
      })
      
      output$inddegauge <- renderGvis({
        sharedValues$indGaugeCaptured
      }) 
      
      output$inddagauge <- renderGvis({
        sharedValues$indGaugeArchived
      }) 
      ###############################
      # verbal autopsy gauges
      output$vadcgauge <- renderGvis({
        sharedValues$vaGaugeCollected
      })
      
      output$vadegauge <- renderGvis({
        sharedValues$vaGaugeCaptured
      }) 
      
      output$vadagauge <- renderGvis({
        sharedValues$vaGaugeArchived
      })
      ###############################
      # INFO BOX
      
      output$caption <- renderText({
        return("You are currently viewing progress gauges for data operations activities in the current round")
      })
      ###############################
      # consent data
      output$consentRate <-  renderChart2({    
        p2 <- nPlot(HIVConsentRate ~ YEAR, 
                    group = 'HIVRefused', 
                    data = consentData, 
                    type = 'stackedAreaChart',
                    height = 250,
                    width = 1000 )
        p2$yAxis( axisLabel = "Percent (%)", width = 40 )
        return(p2)
      })
      
      output$contactRate <-  renderChart2({    
        p2 <- nPlot(HIVContactRate ~ YEAR,                    
                    data = contactData, 
                    type = 'stackedAreaChart',
                    group = 'Contacted',
                    height = 250,
                    width = 1000 )
        p2$yAxis( axisLabel = "Percent (%)", width = 40 )
        return(p2)
      })
      
      output$bsRefusalRate <-  renderChart2({    
      p2 <- nPlot(RefusalRate ~ YEAR,                    
                  data = BSRefusalData, 
                  type = 'lineChart',                    
                  height = 250,
                  width = 1000 )
        p2$yAxis( axisLabel = "Refusals per 100,000", width = 40 )
        p2$chart(useInteractiveGuideline = "true", transitionDuration = 500)  
        return(p2)
      })
      
    }
    
    # If selected "household" screen:
    else if(input$tabs) == "typeA"){
      
      cat("household tab selected...\n")
      
      # Dynamic creation of select input for selecting round of interest
      # in Household page
      output$choose_round_household <- renderUI({
        selectInput("roundhouse", "Select Round", roundsA)
      })
      
      # Dynamic creation of select input for selecting week of interest   
      output$choose_week <- renderUI({
        selectInput("week", "Select Week", getWeeks(),width = '90%')
      })
      
      observe({
        
        if(!is.null(input$roundhouse)){
          
          
          if(!is.null(input$week)){
            # Show Progress Bar
            withProgress(message = 'LOADING DATA...', value = 0, {
              
              n <- 9            
              
              # Increment the progress bar, and update the detail text.
              incProgress(3/n, detail = paste("LOADING DATA... 30%"))
              roundData <- getRoundData(subset(dsDSRoundA,dsDSRoundA$Name==input$roundhouse)["ExtID"],sharedValues$data)
              incProgress(3/n, detail = paste("LOADING DATA... 60%"))
              weekData <- getRoundDataPerWeek(roundData, input$week)
              incProgress(3/n, detail = paste("DATA LOADED... 100%"))         
              
            })
            
            
            
            getContingencyTable(data = weekData,type = "household",period = "week")
            
            output$hhdcgaugeweek <- renderGvis({              
              getGaugeCollected("household")              
            })
            
            output$hhdegaugeweek <- renderGvis({              
              getGaugeCaptured("household")              
            })
            
            output$hhdagaugeweek <- renderGvis({              
              getGaugeArchived("household")              
            })
            
            output$tableHousehold <- renderTable({
              return(weekData4Viz$household)
            })          
          }else{
            return(NULL)
          }
        }else{
          return(NULL)
        }
      })
      
    }
    # If selected "individual" screen:
    else if(input$tabs == "typeB"){
      
      output$choose_round_individual <- renderUI({
        selectInput("roundindividual", "Select Round", roundsB)
      })
      
      output$choose_week <- renderUI({
        selectInput("week", "Select Week", getWeeks(),width = '90%')
      })
      
      observe({        
        if(!is.null(input$roundindividual)){
          
          
          if(!is.null(input$week)){
            
            withProgress(message = 'LOADING DATA...', value = 0, {
              
              n <- 9            
              
              # Increment the progress bar, and update the detail text.
              incProgress(3/n, detail = paste("LOADING DATA... 30%"))
              indData <- getIndividualRoundData(input$roundindividual,sharedValues$data)
              incProgress(3/n, detail = paste("LOADING DATA... 60%"))
              weekData <- getRoundDataPerWeek(indData, input$week)
              incProgress(3/n, detail = paste("DATA LOADED... 100%"))         
              
            })
            
            
            
            getContingencyTable(data = weekData,type = "individual",period = "week")
            
            output$inddcgaugeweek <- renderGvis({
              getGaugeCollected("individual")
            })
            
            output$inddegaugeweek <- renderGvis({
              getGaugeCaptured("individual")
            })
            
            output$inddagaugeweek <- renderGvis({
              getGaugeArchived("individual")
            })
            
            output$tableIndividual <- renderTable({
              validate(
                need(nrow(weekData4Viz$individual) != 0, "No data for this week")
              )
              return(weekData4Viz$individual)
            })          
          
        }
        else{
          return(NULL)
        }
        }
        else{
          return(NULL)
        }
        
      })
      
    }
    
    # If selected "verbal autopsy" screen:
    else if(input$tabs == "typeC"){
      
      output$choose_year <- renderUI({
        selectInput("yearsva", "Select Year", yearsOfDeath)
      })
      
      observe({
        if(!is.null(input$yearsva)){
          
          withProgress(message = 'LOADING DATA...', value = 0, {
            
            n <- 9            
            
            # Increment the progress bar, and update the detail text.
            incProgress(3/n, detail = paste("LOADING DATA... 30%"))
            vadata <- sharedValues$vadata
            incProgress(3/n, detail = paste("LOADING DATA... 60%"))
            vaDataForYear <- subset(vadata,vadata$YearOfDeath == input$yearsva)
            getContingencyTable(data = vaDataForYear,type = "va",period = "week")            
            incProgress(3/n, detail = paste("DATA LOADED... 100%"))         
            
          })        
          
          output$vadcgaugeweek <- renderGvis({
            getGaugeCollected("va")
          })
          
          output$vadegaugeweek <- renderGvis({
            getGaugeCaptured("va")
          })
          
          output$vadagaugeweek <- renderGvis({
            getGaugeArchived("va")
          })
          
          output$tableVA <- renderTable({
            return(weekData4Viz$va)
          }) 
          
        }else{
          return(NULL)
        }
      }) 
    }
    

  })
}

shinyApp(ui, server)