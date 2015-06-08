## app.R ##
library(shiny)
library(shinydashboard)
library(googleVis)
library(rCharts)

# source("dataeverywhere_ops/householdsetup.R")
# source("dataeverywhere_ops/individualsetup.R")
# source("dataeverywhere_ops/vasetup.R")

roundsA <- as.vector(unique(dsDSRoundA$Name))
roundsB <- as.vector(rev(sort(dsDSRoundB$ExtID)))
yearsOfDeath <- as.vector(dsYearOfDeath$YearOfDeath)
vadata <<- getVAData()

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
          box(title = "Household Refusals",width = 6, status = "primary", solidHeader = TRUE, background = "black"),
          box(showOutput("consentRate",lib="nvd3"), width = 6,title = "HIV Consent & Refusal Rate", status = "primary", solidHeader = TRUE
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

server <- function(input, output) { 
  
  
  #observe({  
  
  #if(TRUE){
  roundData <<- getRoundData(dsDSRoundA[1,]["ExtID"])
  #}
  # Dynamic creation of select input for selecting round of interest
  # in Household page
  output$choose_round_household <- renderUI({
    selectInput("roundhouse", "Select Round", roundsA)
  })
  
    output$choose_round_individual <- renderUI({
      selectInput("roundindividual", "Select Round", roundsB)
    })
    
    output$choose_year <- renderUI({
      selectInput("yearsva", "Select Year", yearsOfDeath)
   })
  
  # Dynamic creation of select input for selecting week of interest   
  output$choose_week <- renderUI({
    selectInput("week", "Select Week", getWeeks(),width = '90%')
  })
  
  
  
  
  #observe({
  
  #if(TRUE){
  contingencyTable <- getContingencyTable(roundData,"household",period = "round")
  
  output$hhdcgauge <- renderGvis({
    percent <- round((getTotalField("household")/getTotalDocs("household"))*100,digits = 2)
    label <- "Field %"
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
    percent <- round((getTotalCaptured("household")/getTotalDocs("household"))*100,digits = 2)
    label <- "Captured %"
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
    percent <- round((getTotalArchived("household")/getTotalDocs("household"))*100,digits = 2)
    label <- "Archived %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  #################################################################
  #                           INDIVIDUAL                      
  #################################################################
  
  indData <- getIndividualRoundData(getCurrentRound())
  indContingencyTable <- getContingencyTable(indData,"individual","round")
  
  output$inddcgauge <- renderGvis({
    percent <- round((getTotalField("individual")/getTotalDocs("individual"))*100,digits = 2)
    label <- "Field %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  output$inddegauge <- renderGvis({
    percent <- round((getTotalCaptured("individual")/getTotalDocs("individual"))*100,digits = 2)
    label <- "Captured %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  output$inddagauge <- renderGvis({
    percent <- round((getTotalArchived("individual")/getTotalDocs("individual"))*100,digits = 2)
    label <- "Archived %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  #############################################################################
  #                          VERBAL AUTOPSY
  #############################################################################
  
  vaData <-  getVAData()
  vaContingencyTable <- getContingencyTable(vaData,"va","round")
  
  output$vadcgauge <- renderGvis({
    percent <- round((getTotalField("va")/getTotalDocs("va"))*100,digits = 2)
    label <- "Field %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  output$vadegauge <- renderGvis({
    percent <- round((getTotalCaptured("va")/getTotalDocs("va"))*100,digits = 2)
    label <- "Captured %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  output$vadagauge <- renderGvis({
    percent <- round((getTotalArchived("va")/getTotalDocs("va"))*100,digits = 2)
    label <- "Archived %"
    dat <- cbind(label,percent)
    df <- as.data.frame(dat, stringsAsFactors = FALSE)
    df$percent <- as.numeric(df$percent)
    gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                        greenTo=100, yellowFrom=30, yellowTo=70,
                                        redFrom=0, redTo=30))
    return(gauge)
  })
  
  observe({
    if(!is.null(input$roundhouse)){
      #browser()
      roundData <- getRoundData(subset(dsDSRoundA,dsDSRoundA$Name==input$roundhouse)["ExtID"])
      if(!is.null(input$week)){
        # create a shared variable so all pages are set to this week
        
        weekData <- getRoundDataPerWeek(roundData, input$week)
        getContingencyTable(data = weekData,type = "household",period = "week")
        
        output$hhdcgaugeweek <- renderGvis({
          percent <- round((getTotalField("household")/getTotalDocs("household"))*100,digits = 2)
          label <- "Field %"
          dat <- cbind(label,percent)
          df <- as.data.frame(dat, stringsAsFactors = FALSE)
          df$percent <- as.numeric(df$percent)
          gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                              greenTo=100, yellowFrom=30, yellowTo=70,
                                              redFrom=0, redTo=30))
          return(gauge)
        })
        
        output$hhdegaugeweek <- renderGvis({
          percent <- round((getTotalCaptured("household")/getTotalDocs("household"))*100,digits = 2)
          label <- "Captured %"
          dat <- cbind(label,percent)
          df <- as.data.frame(dat, stringsAsFactors = FALSE)
          df$percent <- as.numeric(df$percent)
          gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                              greenTo=100, yellowFrom=30, yellowTo=70,
                                              redFrom=0, redTo=30))
          return(gauge)
        })
        
        output$hhdagaugeweek <- renderGvis({
          percent <- round((getTotalArchived("household")/getTotalDocs("household"))*100,digits = 2)
          label <- "Archived %"
          dat <- cbind(label,percent)
          df <- as.data.frame(dat, stringsAsFactors = FALSE)
          df$percent <- as.numeric(df$percent)
          gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                              greenTo=100, yellowFrom=30, yellowTo=70,
                                              redFrom=0, redTo=30))
          return(gauge)
        })
        
        output$tableHousehold <- renderTable({
          return(weekData4Viz$household)
        })
        
      }
    }else{
      return(NULL)
    }
  })
    
    observe({
      if(!is.null(input$roundindividual)){
        #browser()
        indData <- getIndividualRoundData(input$roundindividual)
        
        if(!is.null(input$week)){
          #browser()
          week <- input$week
          weekData <- getRoundDataPerWeek(indData, week)
          
          getContingencyTable(data = weekData,type = "individual",period = "week")
          
          output$inddcgaugeweek <- renderGvis({
            percent <- round((getTotalField("individual")/getTotalDocs("individual"))*100,digits = 2)
            label <- "Field %"
            dat <- cbind(label,percent)
            df <- as.data.frame(dat, stringsAsFactors = FALSE)
            df$percent <- as.numeric(df$percent)
            gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                                greenTo=100, yellowFrom=30, yellowTo=70,
                                                redFrom=0, redTo=30))
            return(gauge)
          })
          
          output$inddegaugeweek <- renderGvis({
            percent <- round((getTotalCaptured("individual")/getTotalDocs("individual"))*100,digits = 2)
            label <- "Captured %"
            dat <- cbind(label,percent)
            df <- as.data.frame(dat, stringsAsFactors = FALSE)
            df$percent <- as.numeric(df$percent)
            gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                                greenTo=100, yellowFrom=30, yellowTo=70,
                                                redFrom=0, redTo=30))
            return(gauge)
          })
          
          output$inddagaugeweek <- renderGvis({
            percent <- round((getTotalArchived("individual")/getTotalDocs("individual"))*100,digits = 2)
            label <- "Archived %"
            dat <- cbind(label,percent)
            df <- as.data.frame(dat, stringsAsFactors = FALSE)
            df$percent <- as.numeric(df$percent)
            gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                                greenTo=100, yellowFrom=30, yellowTo=70,
                                                redFrom=0, redTo=30))
            return(gauge)
          })
          
          output$tableIndividual <- renderTable({
            validate(
              need(nrow(weekData4Viz$individual) != 0, "No data for this week")
            )
            return(weekData4Viz$individual)
          })
          
          
          
        }else{
          return(NULL)
        }
        
        
      }
        
    })
  
  observe({
    if(!is.null(input$yearsva)){
      
      vaDataForYear <- subset(vadata,vadata$YearOfDeath == input$yearsva)
      getContingencyTable(data = vaDataForYear,type = "va",period = "week")
      
      output$vadcgaugeweek <- renderGvis({
        percent <- round((getTotalField("va")/getTotalDocs("va"))*100,digits = 2)
        label <- "Field %"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$vadegaugeweek <- renderGvis({
        percent <- round((getTotalCaptured("va")/getTotalDocs("va"))*100,digits = 2)
        label <- "Captured %"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$vadagaugeweek <- renderGvis({
        percent <- round((getTotalArchived("va")/getTotalDocs("va"))*100,digits = 2)
        label <- "Archived %"
        dat <- cbind(label,percent)
        df <- as.data.frame(dat, stringsAsFactors = FALSE)
        df$percent <- as.numeric(df$percent)
        gauge <- gvisGauge(df, options=list(min=0, max=100, greenFrom=70,
                                            greenTo=100, yellowFrom=30, yellowTo=70,
                                            redFrom=0, redTo=30))
        return(gauge)
      })
      
      output$tableVA <- renderTable({
        return(weekData4Viz$va)
      })
      
    }else{
      return(NULL)
    }
    
  })
  
  output$consentRate <-  renderChart2({    
    p2 <- nPlot(HIVConsentRate ~ YEAR, 
                group = 'HIVRefused', 
                data = consentData, 
                type = 'stackedAreaChart',
                height = 250,
                width = 500 )
    return(p2)
  })
}

shinyApp(ui, server)