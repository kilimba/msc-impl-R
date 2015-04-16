## app.R ##

# list.of.packages <- c("ggplot2", "Rcpp")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

library(shinydashboard)
library(shiny)
library(rcdimple) #devtools::install_github("timelyportfolio/rcdimple")
library(curl)  #devtools::install_github("jeroenooms/curl")
library(plyr)  # for round_any
library(rCharts)
library(ggvis)
library(reshape2)

agegroup_mapping <- read.csv("agegroup_mapping.csv")
indicators <- read.csv("//acs-fs02/shared/RDM/Datasets/Data Everywhere/Research Portal/indicators.csv")

choices <- as.vector(indicators$label)
indicators$label <- as.character(indicators$label)
indicators$file <- paste("",indicators$file,"",sep="")

getHeatMapData <- function(data){
  df <- data
  df$sex <- ifelse(df$sex == 1, "MAL", "FEM")
  hmap_df <- (dcast(df,year+agegrp ~ sex,value.var="denominator"))
  hmap_df$rowtotal <- hmap_df$MAL + hmap_df$FEM
  
  hmap_df$year <- factor(hmap_df$year)
  
  # Helps to order the y-axis labels otherwise labels appear in 
  # mixed order
  hmap_df$agegrp <- factor(hmap_df$agegrp, levels = rev(as.vector(unique(hmap_df$agegrp))))
  return(hmap_df)
}

getData <- function(startyr,endyear,outcome_data) {
  data <- subset(outcome_data,(year >= startyr & year <= endyear))
  
  return(data)
} 
getDataByAgeGroup <-function(data,agegrp){  
  
  d <- data 
  a <- agegrp
  
  selection <- subset(d,agegrp==a) 
  return(selection)
} 

# Line Chart 
lineChart <- function(data,agegrp,indicator){ 
  
  if(indicator$rate == "Y"){
    
    selection <- data
    selection$rate <- (selection$numerator/selection$denominator)*indicator$multiplier
    selection$sex <- ifelse(selection$sex == 1, "MALE", "FEMALE")
    max_y <- round_any(max(selection$rate), 10, f = ceiling)   
    min_y <- round_any(min(selection$rate), 10, f = floor)    
    selection <- getDataByAgeGroup(selection,agegrp)
    
    plot <- nPlot(rate ~ year,
                  data = selection,
                  type = "lineChart",
                  group = "sex",
                  height = 250,
                  width = 450 )    
    
    # Explicitly set year tick values for every year
    plot$xAxis(tickValues = do.call(seq, c(as.list(range(selection$year)), 1)),rotateLabels= -40,showMaxMin = "true")
    
    plot$chart(useInteractiveGuideline = "true", transitionDuration = 500)  
    
    plot$chart(forceY = c(min_y, max_y))
    # Add axis labels and format the tooltip
    plot$yAxis(axisLabel = paste("Rate per",indicator$multiplier), width = 62)
    
    plot$xAxis(axisLabel = "Year")
    
    return(plot)     
    
    
  }else{
    selection <- data
    selection$sex <- ifelse(selection$sex == 1, "MALE", "FEMALE")
    #cat("MaxY:",round_any(max(selection$denominator), 100, f = ceiling))
    max_y <- round_any(max(selection$denominator), 10, f = ceiling)   
    min_y <- round_any(min(selection$denominator), 10, f = floor)    
    selection <- getDataByAgeGroup(selection,agegrp)
    
    plot <- nPlot(denominator ~ year,
                  data = selection,
                  type = "lineChart",
                  group = "sex",
                  height = 250,
                  width = 450 )    
    
    # Explicitly set year tick values for every year
    plot$xAxis(tickValues = do.call(seq, c(as.list(range(selection$year)), 1)),rotateLabels= -40,showMaxMin = "true")
    
    plot$chart(useInteractiveGuideline = "true", transitionDuration = 500)  
    
    plot$chart(forceY = c(min_y, max_y))
    # Add axis labels and format the tooltip
    plot$yAxis(axisLabel = "Population", width = 62)
    
    plot$xAxis(axisLabel = "Year")
    
    return(plot)     
    
    
  }
  
    
    
}


# DimpleJS pyramid

dPyramid <- function(startyear, endyear, data, colors=NULL,indicator) {
  
  dat <- getData(startyear,endyear,data)  
  
  if(indicator$rate == "Y"){
    dat$denominator <- ifelse(dat$sex == 1, -1 * dat$denominator, 1 * dat$denominator)
    dat$rate <- (dat$numerator/dat$denominator)*indicator$multiplier
    
    max_x <- round_any(max(dat$rate), 10, f = ceiling)
    min_x <- round_any(min(dat$rate), 10, f = floor)    
    
    d1 <- dimple(
      x = "rate", 
      y = "agegrp", 
      groups = "sex", 
      data = dat, 
      type = 'bar')
    
    
    d1 <- yAxis(d1, type = "addCategoryAxis", orderRule = "agegrp")
    d1 <- xAxis(d1,type = "addMeasureAxis",label = "LABEL")
    d1 <- add_legend(d1)
    # Ensure fixed x-axis independent of year selected
    d1 <- xAxis(d1, overrideMax = max_x, overrideMin = min_x)
    
    if (!is.null(colors)){
      d1 <- colorAxis(
        d1,
        type = "addColorAxis", 
        colorSeries = "sex", 
        palette = colors
      )
    }
    
    # For storyboarding
    if (endyear - startyear >= 1) {
      d1 <- tack(d1, options = list( storyboard = "year" ) )    
    }
    
    return(d1)
  }
  
  
  else{
    dat$denominator <- ifelse(dat$sex == 1, -1 * dat$denominator, 1 * dat$denominator)
    
    max_x <- round_any(max(dat$denominator), 10, f = ceiling)
    min_x <- round_any(min(-1*dat$denominator), 10, f = floor)    
    
    d1 <- dimple(
      x = "denominator", 
      y = "agegrp", 
      groups = "sex", 
      data = dat, 
      type = 'bar')
    
    
    d1 <- yAxis(d1, type = "addCategoryAxis", orderRule = "agegrp")
    d1 <- xAxis(d1,type = "addMeasureAxis")
    d1 <- add_legend(d1)
    # Ensure fixed x-axis independent of year selected
    d1 <- xAxis(d1, overrideMax = max_x, overrideMin = min_x)
    
    if (!is.null(colors)){
      d1 <- colorAxis(
        d1,
        type = "addColorAxis", 
        colorSeries = "sex", 
        palette = colors
      )
    }
    
    # For storyboarding
    if (endyear - startyear >= 1) {
      d1 <- tack(d1, options = list( storyboard = "year" ) )    
    }
    
    return(d1)
  }
}

suppressMessages(
  singleton(
    addResourcePath(
      get_lib("nvd3")$name
      ,get_lib("nvd3")$url
    )
  )
)



ui <- dashboardPage(
  
  dashboardHeader(title = "Data Everywhere"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Presentations", tabName = "widgets", icon = icon("file-powerpoint-o"))
    ),
    tags$br(),
    tags$fieldset(checkboxInput("doAnimate", "Animate Pyramid",value = TRUE),
                  tags$p("(Uncheck to select specific year)")),
    conditionalPanel(
      condition = "input.doAnimate == false",
      selectInput(    
        inputId = "startyr",
        label = "Select Pyramid Year",
        c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
      
    ),
    
    uiOutput("choose_agegrp")
    ,
    
    uiOutput("choose_dataset")
  ),
  dashboardBody(
    tags$head(get_assets_shiny(get_lib("nvd3"))[-3]),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(dimpleOutput("distPlot", height = 250)),
                
                box(showOutput("distPlot2","nvd3",add_lib=F))
              ),
              fluidRow(
                #box(infoBoxOutput("informationBox")),
                infoBox("About", textOutput("caption"), icon = icon("info-circle"),width = 6),
                
                (
                  #uiOutput("ggvis_ui"),
                  box(ggvisOutput("heatmap"))
                )
              )                    
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Links to interactive scientific presentations will go here...")
      )
    )
  )
)


server <- function(input, output) {
  
  output$choose_dataset <- renderUI({
    selectInput("outcome", "Select Outcome", choices, selected="Population Structure",width="95%")
  })
  
  output$choose_agegrp <- renderUI({
    selectInput("agegrp", "Select Age Group", 
                choices = c("00-04","05-09","10-14","15-19","20-24","25-29",
                            "30-34","35-39","40-44","45-49","50-54",
                            "55-59","60-64","65-69","70-74","75-79",
                            "80-84","85+"), selected="00-04",width="95%")
  })
  #############################################################
  observe({
    if(!is.null(input$outcome)){
      selected_outcome <- input$outcome          
      selected_indicator <- subset(indicators,indicators$label == selected_outcome)
      
      outcome_data <- reactive({
        read.csv(selected_indicator$file)
      })
      
      d <- reactive({outcome_data()})
      
      minYear <- reactive({min(d()$year)})
      maxYear <- reactive({max(d()$year)})
      
      observe({
        if(input$doAnimate){
          output$distPlot <- renderDimple({
            dPyramid(minYear(), maxYear(),data = outcome_data(), indicator = selected_indicator)
          })
          
        }else{
          output$distPlot <- renderDimple({
            startyear <- as.numeric(input$startyr) 
            # Start year and end year are equal we only want cross-sectional pyramid
            # for a single selected year
            dPyramid(startyear, startyear, data = outcome_data(),indicator = selected_indicator)
          })    
        }
      })
      
    }
    
  })
  
  observe({
    
    if(!is.null(input$outcome)
       & !is.null(input$agegrp)){
      
      selected_outcome <- input$outcome          
      selected_indicator <- subset(indicators,indicators$label == selected_outcome)
      
      outcome_data <- reactive({
        read.csv(selected_indicator$file)
      })
      
      d <- reactive({outcome_data()})
      
      output$caption <- renderText({
        return(paste("You are currently viewing",
                     ifelse(selected_indicator$rate=="N",paste(input$outcome,".\n",selected_indicator$description),
                            paste(input$outcome,"(per",selected_indicator$multiplier,"population).\n",selected_indicator$description))))
      })
      ################################################
      # Line chart
      ################################################
      output$distPlot2 <- renderChart2({    
        lineChart(outcome_data(),input$agegrp,selected_indicator)    
      })
      
      #################################################
      # HEATMAP 
      #################################################
      
      reactive({getHeatMapData(outcome_data())}) %>% 
        ggvis(~year, ~agegrp, fill = ~rowtotal) %>% 
        layer_rects(width = band(), height = band()) %>%
        add_relative_scales() %>%
        set_options(height = 200, width = 410, keep_aspect = TRUE) %>% 
        add_axis("y", title="")%>%
        scale_nominal("x", padding = 0, points = FALSE) %>%
        scale_nominal("y", padding = 0, points = FALSE) %>% 
        scale_numeric("fill",range = c("lightsteelblue","red")) %>% 
        hide_legend("fill") %>%
        add_tooltip(function(d) {
          if(is.null(d)) return(NULL)
          paste0(names(d), ": ", format(d), collapse = "<br />") 
        }         
        
        ) %>%        
        
        bind_shiny("heatmap")
      
      
    }
    
  })
  
  
}

shinyApp(ui, server)