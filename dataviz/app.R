## app.R ##
library(shinydashboard)
library(shiny)
#require(htmltools)
require(htmlwidgets)
library(rcdimple) #devtools::install_github("timelyportfolio/rcdimple")
library(curl)  #devtools::install_github("jeroenooms/curl")
library(plyr)  # for round_any
library(rCharts)
library(ggvis)
library(reshape2)

indicators <- read.csv(curl("https://raw.githubusercontent.com/kilimba/msc-impl-R/master/dataviz/data/testindicators.csv"))
choices <- as.vector(indicators$label)
indicators$label <- as.character(indicators$label)
indicators$file <- paste("",indicators$file,"",sep="")

getHeatMapData <- function(data,indicator){
  df <- data
  
  if(indicator$rate == "Y"){
    
    hmap_df <- aggregate(cbind(denominator,numerator) ~ year + agegrp,df,sum)
    hmap_df$rate <- round((hmap_df$numerator/hmap_df$denominator)*indicator$multiplier,2)
    names(hmap_df)
    
  }else{  
    hmap_df <- aggregate(denominator ~ agegrp + year,df, sum)     
  }
  
  hmap_df$year <- factor(hmap_df$year)
  
  # Helps to order the y-axis labels otherwise labels appear in mixed order
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

lineChart <- function(data,agegrp,indicator){ 
  
  if(indicator$rate == "Y"){
    
    selection <- data
    selection$rate <- round((selection$numerator/selection$denominator)*indicator$multiplier,2)
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
    plot$chart(color = c("steelblue","firebrick"))
    # Add axis labels and format the tooltip
    plot$yAxis(axisLabel = paste("Rate per",indicator$multiplier), width = 62)
    
    plot$xAxis(axisLabel = "Year")
    
    return(plot)     
    
    
  }else{
    selection <- data
    selection$sex <- ifelse(selection$sex == 1, "MALE", "FEMALE")
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
    plot$chart(color = c("steelblue","firebrick"))
    # Add axis labels and format the tooltip
    plot$yAxis(axisLabel = "Population", width = 62)
    
    plot$xAxis(axisLabel = "Year")
    
    return(plot)    
  }    
}
# Heat Map

heatmap <- function(data,indicator){
  dat <- getHeatMapData(data,indicator)
  
  if(indicator$rate == "Y"){   
    
    dat <- rename(dat, c("agegrp" = "Age","year" = "Year", "rate" = "Rate"))
    
    dat %>% 
      ggvis(~Year, ~Age, fill = ~Rate) %>% 
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
    
  }else{
    dat <- rename(dat, c("agegrp" = "Age","year" = "Year", "denominator" = "Count"))
    
    dat %>% 
      ggvis(~Year, ~Age, fill = ~Count) %>% 
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
  
}

# DimpleJS pyramid

dPyramid <- function(startyear, endyear, data, colors=c("steelblue","firebrick"),indicator) {
  
  dat <- getData(startyear,endyear,data)  
  
  if(indicator$rate == "Y"){
    dat$denominator <- ifelse(dat$sex == 1, -1 * dat$denominator, 1 * dat$denominator)
    dat$Gender <- ifelse(dat$sex == 1,"Male", "Female")
    dat$Rate <- (dat$numerator/dat$denominator)*indicator$multiplier
    max_x <- round_any(max(dat$Rate), 10, f = ceiling)
    min_x <- round_any(min(dat$Rate), 10, f = floor)  
    
    dat <- rename(dat, c("agegrp" = "Age"))
    
    d1 <- dimple(
      x = "Rate", 
      y = "Age", 
      groups = "Gender", 
      data = dat, 
      type = 'bar') 
    
    
    d1 <- yAxis(d1, type = "addCategoryAxis", orderRule = "Age")
    d1 <- xAxis(d1,type = "addMeasureAxis")
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
      d1 <- tack(d1, options = list( storyboard = "year",
                                     chart = htmlwidgets::JS("
                            function(){
                            var self = this;
                            // x axis should be first or [0] but filter to make sure
                            self.axes.filter(function(ax){
                            return ax.position == 'x'
                            })[0] // now we have our x axis set _getFormat as before
                            ._getFormat = function () {
                            return function(d) {
                            return Math.abs(Math.round(d*100)/100);
                            };
                            };
                            // return self to return our chart
                            return self;
                            }
                            ")) )
      d1 <- add_title(d1,html = paste("<h6 style='font-family:Helvetica; text-align: center;'>",indicator$label,",",startyear,"-",endyear,"</h3>"))
      
    }else{
      d1 <- tack(d1, options = list( chart = htmlwidgets::JS("
                            function(){
                            var self = this;
                            // x axis should be first or [0] but filter to make sure
                            self.axes.filter(function(ax){
                            return ax.position == 'x'
                            })[0] // now we have our x axis set _getFormat as before
                            ._getFormat = function () {
                            return function(d) {
                            return Math.abs(Math.round(d*100)/100);
                            };
                            };
                            // return self to return our chart
                            return self;
                            }
                            ")) )
      d1 <- add_title(d1,html = paste("<h6 style='font-family:Helvetica; text-align: center;'>",indicator$label,",",startyear,"</h3>"))
    }
    
    return(d1)
  }
  
  
  else{
    dat$denominator <- ifelse(dat$sex == 1, -1 * dat$denominator, 1 * dat$denominator)
    dat$Gender <- ifelse(dat$sex == 1,"Male", "Female")
    max_x <- round_any(max(dat$denominator), 10, f = ceiling)
    min_x <- round_any(min(-1*dat$denominator), 10, f = floor)   
    
    dat <- rename(dat,c("denominator" = "Count","agegrp" = "Age"))
    
    d1 <- dimple(
      x = "Count", 
      y = "Age", 
      groups = "Gender", 
      data = dat, 
      type = 'bar')
    
    
    d1 <- yAxis(d1, type = "addCategoryAxis", orderRule = "Age")
    d1 <- xAxis(d1,type = "addMeasureAxis")
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
      d1 <- tack(d1, options = list( storyboard = "year",
                                     chart = htmlwidgets::JS("
                            function(){
                            var self = this;
                            // x axis should be first or [0] but filter to make sure
                            self.axes.filter(function(ax){
                            return ax.position == 'x'
                            })[0] // now we have our x axis set _getFormat as before
                            ._getFormat = function () {
                            return function(d) {
                            return d3.format(',.0f')(Math.abs(d));
                            };
                            };
                            // return self to return our chart
                            return self;
                            }
                            ")) )
      d1 <- add_title(d1,html = paste("<h6 style='font-family:Helvetica; text-align: center;'>",indicator$label,",",startyear,"-",endyear,"</h3>"))
    }else{
      d1 <- tack(d1, options = list(chart = htmlwidgets::JS("
                            function(){
                            var self = this;
                            // x axis should be first or [0] but filter to make sure
                            self.axes.filter(function(ax){
                            return ax.position == 'x'
                            })[0] // now we have our x axis set _getFormat as before
                            ._getFormat = function () {
                            return function(d) {
                            return d3.format(',.0f')(Math.abs(d));
                            };
                            };
                            // return self to return our chart
                            return self;
                            }
                            ")) )  
      d1 <- add_title(d1,html = paste("<h6 style='font-family:Helvetica; text-align: center;'>",indicator$label,",",startyear,"</h3>"))
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
  
  dashboardHeader(title = "HealthData Viz"),
  
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
    uiOutput("choose_year")
      
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
                infoBox("About", textOutput("caption"), icon = icon("info-circle"),width = 6),
                
                (
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
  # PYRAMID
  #############################################################
  
  observe({
    if(!is.null(input$outcome)){
      selected_outcome <- input$outcome          
      selected_indicator <- subset(indicators,indicators$label == selected_outcome)
      
      outcome_data <- reactive({
        read.csv(curl(as.character(selected_indicator$file)))
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
          years <- as.vector(unique(outcome_data()$year))
          output$choose_year <- renderUI({
            selectInput("startyr", "Select Pyramid Year", years, width="95%")
          })
          
          output$distPlot <- renderDimple({            
            
            if(!is.null(input$startyr)){
              
              startyear <- as.numeric(input$startyr) 
              # Start year and end year are equal we only want cross-sectional pyramid
              # for a single selected year
              dPyramid(startyear, startyear, data = outcome_data(),indicator = selected_indicator)
            }
            
          })    
        }
      })
      
      
      
      
    }
    
  })
  ###############################################
  
  observe({
    
    if(!is.null(input$outcome)
       & !is.null(input$agegrp)){     
      
      
      selected_outcome <- input$outcome 
      selected_indicator <- subset(indicators,indicators$label == selected_outcome)
      
      outcome_data <- reactive({
        read.csv(curl(as.character(selected_indicator$file)))
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
    
      heatmap(outcome_data(),selected_indicator)      
      
    }
    
  })
  
  
}

shinyApp(ui, server)