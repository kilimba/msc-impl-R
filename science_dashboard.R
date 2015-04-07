## app.R ##
library(shinydashboard)
library(shiny)
library(rcdimple)
library(curl)  #devtools::install_github("jeroenooms/curl")
library(plyr)  # for round_any
library(rCharts)
library(ggvis)
library(reshape2)

df <- read.csv("C:\\Program Files\\Apache Software Foundation\\Tomcat 6.0\\webapps\\ROOT\\d3\\pyramid.csv")
agegroup_mapping <- read.csv("C:\\Users\\Tee\\Desktop\\Masters Project\\Masters Project\\ACDIS-DB\\agegroup_mapping.csv")
df <- merge(df,agegroup_mapping,by.x="agegrp",by.y="agegroup")
# Max and minimum year to determine range of years to animate by
maxYear <- max(df$year)
minYear <- min(df$year)
# maximum and minimum values for population to determine x-axis on pyramid
max_x <- round_any(max(df$n), 1000, f = ceiling)
min_x <- round_any(min(-1*df$n), 1000, f = floor)
# maximum and minimum values for population to determine y-axis on linechart
max_y <- round_any(max(df$n), 1000, f = ceiling)
min_y <- round_any(min(df$n), 100, f = floor)

#####################################
# HEATMAP DATA
#####################################

hmap_df <- (dcast(df,year+mapping ~ sex,value.var="n"))
hmap_df$rowtotal <- hmap_df$FEM + hmap_df$MAL

hmap_df$year <- factor(hmap_df$year)

# Helps to order the y-axis labels otherwise labels appear in 
# mixed order
hmap_df$mapping <- factor(hmap_df$mapping, levels = rev(c("0-4","5-9","10-14",
                                                "15-19","20-24","25-29",
                                                "30-34","35-39","40-44",
                                                "45-49","50-54","55-59",
                                                "60-64","65-69","70-74",
                                                "75-79","80-84","85+")))

##########################################
# END OF HEATMAP DATA
##########################################



getData <- function(startyr,endyear) {
  df <- subset(df,(year >= startyr & year <= endyear))
  
  return(df)
}

# DimpleJS pyramid

dPyramid <- function(startyear, endyear, colors=NULL) {
  #endyear = endyear + 3 #to test storyboard
  dat <- getData(startyear, endyear)
  dat$n <- ifelse(dat$sex == 'MAL', -1 * dat$n, 1 * dat$n)
  dat$gencode <- ifelse(dat$sex == 'MAL', 1, 2)
  
  d1 <- dimple(
    x = "n", 
    y = "agegrp", 
    groups = "sex", 
    data = dat, 
    type = 'bar')
  
  
  d1 <- yAxis(d1, type = "addCategoryAxis", orderRule = "ord")
  d1 <- xAxis(d1,type = "addMeasureAxis")
  d1 <- add_legend(d1)
  # Ensure fixed x-axis independent of year selected
  d1 <- xAxis(d1, overrideMax = max_x, overrideMin = min_x)
  
  if (!is.null(colors)){
    d1 <- colorAxis(
      d1,
      type = "addColorAxis", 
      colorSeries = "gencode", 
      palette = colors
    )
  }
  
  # For storyboarding
  if (endyear - startyear >= 1) {
    d1 <- tack(d1, options = list( storyboard = "year" ) )    
  }
  
  d1
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
  
  dashboardHeader(title = "Data Everywhere",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Dr Ndlovu",
                                 message = "How to access data set?"
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Kobus Herbst",
                                 message = "New presentation added to archived presentations.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  ),
                  dropdownMenu(type = "notifications",
                               notificationItem(
                                 text = "5 new users today",
                                 icon("users")
                               ),
                               notificationItem(
                                 text = "54 views today",
                                 icon("truck"),
                                 status = "success"
                               ),
                               notificationItem(
                                 text = "3 datasets downloaded",
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Documentation"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Project X"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Server deployment"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Overall project"
                               )
                  )
                  
                  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Presentations", tabName = "widgets", icon = icon("file-powerpoint-o"))
    ),
    tags$br(),
    #tags$hr(),
    tags$fieldset(checkboxInput("doAnimate", "Animate Pyramid",value = TRUE),
    tags$p("(Uncheck to select specific year)")),
    conditionalPanel(
      condition = "input.doAnimate == false",
      selectInput(    
        inputId = "startyr",
        label = "Select Pyramid Year",
        c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))
      
    ),
    selectInput(inputId = "agegrp",
                label = "Choose Age Group",
                choices = c("0-4","5-9","10-14","15-19","20-24","25-29",
                            "30-34","35-39","40-44","45-49","50-54",
                            "55-59","60-64","65-69","70-74","75-79",
                            "80-84","85+")
#                 choices = setNames(df$mapping)
                ,
                selected = "0-4"),
    
    selectInput(inputId = "outcome",
                label = "Choose Outcome",
                choices = c("Population",
                            "TB Infections",
                            "HIV Infections",
                            "Malaria",
                            "STI"
                ),
                selected = "Population")
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
                infoBox("About", "You are currently viewing <outcome_placeholder>. More information 
                        on the use of multi-panel graphs such as these in epidemiology can be found at
                        ncbi.nlm.nih.gov/pubmed/21347221", icon = icon("info-circle"),width = 6),
                
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
  
  observe({
    
    if(input$doAnimate){
      
      output$distPlot <- renderDimple({
        dPyramid(minYear, maxYear)
      })
      
    }else{
      
      output$distPlot <- renderDimple({
        startyear <- as.numeric(input$startyr)
        # Start year and end year are equal we only want cross-sectional pyramid
        # for a single selected year
        dPyramid(startyear, startyear)
      })    
    }
  })
  
  ################################################
  # Line chart
  ################################################
  
  output$distPlot2 <- renderChart2({
    
    selection <- subset(df,mapping == input$agegrp)
    
    plot <- nPlot(n ~ year,
                  data = selection,
                  type = "lineChart",
                  group = "sex",
                  height = 250,
                  width = 450 )    
    
    # Explicitly set year tick values for every year
    plot$xAxis(tickValues = do.call(seq, c(as.list(range(df$year)), 1)),rotateLabels= -40,showMaxMin = "true")
    
    plot$chart(useInteractiveGuideline = "true", transitionDuration = 500)
    plot$chart(forceY = c(min_y, max_y))
    # Add axis labels and format the tooltip
    plot$yAxis(axisLabel = "Population", width = 62)
    
    plot$xAxis(axisLabel = "Year")
    
    plot$save("ac.html")
    return(plot) 
    
    

    
  })
  
  #################################################
  # HEATMAP 
  #################################################
  
  hmap_df %>% 
    ggvis(~year, ~mapping, fill = ~rowtotal) %>% 
    layer_rects(width = band(), height = band()) %>%
    add_relative_scales() %>%
        add_legend("fill", title = "Count",
                   properties = legend_props(
                     legend = list(
                       x = scaled_value("x_rel", 0),
                       y = scaled_value("y_rel", 0)
                     )
                   )
        ) %>%
    set_options(height = 200, width = 410, keep_aspect = TRUE) %>% 
    add_axis("y", title="")%>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    scale_nominal("y", padding = 0, points = FALSE) %>% 
    add_tooltip(function(d) d$rowtotal) %>%

    #hide_legend("fill") %>%
    bind_shiny("heatmap")
}

shinyApp(ui, server)
