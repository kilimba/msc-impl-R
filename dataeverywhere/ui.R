## ui.R ##
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Health Data Viz"),
  dashboardSidebar(sidebarMenu(
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
  
  uiOutput("choose_dataset")),
  dashboardBody( tags$head(get_assets_shiny(get_lib("nvd3"))[-3]),
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
                 ))
)