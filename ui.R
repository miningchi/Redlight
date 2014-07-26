suppressMessages(library(shiny))
suppressMessages(library(rCharts))
suppressMessages(library(doSNOW))
suppressMessages(library(foreach))

shinyUI(pageWithSidebar(
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Application title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  headerPanel("MiningChi - Red Light Cameras"),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sidebar Panel
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sidebarPanel(
    
    wellPanel(
      helpText(HTML("<b>READY?</b>")),
      HTML("Continue to scroll down and modify the settings. Come back and click this when you are ready to render new plots."),
      submitButton("Update Graphs and Tables")
    ),
    
    wellPanel(
      helpText(HTML("<b>BASIC SETTINGS</b>")),
      
      selectInput('Intersection', 'Options', df$INTERSECTION, selected = "Pulaski-55th",selectize=TRUE),
      
      dateInput("startdate", "Start Date of Data Collection:", value = "2009-01-01", format = "mm-dd-yyyy",
                min = "2009-01-01", max = "2012-12-31"),
      
      dateInput("enddate", "End Date of Data Collection:", value = "2013-01-02", format = "mm-dd-yyyy",
                min = "2009-01-02", max = "2013-01-02"),
      ##Need some validation that enddate is after start date
      helpText("MM-DD-YEAR as Date Format")
    )
    
   ),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main Panel 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
#just need to find the right HTML formatting

  mainPanel(
    tabsetPanel(
      tabPanel("Introduction", includeMarkdown("docs/introduction.md")),
      tabPanel("Data", dataTableOutput("datatable")),
      tabPanel("Analysis by Intersection", div(class="span4",uiOutput("accperiod")), plotOutput("accchart",width="100%"),
               textOutput("totalaccidents"), textOutput("totalkilled"), textOutput("totalinjured"), tableOutput("totalcolltype")   ),
               
#       tabPanel("Camera Map", uiOutput("maptitle"), uiOutput("mapcenter"), div(class="span6",uiOutput("mapzoom")),
#                div(class="span8", plotOutput("map",height=600,width=600)),div(class="span4",uiOutput("maptype")),div(class="span2",uiOutput("mapres")),
#                div(class="span2",uiOutput("mapbw"))),
#       tabPanel("Accident Heat Map", uiOutput("hmapcenter"), div(class="span6",uiOutput("hmapzoom")),
#                div(class="span8", plotOutput("heatmap",height=600,width=600)),div(class="span4",uiOutput("hmaptype")),div(class="span2",uiOutput("hmapres")),
#                div(class="span2",uiOutput("hmapbw")), div(class="span2",uiOutput("halpharange")), div(class="span2",uiOutput("hbins")),
#                div(class="span2",uiOutput("hboundwidth")), div(class="span2",uiOutput("hboundcolor")), div(class="span2",uiOutput("hlow")),
#                div(class="span2",uiOutput("hhigh"))
#       ),
      #tabPanel("Analysis", uiOutput("analperiod"),plotOutput("analplot")),
      #tabPanel("Weather", div(class="span4",uiOutput("weatherperiod")), plotOutput("weather")),
      #tabPanel("Traffic", uiOutput("tmaptitle"),uiOutput("tmapcenter"), div(class="span6",uiOutput("tmapzoom")),
        #       div(class="span8", plotOutput("tmap",height=600,width=600)),div(class="span4",uiOutput("tmaptype")),div(class="span2",uiOutput("tmapres")),
         #      div(class="span2",uiOutput("tmapbw"))),
      tabPanel("Credits", includeMarkdown("docs/credits.md"))
    ) 
  )

))
