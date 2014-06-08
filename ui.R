suppressMessages(library(shiny))
suppressMessages(library(rCharts))
suppressMessages(library(doSNOW))
suppressMessages(library(foreach))

shinyUI(pageWithSidebar(
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Application title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  headerPanel("MiningChi - Chicago Red Light Cameras Data Visualization"),
  
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
      selectInput("crimetype", "Choose Crime Type:", choice = c("HOMICIDE","THEFT","CRIM SEXUAL ASSAULT","BURGLARY","BATTERY","ROBBERY",
                                            "INTERFERENCE WITH PUBLIC OFFICER","DECEPTIVE PRACTICE","ARSON","CRIMINAL DAMAGE",
                                            "ASSAULT","NARCOTICS","CRIMINAL TRESPASS","OTHER OFFENSE","PUBLIC PEACE VIOLATION",
                                            "SEX OFFENSE","OFFENSE INVOLVING CHILDREN","PROSTITUTION","WEAPONS VIOLATION","KIDNAPPING",
                                            "LIQUOR LAW VIOLATION","STALKING","NON-CRIMINAL","INTIMIDATION","OBSCENITY",
                                            "PUBLIC INDECENCY","OTHER NARCOTIC VIOLATION","GAMBLING","OTHER OFFENSE ","NON - CRIMINAL",
                                            "NON-CRIMINAL (SUBJECT SPECIFIED)","INTERFERE WITH PUBLIC OFFICER","OFFENSES INVOLVING CHILDREN","RITUALISM")),
      helpText("Examples: BATTERY, THEFT etc."),
      
      dateInput("startdate", "Start Date of Data Collection:", value = "2000-01-01", format = "mm-dd-yyyy",
                min = "2000-01-01", max = "2014-09-29"),
      
      dateInput("enddate", "End Date of Data Collection:", value = "2015-01-02", format = "mm-dd-yyyy",
                min = "startdate", max = "2014-09-30"),
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
      tabPanel("Red Light Camera Map", uiOutput("mapcenter"), div(class="span6",uiOutput("mapzoom")),
               div(class="span8", plotOutput("map",height=600,width=600)),div(class="span4",uiOutput("maptype")),div(class="span2",uiOutput("mapres")),
               div(class="span2",uiOutput("mapbw"))),
      #tabPanel("Analysis", uiOutput("analperiod"),plotOutput("analplot")),
      #tabPanel("Weather", div(class="span4",uiOutput("weatherperiod")), plotOutput("weather")),
      #tabPanel("Traffic", uiOutput("tmaptitle"),uiOutput("tmapcenter"), div(class="span6",uiOutput("tmapzoom")),
        #       div(class="span8", plotOutput("tmap",height=600,width=600)),div(class="span4",uiOutput("tmaptype")),div(class="span2",uiOutput("tmapres")),
         #      div(class="span2",uiOutput("tmapbw"))),
      tabPanel("Credits", includeMarkdown("docs/credits.md"))
    ) 
  )

))
