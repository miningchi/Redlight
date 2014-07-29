suppressMessages(library(shiny))
suppressMessages(library(rCharts))
suppressMessages(library(doSNOW))
suppressMessages(library(foreach))
load(file = "./data/redlight2.rda")

googleAnalytics <- function(account="UA-53239073-2"){
  HTML(paste("<script type=\"text/javascript\">
             
             var _gaq = _gaq || [];
             _gaq.push(['_setAccount', '",account,"']);
             _gaq.push(['_setDomainName', 'miningchi2.shinyapps.io']);
             _gaq.push(['_trackPageview']);
             
             (function() {
             var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
             ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
             var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
             })();
             
             </script>", sep=""))
}


shinyUI(pageWithSidebar(
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Application title
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  headerPanel("Red Light Camera Analysis"),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Sidebar Panel
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sidebarPanel(
    
    wellPanel(
      helpText(HTML("<b>READY?</b>")),
      HTML("You can modify the basic settings. Click this to refresh with new data."),
      submitButton("Update Graphs and Tables")
    ),
    
    wellPanel(
      helpText(HTML("<b>BASIC SETTINGS</b>")),
      
      selectInput('Intersection', 'Options', df1$INTERSECTION, selected = "Halsted-119th",selectize=TRUE),
      
      dateInput("startdate", "Start Date:", value = "2009-01-01", format = "mm-dd-yyyy",
                min = "2009-01-01", max = "2012-12-31"),
      
      dateInput("enddate", "End Date:", value = "2013-01-02", format = "mm-dd-yyyy",
                min = "2009-01-02", max = "2013-01-02"),
      ##Need some validation that enddate is after start date
      helpText("MM-DD-YEAR as date format"), 
      helpText("Ensure your period to analyze is less than the data range")
    )
    
   ),
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main Panel 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  
#just need to find the right HTML formatting

  mainPanel(
    tabsetPanel(
      tabPanel("Analysis", div(class="span4",uiOutput("accperiod")), showOutput("accidents","highcharts"),uiOutput("space"),
               showOutput("ticketchart","highcharts"),uiOutput("heading"),
               textOutput("totaltickets"),textOutput("totalaccidents"), textOutput("totalkilled"), textOutput("totalinjured"), 
               tableOutput("totalcolltype")   ),
      tabPanel("Map", uiOutput("mapcenter"),div(class="span6",uiOutput("mapzoom")),plotOutput("map"), uiOutput("maptype"),
               textOutput("totaltickets2"), textOutput("totalaccidents2")),
      tabPanel("FAQ", includeMarkdown("docs/introduction.md")),
      tabPanel("Accident Data", dataTableOutput("datatable")),
      tabPanel("Ticket Data", dataTableOutput("datatickettable")),
      tabPanel("RLC Data", dataTableOutput("dataRLC"))
    ),
    googleAnalytics()  
    )  
  )

)
