suppressMessages(library(shiny))
suppressMessages(library(ggplot2))
suppressMessages(library(ggmap))
suppressMessages(library(RJSONIO))
suppressMessages(library(png))
suppressMessages(library(grid))
suppressMessages(library(RCurl))
suppressMessages(library(plyr))
suppressMessages(library(markdown))
suppressMessages(library(rCharts))
suppressMessages(library(parallel))
#load(file = "./data/weather.rda")
load(file = "./data/redlight.rda")
#load(file = "./data/redlightaccidents.rda")
load(file = "./data/total.rda")



## Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reactive Functions
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  datesubset <- reactive({
          total1 <- subset(total, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
        
          if (is.null(input$Intersection)) {
            temp.inter <- 2
            print ("hi")}
          else 
          {temp.inter <- grep (input$Intersection,df$INTERSECTION) }
          
          total1 <- subset(total1, total1$IntersectionID == temp.inter)
          return(total1)
          })

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 1 - Data Table
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$datatable <- renderDataTable({
   datesubset() 
    }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))
  
 
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 1 - Accident Graphs
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$accperiod <- renderUI({selectInput("aperiod", "Choose Period to Analyze:", choice = c("yearly", "monthly","weekly", "daily"))})
  
  
  output$accchart <- renderPlot({
    library(xts)
    print ("hi")
    #sum by crime type - and take into account different scales
    if (is.null(input$aperiod)) {temp.aperiod <- "yearly"}
    else {temp.aperiod <- input$aperiod }
    xtstemp <- datesubset() 
    totalxts1 <- xts(xtstemp$IntersectionID>1,xtstemp$PosixDate)
    if (temp.aperiod == "daily") {TotalAccidents <- apply.daily(totalxts1,sum)}
    if (temp.aperiod == "weekly") {TotalAccidents <- apply.weekly(totalxts1,sum)}
    if (temp.aperiod == "monthly") {TotalAccidents <- apply.monthly(totalxts1,sum)}
    if (temp.aperiod == "yearly") {TotalAccidents <- apply.yearly(totalxts1,sum)}

   # zoo.basket <- as.zoo(totalxts)
    
    plot(TotalAccidents, type='bars')},  height = 300, width = 500)
 # })

 output$totalaccidents <- renderText({
   temp <- datesubset()
   total <- nrow (temp)
   paste("Total Accidents:", total)
 })
   
  output$totalkilled <- renderText({
    temp <- datesubset()
    totalkilled <- sum(temp$totalkilled)
    paste("Total Killed:", totalkilled)
  })
 
 output$totalinjured <- renderText({
   temp <- datesubset()
   totalinjured <- sum(temp$total.injured)
   paste("Total Injured:", totalinjured)
 })
 
 output$totalcolltype <- renderTable({
   temp <- datesubset()
   collisiontype <- (temp$collisiontypecode)
   collisioncodes <- c(Pedestrian=1, Pedalcyclist=2, Train=3, Animal=4,Overturned=5, 
                       "Fixed Object"=6, "Other Object"=7, "Other non-collision"=8, "Parked Motor vehicle"=9, Turning= 10,
                       "Read-end"=11, "Sideswipe-same direction"=12, "Sideswipe-opposite direction"=13, "Head-on"=14, Angle=15)
   CollisionType <- names(collisioncodes)[match(collisiontype,collisioncodes)]
   temp3 <- table(CollisionType)
   as.data.frame(temp3)
 })
 
  })
    
