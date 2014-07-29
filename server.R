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
#Load data files
load(file = "./data/redlight2.rda")
load(file = "./data/redlight.rda")
#load(file = "./data/redlightaccidents.rda")
load(file = "./data/total.rda")
load(file = "./data/ticket.rda")
#df <- subset(df,Red.Speed==1)

xtsMelt <- function(data) {
  require(reshape2)
  #translate xts to time series to json with date and data
  #for this behavior will be more generic than the original
  #data will not be transformed, so template.rmd will be changed to reflect
  #convert to data frame
  data.df <- data.frame(cbind(format(index(data),"%Y-%m-%d"),coredata(data)))
  colnames(data.df)[1] = "date"
  data.melt <- melt(data.df,id.vars=1,stringsAsFactors=FALSE)
  colnames(data.melt) <- c("date","indexname","value")
  #remove periods from indexnames to prevent javascript confusion
  #these . usually come from spaces in the colnames when melted
  data.melt[,"indexname"] <- apply(matrix(data.melt[,"indexname"]),2,gsub,pattern="[.]",replacement="")
  return(data.melt)
  #return(df2json(na.omit(data.melt)))
}


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
          {temp.inter <- grep (input$Intersection,df1$INTERSECTION) }
          
          total1 <- subset(total1, total1$IntersectionID == temp.inter)
          return(total1)
          })
  
  datesubsetticket <- reactive({
    temp <- subset(ticket, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
    if (is.null(input$Intersection)) 
      {tempinter <- 2}
     else 
     {tempinter <- grep (input$Intersection,df1$INTERSECTION)}
    temp <- subset(temp, temp$IntersectionID == tempinter)
    
    return(temp)
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
  
  output$accidents <- renderChart2({
    library(xts)
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

  ust.melt <- na.omit(xtsMelt(TotalAccidents))
  ust.melt$date2 <- as.Date(ust.melt$date, format = "%Y-%m-%d")
  ust.melt$Accidents <- as.numeric(as.character(ust.melt$value))
  ust.melt$date4  <- as.numeric(as.POSIXct(ust.melt$date2, origin="1970-01-01")) * 1000
  
  h1 <- hPlot(
    Accidents ~ date4,  #or x="date", y="value"
    data = ust.melt, 
    color = '#4572A7',
    type = 'spline',
    title = paste("Accidents at",input$Intersection)
  ) 
  h1$xAxis(type = "datetime")
  
  h1
})

   # plot(TotalAccidents,main=paste("Accidents at",input$Intersection), type='bars')},  height = 300, width = 500)

output$space <- renderUI({helpText(HTML("<br>"))})

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Ticket Plot
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
 output$ticketchart <- renderChart2({
   library(xts)
   if (is.null(input$aperiod)) {temp.aperiod <- "yearly"}
   else {temp.aperiod <- input$aperiod }
   xtstemp <- datesubsetticket() 
#   temp12 <- nrow(xtstemp)
   
 totalxts1 <- xts(xtstemp$IntersectionID>1,xtstemp$PosixDate)
   if (temp.aperiod == "daily") {TotalTickets <- apply.daily(totalxts1,sum)}
   if (temp.aperiod == "weekly") {TotalTickets <- apply.weekly(totalxts1,sum)}
   if (temp.aperiod == "monthly") {TotalTickets <- apply.monthly(totalxts1,sum)}
   if (temp.aperiod == "yearly") {TotalTickets <- apply.yearly(totalxts1,sum)}

ust.melt <- na.omit(xtsMelt(TotalTickets))
ust.melt$date2 <- as.Date(ust.melt$date, format = "%Y-%m-%d")
ust.melt$Tickets <- as.numeric(as.character(ust.melt$value))
ust.melt$date4  <- as.numeric(as.POSIXct(ust.melt$date2, origin="1970-01-01")) * 1000

h2 <- hPlot(
  Tickets ~ date4,  #or x="date", y="value"
  data = ust.melt,
  type = 'spline',
  title = paste("Tickets at",input$Intersection)
) 
h2$xAxis(type = "datetime")
h2
})

  # plot(TotalTickets, main=paste("Tickets at",xtstemp$Address[1]), type='bars')},  height = 300, width = 500)
# plot (temp12)
# })
 
output$heading <- renderUI({helpText(HTML("<b>Some statistics for the intersection:</b><br>"))})
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Outputs of Analysis
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


output$totaltickets <- renderText({
  temp <- datesubsetticket()
  total <- nrow (temp)
  paste("Total Tickets:", total)
})

output$totaltickets2 <- renderText({
  temp <- datesubsetticket()
  total <- nrow (temp)
  paste("Total Tickets:", total)
})

 output$totalaccidents <- renderText({
   temp <- datesubset()
   total <- nrow (temp)
   paste("Total Accidents:", total)
 })
   
output$totalaccidents2 <- renderText({
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
 
 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output 1 - Ticket Data Table
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$datatickettable <- renderDataTable({
  datesubsetticket() 
}, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output 1 - RLC Data Table
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$dataRLC <- renderDataTable({
  df1 
}, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output - Map
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output$maptitle <- renderUI({helpText(HTML("<b>Red Light cameras in Red, Speed cameras in Blue</b>"))})
output$mapcenter <- renderUI({textInput("center", "Enter a Location to Center Map, such as city or zipcode, the click Update", "Chicago")})
output$maptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
output$mapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
output$mapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
output$mapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 20):", min = 9, max = 24, step = 1, value = 20)})

output$map <- renderPlot({
  redlightdatabase <- datesubset() 
  map.center <- head(redlightdatabase,n=1)
  map.center <- map.center[c("CrashLongitude","CrashLatitude")]
  colnames(map.center)[1] <- "long"
  colnames(map.center)[2] <- "lat"
  print(map.center)
 
  # Set Defaults for when Map starts
 # if (is.null(input$Intersection)) {map.center <- geocode("Chicago")}
 # else {map.center = geocode(input$Intersection)}
  
  if (is.null(input$bw)) {temp.color <- "color"}
  else {
    temp.color <- "color"
    if (input$bw) {temp.color <- "bw"}}
  
  if (is.null(input$res)) {temp.scale <- 2}
  else {
    temp.scale <- 1
    if (input$res) {temp.scale <- 2}}
  
  if (is.null(input$zoom)) {temp.zoom <- 20}
  else {temp.zoom <- input$zoom }
  
  
  
  #Get Base Map
  map.base <- get_googlemap(
    as.matrix(map.center),
    maptype = input$type, ## Map type as defined above (roadmap, terrain, satellite, hybrid)
    # markers = map.center,
    zoom = temp.zoom,            ## 14 is just about right for a 1-mile radius
    color = temp.color,   ## "color" or "bw" (black & white)
    scale = temp.scale,  ## Set it to 2 for high resolution output
    messaging = FALSE,
  )
  
  ## Convert the base map into a ggplot object
  ## All added Cartesian coordinates to enable more geom options later on
  map.base <- ggmap(map.base, extend = "panel", messaging = FALSE) + coord_cartesian() + coord_fixed(ratio = 1.5)
  
  ## add points
  p <- map.base + geom_point(aes(x=CrashLongitude, y=CrashLatitude), colour="red", size = 4, na.rm=TRUE, subset(redlightdatabase,IntersectionID != 0))
  plot(p)},
  height = 500, width = 800)


  })
    
