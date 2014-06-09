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


## Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reactive Functions
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  datesubset <- reactive({
          subset(df, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
        })
  

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 1 - Data Table
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$datatable <- renderDataTable({
   datesubset() 
    }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))
  
  
  

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 2 - Map
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  output$maptitle <- renderUI({helpText(HTML("<b>Red Light cameras in Red, Speed cameras in Blue</b>"))})
  output$mapcenter <- renderUI({textInput("center", "Enter a Location to Center Map, such as city or zipcode, the click Update", "Chicago")})
  output$maptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
  output$mapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
  output$mapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
  output$mapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 12):", min = 9, max = 20, step = 1, value = 12)})
  
  output$map <- renderPlot({
     
    # Set Defaults for when Map starts
    if (is.null(input$center)) {map.center <- geocode("Chicago")}
      else {map.center = geocode(input$center)}
    
    if (is.null(input$bw)) {temp.color <- "color"}
      else {
         temp.color <- "color"
        if (input$bw) {temp.color <- "bw"}}
      
    if (is.null(input$res)) {temp.scale <- 2}
      else {
       temp.scale <- 1
        if (input$res) {temp.scale <- 2}}
    
    if (is.null(input$zoom)) {temp.zoom <- 12}
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
 
    ## add crime points
    redlightdatabase <- datesubset() 
 p <- map.base + geom_point(aes(x=long, y=lat), colour="red", size = 4, na.rm=TRUE, data=subset(redlightdatabase,Red.Speed == 1))
p <- p + geom_point(aes(x=long, y=lat), colour="blue", size = 4, na.rm=TRUE, data=subset(redlightdatabase,Red.Speed == 0))
 plot(p)
  })
 #, width = 1800, height = 1800)
  


  })
    
