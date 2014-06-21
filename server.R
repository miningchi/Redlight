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
load(file = "./data/redlightaccidents.rda")



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
 
    ## add points
    redlightdatabase <- datesubset() 
 p <- map.base + geom_point(aes(x=long, y=lat), colour="red", size = 4, na.rm=TRUE, data=subset(redlightdatabase,Red.Speed == 1))
p <- p + geom_point(aes(x=long, y=lat), colour="blue", size = 4, na.rm=TRUE, data=subset(redlightdatabase,Red.Speed == 0))
 plot(p)
  })
 #, width = 1800, height = 1800)
  
###########
####HEAT MAP
############


output$hmaptitle <- renderUI({helpText(HTML("<b>DENSITY PLOT SETTINGS</b>"))})
output$hmapcenter <- renderUI({textInput("center", "Enter a Location to Center Map, such as city or zipcode, the click Update", "Chicago")})
output$hmaptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
output$hmapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
output$hmapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
output$hmapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 14):", min = 9, max = 20, step = 1, value = 14)})
output$halpharange <-renderUI({sliderInput("halpharanage", "Alpha Range:",
                                           min = 0, max = 1, step = 0.1, value = c(0.1, 0.4))})
output$hbins <-renderUI({sliderInput("hbins", "Number of Bins:", 
                                     min = 5, max = 50, step = 5, value = 15)})
output$hboundwidth <-renderUI({sliderInput("hboundwidth", "Boundary Lines Width:", 
                                           min = 0, max = 1, step = 0.1, value = 0.1)})
output$hboundcolor <-renderUI({selectInput("hboundcolor", "Boundary Lines Colour:", 
                                           choice = c("grey95","black", "white", "red", "orange", "yellow", "green", "blue", "purple"))})
output$hlow <-renderUI({selectInput("hlow", "Fill Gradient (Low):", 
                                    choice = c("yellow", "red", "orange", "green", "blue", "purple", "white", "black", "grey"))})
output$hhigh <-renderUI({selectInput("hhigh", "Fill Gradient (High):", 
                                     choice = c("red", "orange", "yellow", "green", "blue", "purple", "white", "black", "grey"))})



output$heatmap <- renderPlot({
  
  crimetypedatabase <- dfall
  
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
  
  if (is.null(input$zoom)) {temp.zoom <- 14}
  else {temp.zoom <- input$zoom }
  
  if (is.null(input$halpharange)) {temp.halpharange <- c(0.1, 0.4)}
  else {temp.halpharange<- input$halpharange}
  
  if (is.null(input$hbins)) {temp.hbins <- 15}
  else {temp.hbins<- input$hbins}
  
  if (is.null(input$hboundwidth)) {temp.hboundwidth <- .1}
  else {temp.hboundwidth<- input$hboundwidth}
  
  if (is.null(input$hboundcolor)) {temp.hboundcolor <- "grey95"}
  else {temp.hboundcolor<- input$hboundcolor}
  
  if (is.null(input$hlow)) {temp.hlow <- "yellow" }
  else {temp.hlow<- input$hlow}
  
  if (is.null(input$hhigh)) {temp.hhigh <- "red"}
  else {temp.hhigh<- input$hhigh}
  
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
  
  ## add heat map
  map.final <- map.base  +    
    
    ## Create a density plot
    ## based on the ggmap's crime data example
    stat_density2d(aes(x = CrashLongitude, 
                       y = CrashLatitude, 
                       fill = ..level.., 
                       alpha = ..level..),
                   size = temp.hboundwidth, 
                   bins = temp.hbins,  ## Change and experiment with no. of bins
                   data = crimetypedatabase, 
                   geom = "polygon", 
                   colour = temp.hboundcolor) +
    
    ## Configure the scale and panel
    scale_fill_gradient(low = temp.hlow, high = temp.hhigh) +
    scale_alpha(range = temp.halpharange) +
    
    ## Title and labels    
    labs(x = "Longitude", y = "Latitude") +
    # ggtitle(paste("Crimes in/around ", map.center))+ 
    #     " from ", temp.period[1],
    #     " to ", temp.period[length(temp.period)], sep="")) +
    
    ## Other theme settings
    theme_bw() +
    theme(
      plot.title = element_text(size = 36, face = 'bold', vjust = 2),
      #title = element_text(face = 'bold'),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      #axis.text.x = element_text(size = 28),
      #axis.text.y = element_text(size = 28),
      #axis.title.x = element_text(size = 32),
      #axis.title.y = element_text(size = 32),
      strip.background = element_rect(fill = 'grey80'),
      strip.text = element_text(size = 26)
    )
  
  redlightdatabase <- datesubset() 
  p <- map.final + geom_point(aes(x=long, y=lat), colour="red", size = 4, na.rm=TRUE, data=subset(redlightdatabase,Red.Speed == 1))
  p <- p + geom_point(aes(x=long, y=lat), colour="blue", size = 4, na.rm=TRUE, data=subset(redlightdatabase,Red.Speed == 0))
  
  plot(p)
})


  })
    
