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
suppressMessages(library(xts)) #added this for trends
suppressMessages(library(stringr)) #added this for time, not sure if still needed
suppressMessages(library(gtable)) #added this for trends
library("rjson") #added this for json traffic data
#load(file = "./data/weather.rda")
load(file = "./data/crimestest.rda")
#load(file = "./data/crimesfull.rda")

## Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reactive Functions
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  datesubset <- reactive({
          subset(df, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
          })
  
  datetypesubset <- reactive({
                 tempdate   <- subset(df, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
                 tempdatetype <- subset(tempdate, Primary.Type == input$crimetype)
                 return (tempdatetype)
                 })  
  


  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 1 - Data Table
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$datatable <- renderDataTable({
   
    datetypesubset() 
  
    }, options = list(aLengthMenu = c(10, 25, 50, 100, 1000), iDisplayLength = 10))
  
  
  

  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output 2 - Map
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  output$maptitle <- renderUI({helpText(HTML("<b>MAP SETTINGS</b>"))})
  output$mapcenter <- renderUI({textInput("center", "Enter a Location to Center Map, such as city or zipcode, the click Update", "Chicago")})
  output$maptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
  output$mapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
  output$mapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
  output$mapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 14):", min = 9, max = 20, step = 1, value = 14)})
  
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
    
    if (is.null(input$zoom)) {temp.zoom <- 14}
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
    crimetypedatabase <- datetypesubset() 
 p <- map.base + geom_point(aes(x=Longitude, y=Latitude), colour="red", size = 4, na.rm=TRUE, data=crimetypedatabase)
  
 plot(p)
  })
 #, width = 1800, height = 1800)
  
 
  ###### Weather variable ###########
   
 output$weatherperiod <- renderUI({selectInput("wperiod", "Choose Period to Analyze:", choice = c("yearly", "monthly","weekly", "daily"))})
 
 output$weather <- renderPlot({  
    crimetypedatabase <- datetypesubset()   
   
  #Convert to XTS for analysis Columns should be Primary Type and PosixData
    df.xts <- xts(x = crimetypedatabase[, c("Primary.Type","PosixDate")], order.by = crimetypedatabase[, "PosixDate"])
    #dyearly <- apply.yearly(df.xts, function(d) {print(d)}) # Troubleshooting
  
  #sum by crime type - and take into account different scales
  if (is.null(input$wperiod)) {temp.wperiod <- "yearly"}
  else {temp.wperiod <- input$wperiod }
  
  if (temp.wperiod == "daily") {crimebytime <- apply.daily(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
  if (temp.wperiod == "weekly") {crimebytime <- apply.weekly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
  if (temp.wperiod == "monthly") {crimebytime <- apply.monthly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
  if (temp.wperiod == "yearly") {crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
    
  crimebytime<-data.frame(index(crimebytime),coredata(crimebytime[,1]))
    colnames(crimebytime)<-c("dates","crime")
  #print(crimebytime)
  
 ##ADD WEATHER
 weatherdata <- subset(weatherdata, PosixDate > as.POSIXct(strptime(input$startdate, format="%Y-%m-%d")) & PosixDate < as.POSIXct(strptime(input$enddate, format="%Y-%m-%d")))
 weatherxts <- xts(weatherdata$TempFahr,weatherdata$PosixDate)
 weatherxts<-data.frame(index(weatherxts),coredata(weatherxts[,1]))
 colnames(weatherxts)<-c("dates","temperature")
 
 #Use central average to smooth out the data
 if (temp.wperiod == "weekly") {mavwindow=3}
 if (temp.wperiod == "monthly") {mavwindow=9}
 if (temp.wperiod == "yearly") {mavwindow=31}
 if (temp.wperiod != "daily") { mav <- function(x,n=mavwindow){filter(x,rep(1/n,n), sides=2)}
                              weatherxts$temperature <- mav(weatherxts$temperature)}                           
   
#New approach to get two Y lines:
grid.newpage()

# two plots
# from http://rpubs.com/kohske/dual_axis_in_ggplot2

p1 <-ggplot(crimebytime,aes(dates,crime)) + geom_line(colour="red", size=2) + theme_bw()
p2 <-ggplot(weatherxts,aes(dates,temperature)) + geom_line(colour="blue", size=2) + theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

#print(data3)
  }, width = 1280, height = 1280)

########################
###ANALYSIS ###################
##############################

output$analperiod <- renderUI({selectInput("wperiod", "Choose Period to Analyze:", choice = c("yearly", "monthly","weekly", "daily"))})

output$analplot <- renderPlot({ 
crimetypedatabase <- datetypesubset()   

#Convert to XTS for analysis Columns should be Primary Type and PosixData
df.xts <- xts(x = crimetypedatabase[, c("Primary.Type","PosixDate")], order.by = crimetypedatabase[, "PosixDate"])
#dyearly <- apply.yearly(df.xts, function(d) {print(d)}) # Troubleshooting

#sum by crime type - and take into account different scales
if (is.null(input$wperiod)) {temp.wperiod <- "yearly"}
else {temp.wperiod <- input$wperiod }

if (temp.wperiod == "daily") {crimebytime <- apply.daily(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
if (temp.wperiod == "weekly") {crimebytime <- apply.weekly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
if (temp.wperiod == "monthly") {crimebytime <- apply.monthly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}
if (temp.wperiod == "yearly") {crimebytime <- apply.yearly(df.xts, function(d) {sum(str_count(d, input$crimetype ))})}

crimebytime<-data.frame(index(crimebytime),coredata(crimebytime[,1]))
colnames(crimebytime)<-c("dates","crime")
plot(crimebytime)
  })



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Output - Heat Crime Map
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  crimetypedatabase <- datetypesubset() 
  
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
    stat_density2d(aes(x = Longitude, 
                       y = Latitude, 
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

  plot(map.final)
  })

###############################################
#  Traffic
###############################################

output$tmaptitle <- renderUI({helpText(HTML("<b>MAP SETTINGS **NOT LIVE YET**</b>"))})
output$tmapcenter <- renderUI({textInput("center", "Enter a Location to Center Map, such as city or zipcode, the click Update", "Chicago")})
output$tmaptype <- renderUI({selectInput("type", "Choose Google Map Type:", choice = c("roadmap", "satellite", "hybrid","terrain"))})
output$tmapres <- renderUI({checkboxInput("res", "High Resolution?", FALSE)})
output$tmapbw <- renderUI({checkboxInput("bw", "Black & White?", FALSE)})
output$tmapzoom <- renderUI({sliderInput("zoom", "Zoom Level (Recommended - 14):", min = 9, max = 20, step = 1, value = 14)})

output$tmap <- renderPlot({
 
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
  load(file = "./data/traffic.rda")
 
 #traffic <- trafficr()
 
 #traffic = head(traffic,4)

 #print(traffic)
 #print ("hi")
 #print(tnames)
  ## add traffic
  #crimetypedatabase <- datetypesubset() 
 p <- map.base + geom_segment(aes(x=START_LONGITUDE, y=START_LATITUDE,xend=END_LONGITUDE, yend=END_LATITUDE, colour=ifelse(CURRENT_SPEED > "10", "green", "red")), size = 2, data=traffic)
  #print(traffic$CURRENT_SPEED)
  plot(p)
})
#, width = 1800, height = 1800)

  })
    
