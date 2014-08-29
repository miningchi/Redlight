setwd("~/Documents/R/redlight/data/")
print ("The CSV file should be named RedlighttoExport.csv")
df <- read.csv("~/Documents/R/redlight/data/RedlighttoExport.csv")
df$PosixDate <- as.POSIXct(strptime(df$GOLIVEDATE, format="%m/%d/%Y"))
#df <- df[c("Primary.Type", "Description", "Latitude", "Longitude", "PosixDate")]

df1$INTERSECTION <- as.character(df1$INTERSECTION)

save(df1,file="redlight2.rda")
setwd("~/Documents/R/redlight/")
#Initial files
setwd("/Users/rajivshah/Box Sync/Accident/2009-12datafromIDOT/csv/")
df2009 <- read.csv("2009Cleaned.csv")
df2010 <- read.csv("2010Cleaned.csv")
setwd("/Users/rajivshah/Documents/Projects/Cameras/Red Lights Cameras/Data2009to2012/")
df2011<-read.csv("RLC2011.csv", header = TRUE, sep = ",")
df2012<-read.csv("RLC2012.csv", header = TRUE, sep = ",")
df2009$CrashLongitude = (df2009$CrashLongitude * (-1))
df2010$CrashLongitude = (df2010$CrashLongitude * (-1))
head(df2009$CrashLongitude)

dfall <- rbind(df2009,df2010,df2011,df2012)
dfall$PosixDate <- as.POSIXct(strptime(dfall$CrashDate, format="%m/%d/%y"))


dfall$CollisionType[dfall$CollisionType == 'Rear end'] <- 'Rear End'
dfall$CollisionType[dfall$CollisionType == 'Head on'] <- 'Head On'
dfall$CollisionType[dfall$CollisionType == 'Sideswipe same direction'] <- 'Sideswipe Same Direction'
dfall$CollisionType[dfall$CollisionType == 'Parked motor vehicle'] <- 'Parked Motor Vehicle'
dfall$CollisionType[dfall$CollisionType == 'Sideswipe opp direction'] <- 'Sideswipe Opposite Direction'
dfall$CollisionType[dfall$CollisionType == 'Other object'] <- 'Other Object'
dfall$CollisionType[dfall$CollisionType == 'Other non collision'] <- 'Other Non-Collision'
dfall$CollisionType[dfall$CollisionType == 'Fixed object'] <- 'Fixed Object'
dfall$CollisionType[dfall$CollisionType == 'Fixed object'] <- 'Fixed Object'

dfall$CollisionType <- as.character(dfall$CollisionType)
dfall$CollisionType <- as.factor(dfall$CollisionType)
str(dfall$CollisionType)
spg <- dfall
spg <- subset(spg,(CrashLatitude>=30))

spg <- subset(spg,(CrashLatitude>30 & CrashLongitude<=-50))

total <- spg
total1 <- total[c("CrashID", "CollisionType", "collisiontypecode", "totalkilled","total.injured",
                 "Crashseverity", "TrafficControlDevice.1","LightCondition.1",
                 "Weather.1","RoadSurface.1","IntersectionRelated", "ContrCause1","ContrCause2","HitandRun",
                 "CrashLatitude", "CrashLongitude", "CrashCoordindateY","CrashCoordinateX","PosixDate")]
totalALL <- total1

save(totalALL,file ="totalALL.rda")

getwd()

