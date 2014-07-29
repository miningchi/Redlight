setwd("~/Documents/R/redlight/data/")
print ("The CSV file should be named RedlighttoExport.csv")
df <- read.csv("~/Documents/R/redlight/data/RedlighttoExport.csv")
df$PosixDate <- as.POSIXct(strptime(df$GOLIVEDATE, format="%m/%d/%Y"))
#df <- df[c("Primary.Type", "Description", "Latitude", "Longitude", "PosixDate")]

df1$INTERSECTION <- as.character(df1$INTERSECTION)

save(df1,file="redlight2.rda")
setwd("~/Documents/R/redlight/")

