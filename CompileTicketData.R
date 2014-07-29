setwd("~/Documents/R/redlight/")
setwd("/Users/rajivshah/Box Sync/RLC/Tickets/chicago-rlc-data/")
tickets <- read.csv("all_rlc_tickets.txt", header=FALSE)
tickets <- tickets[c("V2","V6")]
tickets$PosixDate <- as.POSIXct(strptime(tickets$V2, format="%Y-%m-%d %H:%M:%S"))
tickets <- tickets[c("V6","PosixDate")]
colnames(tickets)[1] <- "Address"
#intersections <- unique(tickets$V6) 
#write.csv(intersections, file = "/data/intersections.csv")
setwd("~/Documents/R/redlight/")
intersections <- read.csv("./data/intersections.csv")
intersections <- intersections[c("Address","IntersectionID","Comment")]
#intersections$IntersectionID <- as.numeric(as.character(intersections$IntersectionID))
ticket <- merge (tickets, intersections)
#ticket$IntersectionID <- as.numeric(ticket$IntersectionID)
save(ticket,file="./data/ticket.rda")





