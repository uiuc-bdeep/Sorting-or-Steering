# ---------------------------------------------------------------------------------------------- #
#   Figure 1. Markets in 2012 HUD Buyer Experiment                                               #
#                                                                                                # 
#   R-Version: 4.04                                                                              #                                                             #
#   Date Last Modification: 12/01/2021                                                           #
# -----------------------------------------------------------------------------------------------#
#You Must have a Key from Google API to Replicate this Map                                       #
##################################################################################################
# Clear workspace
rm(list = ls()) 

# Set working directory
setwd("~/")

# Define function for loading packages

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE))
  {
    install.packages(x, dep = TRUE)
    if(!require(x, character.only = TRUE)) stop("Package not found")
  }
}
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer","ggmap","maps")
lapply(packages, pkgTest)


#You Must have a Key from Google API to Replicate this Map
register_google(key = "")

#Load Data 

recs = readRDS("HUDprocessed_JPE_census_042021.rds")
recs= readRDS("recsprocessed_JPE.rds") 
ads = readRDS("adsprocessed_JPE.rds")

# Output
out <- "~/"

# Map HUD
HUD_pts <- data.matrix(recs[c("Longitude","Latitude")], rownames.force = NA)

recs$Longitude = recs$INTPTLON10
recs$Latitude = recs$INTPTLAT10

### MAPS  
# plot sample
#######################################
# Chicago Map

CH6508ads <- subset(ads, CONTROL=="CH-SA-6508-1")
CH6508ads$Race <- as.factor(0)
CH6508 <- subset(recs, CONTROL=="CH-SA-6508-1")
CH6508$Race <- CH6508$APRACE

CH6508 <- subset(CH6508, select=-c(SEQRH))
CH6508 <- CH6508[!duplicated(CH6508[,2:3]),]
CH6508$both <- as.numeric(duplicated(CH6508[,3]))
CH6508$Race[CH6508$both==1] <- 5

lat6508 <-CH6508ads$Latitude[1] 
lon6508 <-CH6508ads$Longitude[1] 

chicago = c(lon = lon6508, lat = lat6508)
chicago.map = get_map(location = chicago,  maptype = c("hybrid"), source = c("google"), zoom = 14)
chicago <- ggmap(chicago.map)
chicago + geom_point(aes(x = Longitude, Latitude, colour = Race), data = CH6508, size=3) +
  geom_point(aes(x = Longitude, Latitude, colour = Race), data = CH6508ads, size=5) + 
  scale_colour_manual(values=c("black","red", "blue", "green", "orange"))

ggsave("HUDtrialmap_CH.pdf", width = 8, height = 5, dpi = 1500)


## Los Angeles Map

LA3306ads <- subset(ads, CONTROL=="LA-SA-3306-1")
LA3306ads <- LA3306ads[1,]
LA3306ads$Race <- as.factor(0)
LA3306 <- subset(recs, CONTROL=="LA-SA-3306-1")

LA3306$Race <- LA3306$APRACE
LA3306 <- LA3306[!duplicated(LA3306[,2:3]),]
LA3306$both <- as.numeric(duplicated(LA3306[,3]))
LA3306$Race[LA3306$both==1] <- 5
lat3306 <-mean(LA3306$INTPTLAT10) 
lon3306 <-mean(LA3306$INTPTLON10) 

CA = c(lon = lon3306, lat = lat3306)
CA.map = get_map(location = CA,  maptype = c("hybrid"), source = c("google"), zoom = 11)
CA <- ggmap(CA.map)
CA + geom_point(aes(x = Longitude, Latitude, colour = Race), data = LA3306, size=3) +
  geom_point(aes(x = Longitude, Latitude, colour = Race), data = LA3306ads, size=5) + 
  scale_colour_manual(values=c("black","red", "blue", "green", "orange"))

ggsave("HUDtrialmap_LA.pdf", width = 8, height = 5, dpi = 1500)

