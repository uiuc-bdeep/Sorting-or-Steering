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

#Set WD local
setwd("~/")

#You Must have a Key from Google API to Replicate this Map
register_google(key = "")

#Load Data Shared Drive
recs = readRDS("recsprocessed_JPE.rds")
ads = readRDS("adsprocessed_JPE.rds")

# Output
out <- "~/"

# Map HUD
HUD_pts <- data.matrix(recs[c("Longitude","Latitude")], rownames.force = NA)
  
# get the coordinates
  records <- recs
  coordinates <- HUD_pts
  xmin <- as.numeric(min(recs$Longitude))
  xmax <- as.numeric(max(recs$Longitude))
  ymin <- as.numeric(min(recs$Latitude))
  ymax <- as.numeric(max(recs$Latitude))
  
### MAPS  
# plot sample
  # United States
  ads$market <- as.factor(sapply(strsplit(as.character(ads$CONTROL), "-"), `[`, 1))
  Ads_u <- ads[!duplicated(ads$market), ]
  ads$count <- 1
  Ads_agg <- aggregate(ads$count, by=list(ads$market), FUN = sum)
  Ads_agg$market <- Ads_agg$Group.1
  AdsSample <- merge(Ads_agg, Ads_u, by=c("market"))
  
  USc = c(lon = -97, lat = 41.86)
  bbox <- c(left = -125, bottom = 22, right = -67, top = 50)
  US.map = get_stamenmap(bbox,  maptype = c("terrain-background"), source = c("stamen"), zoom = 4)
  US.map = get_map(location = USc,  maptype = c("hybrid"), source = c("google"), zoom = 4)
  
  US <- ggmap(US.map)
  US + geom_point(aes(x = Longitude, Latitude), colour = "red", size = 1, data = AdsSample)
  
  ggsave("ADS_US.pdf", width = 8, height = 5, dpi = 1500)
  
  