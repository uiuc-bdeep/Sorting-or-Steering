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
setwd("C:/Users/genin/OneDrive/Documents/Git/Discrimination/data")

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
## You need to put the key here
register_google(key = "")

#Load Data 

recs = readRDS("recsprocessed_JPE.rds")
ads = readRDS("adsprocessed_JPE.rds")
rechomes <- read.csv("rechomes.csv")
rhgeo <- read.csv("rhgeo_final.csv")
sales <- read.csv("sales.csv")
tester <- read.csv("tester_1.csv")
assignment <- read.csv("assignment.csv")
rhgeo_rechomes_merged_full <- read.csv("rhgeo_rechomes_merged_full.csv")
rechomes <- rhgeo_rechomes_merged_full

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



### PROCESS rechomes DATA
# create aggregate race factor variables using other-designated racial categories

rechomes$white <- 0
rechomes$white[rechomes$RACEID == 11 | rechomes$RACEID == 21 | rechomes$RACEID == 31] <- 1

rechomes$black <- 0
rechomes$black[rechomes$RACEID == 12 | rechomes$RACEID == 22 | rechomes$RACEID == 32] <- 1

rechomes$hispanic <- 0
rechomes$hispanic[rechomes$RACEID == 13 | rechomes$RACEID == 23 | rechomes$RACEID == 33] <- 1

rechomes$asian <- 0
rechomes$asian[rechomes$RACEID == 14 | rechomes$RACEID == 24 | rechomes$RACEID == 34] <- 1

rechomes$natamer <- 0
rechomes$natamer[rechomes$RACEID == 15 | rechomes$RACEID == 25 | rechomes$RACEID == 35] <- 1

rechomes$other <- 0
rechomes$other[rechomes$RACEID == 16 | rechomes$RACEID == 26 | rechomes$RACEID == 36] <- 1

# create aggregate factor variable for minority testers using other-designated racial categories
rechomes$minority <- 0
rechomes$minority[rechomes$RACEID == 12 | rechomes$RACEID == 22 | rechomes$RACEID == 32] <- 1
rechomes$minority[rechomes$RACEID == 13 | rechomes$RACEID == 23 | rechomes$RACEID == 33] <- 2
rechomes$minority[rechomes$RACEID == 14 | rechomes$RACEID == 24 | rechomes$RACEID == 34] <- 3
rechomes$minority[rechomes$RACEID == 15 | rechomes$RACEID == 25 | rechomes$RACEID == 35] <- 4
rechomes$minority[rechomes$RACEID == 16 | rechomes$RACEID == 26 | rechomes$RACEID == 36] <- 5
rechomes$minority <- as.factor(rechomes$minority)

# create factor variables for homes covariates
rechomes$HHMTYPE <- as.factor(rechomes$HHMTYPE)

### PROCESS TESTER DATA
# create matching tester variable
tester$TESTERID <- tester$TesterID

# create tester age variable
tester$date <- as.Date(tester$TDOB, format="%m/%d/%Y")
tester$datepos <- as.POSIXlt(tester$date)
tester$year <- as.numeric(tester$datepos$year+1900)
tester$age <- as.numeric(2012-tester$year)

# create Self-Identified Race factor Variable that includes Asian and Hispanic 
# "other" subcategories are reference
tester$SubAsian <- as.numeric(tester$TASIANG)
tester$SubHispanic <- as.numeric(tester$THISPUBG)
tester$APRACE[tester$SubAsian ==1 | tester$SubAsian == 2 | tester$SubAsian == 3| tester$SubAsian == 4 | tester$SubAsian == 5 | tester$SubAsian == 6] <- 4
tester$APRACE[tester$SubHispanic ==1 | tester$SubHispanic == 2 | tester$SubHispanic == 3| tester$SubHispanic == 4 | tester$SubHispanic == 5 | tester$SubHispanic == 6 | tester$SubHispanic == 7 | tester$SubHispanic == 8] <- 3
tester$APRACE <- as.factor(tester$APRACE)
tester$TASIANG <- as.factor(tester$TASIANG)
tester$THISPUBG[tester$THISPUBG ==8] <- -1
tester$THISPUBG <- as.factor(tester$THISPUBG)

# create factor variables for income and education categories
tester$HH40k <- 0
tester$HH40k[tester$THHEGAI <5] <- 1
tester$HH40k <- as.factor(tester$HH40k)

tester$THHEGAI <- as.factor(tester$THHEGAI)
tester$TPEGAI <- as.factor(tester$TPEGAI)
tester$THIGHEDU <- as.factor(tester$THIGHEDU)
tester$TCURTENR <- as.factor(tester$TCURTENR)

# Merge with addresses on trial
#rechomes_rhgeo <- merge(rechomes, rhgeo, by = c("CONTROL", "TESTERID", "SEQRH"))
rechomes_rhgeo_sales <- merge(rechomes,sales, by = c("CONTROL", "TESTERID"))
rechomes_rhgeo_sales_tester <- merge(rechomes_rhgeo_sales, tester, by = c("TESTERID"))
rechomes_rhgeo_sales_assignment <- merge(rechomes_rhgeo_sales_tester, assignment, by = c("CONTROL", "TESTERID"))


### MAPS  
# plot sample
#######################################
# Chicago Map

CH6508ads <- subset(ads, CONTROL=="CH-SA-6508-1")
CH6508ads$Race <- as.factor(0)
CH6508 <- subset(rechomes_rhgeo_sales_assignment, CONTROL=="CH-SA-6508-1")
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

ggsave("HUDtrialmap_CH.png", width = 8, height = 5)

  
## Los Angeles Map

LA3306ads <- subset(ads, CONTROL=="LA-SA-3306-1")
LA3306ads$Race <- as.factor(0)
LA3306 <- subset(rechomes_rhgeo_sales_assignment, CONTROL=="LA-SA-3306-1")

LA3306$Race <- LA3306$APRACE
LA3306 <- LA3306[!duplicated(LA3306[,2:3]),]
LA3306$both <- as.numeric(duplicated(LA3306[,3]))
LA3306$Race[LA3306$both==1] <- 5
lat3306 <-LA3306ads$Latitude[1] 
lon3306 <-LA3306ads$Longitude[1] 


CA = c(lon = lon3306, lat = lat3306)
CA.map = get_map(location = CA,  maptype = c("hybrid"), source = c("google"), zoom = 11)
CA <- ggmap(CA.map)
CA + geom_point(aes(x = Longitude, Latitude, colour = Race), data = LA3306, size=3) +
  geom_point(aes(x = Longitude, Latitude, colour = Race), data = LA3306ads, size=5) + 
  scale_colour_manual(values=c("black","red", "blue", "green", "orange"))

ggsave("HUDtrialmap_LA.png", width = 8, height = 5)

