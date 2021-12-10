# ---------------------------------------------------------------------------------------------- #
#   Generate Figure B.1 Racial Composition, Incomes, and Neighborhood Characteristics            #
#                                                                                                # 
#   R-Version: 4.04                                                                              #                                                             #
#   Date Last Modification: 12/01/2021                                                           #
# -----------------------------------------------------------------------------------------------#

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

# Load packages
packages <- c("readxl", "readstata13", "lfe", "Synth","data.table", "plm", "ggplot2", "MatchIt", "experiment", "stargazer","Hmisc")
lapply(packages, pkgTest)

# Output
out <- "/~"

##Load Data
recs_trial_final <- readRDS("HUDprocessed_JPE_census_042021.rds")

### Pot Sample

### Poverty Rate

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = povrate_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  labs(y = "Poverty Rate") + labs(x = "share white") + labs(colour = "High Income White") +
  geom_smooth(method='lm')

ggsave("Cor_White_Poverty.pdf", width = 8, height = 5)


### Share High Skill

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = skill_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  labs(y = "Share High Skill") + labs(x = "share white") + labs(colour = "High Income") +
  geom_smooth(method='lm')
ggsave("Cor_White_highskill.pdf", width = 8, height = 5)

### Share College Educated

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = college_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  labs(y = "Share College Educated") + labs(x = "share white") + labs(colour = "High Income") +
  geom_smooth(method='lm')  

ggsave("Cor_White_college.pdf", width = 8, height = 5)

### Elementary School Quality

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = Elementary_School_Score_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  labs(y = "Elementary School Quality") + labs(x = "share white") + labs(colour = "High Income") +
  geom_smooth(method='lm')
ggsave("Cor_White_School.pdf", width = 8, height = 5)

### Assaults

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = Assault_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  labs(y = "Assaults") + labs(x = "share white") + labs(colour = "High Income") +
  geom_smooth(method='lm')
ggsave("Cor_White_assault.pdf", width = 8, height = 5)

### Superfund Sites

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = SFcount_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  scale_y_sqrt() +
  labs(y = "Superfund Sites <5km") + labs(x = "share white") + labs(colour = "High Income") +
  geom_smooth(method='lm')
ggsave("Cor_White_Superfund.pdf", width = 8, height = 5)

### Toxic Releases

p1 <- ggplot(recs_trial_final, aes( x = w2012pc_Rec, y = RSEI_Rec))
p1 + geom_point(aes(color = WhiteHI_Rec)) +
  scale_colour_gradient(low = "yellow", high = "red") +
  scale_y_sqrt() +
  labs(y = "Toxic Releases") + labs(x = "share white") + labs(colour = "High Income") +
  geom_smooth(method='lm')
ggsave("Cor_White_RSEI.pdf", width = 8, height = 5)

