install.packages("tidyverse")
install.packages("stringdist")
library(tidyverse)
library(stringdist)



Origin_dir <- getwd()
if(!file.exists('repdata_data_stormdata.csv.bz2')){
  download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', 'repdata_data_stormdata.csv.bz2')
}

stormDataraw <- read.csv(bzfile('repdata_data_stormdata.csv.bz2')) 
stormData <- stormDataraw %>% select(EVTYPE, BGN_DATE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)  %>%
                          mutate(BGN_YEAR = as.numeric(format(as.Date(BGN_DATE, format = "%m/%d/%Y"), "%Y"))) %>% 
                          filter(BGN_YEAR >= 1996)
stormData$EVTYPE <- str_trim(toupper(stormData$EVTYPE))


EV <- read.csv('C:/Users/e547839/Downloads/EventName.csv')
EVunique <- as_data_frame(unique(stormData$EVTYPE))

m <- amatch(EVunique$value, EV$ï..EventName, maxDist = 5)
EVunique$EVTYPE <- EV$ï..EventName[m]

EXP <- function(PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) {
  PROPDMGEXP <- toupper(PROPDMGEXP)
  CROPDMGEXP <- toupper(CROPDMGEXP)
  if (grepl("B", PROPDMGEXP) == TRUE) { y <- PROPDMG * 10^9}
  else if (grepl("M", PROPDMGEXP) == TRUE) { y <- PROPDMG * 10^6}
  else if (grepl("K", PROPDMGEXP) == TRUE) { y <- PROPDMG * 10^3}
  else if (grepl("H", PROPDMGEXP) == TRUE) { y <- PROPDMG * 10^2}
  else if (grepl("[0123456789]", PROPDMGEXP) == TRUE) { y <- PROPDMG *10 + as.numeric(PROPDMGEXP)}
  else  { y <- PROPDMG}
  
  if (grepl("B", CROPDMGEXP) == TRUE) { z <- 10^9*CROPDMG}
  else if (grepl("M", CROPDMGEXP) == TRUE) { z <- 10^6*CROPDMG}
  else if (grepl("K", CROPDMGEXP) == TRUE) { z <- 10^3*CROPDMG}
  else if (grepl("H", CROPDMGEXP) == TRUE) { z <- 10^2*CROPDMG}
  else if (grepl("[0123456789]", CROPDMGEXP) == TRUE) { z <- 10*CROPDMG + as.numeric(CROPDMGEXP)}
  else  { z <- CROPDMG}
  
  return(y+z)
}

stormData$totalDMG <- mapply(EXP, stormData$PROPDMG, stormData$PROPDMGEXP, stormData$CROPDMG, stormData$CROPDMGEXP) 
EVdmg <- stormData %>% group_by(EVTYPE) %>% summarise(DMG = sum(totalDMG)) 

topEVdmg <- EVdmg[order(EVdmg$DMG, decreasing = TRUE),] [1:10,]

ggplot(topEVdmg, aes(x = reorder(EVTYPE, DMG), y = DMG))+ 
  geom_bar(stat = 'identity',  color= "white", fill ="lightblue", width = 0.7)+
  labs(title = "Top Event Type by damage", y = "Damage ( Property + Corp)", x = "Event Type")+
  coord_flip() 

EVhealth <- stormData %>% group_by(EVTYPE) %>% summarise(healthImpact = sum(FATALITIES) + sum(INJURIES))
                                                                                     
topEVhealth <- EVhealth[order(EVhealth$healthImpact, decreasing = TRUE), ][1:10, ]
                                                        
ggplot(topEVhealth, aes(x = reorder(EVTYPE, healthImpact), y = healthImpact))+ 
  geom_bar(stat = 'identity',  color= "white", fill ="yellow", width = 0.7)+
  labs(title = "Top Event Type by Health Impact", y = "Impact ( Fatalities + Injuries)", x = "Event Type")+
  coord_flip() 




