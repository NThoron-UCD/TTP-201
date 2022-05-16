library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)

WorkingData <- fread("Data/SepToMarData.csv")

WorkingData <- subset(WorkingData,`Line Long` != "ANY - On-Call")
WorkingData <- WorkingData[!(is.na(WorkingData$`Departure Time (Actual)`)), ]
WorkingData <- subset(WorkingData, `Departure Time (Actual)` < `Arrival Time (Actual)`)

WorkingData$StartHour <- hour(WorkingData$`Start Time (Scheduled)`)
WorkingData$StartHourSqrd <- WorkingData$StartHour^2
WorkingData$StartHourCubd <- WorkingData$StartHour^3


summary(lm(Late_Early ~ as.factor(`Line Short`) + WC + In + Out + Miles + `Pay Time` + as.factor(Capac) + Crowd
           + am(`Start Time (Scheduled)`) + WorkingData$StartHour + WorkingData$StartHourSqrd + WorkingData$StartHourCubd
           , data = WorkingData))


summary(lm(Late_Early ~ WorkingData$StartHour + WorkingData$StartHourSqrd + WorkingData$StartHourCubd
           , data = WorkingData))

plot(WorkingData$`Start Time (Scheduled)`, WorkingData$Late_Early)

basicAgg <- aggregate(Late_Early ~ Date, data = WorkingData, mean)
