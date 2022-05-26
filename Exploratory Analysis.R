rm(list = ls())

library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork) # For dual axis charts
library(MASS) # For stepwise regression

WorkingData <- fread("Data/SepToMarData.csv")

WorkingData <- subset(WorkingData,`Line Long` != "ANY - On-Call")
WorkingData <- subset(WorkingData,!is.na(WorkingData$`Departure Time (Actual)`))
WorkingData <- subset(WorkingData, `Departure Time (Actual)` < `Arrival Time (Actual)`)
WorkingData <- subset(WorkingData, !is.na(In) & ! is.na(Out))

WorkingData$StartHour <- hour(WorkingData$`Start Time (Scheduled)`)
WorkingData$StartHourSqrd <- WorkingData$StartHour^2
WorkingData$StartHourCubd <- WorkingData$StartHour^3

WorkingData$LateOrEarly <- ifelse(WorkingData$Late_Early > 0 , 1, 0)
WorkingData$VeryLOE <- ifelse(abs(WorkingData$Late_Early) >= 60, 1, 0)

WorkingData$AMPeak <- ifelse(WorkingData$StartHour == 8 , 1, 0)
WorkingData$PMPeak <- ifelse((WorkingData$StartHour == 4 | WorkingData$StartHour == 5), 1, 0)
WorkingData$EarlyMorLateNit <- ifelse((WorkingData$StartHour <= 6 | WorkingData$StartHour >= 21), 1, 0)
WorkingData$Weekend <- ifelse((WorkingData$Column <= 70 & WorkingData$Column >= 60) , 1, 0)

WorkingData$Terminal <- ifelse(WorkingData$`Line Short` %in% c('A','C','D','J','L','W','X'), 'Silo', 'MU')
WorkingData$Run <- ifelse((minute(WorkingData$`Start Time (Scheduled)`)==0 | minute(WorkingData$`Start Time (Scheduled)`)==10), 'First run', 'Second run')
WorkingData$FriPM <- ifelse(wday(WorkingData$Date)==5 & WorkingData$PMPeak==1, 1, 0)
WorkingData <- subset(WorkingData,`Line Short` != "T")


#Exploratory regressions
summary(lm(Late_Early ~ as.factor(`Line Short`) + WC + In + Out + Miles + `Pay Time` + as.factor(Capac) + Crowd
           + am(`Start Time (Scheduled)`) + WorkingData$StartHour + WorkingData$StartHourSqrd + WorkingData$StartHourCubd
           , data = WorkingData))

summary(lm(Late_Early ~ WorkingData$StartHour + WorkingData$StartHourSqrd + WorkingData$StartHourCubd
           , data = WorkingData))

summary(lm(Late_Early ~ Total + In + Out
           , data = WorkingData))

summary(lm(Late_Early ~ cases + deaths
           , data = WorkingData))

summary(lm(Late_Early ~ OutgoingAtScheduledStart + IncomingAtScheduledEnd
           , data = WorkingData))

summary(lm(Late_Early ~ OutgoingAtRealStart + IncomingAtRealEnd
           , data = WorkingData))


#Stepwise Regression
full.model <- lm(Late_Early ~ Date + `Start Time (Scheduled)` +
                   `Pay Time` +
                   WC + In + Out + Total + Miles +
                   #as.factor(`Line Short`) +
                   #as.factor(Column) +
                   as.factor(Capac) + Crowd + StartHour + 
                   StartHourSqrd + StartHourCubd + cases + deaths +
                   OutgoingAtScheduledStart + IncomingAtScheduledEnd +
                   OutgoingAtRealStart + IncomingAtRealEnd +
                   AMPeak + PMPeak + EarlyMorLateNit + Weekend +
                 , data = WorkingData)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)



# Some plots to visualize average daily stuff.
basicAgg <- aggregate(Late_Early ~ Date, data = WorkingData, mean)
plot(basicAgg)

passAgg <- aggregate(Total ~ Date, data = WorkingData, mean)
plot(passAgg)

caseAgg <- aggregate(cases ~ Date, data = WorkingData, mean)
plot(caseAgg)

coeff = 5
ggplot() +
  geom_point(data = passAgg, aes(x = Date, y = Total), color = "blue") +
  geom_point(data = basicAgg, aes(x = Date, y = (Late_Early+5)*coeff), color = "green") + 
  scale_y_continuous(
    name = "Total Passengers",
    sec.axis = sec_axis(~./coeff-5, name = "Late/Early")
  )
