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
WorkingData <- WorkingData[!(is.na(WorkingData$`Departure Time (Actual)`)), ]
WorkingData <- subset(WorkingData, `Departure Time (Actual)` < `Arrival Time (Actual)`)

WorkingData$StartHour <- hour(WorkingData$`Start Time (Scheduled)`)
WorkingData$StartHourSqrd <- WorkingData$StartHour^2
WorkingData$StartHourCubd <- WorkingData$StartHour^3

WorkingData$LateOrEarly <- ifelse(WorkingData$Late_Early > 0 , 1, 0)
WorkingData$VeryLOE <- ifelse(abs(WorkingData$Late_Early) >= 60, 1, 0)

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


#Stepwise Regression
full.model <- lm(Late_Early ~ Date + `Start Time (Scheduled)` + `Pay Time` + WC + In + Out + Total + Miles +
                   as.factor(`Line Short`) + as.factor(Column) + as.factor(Capac) + Crowd + StartHour + 
                   StartHourSqrd + StartHourCubd + cases + deaths
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
