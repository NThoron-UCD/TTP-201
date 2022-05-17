library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(patchwork)

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
plot(basicAgg)

passAgg <- aggregate(Total ~ Date, data = WorkingData, mean)
plot(passAgg)

coeff = 5
ggplot() +
  geom_point(data = passAgg, aes(x = Date, y = Total), color = "blue") +
  geom_point(data = basicAgg, aes(x = Date, y = (Late_Early+5)*coeff), color = "red") + 
  scale_y_continuous(
    name = "Total Passengers vs Late/Early"#,
    #sec.axis = sec.axis(~.*coeff, name = "Late/Early")
  )
