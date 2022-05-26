## K means clustering

library(ISLR)
library(readxl)
library(tidyverse)

main_wd <- getwd()
#temp_wd <- "C:/Users/nthor/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
temp_wd <- "C:/Users/Noah/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
setwd(temp_wd)
Lines <- read_excel("pax_December_2021_analysis.xlsx", sheet = "Lines", range = cell_cols("A:E"))
setwd(main_wd)
WorkingData <- fread("Data/SepToMarData.csv")

Lines <- Lines %>% mutate(
  SumOfTotal = as.numeric(SumOfTotal),
  Line_Group = as.factor(...3),
  Miles = as.numeric(Miles),
  Time = as.numeric(Time)
)

Lines <- merge(Lines, aggregate(In ~ `Line Long`, WorkingData, mean), by.x = "Line", by.y = "Line Long", all.x = TRUE)
Lines <- merge(Lines, aggregate(Out ~ `Line Long`, WorkingData, mean), by.x = "Line", by.y = "Line Long", all.x = TRUE)
Lines <- merge(Lines, aggregate(Total ~ `Line Long`, WorkingData, mean), by.x = "Line", by.y = "Line Long", all.x = TRUE)
Lines <- merge(Lines, aggregate(`Pay Time` ~ `Line Long`, WorkingData, mean), by.x = "Line", by.y = "Line Long", all.x = TRUE)
Lines$Terminal <- as.factor(ifelse(Lines$Line_Group %in% c('A','C','D','J','L','W','X'), 'Silo', 'MU'))
Lines <- subset(Lines, !is.na(`Pay Time`))

Lines.Scaled <- scale(Lines[c(2,4,5,7,8,9,10,11)])


summary(Lines)
