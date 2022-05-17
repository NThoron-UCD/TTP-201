# Importing data here:

rm(list = ls())

library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)

main_wd <- getwd()
#temp_wd <- "C:/Users/nthor/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
temp_wd <- "C:/Users/Noah/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
setwd(temp_wd)

Sep21 <- read_excel("pax_September_2021_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))
Oct21 <- read_excel("pax_October_2021_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))
Nov21 <- read_excel("pax_November_2021_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))
Dec21 <- read_excel("pax_December_2021_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))
Jan22 <- read_excel("pax_January_2022_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))
Feb22 <- read_excel("pax_February_2022_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))
Mar22 <- read_excel("pax_March_2022_analysis.xlsx", sheet = "pax_data", range = cell_cols("A:AB"))

covid <- fread("covid19cases_test.csv")
# From:
# https://data.chhs.ca.gov/dataset/covid-19-time-series-metrics-by-county-and-state


setwd(main_wd)

#names(Feb22)[1:length(names(Sep21))] <- names(Sep21)

SepToMar <- bind_rows(Sep21, Oct21, Nov21, Dec21, Jan22, Feb22, Mar22)

# Renaming
names(SepToMar)[names(SepToMar) == "Line...11"] <- 'Line Long'
names(SepToMar)[names(SepToMar) == "Line...13"] <- 'Line Short'

# Fixing Dates
SepToMar <- SepToMar %>% mutate(
  Date = as_date(Date),
  `Start Time (Scheduled)` = ymd_hms(paste(Date, format(`Start Time (Scheduled)`, format = "%H:%M:%S"))),
  `End Time (Scheduled)` = ymd_hms(paste(Date, format(`End Time (Scheduled)`, format = "%H:%M:%S"))),
  `Departure Time (Actual)` = ymd_hms(paste(Date, format(`Departure Time (Actual)`, format = "%H:%M:%S"))),
  `Arrival Time (Actual)` = ymd_hms(paste(Date, format(`Arrival Time (Actual)`, format = "%H:%M:%S"))),
  `Adj Arr` = ymd_hms(paste(Date, format(`Adj Arr`, format = "%H:%M:%S"))),
  `Adj Dep` = ymd_hms(paste(Date, format(`Adj Dep`, format = "%H:%M:%S")))
)

covid <- covid %>% mutate(
  Date = as.Date(date, "%m/%d/%Y")
)

# Setting up COVID data
SepToMar <- merge(x = SepToMar, y = covid[,c("Date", "cases", "deaths")], by = "Date", all.x = TRUE)

write.csv(SepToMar, "Data/SepToMarData.csv")
