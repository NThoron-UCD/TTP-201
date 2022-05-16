# Importing data here:

library(data.table)
library(readxl)
library(tidyverse)
library(lubridate)

main_wd <- getwd()
#temp_wd <- "C:/Users/nthor/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
temp_wd <- "C:/Users/Noah/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
setwd(temp_wd)

Sep21 <- read_excel("pax_September_2021_analysis.xlsx", sheet = "pax_data")
Oct21 <- read_excel("pax_October_2021_analysis.xlsx", sheet = "pax_data")
Nov21 <- read_excel("pax_November_2021_analysis.xlsx", sheet = "pax_data")
Dec21 <- read_excel("pax_December_2021_analysis.xlsx", sheet = "pax_data")
Jan22 <- read_excel("pax_January_2022_analysis.xlsx", sheet = "pax_data")
Feb22 <- read_excel("pax_February_2022_analysis.xlsx", sheet = "pax_data")
Mar22 <- read_excel("pax_March_2022_analysis.xlsx", sheet = "pax_data")

setwd(main_wd)

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

write.csv(SepToMar, "Data/SepToMarData.csv")
