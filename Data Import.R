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

Enrollment <- read_excel("Enrollment.xlsx", sheet = "All Students", range = cell_cols("A:F"))
Calendar <- read_excel("Academic Calendar.xlsx", sheet = "Sheet1")

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

Enrollment_saved <- Enrollment
Enrollment <- Enrollment_saved

Enrollment <- Enrollment %>% mutate(
  TERM = ifelse(TERM == 202109, "Fall Semester", 
                 ifelse(TERM == 202110, "Fall Quarter",
                        ifelse(TERM == 202201, "Winter Quarter", NA))),
  StartTime = str_split(CRSE_TIME, "-", simplify = TRUE)[,1],
  EndTime = str_split(CRSE_TIME, "-", simplify = TRUE)[,2],
  Monday = ifelse(grepl("M", CRSE_DAYS, fixed = TRUE), 1, 0),
  Tuesday = ifelse(grepl("T", CRSE_DAYS, fixed = TRUE), 1, 0),
  Wednesday = ifelse(grepl("W", CRSE_DAYS, fixed = TRUE), 1, 0),
  Thursday = ifelse(grepl("R", CRSE_DAYS, fixed = TRUE), 1, 0),
  Friday = ifelse(grepl("F", CRSE_DAYS, fixed = TRUE), 1, 0),
  Saturday = ifelse(grepl("S", CRSE_DAYS, fixed = TRUE), 1, 0),
  Sunday = ifelse(grepl("U", CRSE_DAYS, fixed = TRUE), 1, 0),
  StartTimeNum = as.numeric(substr(StartTime, 1, 2)) + as.numeric(substr(StartTime, 3, 4))/60,
  EndTimeNum = as.numeric(substr(EndTime, 1, 2)) + as.numeric(substr(EndTime, 3, 4))/60
)

Enrollment <- subset(Enrollment, CRSE_TIME != "-")
Enrollment <- subset(Enrollment, BLDG != "OFFCAM")
Enrollment <- subset(Enrollment, BLDG != "ONLINE")
Enrollment <- subset(Enrollment, BLDG != "REMOTE")
Enrollment <- subset(Enrollment, BLDG != "BIMH")

Precision <- 10 # This is the level of precision in our schedule, or how many minutes between each block
Passing_Period <- 15 # This is the number of minutes before and after to look at

Schedule <- data.frame(
  Day = c(
    rep("Monday", 24*60/Precision+1), rep("Tuesday", 24*60/Precision+1), rep("Wednesday", 24*60/Precision+1), rep("Thursday", 24*60/Precision+1), rep("Friday", 24*60/Precision+1), rep("Saturday", 24*60/Precision+1), rep("Sunday", 24*60/Precision+1)
    ),
  Time = c(
    seq(0, 24, by = Precision/60), seq(0, 24, by = Precision/60), seq(0, 24, by = Precision/60), seq(0, 24, by = Precision/60), seq(0, 24, by = Precision/60), seq(0, 24, by = Precision/60), seq(0, 24, by = Precision/60)
    )
  )
len <- nrow(Schedule)
Schedule <- data.frame(
  Term = c(
    rep("Fall Semester", len), rep("Fall Quarter", len), rep("Winter Quarter", len)
  ),
  Day = c(
    Schedule$Day, Schedule$Day, Schedule$Day
  ),
  Time = c(
    Schedule$Time, Schedule$Time, Schedule$Time
  )
)
Schedule$IncomingStudents <- 0
Schedule$OutgoingStudents <- 0

# This is a pile of stinking garbage code and I hate it but I don't know how else to do it. This also needs to be setup for the quarters.
for (x in 1:nrow(Schedule)) {
  if (Schedule[x,]$Day == "Monday") {
    dayofweek <- subset(Enrollment, Monday == 1)
  }
  if (Schedule[x,]$Day == "Tuesday") {
    dayofweek <- subset(Enrollment, Tuesday == 1)
  }
  if (Schedule[x,]$Day == "Wednesday") {
    dayofweek <- subset(Enrollment, Wednesday == 1)
  }
  if (Schedule[x,]$Day == "Thursday") {
    dayofweek <- subset(Enrollment, Thursday == 1)
  }
  if (Schedule[x,]$Day == "Friday") {
    dayofweek <- subset(Enrollment, Friday == 1)
  }
  if (Schedule[x,]$Day == "Saturday") {
    dayofweek <- subset(Enrollment, Saturday == 1)
  }
  if (Schedule[x,]$Day == "Sunday") {
    dayofweek <- subset(Enrollment, Sunday == 1)
  }
  
  termsubset <- subset(dayofweek, TERM == Schedule[x,]$Term)
  
  ins <- subset(termsubset, 
                StartTimeNum >= Schedule[x,]$Time - Passing_Period/60 & 
                  StartTimeNum <= Schedule[x,]$Time + Passing_Period/60
                )
  outs <- subset(termsubset, 
                 EndTimeNum >= Schedule[x,]$Time - Passing_Period/60 & 
                   EndTimeNum <= Schedule[x,]$Time + Passing_Period/60
  )
  Schedule[x,]$IncomingStudents <- ifelse(!is.na(sum(ins$ENROLLED)),sum(ins$ENROLLED),0)
  Schedule[x,]$OutgoingStudents <- ifelse(!is.na(sum(outs$ENROLLED)),sum(outs$ENROLLED),0)
}

Combined_Schedule_Calendar <- data.frame(
  Date = c(),
  Time = c(),
  Week = c(),
  Holiday = c(),
  Finals = c(),
  IncomingStudents = c(),
  OutgoingStudents = c()
)

for (n in 1:nrow(Calendar)) {
  weekday_ref <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  times <- subset(Schedule, Day == weekday_ref[Calendar[n,]$`Day Of Week`] & Term == Calendar[n,]$Term)
  for (i in 1:nrow(times)) {
    append_frame <- data.frame(
      Date = Calendar[n,]$Date,
      Time = Schedule[i,]$Time,
      Week = Calendar[n,]$Week,
      Holiday = Calendar[n,]$Holiday,
      Finals = Calendar[n,]$`Finals?`,
      IncomingStudents = Schedule[i,]$IncomingStudents,
      OutgoingStudents = Schedule[i,]$OutgoingStudents
    )
    Combined_Schedule_Calendar <- rbind(Combined_Schedule_Calendar, append_frame)
  }
}

Combined_Schedule_Calendar$DateTime <- ymd_hm(paste(Combined_Schedule_Calendar$Date, " ", floor(Combined_Schedule_Calendar$Time), ":", round(Combined_Schedule_Calendar$Time%%1*60), sep = ""))


# This is more garbage code and it's terrible, but it should work...
SepToMar$OutgoingAtScheduledStart <- NA
SepToMar$IncomingAtScheduledEnd <- NA
SepToMar$OutgoingAtRealStart <- NA
SepToMar$IncomingAtRealEnd <- NA

for (n in 1:nrow(SepToMar)){
  target_time <- lubridate::round_date(SepToMar[n,]$`Start Time (Scheduled)`, "10 minutes")
  working_subset <- subset(Combined_Schedule_Calendar, as_datetime(DateTime) == as_datetime(target_time))
  if (nrow(working_subset) != 1) {
    next
  }
  
  SepToMar[n,]$OutgoingAtScheduledStart <- 
    working_subset$OutgoingStudents
  if(n%%500==0){print(n)}
}
  for (n in 1:nrow(SepToMar)){    
  target_time <- lubridate::round_date(SepToMar[n,]$`End Time (Scheduled)`, "10 minutes")
  working_subset <- subset(Combined_Schedule_Calendar, as_datetime(DateTime) == as_datetime(target_time))
  if (nrow(subset(Combined_Schedule_Calendar, DateTime == target_time)) != 1) {
    next
  }
  
  SepToMar[n,]$IncomingAtScheduledEnd <- 
    working_subset$IncomingStudents
  if(n%%500==0){print(n)}
}
for (n in 1:nrow(SepToMar)){  
  target_time <- lubridate::round_date(SepToMar[n,]$`Departure Time (Actual)`, "10 minutes")
  working_subset <- subset(Combined_Schedule_Calendar, as_datetime(DateTime) == as_datetime(target_time))
  if (nrow(subset(Combined_Schedule_Calendar, DateTime == target_time)) != 1) {
    next
  }
  
  SepToMar[n,]$OutgoingAtRealStart <- 
    working_subset$OutgoingStudents
  if(n%%500==0){print(n)}
}
for (n in 1:nrow(SepToMar)){  
  target_time <- lubridate::round_date(SepToMar[n,]$`Arrival Time (Actual)`, "10 minutes")
  working_subset <- subset(Combined_Schedule_Calendar, as_datetime(DateTime) == as_datetime(target_time))
  if (nrow(subset(Combined_Schedule_Calendar, DateTime == target_time)) != 1) {
    next
  }
  
  SepToMar[n,]$IncomingAtRealEnd <- 
    working_subset$IncomingStudents
  
  if(n%%500==0){print(n)}
}


# Setting up COVID data
SepToMar <- merge(x = SepToMar, y = covid[,c("Date", "cases", "deaths")], by = "Date", all.x = TRUE)

write.csv(SepToMar, "Data/SepToMarData.csv")
