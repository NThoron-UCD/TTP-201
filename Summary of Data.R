library(tidyverse)
library(data.table)
library(lubridate)
test_data <- fread("Data/Oct Unitrans buses.csv")
summary(test_data)
test_data <- test_data %>% mutate(
  Vehicle = as.factor(Vehicle),
  Date = as.Date(Date),
  Day = as.factor(Day),
  `Start Time (Scheduled)` = parse_date_time(`Start Time (Scheduled)`, '%I:%M:%S'),
  `End Time (Scheduled)` = parse_date_time(`End Time (Scheduled)`, '%I:%M:%S'),
  `Departure Time (Actual)` = parse_date_time(`Departure Time (Actual)`, '%I:%M:%S'),
  `Arrival Time (Actual)` = parse_date_time(`Arrival Time (Actual)`, '%I:%M:%S'),
  Line = as.factor(Line),
  Column = as.factor(Column)
)
summary(test_data)

test_data_summary <- data.frame(unclass(summary(test_data)), check.names = FALSE, stringsAsFactors = FALSE)
write.csv(test_data_summary, "Test Data Summary.csv")