## K means clustering

rm(list = ls())

library(ISLR)
library(readxl)
library(data.table)
library(tidyverse)
library(ggplot2)

main_wd <- getwd()
temp_wd <- "C:/Users/nthor/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
#temp_wd <- "C:/Users/Noah/University of California, Davis/TTP 210 - General/TTP 201 ADA/DataAnalysis"
setwd(temp_wd)
Lines <- read_excel("pax_December_2021_analysis.xlsx", sheet = "Lines", range = cell_cols("A:E"))
setwd(main_wd)
WorkingData <- fread("Data/SepToMarData.csv")

WorkingData <- subset(WorkingData,`Line Long` != "ANY - On-Call")
WorkingData <- subset(WorkingData,!is.na(WorkingData$`Departure Time (Actual)`))
WorkingData <- subset(WorkingData, `Departure Time (Actual)` < `Arrival Time (Actual)`)
WorkingData <- subset(WorkingData, !is.na(In) & ! is.na(Out))
WorkingData <- subset(WorkingData,`Line Short` != "T")
WorkingData <- subset(WorkingData, `Line Short` != "AIR")


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
Lines$Terminal <- ifelse(Lines$Terminal == "MU", 1, 0)
Lines <- subset(Lines, !is.na(`Pay Time`))


Lines.Scaled <- as.data.frame(scale(Lines[c(4,5,9,10, 11)]))


hc.complete <- hclust(dist(Lines.Scaled),method='complete')
hc.average <- hclust(dist(Lines.Scaled),method='average')
hc.single <- hclust(dist(Lines.Scaled),method='single')

plot(hc.complete,xlab='',sub='',cex=0.9,main=NULL)
plot(hc.average,xlab='',sub='',cex=0.9,main=NULL)
plot(hc.single,xlab='',sub='',cex=0.9,main=NULL)

Lines <- as.data.table(Lines)

Lines[,hc.Clusters.2:=cutree(hc.complete,2)]
Lines[,hc.Clusters.3:=cutree(hc.complete,3)]
Lines[,hc.Clusters.4:=cutree(hc.complete,4)]
Lines[,hc.Clusters.5:=cutree(hc.complete,5)]

Lines$hc.Clusters.2 <- as.factor(Lines$hc.Clusters.2)
Lines$hc.Clusters.3 <- as.factor(Lines$hc.Clusters.3)
Lines$hc.Clusters.4 <- as.factor(Lines$hc.Clusters.4)
Lines$hc.Clusters.5 <- as.factor(Lines$hc.Clusters.5)

summary(Lines.Scaled)

summary(Lines)


plot(Lines, col = Lines$hc.Clusters.5)

c2 <- aggregate(Total ~ hc.Clusters.2, Lines, mean)
c2$PT <- aggregate(`Pay Time` ~ hc.Clusters.2, Lines, mean)$`Pay Time`
c2$Miles <- aggregate(Miles ~ hc.Clusters.2, Lines, mean)$Miles
#c2$SOT <- aggregate(SumOfTotal ~ hc.Clusters.2, Lines, mean)$SumOfTotal
c2$Time <- aggregate(Time ~ hc.Clusters.2, Lines, mean)$Time
c2

c3 <- aggregate(Total ~ hc.Clusters.3, Lines, mean)
c3$PT <- aggregate(`Pay Time` ~ hc.Clusters.3, Lines, mean)$`Pay Time`
c3$Miles <- aggregate(Miles ~ hc.Clusters.3, Lines, mean)$Miles
#c3$SOT <- aggregate(SumOfTotal ~ hc.Clusters.3, Lines, mean)$SumOfTotal
c3$Time <- aggregate(Time ~ hc.Clusters.3, Lines, mean)$Time
c3

c4 <- aggregate(Total ~ hc.Clusters.4, Lines, mean)
c4$PT <- aggregate(`Pay Time` ~ hc.Clusters.4, Lines, mean)$`Pay Time`
c4$Miles <- aggregate(Miles ~ hc.Clusters.4, Lines, mean)$Miles
#c4$SOT <- aggregate(SumOfTotal ~ hc.Clusters.4, Lines, mean)$SumOfTotal
c4$Time <- aggregate(Time ~ hc.Clusters.4, Lines, mean)$Time
c4

c5 <- aggregate(Total ~ hc.Clusters.5, Lines, mean)
c5$PT <- aggregate(`Pay Time` ~ hc.Clusters.5, Lines, mean)$`Pay Time`
c5$Miles <- aggregate(Miles ~ hc.Clusters.5, Lines, mean)$Miles
#c5$SOT <- aggregate(SumOfTotal ~ hc.Clusters.5, Lines, mean)$SumOfTotal
c5$Time <- aggregate(Time ~ hc.Clusters.5, Lines, mean)$Time
c5

write.csv(Lines, "Lines List.csv")


#ggplot(data = Lines) + geom_point() + facet_grid()
