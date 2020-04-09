library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(dataRetrieval)

#QP data from ODM2 server
qpczo <- read.csv("QP_Qstage.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

###dates and column names
qpczo$DATETIME = as.POSIXct(strptime(qpczo$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

colnames(qpczo)[4] <- "Stage_ft"
colnames(qpczo)[7] <- "discharge_cfs"

qpczo$discharge_cms <- qpczo$discharge_cfs * 0.028316847

###view it
ggplot(qpczo, aes(DATETIME,Stage_ft)) +
  geom_line()

ggplot(qpczo, aes(DATETIME,discharge_cfs)) +
  geom_line() +
  scale_y_log10()

##apply new curve
## y = 0.0005 * X^8.8846
qpczo$newQ_cms <- 0.0005 * qpczo$Stage_ft^8.8846

##how do they compare?

ggplot(qpczo, aes(DATETIME,discharge_cms)) +
  geom_line() +
  geom_line(aes(DATETIME,newQ_cms), color="blue") +
  scale_y_log10()

sum(qpczo$discharge_cms, na.rm=T)
sum(qpczo$newQ_cms, na.rm=T)
