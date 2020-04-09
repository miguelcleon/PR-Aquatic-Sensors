library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)
library(grid)
library(scales)
library(viridis)

#pull data - from ODM2 server
QS <- read.csv2("QS NO3 new.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "NO3"
QS$NO3 <- as.numeric(as.character(QS$NO3))

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))



#pull finalized data
QSF <- read.csv2("QS Maria Final Export.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
for(i in 3:ncol(QSF)) {
  QSF[,i] <- as.numeric(as.character(QSF[,i]))
}

#convert serial date to POSIX object to allow the timestamp to be compliant with R
colnames(QSF)[2] <- "Date.and.Time"
QSF$DATETIME = as.POSIXct(strptime(QSF$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

##subset newer data for just after end of final data
tail(QSF)
QS2 <- subset(QS, as.Date(DATETIME) > "2019-04-12 07:00:00 AST")
head(QS2)

##plot
QSL <- subset(QSF, as.Date(DATETIME) > "2017-06-01 23:01:00 UTC")
QSLL <- subset(QS2, as.Date(DATETIME) < "2019-07-15 23:01:00 UTC")


ggplot(NULL, aes(DATETIME, NO3)) +
  geom_line(data = QSL, color = "darkblue", size = 1) +
  geom_line(data = QSLL, color = "darkblue", size = 1) +
  scale_y_continuous(limits = c(0,1.2)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3, fill = "darkgreen") +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3, fill = "darkgreen") +
  annotate("text", x = as.POSIXct("2017-08-29 10:00:00 AST"), y = 1.0, label = "Irma", angle = 90, 
           color="black", size = 8) +
  annotate("text", x = as.POSIXct("2017-09-27 10:00:00 AST"), y = 1.0, label = "Maria", angle = 90, 
           color="black", size = 8) +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=18), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())





