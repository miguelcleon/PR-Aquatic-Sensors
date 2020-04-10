library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)
library(plyr)

##working on just QS here for now####

#pull data - from ODM2 server
QS <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QSmydatashort.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

#grab data
czo <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/PR Sensors CSV Query.csv", header = T, fill = TRUE,
                 sep = ",", na.strings=c("","NA"))
QSczo <- subset(czo, Sample.Name == "QS")

lter <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/PR Sensors LTER Query.csv", header = T, fill = TRUE,
                  sep = ",", na.strings=c("","NA"))
QSlter <- subset(lter, Sample_ID == "QS")



#need to convert data to numeric
for(i in 9:ncol(QSczo)) {
  QSczo[,i] <- as.numeric(as.character(QSczo[,i]))
}

colnames(QSczo)[17] <- "NO3mgL"

for(i in 10:ncol(QSlter)) {
  QSlter[,i] <- as.numeric(as.character(QSlter[,i]))
}

colnames(QSlter)[14] <- "NO3mgL"


#need to convert sensor data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[13] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[4] <- "NO3mgL"
QS$NO3mgL <- as.numeric(as.character(QS$NO3mgL))
colnames(QS)[7] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))
colnames(QS)[10] <- "Stage_adjusted_ft"
QS$Stage_adjusted_ft <- as.numeric(as.character(QS$Stage_adjusted_ft))
colnames(QS)[16] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[19] <- "fDOMmV"
QS$fDOMmV <- as.numeric(as.character(QS$fDOMmV))
colnames(QS)[22] <- "TurbmV"
QS$TurbmV <- as.numeric(as.character(QS$TurbmV))
colnames(QS)[25] <- "Stage_cm"
QS$Stage_cm <- as.numeric(as.character(QS$Stage_cm))


##fix time in LTER df
QSlter$Sample_Time <- as.numeric(as.character(QSlter$Sample_Time)) 
QSlter$Sample_Time <- sprintf("%04d",QSlter$Sample_Time)
QSlter$Sample_Time = paste0(substr(QSlter$Sample_Time,1,2),":",substr(QSlter$Sample_Time,3,4),":", 
                            substr(QSlter$Sample_Time,5,6),00)

QSlter$DATETIME <- with(QSlter, ymd(QSlter$Sample_Date) + hms(QSlter$Sample_Time))
QSlter$DATETIME = as.POSIXct(strptime(QSlter$DATETIME, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))


#QSlter$Sample_Time <- as.POSIXct(QSlter$Sample_Time, format="%H%M")

#QSlter$DATETIME <- as.POSIXct(paste(QSlter$Sample_Date, QSlter$Sample_Time), format="%Y-%m-%d %H:%M", tz="UTC")


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
QSczo$DATETIME = as.POSIXct(strptime(QSczo$DateTime, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))


###want to round the time to 15 minutes#####
QSczo$DATETIME <- as.POSIXct(round(as.numeric(QSczo$DATETIME)/(15*60))*(15*60),
                             origin='1970-01-01 12:00:00 America/Puerto_Rico', tz="America/Puerto_Rico")
QSlter$DATETIME <- as.POSIXct(round(as.numeric(QSlter$DATETIME)/(15*60))*(15*60),
                              origin='1970-01-01 12:00:00 America/Puerto_Rico', tz="America/Puerto_Rico")
QS$DATETIME <- round_date(QS$DATETIME, "15 minutes")


##need to condense sensor data, so only one sample per time

QS2 <- ddply(QS,.(DATETIME),colwise(mean, na.rm=TRUE))


#need to combine these to plot NO3 vs NO3

qsall <- merge(QS2, QSczo, by="DATETIME", all.x = TRUE)
qsall2 <- merge(qsall, QSlter, by="DATETIME", all.x = TRUE)


##plots
ggplot(qsall2, aes(NULL)) +
  geom_point(aes(DATETIME,NPOC..mg.C.L.)) +
  geom_point(aes(DATETIME,DOC..mg.L.)) +
  geom_line(aes(DATETIME,DischargeCFS/100), color = "blue") +
  scale_y_continuous(limits = c(0,7), 
                     sec.axis = sec_axis(~.*100 , name = "Discharge (cfs)")) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 7,
           alpha = .3) +
  labs(x = "Date",
       y = "DOC (mg/L)") +
  theme_bw()

isco <- subset(qsall2, as.Date(DATETIME) > "2018-04-01 23:01:00 UTC")
isco2 <- subset(isco, as.Date(DATETIME) < "2018-04-25 12:46:00 UTC")

ggplot(isco2, aes(NULL)) +
  geom_point(aes(DATETIME,NPOC..mg.C.L.)) +
  geom_point(aes(DATETIME,DOC..mg.L.)) +
  geom_line(aes(DATETIME,DischargeCFS/25), color = "blue") +
  scale_y_continuous(limits = c(0,7), 
                     sec.axis = sec_axis(~.*25 , name = "Discharge (cfs)")) +
  labs(x = "Date",
       y = "DOC (mg/L)") +
  theme_bw()


##fDOMmV###

ggplot(qsall2, aes(DATETIME,fDOMmV)) +
  geom_point()


