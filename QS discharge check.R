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

##pull in all stage & Q from LTER DB
#load RODBC package into 'R'
library(RODBC)

#Connect 'R' to Access Database
#This "file name" needs to match the DSN in the Access ODBC
#To change this go to Control Panel-Administrative Tools-Data Sources (ODBC)
#     and create a new Access data source under the User DSN tab
PRLTER.db <- odbcConnect("PRLTER database") 

data <- sqlFetch(PRLTER.db,"USGS QS Useable 15Minute Discharge Table")

colnames(data)[5] <- "Sample_Date"

##add the time
data$MINUTE <- as.numeric(as.character(data$MINUTE)) 
data$MINUTE <- sprintf("%04d",data$MINUTE)
data$MINUTE = paste0(substr(data$MINUTE,1,2),":",substr(data$MINUTE,3,4),":", 
                            substr(data$MINUTE,5,6),00)

data$DATETIME <- with(data, ymd(data$Sample_Date) + hms(data$MINUTE))
data$DATETIME = as.POSIXct(strptime(data$DATETIME, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

#date stuff working
col_idx <- grep("DATETIME", names(data))
data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

#numeric
for(i in 8:ncol(data)) {
  data[,i] <- as.numeric(as.character(data[,i]))
}


#pull data - from ODM2 server
QS <- read.csv2("QS_QstageNO3.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="UTC"))

###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 04:00:00 UTC')

#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "NO3mgL"
QS$NO3mgL <- as.numeric(as.character(QS$NO3mgL))
colnames(QS)[5] <- "NO3QC"
colnames(QS)[7] <- "Stageft_sensor"
QS$Stageft_sensor <- as.numeric(as.character(QS$Stageft_sensor)) 
colnames(QS)[8] <- "StageftQC_sensor"
colnames(QS)[10] <- "Qcfs"
QS$Qcfs <- as.numeric(as.character(QS$Qcfs)) 
colnames(QS)[11] <- "QcfsQC"
colnames(QS)[13] <- "Stageft_manual"
QS$Stageft_manual <- as.numeric(as.character(QS$Stageft_manual)) 
colnames(QS)[16] <- "Stageft"
QS$Stageft <- as.numeric(as.character(QS$Stageft)) 
colnames(QS)[17] <- "StageftQC"
colnames(QS)[22] <- "Stagecm"
QS$Stagecm <- as.numeric(as.character(QS$Stagecm)) 
colnames(QS)[23] <- "StagecmQC"



####plots

ggplot(NULL) +
  geom_point(data=data, aes(DATETIME, USGS_cfps))
ggplot(NULL) +
  geom_point(data=QS, aes(DATETIME, Qcfs))

###subset
newqs <- subset(data, as.POSIXct(DATETIME) > "2000-01-01 01:00:00 EDT")
newqs2 <- subset(newqs, USGS_cfps > 0)

ggplot(NULL) +
  geom_point(data=newqs2, aes(DATETIME, USGS_cfps))

###full plot
ggplot(NULL) +
  geom_point(data=newqs2, aes(DATETIME, USGS_cfps)) +
  geom_point(data=QS, aes(DATETIME, Qcfs)) +
  scale_y_log10() +
  labs(x = "Date",
     y = "Q (cfs)") +
  theme_bw()

###Q-stage
qmodel <- lm(USGS_cfps ~ USGS_STAGE, data=newqs2)
qmodel

ggplot(newqs2, aes(USGS_STAGE, USGS_cfps)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  scale_y_log10() +
  scale_x_log10() +
  annotate("text", x = 5, y = 10000, label = "y = 17.24x - 49.75", color="red", size = 6) +
  theme_bw()

qmodel2 <- lm(Qcfs ~ Stageft, data=QS)
qmodel2

ggplot(QS, aes(Stageft, Qcfs)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  annotate("text", x = 5, y = 1000, label = "y = 41.41x - 147.56", color="red", size = 6) +
  scale_y_log10() +
  scale_x_log10() +
  theme_bw()





