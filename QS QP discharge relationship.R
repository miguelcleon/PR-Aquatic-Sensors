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

#####can we get recent 

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

col_idx <- grep("DATETIME", names(data))
data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

#numeric
for(i in 8:ncol(data)) {
  data[,i] <- as.numeric(as.character(data[,i]))
}

QS <- subset(data, as.POSIXct(DATETIME) > "2006-06-27 12:00:00 EDT")

ggplot(QS, aes(DATETIME,USGS_cfps)) +
  geom_point()

#convert USGS data to m3/s, easier to work with
QS$USGS_cms <- QS$USGS_cfps * 0.028316847

ggplot(QS, aes(DATETIME,USGS_cms)) +
  geom_point()

#pull data - from ODM2 server
QP <- read.csv2("Prieta_Pool0_Discharge_2006-2014.csv", header = T, 
                fill = TRUE, sep = ",", na.strings=c("","NA"))

QP$timehr <- format(strptime(QP$Time, "%I:%M %p"), format="%H:%M:%S")
QP$Dateyr <- as.Date(strptime(QP$Date, "%m/%d/%Y"))
QP$DATETIME <- with(QP, ymd(QP$Dateyr, tz="America/Puerto_Rico") + 
                      hms(QP$timehr))

QP[,3] <- as.numeric(as.character(QP[,3]))
QP[,4] <- as.numeric(as.character(QP[,4]))

ggplot(QP, aes(DATETIME,Discharge)) +
  geom_point() +
  scale_y_log10() +
  theme_bw()


###ok, need relationship between them
###bring together 1st
qsqp <- merge(QS,QP,by = "DATETIME")

ggplot(qsqp, aes(Discharge, USGS_cms)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method='lm',formula=y~x) +
  labs(x = "QP Discharge (cms)",
       y = "QS Discharge (cms)") +
  annotate("text", x=0.5, y=0.05, label = "r2 = 0.27") +
  theme_bw()

###let's get the equation
qmodel <- lm(log(USGS_cms) ~ log(Discharge), data=qsqp)
summary(qmodel)


###check runoff to see if same. 2009 looks like best complete year for QP
runoff <- subset(qsqp, as.POSIXct(DATETIME) > "2008-12-31 23:45:00 EDT" & 
                as.POSIXct(DATETIME) < "2010-01-01 00:15:00 EDT")

####QS#####
##area QS is 2622783 m2
##area QP is 350021 m2
###m/s, divide by area
runoff$runoffqs <- runoff$USGS_cms / 2622783
runoff$runoffqp <- runoff$Discharge / 350021

###m/d
runoff$runoffqsd <- runoff$runoffqs * 900
runoff$runoffqpd <- runoff$runoffqp * 900
runoff3 <- aggregate(cbind(USGS_cms, Discharge, USGS_STAGE, runoffqsd, Stage, runoffqpd) 
                     ~ Sample_Date, runoff2, sum)

ggplot(runoff3, aes(Sample_Date,Discharge)) +
  geom_point()

ggplot(runoff3, aes(Sample_Date,USGS_cms)) +
  geom_point()

###sum for m/yr
runoffqstotal <- sum(runoff3$runoffqsd)
runoffqstotal

runoffqptotal <- sum(runoff3$runoffqpd)
runoffqptotal







