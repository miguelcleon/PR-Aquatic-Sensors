library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(RColorBrewer)

#pull data - from ODM2 server
QS <- read.csv2("QS_QstageNO3.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="UTC"))

###want to round the time to 15 minutes#####
QS$DATETIME2 <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 04:00:00 UTC')


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "NO3mgL"
QS$NO3mgL <- as.numeric(as.character(QS$NO3mgL))
colnames(QS)[5] <- "NO3QC"
colnames(QS)[10] <- "Qcfs"
QS$Qcfs <- as.numeric(as.character(QS$Qcfs)) 
colnames(QS)[11] <- "QcfsQC"
colnames(QS)[7] <- "Stageft_sensor"
QS$Stageft <- as.numeric(as.character(QS$Stageft)) 
colnames(QS)[8] <- "StageftQC_sensor"
colnames(QS)[16] <- "Stageft"
QS$Stageft <- as.numeric(as.character(QS$Stageft)) 
colnames(QS)[17] <- "StageftQC"
colnames(QS)[22] <- "Stagecm"
QS$Stageft <- as.numeric(as.character(QS$Stageft)) 
colnames(QS)[23] <- "StagecmQC"


##separate out by variable
no <- subset(QS, select = c(NO3mgL,DATETIME2,NO3QC), QS$NO3QC != 'Bad')
q <- subset(QS, select = c(Qcfs,DATETIME2,QcfsQC), QS$QcfsQC != 'Bad')
s <- subset(QS, select = c(Stageft,DATETIME2,StageftQC), QS$StageftQC != 'Bad')


###bring them back together
qsf <- merge(no,q,by = "DATETIME2")
qsd <- merge(qsf,s,by = "DATETIME2")

###ok, now want plots logQ vs NO3 and SC before and after storms

##just plot os log Q vs NO3 & stage
ggplot(qsd, aes(Qcfs, NO3mgL)) +
  geom_point() +
  scale_x_log10() +
  scale_y_continuous(limits = c(0,1.25)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw()


#now look for some storms
ggplot(qsd, aes(DATETIME2,Qcfs)) +
  geom_line()

ggplot(qsd, aes(DATETIME2,NO3mgL)) +
  geom_point()




