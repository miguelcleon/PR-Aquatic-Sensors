library(xts)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(gridExtra)
library(units)
library(purrr)

#pull data - from ODM2 server
QS <- read.csv2("QS Maria Final Export.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))
QP <- read.csv2("QP Final.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))
RE <- read.csv2("RESSH_DO.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))
QPA <- read.csv2("QPA_DO.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))
QPB <- read.csv2("QPB_DO.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

##

#need to convert data to numeric and fix datetime for final CSVs
for(i in 3:ncol(QS)) {
  QS[,i] <- as.numeric(as.character(QS[,i]))
}
for(i in 3:ncol(QP)) {
  QP[,i] <- as.numeric(as.character(QP[,i]))
}
QS$DATETIME = as.POSIXct(strptime(QS$DATETIME, "%Y-%m-%d %H:%M:%S", tz="America/Puerto_Rico"))
QP$DATETIME = as.POSIXct(strptime(QP$DATETIME, "%Y-%m-%d %H:%M:%S", tz="America/Puerto_Rico"))

#need to convert the other files to workable
#convert serial date to POSIX object to allow the timestamp to be compliant with R
RE$DATETIME = as.POSIXct(strptime(RE$Date.and.Time, "%Y-%m-%d %H:%M:%S", tz="America/Puerto_Rico"))
QPA$DATETIME = as.POSIXct(strptime(QPA$Date.and.Time, "%Y-%m-%d %H:%M:%S", tz="America/Puerto_Rico"))
QPB$DATETIME = as.POSIXct(strptime(QPB$Date.and.Time, "%Y-%m-%d %H:%M:%S", tz="America/Puerto_Rico"))

colnames(RE)[4] <- "DOPer_Sat"
RE$DOPer_Sat <- as.numeric(as.character(RE$DOPer_Sat))
colnames(QPA)[4] <- "DOPer_Sat"
QPA$DOPer_Sat <- as.numeric(as.character(QPA$DOPer_Sat))
colnames(QPB)[4] <- "DOPer_Sat"
QPB$DOPer_Sat <- as.numeric(as.character(QPB$DOPer_Sat))

##plots
ggplot(QPA, aes(DATETIME,DOPer_Sat)) +
  geom_point()

ggplot(QPB, aes(DATETIME,DOPer_Sat)) +
  geom_point()

ggplot(RE, aes(DATETIME,DOPer_Sat)) +
  geom_point()

ggplot(QS, aes(DATETIME,DOPer_Sat)) +
  geom_point()

ggplot(QP, aes(DATETIME,DOPer_final)) +
  geom_point()

###plot of same period
QPA2019 <- subset(QPA, DATETIME > '2019-04-01 00:15:00' & DATETIME < '2019-08-01 00:15:00')
QPB2019 <- subset(QPB, DATETIME > '2019-04-01 00:15:00' & DATETIME < '2019-08-01 00:15:00')
RE2019 <- subset(RE, DATETIME > '2019-04-01 00:15:00' & DATETIME < '2019-08-01 00:15:00')
QS2019 <- subset(QS, DATETIME > '2018-04-01 00:15:00' & DATETIME < '2018-08-01 00:15:00')
QP2019 <- subset(QP, DATETIME > '2019-04-01 00:15:00' & DATETIME < '2019-08-01 00:15:00')

##individual plots
ggplot(QPA2019, aes(DATETIME,DOPer_Sat)) +
  geom_point() +
  labs(y="DO (% Sat)") +
  scale_y_continuous(limits = c(60,120)) +
  annotate("text", x = as.POSIXct("2019-05-22 10:00:00"), y = 110, label = "QPA", color="blue", size = 8) +
  theme_bw() + 
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())

ggplot(QPB2019, aes(DATETIME,DOPer_Sat)) +
  geom_point() +
  labs(y="DO (% Sat)") +
  scale_y_continuous(limits = c(60,120)) +
  annotate("text", x = as.POSIXct("2019-05-22 10:00:00"), y = 110, label = "QPB", color="blue", size = 8) +
  theme_bw() + 
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())

ggplot(RE2019, aes(DATETIME,DOPer_Sat)) +
  geom_point() +
  labs(y="DO (% Sat)") +
  scale_y_continuous(limits = c(60,120)) +
  annotate("text", x = as.POSIXct("2019-05-22 10:00:00"), y = 110, label = "RESSH", color="blue", size = 8) +
  theme_bw() + 
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())

ggplot(QS2019, aes(DATETIME,DOPer_Sat)) +
  geom_point()  +
  labs(y="DO (% Sat)") +
  scale_y_continuous(limits = c(60,120)) +
  annotate("text", x = as.POSIXct("2018-05-22 10:00:00"), y = 110, label = "QS", color="blue", size = 8) +
  theme_bw() + 
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())


ggplot(QP2019, aes(DATETIME,DOPer_final)) +
  geom_point() +
  labs(y="DO (% Sat)") +
  scale_y_continuous(limits = c(60,120)) +
  annotate("text", x = as.POSIXct("2019-07-22 10:00:00"), y = 70, label = "QP", color="blue", size = 8) +
  theme_bw() + 
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())





