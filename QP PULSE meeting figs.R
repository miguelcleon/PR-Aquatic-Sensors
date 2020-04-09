library(xts)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(gridExtra)
library(units)
library(purrr)

#pull data - from ODM2 server
QS <- read.csv2("QP_metab_data.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "DOPer_Derived"
QS$DOPer_Derived <- as.numeric(as.character(QS$DOPer_Derived))
colnames(QS)[7] <- "DOConc_Adj"
QS$DOConc_Adj <- as.numeric(as.character(QS$DOConc_Adj))
colnames(QS)[10] <- "Stage_ft"
QS$Stage_ft <- as.numeric(as.character(QS$Stage_ft))
colnames(QS)[13] <- "Temp"
QS$Temp <- as.numeric(as.character(QS$Temp))
colnames(QS)[16] <- "DOPer_Sat"
QS$DOPer_Sat <- as.numeric(as.character(QS$DOPer_Sat))
colnames(QS)[19] <- "DOConc"
QS$DOConc <- as.numeric(as.character(QS$DOConc))
colnames(QS)[22] <- "Light_Lux"
QS$Light_Lux <- as.numeric(as.character(QS$Light_Lux))
colnames(QS)[25] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))
colnames(QS)[28] <- "Temp2"
QS$Temp2 <- as.numeric(as.character(QS$Temp2))

colnames(QS)[5] <- "DOPer_DerivedQC"
colnames(QS)[8] <- "DOConc_AdjQC"
colnames(QS)[11] <- "Stage_ftQC"
colnames(QS)[14] <- "TempQC"
colnames(QS)[17] <- "DOPer_SatQC"
colnames(QS)[20] <- "DOConcQC"
colnames(QS)[23] <- "Light_LuxQC"
colnames(QS)[26] <- "DischargeQC"
colnames(QS)[29] <- "Temp2QC"

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M:%S", tz="America/Puerto_Rico"))
attr(QS$DATETIME, "tzone") <- "UTC"

###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UCT')

##need to preserve all datetime records and use that to merge
qsdt <- subset(QS, select = c(DATETIME))
qsdt$Site <- "QP"

##separate out by variable
dod <- subset(QS, select = c(DOPer_Derived,DATETIME,DOPer_DerivedQC), QS$DOPer_DerivedQC != 'Bad')
te <- subset(QS, select = c(Temp,DATETIME,TempQC), QS$TempQC != 'Bad')
dop <- subset(QS, select = c(DOConc,DATETIME,DOConcQC), QS$DOConcQC != 'Bad')
doc <- subset(QS, select = c(DOConc_Adj,DATETIME,DOConc_AdjQC), QS$DOConc_AdjQC != 'Bad')
s <- subset(QS, select = c(Stage_ft,DATETIME,Stage_ftQC), QS$Stage_ftQC != 'Bad')
dos <- subset(QS, select = c(DOPer_Sat,DATETIME,DOPer_SatQC), QS$DOPer_SatQC != 'Bad')
q <- subset(QS, select = c(DischargeCFS,DATETIME,DischargeQC), QS$DischargeQC != 'Bad')
l <- subset(QS, select = c(Light_Lux,DATETIME,Light_LuxQC), QS$Light_LuxQC != 'Bad')
tem <- subset(QS, select = c(Temp2,DATETIME,Temp2QC), QS$Temp2QC != 'Bad')

qsdt2 = qsdt[!duplicated(qsdt$DATETIME),]
s2 = s[!duplicated(s$DATETIME),]
te2 = te[!duplicated(te$DATETIME),]

###bring them back together
qsq <- merge(qsdt2,s2,by = "DATETIME", all.x = TRUE)
qss <- merge(qsq,dod,by = "DATETIME", all.x = TRUE)
qsd <- merge(qss,te2,by = "DATETIME", all.x = TRUE)
qst <- merge(qsd,dop,by = "DATETIME", all.x = TRUE)
qsb <- merge(qst,doc,by = "DATETIME", all.x = TRUE)
qsk <- merge(qsb,dos,by = "DATETIME", all.x = TRUE)
qsp <- merge(qsk,q,by = "DATETIME", all.x = TRUE)
qso <- merge(qsp,l,by = "DATETIME", all.x = TRUE)
qsn <- merge(qso,tem,by = "DATETIME", all.x = TRUE)

##quick plot
ggplot(qsn, aes(DATETIME, DischargeCFS)) +
  geom_point()


#use Temp2

##final sat and conc
qsn$DOPer_final <- ifelse(qsn$DATETIME < '2016-09-16 00:15:00',qsn$DOPer_Derived,qsn$DOPer_Sat)

ggplot(qsn, aes(DATETIME, DOPer_final)) +
  geom_point()

qsn$DOConc_final <- ifelse(qsn$DATETIME < '2016-09-16 00:15:00',qsn$DOConc,qsn$DOConc_Adj)

ggplot(qsn, aes(DATETIME, DOConc_final)) +
  geom_point()


#include this for Bill ... interesting that warm temps stretch into 2019
ggplot(qsn, aes(DATETIME, Temp2)) +
  geom_point() +
  labs(y="Water Temp (degC)") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())

##what does whole DO look like?
ggplot(qsn, aes(DATETIME, DOPer_final)) +
  geom_point() +
  labs(y="DO (% Sat)") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())


##figures
ggplot(qsn, aes(DATETIME, Light_Lux))+
  geom_point()

###figure of diel O2 pre, drought, post-hurricane
pre <- subset(qsn, DATETIME > '2014-09-15 00:15:00' & DATETIME < '2014-10-15 00:15:00')

ggplot(pre, aes(DATETIME, DOPer_final)) +
  geom_point()

drought <- subset(qsn, DATETIME > '2015-07-01 00:15:00' & DATETIME < '2015-08-01 00:15:00')

ggplot(drought, aes(DATETIME, DOPer_final)) +
  geom_point()

hur2018 <- subset(qsn, DATETIME > '2018-03-01 00:15:00' & DATETIME < '2018-04-01 00:15:00')

ggplot(hur2018, aes(DATETIME, DOPer_final)) +
  geom_point()

hur2019 <- subset(qsn, DATETIME > '2019-06-01 00:15:00' & DATETIME < '2019-07-01 00:15:00')

ggplot(hur2019, aes(DATETIME, DOPer_final)) +
  geom_point()


f1 <- ggplot(pre, aes(DATETIME, DOPer_final)) +
  geom_point() +
  labs(y="DO (% Sat)") +
  scale_y_continuous(limits = c(20,120)) +
  annotate("text", x = as.POSIXct("2014-09-22 10:00:00"), y = 50, label = "baseline", color="blue", size = 8) +
  theme_bw() + 
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())

f2 <- ggplot(drought, aes(DATETIME, DOPer_final)) +
  geom_point() +
  scale_y_continuous(limits = c(20,120)) +
  labs(y="DO (% Sat)") +
  annotate("text", x = as.POSIXct("2015-07-20 10:00:00"), y = 110, label = "drought 2015", color="blue", size = 8) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_blank())

f3 <- ggplot(hur2018, aes(DATETIME, DOPer_final)) +
  geom_point() +
  scale_y_continuous(limits = c(20,120)) +
  labs(y="DO (% Sat)") +
  annotate("text", x = as.POSIXct("2018-03-12 10:00:00"), y = 50, label = "post-Maria 2018", color="blue", size = 8) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18),axis.title.x=element_blank())

f4 <- ggplot(hur2019, aes(DATETIME, DOPer_final)) +
  geom_point() +
  scale_y_continuous(limits = c(20,120)) +
  labs(y="DO (% Sat)") +
  annotate("text", x = as.POSIXct("2019-06-17 10:00:00"), y = 50, label = "post-Maria 2019", color="blue", size = 8) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_blank())

grid.arrange(f1, f2, f3, f4, ncol=2)


