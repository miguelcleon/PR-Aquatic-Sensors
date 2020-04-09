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
colnames(QS)[10] <- "Qcfs"
QS$Qcfs <- as.numeric(as.character(QS$Qcfs)) 
colnames(QS)[11] <- "QcfsQC"
colnames(QS)[7] <- "Stageft_sensor"
QS$Stageft_sensor <- as.numeric(as.character(QS$Stageft_sensor)) 
colnames(QS)[8] <- "StageftQC_sensor"
colnames(QS)[16] <- "Stageft"
QS$Stageft <- as.numeric(as.character(QS$Stageft)) 
colnames(QS)[17] <- "StageftQC"
colnames(QS)[22] <- "Stagecm"
QS$Stagecm <- as.numeric(as.character(QS$Stagecm)) 
colnames(QS)[23] <- "StagecmQC"


##separate out by variable
no <- subset(QS, select = c(NO3mgL,DATETIME,NO3QC), QS$NO3QC != 'Bad')
q <- subset(QS, select = c(Qcfs,DATETIME,QcfsQC), QS$QcfsQC != 'Bad')
s <- subset(QS, select = c(Stageft,DATETIME,StageftQC), QS$StageftQC != 'Bad')

#check Q
ggplot(q, aes(DATETIME,Qcfs)) +
  geom_point() +
  theme_bw()


#pull in RES4 data

res <- 
  readNWISuv(
    siteNumbers='50063800', parameterCd=c('00060','00065'), startDate='2016-01-01', 
    endDate='2018-07-25', tz='America/Jamaica') 
renameNWISColumns(res)

colnames(res)[6] <- "RES_Stage_ft"
colnames(res)[4] <- "RES_Discharge_cfs"
colnames(res)[3] <- "DATETIME"

##there is some 5 minute data in record, get it down to 15
res %>%
  group_by(DATETIME = cut(DATETIME, breaks="15 min")) %>%
  summarize(RES_Discharge_cfs = mean(RES_Discharge_cfs), RES_Stage_ft = mean(RES_Stage_ft))

ggplot(res, aes(DATETIME,RES_Discharge_cfs)) +
  geom_point()

#Pull in QP data
#pull data - from ODM2 server
QP <- read.csv2("QP_Qstage.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QP$DATETIME = as.POSIXct(strptime(QP$Date.and.Time, "%Y-%m-%d %H:%M", tz="UTC"))
###want to round the time to 15 minutes#####
QP$DATETIME <- as.POSIXct(round(as.numeric(QP$DATETIME)/(15*60))*(15*60),origin='1970-01-01 04:00:00 UTC')
colnames(QP)[4] <- "QPStageft"
QP$QPStageft <- as.numeric(as.character(QP$QPStageft))
colnames(QP)[7] <- "QPQcfs"
QP$QPQcfs <- as.numeric(as.character(QP$QPQcfs))

ggplot(QP, aes(DATETIME,QPQcfs)) +
  geom_point()

##need to pad the time, so that there is a value every 15 minutes
#####using Alison's function to pad the time#
source("RegularizeTimeSeries.R")

no2 <- regularizeTimeSeries(no)


###bring them back together
qsf <- merge(no2,q,by = "DATETIME",all.x = TRUE)
qss <- merge(qsf,s,by = "DATETIME",all.x = TRUE)
qsd <- merge(qss,res,by = "DATETIME",all.x = TRUE)
qsd <- merge(qss,QP,by = "DATETIME",all.x = TRUE)


#look at data
ggplot(qsd, aes(DATETIME,Qcfs)) +
  geom_point()

ggplot(qsd, aes(DATETIME,RES_Discharge_cfs)) +
  geom_point()

ggplot(qsd, aes(DATETIME,NO3mgL)) +
  geom_point()

ggplot(qsd, aes(DATETIME,QPQcfs)) +
  geom_point()

####need to fill in gaps of QS discharge
####going to use RES4-QS comparison

###regression
qmodel <- lm(Qcfs ~ RES_Discharge_cfs, data=qsd)
qmodel

ggplot(qsd, aes(RES_Discharge_cfs,Qcfs)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10() +
  geom_smooth(method='lm',formula=y~x)

###replace NA's with time period C-Q relationship
##replace numeric values with lm stats
qsd$Qcfs[is.na(qsd$Qcfs)] <- (0.06442 * qsd$RES_Discharge_cfs) + 9.36549

ggplot(qsd, aes(DATETIME,Qcfs)) +
  geom_point()


#need NO3 flux for year before and year after Maria
##year before
pre <- subset(qsd, as.POSIXct(DATETIME) > "2016-09-19 12:00:00 EDT" & 
                as.POSIXct(DATETIME) < "2017-09-19 12:00:00 EDT")
post <- subset(qsd, as.POSIXct(DATETIME) > "2017-09-19 12:00:00 EDT" & 
                as.POSIXct(DATETIME) < "2018-09-19 12:00:00 EDT")

ggplot(pre, aes(DATETIME,NO3mgL)) +
  geom_point()

ggplot(post, aes(DATETIME,NO3mgL)) +
  geom_point()


##need to fill in NO3 with C-Q relationship for each period
n1model <- lm(NO3mgL ~ Qcfs, data=pre)
n1model

ggplot(pre, aes(Qcfs,NO3mgL)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x)

pre$NO3mgL[is.na(pre$NO3mgL)] <- (-0.0001338 * pre$Qcfs) + 0.0871397

ggplot(pre, aes(DATETIME,NO3mgL)) +
  geom_point()

n2model <- lm(NO3mgL ~ Qcfs, data=post)
n2model

ggplot(post, aes(Qcfs,NO3mgL)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x)

post$NO3mgL[is.na(post$NO3mgL)] <- (0.0009635 * post$Qcfs) + 0.2367426

ggplot(post, aes(DATETIME,NO3mgL)) +
  geom_point()



#calculate runoff m/yr 
pre$Qcms <- pre$Qcfs * 0.028316847
post$Qcms <- post$Qcfs * 0.028316847


##area QS is 2622783 m2
###m/s, divide by area
pre$runoffs <- pre$Qcms / 2622783
###m/d
pre$runoffd <- pre$runoffs * 900
pre2 <- data.frame(pre, Day = as.Date(format(pre$DATETIME))) 
pre3 <- aggregate(cbind(Qcms, NO3mgL, Qcfs, runoffd) ~ Day, pre2, sum)

ggplot(pre3, aes(Day,Qcms)) +
  geom_point()

###sum for m/yr
runoffpre <- sum(pre3$runoffd)
runoffpre


##area QS is 2622783 m2
###m/s, divide by area
post$runoffs <- post$Qcms / 2622783
###m/d
post$runoffd <- post$runoffs * 900
post2 <- data.frame(post, Day = as.Date(format(pre$DATETIME))) 
post3 <- aggregate(cbind(Qcms, NO3mgL, Qcfs, runoffd) ~ Day, post2, sum)

ggplot(post3, aes(Day,Qcms)) +
  geom_point()

###sum for m/yr
runoffpre <- sum(post3$runoffd)
runoffpre


###what does flux look like if calculate kg/d, then sum to get kg/yr then do ha
###so kg/s

##kg/s
## mg/L * (m3/s*1000) * 1mg/1000000kg 
pre$CQS <- pre$NO3mgL * ((pre$Qcfs * 0.028316847) * 1000) / 1000000

ggplot(pre, aes(DATETIME,CQS)) +
  geom_point()

##calculate kg/15min
pre$CQD <- pre$CQS * 900

ggplot(pre, aes(DATETIME,CQD)) +
  geom_point()

#new data frame with daily values
#new <- data.frame(pre, Day = as.Date(format(pre$DATETIME))) 
pre$doy <- strftime(as.Date(pre$DATETIME), format = "%j")
pre$doy <- as.numeric(pre$doy)
  
pre2 <- aggregate(cbind(CQD, NO3mgL, Qcfs) ~ doy, pre, sum)

ggplot(new2, aes(Day,CQD)) +
  geom_point()


###sum to get year and divide by ha
flux3 <- sum(new2$CQD) / 262.2783
flux3


##check discharge calc
pre$Qm3s <- pre$Qcfs * 0.028316847

ggplot(pre, aes(DATETIME,Qm3s)) +
  geom_point()

post$Qm3s <- post$Qcfs * 0.028316847

ggplot(post, aes(DATETIME,Qm3s)) +
  geom_point()



###what does flux look like if calculate kg/d, then sum to get kg/yr then do ha
###so kg/s

##kg/s
## mg/L * (m3/s*1000) * 1kg/1000000mg 
post$CQS <- post$NO3mgL * ((post$Qcfs * 0.028316847) * 1000) / 1000000

ggplot(post, aes(DATETIME,CQS)) +
  geom_point()

##calculate kg/15min
post$CQD <- post$CQS * 900

ggplot(post, aes(DATETIME,CQD)) +
  geom_point()

#new data frame with daily values
new3 <- data.frame(post, Day = as.Date(format(post$DATETIME))) 

new4 <- aggregate(cbind(CQD, NO3mgL, Qcfs) ~ Day, new3, sum)

ggplot(new4, aes(Day,CQD)) +
  geom_point()


###sum to get year and divide by ha
flux4 <- sum(new4$CQD) / 262.2783
flux4





#################
##what was weekly grab sample NO3 flux for pre & 2014?

lter <- read.csv2("PR Sensors LTER Query.csv", header = T, fill = TRUE,
                  sep = ",", na.strings=c("","NA"))
QSlter <- subset(lter, Sample_ID == "QS")

for(i in 5:ncol(QSlter)) {
  QSlter[,i] <- as.numeric(as.character(QSlter[,i]))
}

colnames(QSlter)[14] <- "NO3ugL"
colnames(QSlter)[5] <- "Qm3s"

QSlter$NO3mgL <- QSlter$NO3ugL / 1000

##fix time in LTER df
QSlter$Sample_Time <- as.numeric(as.character(QSlter$Sample_Time)) 
QSlter$Sample_Time <- sprintf("%04d",QSlter$Sample_Time)
QSlter$Sample_Time = paste0(substr(QSlter$Sample_Time,1,2),":",substr(QSlter$Sample_Time,3,4),":", 
                            substr(QSlter$Sample_Time,5,6),00)

QSlter$DATETIME <- with(QSlter, ymd(QSlter$Sample_Date) + hms(QSlter$Sample_Time))
QSlter$DATETIME = as.POSIXct(strptime(QSlter$DATETIME, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
#round to 15 mins
QSlter$DATETIME <- as.POSIXct(round(as.numeric(QSlter$DATETIME)/(15*60))*(15*60),
                              origin='1970-01-01 12:00:00 America/Puerto_Rico', tz="America/Puerto_Rico")

ggplot(QSlter, aes(DATETIME, NO3mgL)) +
  geom_point()

###bring in discharge
qsg <- merge(QSlter,qsd2,by = "DATETIME",all.x = TRUE)

colnames(qsg)[40] <- "NO3mgL"

ggplot(qsg, aes(DATETIME, NO3mgL)) +
  geom_point()

ggplot(qsg, aes(DATETIME, Qm3s)) +
  geom_point()

ggplot(qsg, aes(DATETIME, Qcfs)) +
  geom_point()


##2014####
grab14 <- subset(qsg, as.POSIXct(DATETIME) > "2014-01-01 01:00:00 EDT" & 
                   as.POSIXct(DATETIME) < "2015-01-01 01:00:00 EDT")

grab14$Qm3s[is.na(grab14$Qm3s)] <- mean(grab14$Qm3s, na.rm = TRUE)
grab14$NO3mgL[is.na(grab14$NO3mgL)] <- mean(grab14$NO3mgL, na.rm = TRUE)

ggplot(grab14, aes(DATETIME, Qm3s)) +
  geom_point() +
  labs(x = "Date",
       y = "Discharge (cms)") +
  theme_bw()


#calculate runoff m/yr
##area QS is 2622783 m2
###m/s, divide by area
grab14$runoffs <- grab14$Qm3s / 2622783
###m/week
grab14$runoffd <- grab14$runoffs * 86400 * 7
###sum for m/yr
runoff <- sum(grab14$runoffd)
runoff


#calculate flux in kg/s
grab14$CQ <- grab14$NO3mgL * (grab14$Qm3s * 1000) / 1000000

ggplot(grab14, aes(DATETIME,CQ)) +
  geom_point()

##calculate kg/d
grab14$CQD <- grab14$CQ * 86400

ggplot(grab14, aes(DATETIME,CQD)) +
  geom_point()

#convert to week
grab14$CQW <- grab14$CQD * 7

###sum to get year and divide by ha
flux14 <- sum(grab14$CQW) / 262.2783
flux14



###Pre grab####
pregrab <- subset(qsg, as.POSIXct(DATETIME) > "2016-09-19 12:00:00 EDT" & 
                as.POSIXct(DATETIME) < "2017-09-19 12:00:00 EDT")

pregrab$Qcfs[is.na(pregrab$Qcfs)] <- mean(pregrab$Qcfs, na.rm = TRUE)
pregrab$NO3mgL[is.na(pregrab$NO3mgL)] <- mean(pregrab$NO3mgL, na.rm = TRUE)

ggplot(pregrab, aes(DATETIME, NO3mgL)) +
  geom_point()

ggplot(pregrab, aes(DATETIME, Qcfs)) +
  geom_point()

pregrab$Qcms <- pregrab$Qcfs * 0.028316847

ggplot(pregrab, aes(DATETIME, Qcms)) +
  geom_point() +
  labs(x = "Date",
       y = "Discharge (cms)") +
  theme_bw()


#################
###runoff m/yr
##area QS is 2622783 m2
###get cfs in m3/s

###m/s, divide by area
pregrab$runoffs <- pregrab$Qcms / 2622783
###m/week
pregrab$runoffd <- pregrab$runoffs * 86400 * 7
###sum for m/yr
runoffg <- sum(pregrab$runoffd)
runoffg



#calculate flux in kg/s
pregrab$CQ <- pregrab$NO3mgL * (pregrab$Qcms * 1000) / 1000000

ggplot(pregrab, aes(DATETIME,CQ)) +
  geom_point()

##calculate kg/d
pregrab$CQD <- pregrab$CQ * 86400

ggplot(pregrab, aes(DATETIME,CQD)) +
  geom_point()

#convert to week
pregrab$CQW <- pregrab$CQD * 7

###sum to get year and divide by ha
flux14 <- sum(pregrab$CQW) / 262.2783
flux14

