library(ggplot2)
library(RODBC)

#pull data - from ODM2 server
QS <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QSmydatashort.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[7] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[4] <- "NO3mgL"
QS$NO3mgL <- as.numeric(as.character(QS$NO3mgL))
colnames(QS)[10] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))
colnames(QS)[13] <- "Stage_adjusted_ft"
QS$Stage_adjusted_ft <- as.numeric(as.character(QS$Stage_adjusted_ft))
colnames(QS)[16] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[19] <- "fDOMmV"
QS$fDOMmV <- as.numeric(as.character(QS$fDOMmV))
colnames(QS)[22] <- "TurbmV"
QS$TurbmV <- as.numeric(as.character(QS$TurbmV))
colnames(QS)[25] <- "Stage_cm"
QS$Stage_cm <- as.numeric(as.character(QS$Stage_cm))
colnames(QS)[5] <- "NO3QC"
colnames(QS)[8] <- "CondQC"
colnames(QS)[11] <- "DischargeQC"
colnames(QS)[14] <- "StageftQC"
colnames(QS)[17] <- "SCQC"
colnames(QS)[20] <- "fDOMQC"
colnames(QS)[23] <- "turbQC"
colnames(QS)[26] <- "StagecmQC"

#######for use with database connection only (i.e. only Jody & Michelle right now)#####
#Connect 'R' to Access Database
#This "file name" needs to match the DSN in the Access ODBC
#To change this go to Control Panel-Administrative Tools-Data Sources (ODBC)
#     and create a new Access data source under the User DSN tab
PRLTER.db <- odbcConnect("PRLTER database") 
WRRC.db <- odbcConnect("WRRCdatabase", uid="a13579Z", pwd="Jh1188!") 
#pull data
prlter <- sqlFetch(PRLTER.db,"McD All PRLTER Site Data")
wrrc <- sqlFetch(WRRC.db,"PR Sensors CSV Query")
######################################################################################

###using CSVs instead
prlter <- read.csv("C:/Users/jpotter/Box/PR Sensors Data/WQAL Output/McD All PRLTER Site Data.csv", 
                   header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))
wrrc <- read.csv("C:/Users/jpotter/Box/PR Sensors Data/WQAL Output/PR Sensors CSV Query.csv", 
                   header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

#adjust names
colnames(prlter)[13] <- "SO4"
colnames(prlter)[12] <- "NO3"
colnames(prlter)[18] <- "NH4"
colnames(prlter)[19] <- "PO4"
colnames(prlter)[29] <- "Discharge_cms"

colnames(wrrc)[5] <- "DATETIME"
colnames(wrrc)[17] <- "NO3mgL_CZO"

qsprlter <- subset(prlter, Sample_ID == "QS")
qswrrc <- subset(wrrc, Sample_Name == "QS")

qsprlter$NO3mgL_LTER <- qsprlter$NO3 / 1000

###need to converge date+time in prlter data
qsprlter$time <- format(strptime(sprintf("%04d", qsprlter$Sample_Time), "%H%M"), format = "%H:%M")
qsprlter$date <- format(qsprlter$Sample_Date, format="%Y-%m-%d")

qsprlter$DATETIME <- as.POSIXct(paste(qsprlter$date, qsprlter$time), 
                                             format="%Y-%m-%d %H:%M", tz="America/Puerto_Rico")


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
qswrrc$DATETIME = as.POSIXct(strptime(qswrrc$DATETIME, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes for all dataframes, so they will match#####
QS$DATETIME <- round_date(QS$DATETIME, "15 minutes")
qswrrc$DATETIME <- round_date(qswrrc$DATETIME, "15 minutes")
qsprlter$DATETIME <- round_date(qsprlter$DATETIME, "15 minutes")

##separate out by variable
no <- subset(QS, select = c(NO3mgL,DATETIME,NO3QC), QS$NO3QC != 'Bad')
q <- subset(QS, select = c(DischargeCFS,DATETIME,DischargeQC), QS$DischargeQC != 'Bad')
s <- subset(QS, select = c(Stage_adjusted_ft,DATETIME,StageftQC), QS$StageftQC != 'Bad')
sc <- subset(QS, select = c(SpecCondmS,DATETIME,SCQC), QS$SCQC != 'Bad')
t <- subset(QS, select = c(TurbmV,DATETIME,turbQC), QS$turbQC != 'Bad')
c <- subset(QS, select = c(CondmS,DATETIME,CondQC), QS$CondQC != 'Bad')

no2 = no[!duplicated(no$DATETIME),]

###bring them back together
qsf <- merge(no2,q,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,s,by = "DATETIME", all.x = TRUE)
qsd <- merge(qss,sc,by = "DATETIME", all.x = TRUE)
qst <- merge(qsd,t,by = "DATETIME", all.x = TRUE)
qsn <- merge(qst,c,by = "DATETIME", all.x = TRUE)


#need to combine these to plot NO3 vs NO3
qsall <- merge(qsn, qswrrc, by="DATETIME", all.x = TRUE)
qsall2 <- merge(qsall, qsprlter, by="DATETIME", all.x = TRUE)


#is all the data there?
#CZO data
ggplot(qsall2, aes(DATETIME,NO3mgL_CZO)) +
  geom_point()
#sensor data
ggplot(qsall2, aes(DATETIME,NO3mgL)) +
  geom_point()
#prlter data
ggplot(qsall2, aes(DATETIME,NO3mgL_LTER)) +
  geom_point()


ggplot(qsall2, aes(NULL)) +
  geom_point(aes(DATETIME, NO3mgL)) +
  geom_point(aes(DATETIME, NO3mgL_CZO), color="red") +
  geom_point(aes(DATETIME, NO3mgL_LTER), color="blue") +
  annotate("text", x = as.POSIXct("2017-06-03 10:00:00 AST"), y = 1.0, label = "CZO", color="red", size = 6) +
  annotate("text", x = as.POSIXct("2017-06-03 10:00:00 AST"), y = 0.9, label = "LTER", color="blue", size =6) +
  annotate("text", x = as.POSIXct("2017-06-03 10:00:00 AST"), y = 0.8, label = "SUNA", color="black", size =6) +
  labs(x = "Date",
       y = "NO3-N (mg/L)") +
  theme_bw()

#NO3 vs NO3
ggplot(qsall2, aes(NO3mgL_CZO,NO3mgL)) +
  geom_point() +
  #scale_y_continuous(limits = c(0,0.9)) +
  #scale_x_continuous(limits = c(0,0.9)) +
  geom_smooth(method='lm',formula=y~x) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "NO3-N CZO Grab (mg/L)",
       y = "NO3-N Sensor (mg/L)") +
  theme_bw()


ggplot(qsall2, aes(NO3mgL,NO3mgL_LTER)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.8)) +
  scale_x_continuous(limits = c(0,0.8)) +
  geom_smooth(method='lm',formula=y~x) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "NO3-N LTER Grab (mg/L)",
       y = "NO3-N Sensor (mg/L)") +
  theme_bw()

####zoom in####
isco <- subset(qsall2, as.Date(DATETIME) > "2018-04-10 23:01:00 UTC")
isco2 <- subset(isco, as.Date(DATETIME) < "2018-04-20 23:01:00 UTC")

ggplot(isco2, aes(NULL)) +
  geom_point(aes(DATETIME, NO3mgL)) +
  geom_point(aes(DATETIME, NO3mgL_CZO), color="red") +
  geom_point(aes(DATETIME, NO3mgL_LTER), color="blue") +
  labs(x = "Date",
       y = "NO3-N (mg/L)") +
  theme_bw()

###remove points
qsall3 <- subset(qsall2, NO3mgL_CZO < 1.0)

ggplot(qsall3, aes(NO3mgL_CZO,NO3mgL)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_abline(slope = 1, intercept = 0) +
  labs(x = "NO3-N CZO Grab (mg/L)",
       y = "NO3-N Sensor (mg/L)") +
  theme_bw()


#regressions
lm.r=lm(qsall3$NO3mgL~qsall3$NO3mgL_CZO)
summary(lm.r)

lm2.r=lm(qsall2$NO3mgL~qsall2$NO3mgL_CZO)
summary(lm2.r)

#this summary is to figure out if slope is different than 1 by getting the 95% confidence interval
#then seeing if a slope of 1 is contained in it
summary(lm.r)$coefficients
a=summary(lm.r)$coefficients[1,1]
b=summary(lm.r)$coefficients[2,1]
confint(lm.r, level = 0.95)


#best relationship is with our grab data, not LTER
ggplot(qsall3, aes(NO3mgL_CZO,NO3mgL)) +
  geom_point() +
  geom_smooth(method='lm',formula=y~x) +
  geom_abline(slope = 1, intercept = 0) +
  annotate("text",x=0.2,y=0.9,label="r2=0.79, y=1.09x+0.03") +
  labs(x = "NO3-N CZO Grab (mg/L)",
       y = "NO3-N Sensor (mg/L)") +
  theme_bw()


###how about others??

ggplot(qsall2, aes(Chloride,SpecCondmS)) +
  geom_point() +
  scale_x_continuous(limits = c(0,20)) +
  geom_smooth(method='lm',formula=y~x) +
  labs(x = "Cl (mg/L)",
       y = "Spec Cond (uS/cm)") +
  theme_bw()

##fDOM###
ggplot(qsall2, aes(DATETIME,fDOMmV)) +
  geom_point()

fdom <- subset(qsall2, as.Date(DATETIME) < "2018-01-20 23:01:00 UTC")

ggplot(fdom, aes(DATETIME,fDOMmV)) +
  geom_point()

ggplot(fdom, aes(NPOC..mg.C.L.,fDOMmV)) +
  geom_point() +
  scale_y_continuous(limits = c(0,1000)) +
  geom_smooth(method='lm',formula=y~x) +
  labs(x = "DOC (mg/L)",
       y = "fDOM (mV)") +
  theme_bw()











