library(xts)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

#pull data - from ODM2 server
QS <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QS Maria Final.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "DOPer_Derived"
QS$DOPer_Derived <- as.numeric(as.character(QS$DOPer_Derived))
colnames(QS)[7] <- "Temp"
QS$Temp <- as.numeric(as.character(QS$Temp))
colnames(QS)[10] <- "ConduS"
QS$ConduS <- as.numeric(as.character(QS$ConduS))
colnames(QS)[13] <- "DOConc_Pre"
QS$DOConc_Pre <- as.numeric(as.character(QS$DOConc_Pre))
colnames(QS)[16] <- "NO3"
QS$NO3 <- as.numeric(as.character(QS$NO3))
colnames(QS)[19] <- "DOConc_Adj"
QS$DOConc_Adj <- as.numeric(as.character(QS$DOConc_Adj))
colnames(QS)[22] <- "Stage_manual"
QS$Stage_manual <- as.numeric(as.character(QS$Stage_manual))
colnames(QS)[25] <- "Stage_adjusted_ft"
QS$Stage_adjusted_ft <- as.numeric(as.character(QS$Stage_adjusted_ft))
colnames(QS)[28] <- "Light_Lux"
QS$Light_Lux <- as.numeric(as.character(QS$Light_Lux))
colnames(QS)[31] <- "Temp2"
QS$Temp2 <- as.numeric(as.character(QS$Temp2))
colnames(QS)[34] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[37] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[40] <- "fDOMmV"
QS$fDOMmV <- as.numeric(as.character(QS$fDOMmV))
colnames(QS)[43] <- "TurbmV"
QS$TurbmV <- as.numeric(as.character(QS$TurbmV))
colnames(QS)[52] <- "SpecConduS"
QS$SpecConduS <- as.numeric(as.character(QS$SpecConduS))
colnames(QS)[46] <- "pH"
QS$pH <- as.numeric(as.character(QS$pH))
colnames(QS)[49] <- "DOPer_Sat"
QS$DOPer_Sat <- as.numeric(as.character(QS$DOPer_Sat))
colnames(QS)[55] <- "Stage_cm"
QS$Stage_cm <- as.numeric(as.character(QS$Stage_cm))
colnames(QS)[58] <- "Stage_m_HOBO"
QS$Stage_m_HOBO <- as.numeric(as.character(QS$Stage_m_HOBO))
colnames(QS)[61] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))

colnames(QS)[5] <- "DOPer_DerivedQC"
colnames(QS)[8] <- "TempQC"
colnames(QS)[11] <- "ConduSQC"
colnames(QS)[14] <- "DOConc_PreQC"
colnames(QS)[17] <- "NO3QC"
colnames(QS)[20] <- "DOConc_AdjQC"
colnames(QS)[23] <- "Stage_manualQC"
colnames(QS)[26] <- "Stage_adjusted_ftQC"
colnames(QS)[29] <- "Light_LuxQC"
colnames(QS)[32] <- "TempQC2"
colnames(QS)[35] <- "CondmSQC"
colnames(QS)[38] <- "SpecCondmSQC"
colnames(QS)[41] <- "fDOMQC"
colnames(QS)[44] <- "TurbmVQC"
colnames(QS)[53] <- "SpecConduSQC"
colnames(QS)[47] <- "pHQC"
colnames(QS)[50] <- "DOPer_SatQC"
colnames(QS)[56] <- "StagecmQC"
colnames(QS)[59] <- "Stage_m_HOBOQC"
colnames(QS)[62] <- "DischargeQC"

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UCT')

##separate out by variable
dod <- subset(QS, select = c(DOPer_Derived,DATETIME,DOPer_DerivedQC), QS$DOPer_DerivedQC != 'Bad')
te <- subset(QS, select = c(Temp,DATETIME,TempQC), QS$TempQC != 'Bad')
dop <- subset(QS, select = c(DOConc_Pre,DATETIME,DOConc_PreQC), QS$DOConc_PreQC != 'Bad')
no <- subset(QS, select = c(NO3,DATETIME,NO3QC), QS$NO3QC != 'Bad')
co <- subset(QS, select = c(ConduS,DATETIME,ConduSQC), QS$ConduSQC != 'Bad')
doc <- subset(QS, select = c(DOConc_Adj,DATETIME,DOConc_AdjQC), QS$DOConc_AdjQC != 'Bad')
sm <- subset(QS, select = c(Stage_manual,DATETIME,Stage_manualQC), QS$Stage_manualQC != 'Bad')
s <- subset(QS, select = c(Stage_adjusted_ft,DATETIME,Stage_adjusted_ftQC), QS$Stage_adjusted_ftQC != 'Bad')
te2 <- subset(QS, select = c(Temp2,DATETIME,TempQC2), QS$TempQC2 != 'Bad')
c <- subset(QS, select = c(CondmS,DATETIME,CondmSQC), QS$CondmSQC != 'Bad')
sc <- subset(QS, select = c(SpecCondmS,DATETIME,SpecCondmSQC), QS$SpecCondmSQC != 'Bad')
f <- subset(QS, select = c(fDOMmV,DATETIME,fDOMQC), QS$fDOMQC != 'Bad')
t <- subset(QS, select = c(TurbmV,DATETIME,TurbmVQC), QS$TurbmVQC != 'Bad')
ph <- subset(QS, select = c(pH,DATETIME,pHQC), QS$pHQC != 'Bad')
dos <- subset(QS, select = c(DOPer_Sat,DATETIME,DOPer_SatQC), QS$DOPer_SatQC != 'Bad')
scu <- subset(QS, select = c(SpecConduS,DATETIME,SpecConduSQC), QS$SpecConduSQC != 'Bad')
stcm <- subset(QS, select = c(Stage_cm,DATETIME,StagecmQC), QS$StagecmQC != 'Bad')
stm <- subset(QS, select = c(Stage_m_HOBO,DATETIME,Stage_m_HOBOQC), QS$Stage_m_HOBOQC != 'Bad')
q <- subset(QS, select = c(DischargeCFS,DATETIME,DischargeQC), QS$DischargeQC != 'Bad')
l <- subset(QS, select = c(Light_Lux,DATETIME,Light_LuxQC), QS$Light_LuxQC != 'Bad')

s2 = s[!duplicated(s$DATETIME),]
te3 = te2[!duplicated(te2$DATETIME),]
te4 = te[!duplicated(te$DATETIME),]
c2 = c[!duplicated(c$DATETIME),]
sc2 = sc[!duplicated(sc$DATETIME),]
f2 = f[!duplicated(f$DATETIME),]
t2 = t[!duplicated(t$DATETIME),]
ph2 = ph[!duplicated(ph$DATETIME),]
scu2 = scu[!duplicated(scu$DATETIME),]
stcm2 = stcm[!duplicated(stcm$DATETIME),]

###bring them back together
qsf <- merge(s,no,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,dod,by = "DATETIME", all.x = TRUE)
qsd <- merge(qss,te4,by = "DATETIME", all.x = TRUE)
qst <- merge(qsd,dop,by = "DATETIME", all.x = TRUE)
qsa <- merge(qst,co,by = "DATETIME", all.x = TRUE)
qsb <- merge(qsa,doc,by = "DATETIME", all.x = TRUE)
qsc <- merge(qsb,sm,by = "DATETIME", all.x = TRUE)
qsd <- merge(qsc,te3,by = "DATETIME", all.x = TRUE)
qse <- merge(qsd,c2,by = "DATETIME", all.x = TRUE)
qsg <- merge(qse,sc2,by = "DATETIME", all.x = TRUE)
qsh <- merge(qsg,f2,by = "DATETIME", all.x = TRUE)
qsi <- merge(qsh,t2,by = "DATETIME", all.x = TRUE)
qsj <- merge(qsi,ph2,by = "DATETIME", all.x = TRUE)
qsk <- merge(qsj,dos,by = "DATETIME", all.x = TRUE)
qsl <- merge(qsk,scu2,by = "DATETIME", all.x = TRUE)
qsm <- merge(qsl,stcm2,by = "DATETIME", all.x = TRUE)
qso <- merge(qsm,stm,by = "DATETIME", all.x = TRUE)
qsp <- merge(qso,q,by = "DATETIME", all.x = TRUE)
qsn <- merge(qsp,l,by = "DATETIME", all.x = TRUE)

##quick plot
ggplot(qsn, aes(DATETIME, DischargeCFS)) +
  geom_point()

##how does manual stage look on stage_adjusted?
ggplot(qsn, aes(Stage_adjusted_ft, Stage_manual)) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed") +
  geom_point()

##looks fine
###apply new regression to get discharge
###this was done in LCZO/QS new discharge curve

qsn$DischargeCFS_new <- ifelse(qsn$Stage_adjusted_ft > 6.5, 355.0458*qsn$Stage_adjusted_ft-2011.4, 
                               0.001541*qsn$Stage_adjusted_ft^6.518417)

ggplot(qsn, aes(NULL)) +
  geom_point(aes(DATETIME, DischargeCFS)) +
  geom_point(aes(DATETIME, DischargeCFS_new), color="blue")

###condense different sensor data for same variable into 1
###cond - before 7/3/17 use HOBO, then use Campbell
qsn$SpecConduS_C <- qsn$SpecCondmS * 1000
qsn$SpecCond_final <- ifelse(qsn$DATETIME < '2017-07-03 00:15:00',qsn$SpecConduS,qsn$SpecConduS_C)

ggplot(qsn, aes(DATETIME, SpecCondmS)) +
  geom_point() 

ggplot(qsn, aes(DATETIME, SpecCond_final)) +
  geom_point() 

##do not bother with DO, the old DO data is only good through June 2016

##convert turb and fDOM into correct units
##turbidity
ggplot(qsn, aes(DATETIME, TurbmV)) +
  geom_point() +
  scale_y_continuous(limits = c(0,100))

##need multiple relationships because they change quite a bit over time
qsn$TurbNTU <- ifelse(qsn$DATETIME > '2018-10-28 00:15:00', 
                      (12.4/(0.091-0.001))*((qsn$TurbmV/1000)-0.001),
                      (12.4/(0.091-0.015))*((qsn$TurbmV/1000)-0.015))

ggplot(qsn, aes(DATETIME, TurbNTU)) +
  geom_point() +
  scale_y_continuous(limits = c(0,5))

###fDOM
ggplot(qsn, aes(DATETIME, fDOMmV)) +
  geom_point()

qsn$fDOMQSU <- (300/(4875-26))*(qsn$fDOMmV-26)
qsn$fDOMQSU <- ifelse(qsn$fDOMQSU < 5, NA, qsn$fDOMQSU)

ggplot(qsn, aes(DATETIME, fDOMQSU)) +
  geom_point()

###check all data streams that are important
ggplot(qsn, aes(DATETIME, NO3)) +
  geom_point()

#which temp to use?
ggplot(qsn, aes(DATETIME, Temp)) +
  geom_point()
ggplot(qsn, aes(DATETIME, Temp2)) +
  geom_point()
qsn$Temp2 <- ifelse(qsn$Temp2 > 500, NA, qsn$Temp2)

##combine them
qsn$TempC <- ifelse(is.na(qsn$Temp), qsn$Temp2, qsn$Temp)
ggplot(qsn, aes(DATETIME, TempC)) +
  geom_point()

ggplot(qsn, aes(DATETIME, DOConc_Adj)) +
  geom_point()
ggplot(qsn, aes(DATETIME, Stage_manual)) +
  geom_point()
ggplot(qsn, aes(DATETIME, CondmS)) +
  geom_point()
qsn$CondmS <- ifelse(qsn$CondmS > 5, NA, qsn$CondmS)

ggplot(qsn, aes(DATETIME, pH)) +
  geom_point()
##don't include pH, its bad

ggplot(qsn, aes(DATETIME, DOPer_Sat)) +
  geom_point()
ggplot(qsn, aes(DATETIME, Light_Lux)) +
  geom_point()
qsn$Light_Lux <- ifelse(qsn$Light_Lux < 0, NA, qsn$Light_Lux)

###fdom correction
qsn$TempCorrFDOMQSU <- qsn$fDOMQSU / (1 + -0.01*(qsn$Temp-25))

qsn$TurbCorrFDOMQSU <- qsn$TempCorrFDOMQSU / (2.71828182845904 ^ (-0.006*qsn$TurbNTU))

qsn$FDOM_corrected_QSU <- (0.0044*qsn$TurbCorrFDOMQSU^2) + 0.7324*qsn$TurbCorrFDOMQSU

ggplot(qsn, aes(DATETIME, fDOMQSU)) +
  geom_point() +
  geom_point(aes(DATETIME, FDOM_corrected_QSU), color="blue")

qsn$FDOM_corrected_QSU <- ifelse(qsn$FDOM_corrected_QSU > 100, NA, qsn$FDOM_corrected_QSU)


###subset just those columns
qsnfinal <- subset(qsn, select = c("DATETIME","DischargeCFS_new","Stage_adjusted_ft",
                                   "Stage_manual","SpecCond_final","TurbNTU","FDOM_corrected_QSU",
                                   "NO3","TempC","DOConc_Adj","CondmS","DOPer_Sat","Light_Lux"))

#export for Adam, Hannah
write.csv(qsnfinal, "QS Maria Final Export.csv")

#export for PULSE
attr(qsnfinal$DATETIME,"tzone") <- "UTC"
qsnfinal$discharge_cms <- qsnfinal$DischargeCFS_new*0.02832
qsnfinal$Stage_m <- qsnfinal$Stage_adjusted_ft*0.3048

write_csv(qsnfinal, path="PR_QS_2019-06-25_XX.csv")

