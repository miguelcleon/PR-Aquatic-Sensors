library(xts)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

#pull data - from ODM2 server
QP <- read.csv2("PR_QP_data.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QP)[4] <- "Air_Temp"
QP$Air_Temp <- as.numeric(as.character(QP$Air_Temp))
colnames(QP)[7] <- "DOConc_Adj"
QP$DOConc_Adj <- as.numeric(as.character(QP$DOConc_Adj))
colnames(QP)[10] <- "Stage_ft"
QP$Stage_ft <- as.numeric(as.character(QP$Stage_ft))
colnames(QP)[13] <- "ConduS"
QP$ConduS <- as.numeric(as.character(QP$ConduS))
colnames(QP)[16] <- "Temp"
QP$Temp <- as.numeric(as.character(QP$Temp))
colnames(QP)[19] <- "SpecConduS"
QP$SpecConduS <- as.numeric(as.character(QP$SpecConduS))
colnames(QP)[22] <- "Temp2"
QP$Temp2 <- as.numeric(as.character(QP$Temp2))
colnames(QP)[25] <- "DOPer_Sat"
QP$DOPer_Sat <- as.numeric(as.character(QP$DOPer_Sat))
colnames(QP)[28] <- "CDOM_RFU"
QP$CDOM_RFU <- as.numeric(as.character(QP$CDOM_RFU))
colnames(QP)[31] <- "pH"
QP$pH <- as.numeric(as.character(QP$pH))
colnames(QP)[34] <- "DOPer_Manta"
QP$DOPer_Manta <- as.numeric(as.character(QP$DOPer_Manta))
colnames(QP)[37] <- "DOConc_Manta"
QP$DOConc_Manta <- as.numeric(as.character(QP$DOConc_Manta))
colnames(QP)[40] <- "SpecConduS_Manta"
QP$SpecConduS_Manta <- as.numeric(as.character(QP$SpecConduS_Manta))
colnames(QP)[43] <- "Turb_NTU"
QP$Turb_NTU <- as.numeric(as.character(QP$Turb_NTU))
colnames(QP)[46] <- "Light_Lux"
QP$Light_Lux <- as.numeric(as.character(QP$Light_Lux))
colnames(QP)[49] <- "DischargeCFS"
QP$DischargeCFS <- as.numeric(as.character(QP$DischargeCFS))

colnames(QP)[5] <- "Air_TempQC"
colnames(QP)[8] <- "DOConc_AdjQC"
colnames(QP)[11] <- "StageftQC"
colnames(QP)[14] <- "ConduSQC"
colnames(QP)[17] <- "TempQC"
colnames(QP)[20] <- "SpecConduSQC"
colnames(QP)[23] <- "TempQC2"
colnames(QP)[26] <- "DOPer_SatQC"
colnames(QP)[29] <- "CDOMQC"
colnames(QP)[32] <- "pHQC"
colnames(QP)[35] <- "DOPer_MantaQC"
colnames(QP)[38] <- "DOConc_MantaQC"
colnames(QP)[41] <- "SpecConduS_MantaQC"
colnames(QP)[44] <- "TurbQC"
colnames(QP)[47] <- "Light_LuxQC"
colnames(QP)[50] <- "DischargeQC"



#convert serial date to POSIX object to allow the timestamp to be compliant with R
QP$DATETIME = as.POSIXct(strptime(QP$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes#####
QP$DATETIME <- as.POSIXct(round(as.numeric(QP$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UCT')

##separate out by variable
dod <- subset(QP, select = c(SpecConduS_Manta,DATETIME,SpecConduS_MantaQC), QP$SpecConduS_MantaQC != 'Bad')
te <- subset(QP, select = c(Temp,DATETIME,TempQC), QP$TempQC != 'Bad')
dop <- subset(QP, select = c(DOPer_Manta,DATETIME,DOPer_MantaQC), QP$DOPer_MantaQC != 'Bad')
do <- subset(QP, select = c(DOConc_Manta,DATETIME,DOConc_MantaQC), QP$DOConc_MantaQC != 'Bad')
co <- subset(QP, select = c(ConduS,DATETIME,ConduSQC), QP$ConduSQC != 'Bad')
doc <- subset(QP, select = c(DOConc_Adj,DATETIME,DOConc_AdjQC), QP$DOConc_AdjQC != 'Bad')
te2 <- subset(QP, select = c(Temp2,DATETIME,TempQC2), QP$TempQC2 != 'Bad')
f <- subset(QP, select = c(CDOM_RFU,DATETIME,CDOMQC), QP$CDOMQC != 'Bad')
t <- subset(QP, select = c(Turb_NTU,DATETIME,TurbQC), QP$TurbQC != 'Bad')
ph <- subset(QP, select = c(pH,DATETIME,pHQC), QP$pHQC != 'Bad')
dos <- subset(QP, select = c(DOPer_Sat,DATETIME,DOPer_SatQC), QP$DOPer_SatQC != 'Bad')
scu <- subset(QP, select = c(SpecConduS,DATETIME,SpecConduSQC), QP$SpecConduSQC != 'Bad')
stcm <- subset(QP, select = c(Stage_ft,DATETIME,StageftQC), QP$StageftQC != 'Bad')
q <- subset(QP, select = c(DischargeCFS,DATETIME,DischargeQC), QP$DischargeQC != 'Bad')
l <- subset(QP, select = c(Light_Lux,DATETIME,Light_LuxQC), QP$Light_LuxQC != 'Bad')
a <- subset(QP, select = c(Air_Temp,DATETIME,Air_TempQC), QP$Air_TempQC != 'Bad')

dod2 = dod[!duplicated(dod$DATETIME),]
te3 = te2[!duplicated(te2$DATETIME),]
te4 = te[!duplicated(te$DATETIME),]
dop2 = dop[!duplicated(dop$DATETIME),]
do2 = do[!duplicated(do$DATETIME),]
co2 = co[!duplicated(co$DATETIME),]
doc2 = doc[!duplicated(doc$DATETIME),]
f2 = f[!duplicated(f$DATETIME),]
t2 = t[!duplicated(t$DATETIME),]
ph2 = ph[!duplicated(ph$DATETIME),]
dos2 = dos[!duplicated(dos$DATETIME),]
scu2 = scu[!duplicated(scu$DATETIME),]
stcm2 = stcm[!duplicated(stcm$DATETIME),]
q2 = q[!duplicated(q$DATETIME),]
l2 = l[!duplicated(l$DATETIME),]
a2 = a[!duplicated(a$DATETIME),]

###bring them back together
qsf <- merge(doc2,l2,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,dod2,by = "DATETIME", all.x = TRUE)
qsd <- merge(qss,te4,by = "DATETIME", all.x = TRUE)
qst <- merge(qsd,dop2,by = "DATETIME", all.x = TRUE)
qsa <- merge(qst,co2,by = "DATETIME", all.x = TRUE)
qsc <- merge(qsa,do2,by = "DATETIME", all.x = TRUE)
qsd <- merge(qsc,te3,by = "DATETIME", all.x = TRUE)
qsh <- merge(qsd,f2,by = "DATETIME", all.x = TRUE)
qsi <- merge(qsh,t2,by = "DATETIME", all.x = TRUE)
qsj <- merge(qsi,ph2,by = "DATETIME", all.x = TRUE)
qsk <- merge(qsj,dos2,by = "DATETIME", all.x = TRUE)
qsl <- merge(qsk,scu2,by = "DATETIME", all.x = TRUE)
qsm <- merge(qsl,stcm2,by = "DATETIME", all.x = TRUE)
qso <- merge(qsm,a2,by = "DATETIME", all.x = TRUE)
qsn <- merge(qso,q2,by = "DATETIME", all.x = TRUE)

##Discharge
ggplot(qsn, aes(DATETIME, DischargeCFS)) +
  geom_point()

qsn$DischargeCFS <- ifelse(qsn$DischargeCFS > 100, NA, qsn$DischargeCFS)

##check others quick

ggplot(qsn, aes(DATETIME, DOConc_Adj)) +
  geom_point()

ggplot(qsn, aes(DATETIME, DOConc_Manta)) +
  geom_point()

ggplot(qsn, aes(DATETIME, Light_Lux)) +
  geom_point()

ggplot(qsn, aes(DATETIME, SpecConduS_Manta)) +
  geom_point()

ggplot(qsn, aes(DATETIME, SpecConduS)) +
  geom_point()

ggplot(qsn, aes(DATETIME, Temp)) +
  geom_point() +
  geom_point(aes(DATETIME, Temp2), color="blue")

ggplot(qsn, aes(DATETIME, DOPer_Manta)) +
  geom_point()

ggplot(qsn, aes(DATETIME, DOPer_Sat)) +
  geom_point()

ggplot(qsn, aes(DATETIME, ConduS)) +
  geom_point()

ggplot(qsn, aes(DATETIME, CDOM_RFU)) +
  geom_point()

ggplot(qsn, aes(DATETIME, Turb_NTU)) +
  geom_point()

ggplot(qsn, aes(DATETIME, pH)) +
  geom_point()

ggplot(qsn, aes(DATETIME, Stage_ft)) +
  geom_point()

ggplot(qsn, aes(DATETIME, Air_Temp)) +
  geom_point()


qsn$Stage_ft <- ifelse(qsn$Stage_ft > 2.7, NA, qsn$Stage_ft)

###do not use: SpecConduS_Manta, DOPer_Manta, DOConc_Manta, Air_Temp
###use Temp, not Temp2

qsnfinal <- subset(qsn, select = c("DATETIME","DischargeCFS","Stage_ft","DOConc_Adj","Turb_NTU","SpecConduS",
                                   "CDOM_RFU","Temp","DOPer_Sat","Light_Lux","pH"))


#export for PULSE
attr(qsnfinal$DATETIME,"tzone") <- "UTC"
qsnfinal$discharge_cms <- qsnfinal$DischargeCFS*0.02832
qsnfinal$Stage_m <- qsnfinal$Stage_ft*0.3048

#going to upload whole dataset this time
#qspulse <- subset(qsnfinal, DATETIME > '2018-01-01 00:15:00' & DATETIME < '2019-01-01 00:15:00' )

qspulse <- subset(qsnfinal, select = c("DATETIME","discharge_cms","Stage_m",
                                       "SpecConduS","Turb_NTU","CDOM_RFU",
                                       "pH","Temp","DOConc_Adj","DOPer_Sat","Light_Lux"))

write_csv(qspulse, path="PR_QP_2019-08-15_XX.csv")


