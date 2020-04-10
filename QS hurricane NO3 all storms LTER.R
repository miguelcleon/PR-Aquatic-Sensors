library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)

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


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes#####
QS$DATETIME <- round_date(QS$DATETIME, "15 minutes")

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



#Connect 'R' to Access Database
#This "file name" needs to match the DSN in the Access ODBC
#To change this go to Control Panel-Administrative Tools-Data Sources (ODBC)
#     and create a new Access data source under the User DSN tab
PRLTER.db <- odbcConnect("PRLTER database") 
WRRC.db <- odbcConnect("WRRCdatabase", uid="a13579Z", pwd="Jh1188!") 

#pull data
IC.df <- sqlFetch(PRLTER.db,"McD All PRLTER Site Data")
wrrc <- sqlFetch(WRRC.db,"PR Sensors CSV Query")

colnames(IC.df)[13] <- "SO4"
colnames(IC.df)[12] <- "NO3"
colnames(IC.df)[18] <- "NH4"
colnames(IC.df)[19] <- "PO4"
colnames(IC.df)[29] <- "Discharge"
colnames(IC.df)[4] <- "DATETIME"

colnames(wrrc)[5] <- "DATETIME"
colnames(wrrc)[17] <- "NO3mgL_CZO"

qsno3 <- subset(IC.df, Sample_ID == "QS")

qsno3$NO3mgL <- qsno3$NO3 / 1000

qswrrc <- subset(wrrc, Sample_Name == "QS")

##quick plot to see if works
ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = qsn) +
  geom_point(data = qsno3, color = "blue")

ggplot(qswrrc, aes(DATETIME, NO3mgL_CZO)) +
  geom_point() 

qswrrcn <- subset(qswrrc, NO3mgL_CZO < 1.0)


###now need to split into different storms of same length
###6 months before to 1 year after
##Hugo - 9/18/1989
hugo <- subset(qsno3, DATETIME > "1989-03-18" & DATETIME < "1990-09-18")

hugo$diff=as.Date(strptime(hugo$DATETIME, "%Y-%m-%d"))-as.Date(strptime("1989-09-18", "%Y-%m-%d"))
hugo$diffNum=as.numeric(hugo$diff)

###george - 9/21/1998
george <- subset(qsno3, DATETIME > "1998-03-21" & DATETIME < "1999-09-21")

george$diff=as.Date(strptime(george$DATETIME, "%Y-%m-%d"))-as.Date(strptime("1998-09-21", "%Y-%m-%d"))
george$diffNum=as.numeric(george$diff)

###maria - 9/20/2017
maria <- subset(qsn, DATETIME > "2017-03-20" & DATETIME < "2018-09-20")

maria$diff=as.Date(strptime(maria$DATETIME, "%Y-%m-%d"))-as.Date(strptime("2017-09-20", "%Y-%m-%d"))
maria$diffNum=as.numeric(maria$diff)

ggplot(NULL, aes(diffNum, NO3mgL)) +
  geom_point(data = maria, color = "grey50") +
  geom_point(data = hugo, color = "blue", size = 2) +
  geom_point(data = george, color = "orangered1", size = 2) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Days Since Hurricane") +
  annotate("text", x = -100, y = 0.9, label = "Hugo", color = "blue", size = 6) +
  annotate("text", x = -100, y = 0.8, label = "Georges", color = "orangered1", size = 6) +
  annotate("text", x = -100, y = 0.7, label = "Maria", color = "grey50", size = 6) +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title.y=element_text(size=18), axis.title.x=element_text(size=18))

###calculate 1 year post storm NO3 average
hugo1yr <- subset(qsno3, DATETIME > "1989-09-18" & DATETIME < "1990-09-18")
george1yr <- subset(qsno3, DATETIME > "1998-09-21" & DATETIME < "1999-09-21")
maria1yrS <- subset(qsn, DATETIME > "2017-09-20" & DATETIME < "2018-09-20")
maria1yrG <- subset(qswrrcn, DATETIME > "2017-09-20" & DATETIME < "2018-09-20")

mean(hugo1yr$NO3mgL, na.rm = TRUE)
mean(george1yr$NO3mgL, na.rm = TRUE)
mean(maria1yrS$NO3mgL, na.rm = TRUE)
mean(maria1yrG$NO3mgL, na.rm = TRUE)


###calculate last 2 month past 1 year NO3 average
hugo2month <- subset(qsno3, DATETIME > "1990-09-18" & DATETIME < "1990-11-18")
george2month <- subset(qsno3, DATETIME > "1999-09-21" & DATETIME < "1999-11-21")
maria2monthS <- subset(qsn, DATETIME > "2018-09-20" & DATETIME < "2018-11-20")
maria2monthG <- subset(qsno3, DATETIME > "2018-09-20" & DATETIME < "2018-11-20")

mean(hugo2month$NO3mgL, na.rm = TRUE)
mean(george2month$NO3mgL, na.rm = TRUE)
mean(maria2monthS$NO3mgL, na.rm = TRUE)
mean(maria2monthG$NO3mgL, na.rm = TRUE)


###calculate July-Nov past 1 year NO3 average
hugo4month <- subset(qsno3, DATETIME > "1990-07-18" & DATETIME < "1990-11-18")
george4month <- subset(qsno3, DATETIME > "1999-07-21" & DATETIME < "1999-11-21")
maria4monthS <- subset(qsn, DATETIME > "2018-07-20" & DATETIME < "2018-11-20")
maria4monthG <- subset(qsno3, DATETIME > "2018-07-20" & DATETIME < "2018-11-20")

mean(hugo4month$NO3mgL, na.rm = TRUE)
mean(george4month$NO3mgL, na.rm = TRUE)
mean(maria4monthS$NO3mgL, na.rm = TRUE)
mean(maria4monthG$NO3mgL, na.rm = TRUE)
