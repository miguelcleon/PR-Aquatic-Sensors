library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)
library(grid)
library(scales)
library(viridis)

#load RODBC package into 'R'
library(RODBC)
WRRC.db <- odbcConnect("WRRCdatabase", uid="a13579Z", pwd="Jh1188!") 

#pull data
cte <- sqlFetch(WRRC.db,"CTE McD Query")
qs <- sqlFetch(WRRC.db,"LCZO Weekly Query")
qss <- subset(qs, Sample_Name == "QS")

#numeric
for(i in 9:ncol(cte)) {
  cte[,i] <- as.numeric(as.character(cte[,i]))
}
for(i in 8:ncol(qss)) {
  qss[,i] <- as.numeric(as.character(qss[,i]))
}

colnames(cte)[21] <- "Nitrate"
colnames(cte)[16] <- "NH4"
colnames(cte)[17] <- "PO4"
colnames(cte)[19] <- "TDN"
colnames(cte)[22] <- "DON"
colnames(cte)[11] <- "Sulfate"

#remove LYS12 because its shit
cte2 <- subset(cte, Sample_Name != 'LYS12')


###bring in DOE data
doe <- read.csv2("DOE_lysimeters.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

doe$Collection_Date <- ymd(doe$Date)
doe$Collection_Date <- as.POSIXct(doe$Collection_Date, "%Y-%M-%D", tz ="America/Puerto_Rico")
col_idx <- grep("Collection_Date", names(doe))
doe <- doe[, c(col_idx, (1:ncol(doe))[-col_idx])]

#numeric
for(i in 4:ncol(doe)) {
  doe[,i] <- as.numeric(as.character(doe[,i]))
}

#need to convert this data to NO3-N and SO4-S
doe$Nitrate <- doe$Nitrate * 0.2259
doe$Sulfate <- doe$Sulfate * 0.3333


#############################################
##need to pick periods and get mean & sd for them
###so use both NO3 change and spec cond change - do either show changes in chem?


#pull data - from ODM2 data
QS <- read.csv2("QS Maria Final Export.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

#need to convert data to numeric for certain columns (we need code columns to remain text)
for(i in 3:ncol(QS)) {
  QS[,i] <- as.numeric(as.character(QS[,i]))
}

#convert serial date to POSIX object to allow the timestamp to be compliant with R
colnames(QS)[2] <- "Date.and.Time"
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))


#convert discharge cfs to m3s
QS$Qcms <- QS$DischargeCFS_new * 0.028316847

###Fig 1
###long term NO3 plot#####
QSL <- subset(QS, as.Date(DATETIME) > "2017-06-01 23:01:00 UTC")
QSLL <- subset(QSL, as.Date(DATETIME) < "2018-11-01 23:01:00 UTC")

colnames(qss)[3] <- "DATETIME"
qssd <- subset(qss, as.Date(DATETIME) > "2017-06-01 23:01:00 UTC")
qssdd <- subset(qssd, as.Date(DATETIME) < "2018-11-01 23:01:00 UTC")

ggplot(NULL, aes(DATETIME, SpecCond_final)) +
  geom_line(data = QSLL,color = "darkblue", size = 1) +
  geom_line(data = QSLL,aes(y=Qcms*6), color = "blue") +
  geom_point(data = qssdd, aes(y=Spec_Cond), color = "red") +
  scale_y_continuous(limits = c(0,100), 
                     sec.axis = sec_axis(~./6 , name = "Discharge (cms)")) +
  scale_x_datetime(expand = c(0, 0)) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2017-09-20")), linetype=4, color="red") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2018-04-20")), linetype=4, color="red")


#########################################################################  
#####now want to look at Potassium's contribution to spec cond
ggplot(qssdd, aes(DATETIME,Potassium)) +
  geom_point()

ggplot(qssdd, aes(DATETIME,DIC)) +
  geom_point()

###equation: K25 = sum(lambda(i)*C(i))
###lamda is equivalent conductances, have those in a table in EPA directory
###C is concentration in meq/L because lamda is (uS/cm per meq/L)
###extra step - multiple lambda by 0.9995 before multiplying by C and summing
qssdd$H_meq = ((10^(qssdd$pH * -1))*1000)
qssdd$NH4_meq = ((qssdd$NH4/1000/14.007)*1)
qssdd$Na_meq = ((qssdd$Sodium/22.98977)*1)
qssdd$K_meq = ((qssdd$Potassium/39.098)*1)
qssdd$Mg_meq = ((qssdd$Magnesium/24.305)*2)
qssdd$Ca_meq = ((qssdd$Calcium/40.08)*2)

qssdd$pOH_meq = ((10^((14 - qssdd$pH)*-1))*1000)
qssdd$NO3_meq = ((qssdd$Nitrate/14.007)*1)
qssdd$PO4_meq = ((qssdd$PO4/1000/14.007)*3)
qssdd$Cl_meq = ((qssdd$Chloride/35.453)*1)
qssdd$SO4_meq = ((qssdd$Sulfate/32.066)*2)

###how convert DIC to HCO3?

###for now only need potassium though

qssdd$K25_potassium = 73.52*qssdd$K_meq

##proportion of spec cond that is K

qssdd$Kprop <- qssdd$K25_potassium / qssdd$Spec_Cond


ggplot(qssdd, aes(DATETIME,Kprop)) +
  geom_point()

ggplot(qssdd, aes(DATETIME,Spec_Cond)) +
  geom_point()

ggplot(qssdd, aes(DATETIME,Potassium)) +
  geom_point()

