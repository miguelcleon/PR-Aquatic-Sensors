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

##making table of chemistry in QS before, during, and after conductivity spike

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
  
###looks real to me


########################################
##let's try 4 periods for cond as metric
## 1) During - I think month of Oct is a good 
## 2) Pre - earliest DOE data is June 2017 so 6/1/17 - 9/5/17
## 3) Elevated Post 11/1/17 - 4/4/18  
## 4) Back to baseline post - 4/5/18 - 11/1/18

#stream
qsspre <- subset(qss, as.Date(DATETIME) < "2017-09-05 00:01:00 UTC")
qsshigh <- subset(qss, as.Date(DATETIME) > "2017-09-30 00:01:00 UTC" & 
                    as.Date(DATETIME) < "2017-11-01 00:01:00 UTC")
qsselevpost <- subset(qss, as.Date(DATETIME) > "2017-11-01 00:01:00 UTC" & 
                        as.Date(DATETIME) < "2018-04-04 00:01:00 UTC")
qssbasepost <- subset(qss, as.Date(DATETIME) > "2018-04-05 00:01:00 UTC" & 
                        as.Date(DATETIME) < "2018-11-01 00:01:00 UTC")

qss$date = as.integer(as.POSIXct(qss$DATETIME))
pre = as.integer(as.POSIXct('2017-09-20 00:01:00 UTC'))
high = as.integer(as.POSIXct('2017-11-01 00:01:00 UTC'))
elev = as.integer(as.POSIXct('2018-04-04 00:01:00 UTC'))
base = as.integer(as.POSIXct('2018-11-01 00:01:00 UTC'))

for(i in c(1:nrow(qss))){
  if(qss$date[i] < pre)
    qss$period[i] = "PRE"
  else if(qss$date[i] > pre & qss$date[i] < high)
    qss$period[i] = "HIGH"
  else if(qss$date[i] > high & qss$date[i] < elev)
    qss$period[i] = "ELEV"
  else
    qss$period[i] = "POST"
}

ggplot(qss, aes(period, Spec_Cond)) +
  geom_boxplot()

ggplot(qss, aes(period, Nitrate)) +
  geom_boxplot()

ggplot(qss, aes(period, Chloride)) +
  geom_boxplot()

ggplot(qss, aes(period, Sodium)) +
  geom_boxplot()

ggplot(qss, aes(period, Potassium)) +
  geom_boxplot()

ggplot(qss, aes(period, Sulfate)) +
  geom_boxplot()

ggplot(qss, aes(period, NPOC)) +
  geom_boxplot()

ggplot(qss, aes(period, TDN)) +
  geom_boxplot()

#can exclude NH4 & PO4
ggplot(qss, aes(period, NH4)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0,15))

##use this to put boxplots on one plot
qssdata1 <- melt(qss, id=c("DATETIME","period"),measure.vars = c("Chloride","Sodium"))

qssdata2 <- melt(qss, id=c("DATETIME","period"),measure.vars = c("Nitrate","Sulfate","NPOC","TDN","Potassium"))

##trying to get useful boxplots to explore
##might need 2 dataframes for high and low 
##plus need to figure out how to reorder
ggplot(qssdata1) +
  geom_boxplot(aes(x=period, y=value, fill=variable))

ggplot(qssdata2) +
  geom_boxplot(aes(x=period, y=value, fill=variable)) +
  scale_y_continuous(limits = c(0,3))

#seems like the only solutes that are different between high & elevated is SO4 and N, so let's just go with 3

###################################################
#####3 periods
qss$date = as.integer(as.POSIXct(qss$DATETIME))
pre = as.integer(as.POSIXct('2017-09-20 00:01:00 UTC'))
high = as.integer(as.POSIXct('2017-11-01 00:01:00 UTC'))
elev = as.integer(as.POSIXct('2018-04-04 00:01:00 UTC'))
base = as.integer(as.POSIXct('2018-11-01 00:01:00 UTC'))

for(i in c(1:nrow(qss))){
  if(qss$date[i] < pre)
    qss$period[i] = "PRE"
  else if(qss$date[i] > pre & qss$date[i] < elev)
    qss$period[i] = "ELEV"
  else
    qss$period[i] = "POST"
}

##use this to put boxplots on one plot
qssdata1 <- melt(qss, id=c("DATETIME","period"),measure.vars = c("Chloride","Sodium"))

qssdata2 <- melt(qss, id=c("DATETIME","period"),measure.vars = c("Nitrate","Sulfate","NPOC","TDN","Potassium"))

##trying to get useful boxplots to explore
##might need 2 dataframes for high and low 
##plus need to figure out how to reorder
ggplot(qssdata1) +
  geom_boxplot(aes(x=period, y=value, fill=variable))

ggplot(qssdata2) +
  geom_boxplot(aes(x=period, y=value, fill=variable)) +
  scale_y_continuous(limits = c(0,3))


##means and std dev
means <- aggregate(qss[, 8:33], list(qss$period), mean, na.rm=TRUE)
sd <- aggregate(qss[, 8:33], list(qss$period), sd, na.rm=TRUE)

###export as table - using 3 periods for the table

write.csv(means, "QS Periods Chem Means.csv")
write.csv(sd, "QS Periods Chem SD.csv")


#########################################################
###now try high NO3 periods as metric

ggplot(NULL, aes(DATETIME, NO3)) +
  geom_line(data = QSLL,color = "darkblue", size = 1) +
  scale_x_datetime(expand = c(0, 0)) +
  geom_vline(xintercept = as.numeric(as.POSIXct("2017-09-20")), linetype=4, color="red") +
  geom_vline(xintercept = as.numeric(as.POSIXct("2018-04-20")), linetype=4, color="red") +
  labs(y="Nitrate-N mg/L")


##wow same transition period for both Cond and NO3 in April 2018!!!!


########################################################
##Now need tables for DOE and CTE data
cte3 <- subset(cte2, Collection_Date > as.POSIXct("2017-01-01"))
cte4 <- subset(cte3,  Sub_ProjectA == "Control")

ggplot(cte4, aes(Collection_Date, Nitrate)) +
  geom_point()

cte4$date = as.integer(as.POSIXct(cte4$Collection_Date))

for(i in c(1:nrow(cte4))){
  if(cte4$date[i] < pre)
    cte4$period[i] = "PRE"
  else if(cte4$date[i] > pre & cte4$date[i] < elev)
    cte4$period[i] = "ELEV"
  else
    cte4$period[i] = "POST"
}

ctedata1 <- melt(cte4, id=c("Collection_Date","period"),measure.vars = c("Cl","Na","Silica"))

ctedata2 <- melt(cte4, id=c("Collection_Date","period"),measure.vars = c("Nitrate","Sulfate","DOC","TDN","K"))

ggplot(ctedata1) +
  geom_boxplot(aes(x=period, y=value, fill=variable))

ggplot(ctedata2) +
  geom_boxplot(aes(x=period, y=value, fill=variable)) +
  scale_y_continuous(limits = c(0,5))


##means and std dev
ctemeans <- aggregate(cte4[, 10:22], list(cte4$period), mean, na.rm=TRUE)
ctesd <- aggregate(cte4[, 10:22], list(cte4$period), sd, na.rm=TRUE)

###export as table - using 3 periods for the table

write.csv(ctemeans, "CTE Periods Chem Means.csv")
write.csv(ctesd, "CTE Periods Chem SD.csv")


###DOE data- shallow well is 10cm
ten <- subset(doe, Site == 110 | Site == 210 | Site == 310 | Site == 410 | Site == 510 | Site == 610)

ten$date = as.integer(as.POSIXct(ten$Collection_Date))

for(i in c(1:nrow(ten))){
  if(ten$date[i] < pre)
    ten$period[i] = "PRE"
  else if(ten$date[i] > pre & ten$date[i] < elev)
    ten$period[i] = "ELEV"
  else
    ten$period[i] = "POST"
}


tendata <- melt(ten, id=c("Collection_Date","period"),measure.vars = c("Chloride","Nitrate","Sulfate"))

ggplot(tendata) +
  geom_boxplot(aes(x=period, y=value, fill=variable))


##means and std dev
doemeans <- aggregate(ten[, 4:12], list(ten$period), mean, na.rm=TRUE)
doesd <- aggregate(ten[, 4:12], list(ten$period), sd, na.rm=TRUE)

###export as table - using 3 periods for the table

write.csv(doemeans, "DOE Periods Chem Means.csv")
write.csv(doesd, "DOE Periods Chem SD.csv")

