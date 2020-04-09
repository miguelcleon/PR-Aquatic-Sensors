library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)


#pull data - from ODM2 server
QS <- read.csv2("QSmydata.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "DOPercent"
QS$DOPercent <- as.numeric(as.character(QS$DOPercent))
colnames(QS)[7] <- "TempC"
QS$TempC <- as.numeric(as.character(QS$TempC)) 
colnames(QS)[10] <- "ConduS"
QS$ConduS <- as.numeric(as.character(QS$ConduS)) 
colnames(QS)[13] <- "TempC02"
QS$TempC02 <- as.numeric(as.character(QS$TempC02))
colnames(QS)[16] <- "DOmgL"
QS$DOmgL <- as.numeric(as.character(QS$DOmgL))
#colnames(QS)[19] <- "LightLUX"
#QS$LightLUX <- as.numeric(as.character(QS$LightLUX))
colnames(QS)[19] <- "NO3mgL"
QS$NO3mgL <- as.numeric(as.character(QS$NO3mgL))
colnames(QS)[22] <- "DOmgL02"
QS$DOmgL02 <- as.numeric(as.character(QS$DOmgL02))
colnames(QS)[25] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))
colnames(QS)[28] <- "Stage_adjusted_ft"
QS$Stage_adjusted_ft <- as.numeric(as.character(QS$Stage_adjusted_ft))
colnames(QS)[31] <- "LightLUX"
QS$LightLUX02 <- as.numeric(as.character(QS$LightLUX02))
colnames(QS)[34] <- "TempC03"
QS$TempC03 <- as.numeric(as.character(QS$TempC03))
colnames(QS)[37] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS))
colnames(QS)[40] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[43] <- "fDOMmV"
QS$fDOMmV <- as.numeric(as.character(QS$fDOMmV))
colnames(QS)[46] <- "TurbmV"
QS$TurbmV <- as.numeric(as.character(QS$TurbmV))
colnames(QS)[49] <- "CDOMRFU"
QS$CDOMRFU <- as.numeric(as.character(QS$CDOMRFU))
colnames(QS)[52] <- "TurbNTU"
QS$TurbNTU <- as.numeric(as.character(QS$TurbNTU))
colnames(QS)[55] <- "HDOmgL"
QS$HDOmgL <- as.numeric(as.character(QS$HDOmgL))
colnames(QS)[58] <- "SpecConduS"
QS$SpecConduS <- as.numeric(as.character(QS$SpecConduS))
colnames(QS)[61] <- "HDOper"
QS$HDOper <- as.numeric(as.character(QS$HDOper))
colnames(QS)[64] <- "pH"
QS$pH <- as.numeric(as.character(QS$pH))
colnames(QS)[67] <- "TempC04"
QS$TempC04 <- as.numeric(as.character(QS$TempC04))
colnames(QS)[70] <- "pH02"
QS$pH02 <- as.numeric(as.character(QS$pH02))
colnames(QS)[73] <- "DOperHOBO"
QS$DOperHOBO <- as.numeric(as.character(QS$DOperHOBO))
colnames(QS)[76] <- "SpecConduS02"
QS$SpecConduS02 <- as.numeric(as.character(QS$SpecConduS02))


#grab data
grab <- read.csv2("PR Sensors CSV Query.csv", header = T, fill = TRUE,
                  sep = ",", na.strings=c("","NA"))
QSgrab <- subset(grab, Sample.Name == "QS")

#need to convert data to numeric
for(i in 9:ncol(QSgrab)) {
  QSgrab[,i] <- as.numeric(as.character(QSgrab[,i]))
}

colnames(QSgrab)[17] <- "NO3mgL"


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="UTC"))
QSgrab$DATETIME = as.POSIXct(strptime(QSgrab$DateTime, "%Y-%m-%d %H:%M", tz="UTC"))
#QSgrab$DATETIME <- floor_date(QSgrab$DATETIME, "15 minutes") 


###want to round the time to 15 minutes#####
#QS$DATETIME2 <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UTC')

###so we can combine rows###
#QSP <- aggregate(QS[ ,4:78], FUN="sum", by=list(as.POSIXct(QS$DATETIME, "%Y-%m-%d %H:%M"),
 #                                               na.rm=TRUE, na.action=NULL))


#remove flagged data at plot level

#quick plots
ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = QS) +
  geom_point(data = QSgrab, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  theme_bw()

ggplot(subset(QS, quality.code.5 != 'Bad')) +
  geom_point(aes(DATETIME, NO3mgL))

ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(subset(QS, quality.code.5 != 'Bad')) +
  geom_point(data = QSgrab, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  theme_bw()

ggplot(QS, aes(DATETIME, DischargeCFS)) +
  geom_point()
ggplot(QS, aes(DATETIME, TurbmV)) +
  geom_point()
ggplot(QS, aes(DATETIME, TurbNTU)) +
  geom_point()
ggplot(QS, aes(DATETIME, SpecCondmS)) +
  geom_point()

#need to combine these into one plot, can we replace the NANs in 02 with the values in the other?
ggplot(subset(QS, quality.code.24 != 'Bad')) +
  geom_point(aes(DATETIME, SpecConduS02))
ggplot(subset(QS, quality.code.18 != 'Bad')) +
  geom_point(aes(DATETIME, SpecConduS))  



###long term NO3 plot#####
QSL <- subset(QS, as.Date(DATETIME) > "2017-06-01 23:01:00 UTC")
QSGL <- subset(QSgrab, as.Date(DATETIME) > "2017-06-01 23:01:00 UTC")

QSL2 <- subset(QSL, quality.code.5 != "Bad")

ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = QSL2) +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14))


ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = QSL2) +
  geom_point(data = QSGL, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw()


#just SC, NO3, & Turb
QSpost <- subset(QS, as.Date(DATETIME) > "2017-08-01 23:01:00 UTC")
QSpost2 <- subset(QSpost, as.Date(DATETIME) < "2018-01-15 12:46:00 UTC")
QSgpost <- subset(QSgrab, as.Date(DATETIME) > "2017-08-01 23:01:00 UTC")
QSgpost2 <- subset(QSgpost, as.Date(DATETIME) < "2018-01-15 12:46:00 UTC")

QSpost3 <- subset(QSpost2, quality.code.5 != 'Bad')

p1 <- ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = QSpost3) +
  geom_point(data = QSgpost2, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw() +
  theme(plot.margin=unit(c(0.2,1.4,0.2,0.5),"cm")) +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

QSpost4 <- subset(QSpost2, quality.code.14 != 'Bad'| is.na(quality.code.14))
ggplot(QSpost4, aes(DATETIME, TurbmV)) +
  geom_point()
ggplot(QSpost4, aes(DATETIME, DischargeCFS)) +
  geom_point()
ggplot(QSpost4, aes(DATETIME, Stage_adjusted_ft)) +
  geom_point()

p2 <- ggplot(QSpost4, aes(x = DATETIME)) +
  geom_point(aes(y = TurbmV)) +
  geom_point(aes(y = Stage_adjusted_ft*120), color = "blue", size = 1) +
  scale_y_continuous(limits = c(0,1200), 
                     sec.axis = sec_axis(~./120 , name = "Stage (ft)")) +
  scale_x_datetime(expand = c(0, 0)) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 0, ymax = 1100,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 1100,
           alpha = .3) +
  annotate("text", x = as.POSIXct("2017-09-03 10:00:00 AST"), y = 1000, label = "Irma", angle = 90, 
           color="red", size = 6) +
  annotate("text", x = as.POSIXct("2017-09-16 10:00:00 AST"), y = 1000, label = "Maria", angle = 90, 
           color="red", size =6) +
  labs(y = "Turbidity (NTU)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())

ggplot(QSpost2, aes(DATETIME, SpecCondmS)) +
  geom_point()

p3 <- ggplot(subset(QSpost2, variable %in% c('SpecCond_uScm'))) +
  geom_point(aes(DATETIME, value)) +
  scale_y_continuous(limits = c(10,80)) +
  scale_x_datetime(expand = c(0, 0)) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 10, ymax = 80,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 10, ymax = 80,
           alpha = .3) +
  labs(y = "Spec Cond (mS/cm)",
       x = "Date") +
  theme_bw() +
  theme(plot.margin=unit(c(0.2,1.4,0.2,0.6),"cm")) +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())

plot_grid(p2, p3, p1, nrow = 3)


rel_heights = c(0.4, 0.4, 0.5)

####confirmation of storm NO3 with ISCO grabs
QSpostL <- subset(QS, as.Date(DATETIME) > "2018-04-10 23:01:00 UTC")
QSpostL2 <- subset(QSpostL, as.Date(DATETIME) < "2018-04-23 12:46:00 UTC")
QSgpostL <- subset(QSgrab, as.Date(DATETIME) > "2018-04-10 23:01:00 UTC")
QSgpostL2 <- subset(QSgpostL, as.Date(DATETIME) < "2018-04-23 12:46:00 UTC")


ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = QSpostL2) +
  geom_point(data = QSgpostL2, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw()

ggplot(NULL, aes(DATETIME, DischargeCFS)) +
  geom_point(data = QSpostL2) +
  geom_point(data = QSgpostL2, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw()

QSposta <- subset(QS, as.Date(DATETIME) > "2017-08-01 23:01:00 UTC")
QSposta2 <- subset(QSpost, as.Date(DATETIME) < "2018-04-23 12:46:00 UTC")
QSgposta <- subset(QSgrab, as.Date(DATETIME) > "2017-08-01 23:01:00 UTC")
QSgposta2 <- subset(QSgpost, as.Date(DATETIME) < "2018-04-23 12:46:00 UTC")

QSposta3 <- subset(QSposta2, quality.code.5 != 'Bad')

ggplot(NULL, aes(DATETIME, NO3mgL)) +
  geom_point(data = QSposta3) +
  geom_point(data = QSgposta2, color = "red") +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw()



#another


f1 <- ggplot(subset(QSpost2, variable %in% c('Nitrate_mgL'))) +
  geom_point(aes(DATETIME, value)) +
  ylab("NO3-N (mg/L)") +
  theme_bw() +
  theme(axis.text.y=element_text(size=12), axis.text.x=element_blank(),
        axis.title.y=element_text(size=12), axis.title.x=element_blank())

f2 <- ggplot(subset(QSpost2, variable %in% c('CDOM_mV'))) +
  geom_point(aes(DATETIME, value)) +
  ylab("fDOM (mV)") +
  theme_bw() +
  theme(axis.text.y=element_text(size=12), axis.text.x=element_blank(),
        axis.title.y=element_text(size=12), axis.title.x=element_blank())

f3 <- ggplot(data = subset(QSpost2, variable %in% c('Turbidity_NTU'))) +
  geom_point(aes(DATETIME, value)) +
  scale_y_continuous("Turbidity (FNU)", limits = c(0,2500)) +
  theme_bw() +
  theme(axis.text.y=element_text(size=12), axis.text.x=element_blank(),
        axis.title.y=element_text(size=12), axis.title.x=element_blank())

f4 <- ggplot(subset(QSpost2, variable %in% c('SpecCond_uScm'))) +
  geom_point(aes(DATETIME, value)) +
  ylab("Spec Cond (uS/cm)") +
  xlab("Date") +
  theme_bw() +
  theme(axis.text.y=element_text(size=12), axis.text.x=element_blank(),
        axis.title.y=element_text(size=12), axis.title.x=element_blank())

f5 <- ggplot(data = subset(QSpost2, variable %in% c('Discharge_m3s'))) +
  geom_point(aes(DATETIME, value), color = "blue") +
  scale_y_continuous("Discharge (m3/s)") +
  xlab("Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=12), axis.title.x=element_text(size=14))

grid.arrange(f1, f2, f3, f4, f5, ncol= 1)


###plot of logQ vs NO3 and cond###
###need to edit below###

wnit <- align.time(xts(data[,7],as.POSIXct(data[,1])), n=600)
wstage <- align.time(xts(stage[,6],as.POSIXct(stage[,7])), n=600)
wndata <- merge(wnit,wstage)
nitrate <- data.frame(Date=index(wndata), coredata(wndata))
colnames(nitrate)[2] <- "Nitrate"
colnames(nitrate)[3] <- "Stage"

tcond <- align.time(xts(data[,25],as.POSIXct(data[,1])), n=600)
tstage <- align.time(xts(stage[,6],as.POSIXct(stage[,7])), n=600)
wcdata <- merge(tcond,tstage)
cond <- data.frame(Date=index(wcdata), coredata(wcdata))
colnames(cond)[2] <- "SpecCond"
colnames(cond)[3] <- "Stage"


QS$period <- ifelse(QS$DATETIME<=as.POSIXct("2017-09-20 03:30:00 AST"),"Pre","Post")
QS$period <- as.factor(QS$period)
QSN <- subset(QS, (!is.na(NO3mgL)))

ggplot(QSN, aes(DATETIME,DischargeCFS)) +
  geom_point()

n1 <- ggplot(QSN, aes(DischargeCFS,NO3mgL)) +
  geom_point(aes(colour = factor(period))) +
  scale_x_log10() +
  scale_colour_manual(values = c("blue", "orange")) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Stage (cm)") +
  theme(legend.title=element_blank(), legend.position="top", legend.text=element_text(size=16),
        axis.text=element_text(size=16), axis.title=element_text(size=18),
        panel.border = element_rect(colour = "black", fill=NA))

n2 <- ggplot(cond, aes(Stage,SpecCond)) +
  geom_point(aes(colour = factor(period))) +
  scale_y_continuous(limits = c(0.02,0.2)) +
  scale_x_continuous(limits = c(10,200)) +
  labs(y = "Specific Conductance (mS/cm)",
       x = "Stage (cm)") +
  scale_colour_manual(values = c("blue", "orange")) +
  theme(plot.margin=unit(c(1.6,0.2,0.2,0.2),"cm"), legend.position="none",
        axis.text=element_text(size=16), axis.title=element_text(size=18),
        panel.border = element_rect(colour = "black", fill=NA))

grid.arrange(n1, n2, ncol = 2)

