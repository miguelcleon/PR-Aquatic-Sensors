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

#pull data - from ODM2 server
QS <- read.csv2("QSmydatashort.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[10] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[4] <- "NO3"
QS$NO3 <- as.numeric(as.character(QS$NO3))
colnames(QS)[25] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))
colnames(QS)[7] <- "Stage_adjusted_ft"
QS$Stage_adjusted_ft <- as.numeric(as.character(QS$Stage_adjusted_ft))
colnames(QS)[13] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[16] <- "fDOMmV"
QS$fDOMmV <- as.numeric(as.character(QS$fDOMmV))
colnames(QS)[19] <- "TurbmV"
QS$TurbmV <- as.numeric(as.character(QS$TurbmV))
colnames(QS)[22] <- "Stage_cm"
QS$Stage_cm <- as.numeric(as.character(QS$Stage_cm))
colnames(QS)[5] <- "NO3QC"
colnames(QS)[11] <- "CondQC"
colnames(QS)[26] <- "DischargeQC"
colnames(QS)[8] <- "StageftQC"
colnames(QS)[14] <- "SCQC"
colnames(QS)[17] <- "fDOMQC"
colnames(QS)[20] <- "turbQC"
colnames(QS)[23] <- "StagecmQC"

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
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
QSgrab$DATETIME = as.POSIXct(strptime(QSgrab$DateTime, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
#QSgrab$DATETIME <- floor_date(QSgrab$DATETIME, "15 minutes") 


###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UCT')

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

#convert discharge cfs to m3s
qsn$Qcms <- qsn$DischargeCFS * 0.028316847


###Fig 2

aug <- subset(qsn, as.POSIXct(DATETIME) > "2017-08-15 07:00:00 EDT")
aug2 <- subset(aug, as.POSIXct(DATETIME) < "2017-08-15 19:00:00 EDT")

ggplot(aug2, aes(NULL)) +
  geom_line(aes(DATETIME,DischargeCFS)) +
  geom_point(aes(DATETIME,NO3mgL), color = "blue")


##ok, nice one
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sn <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))


##post storm
oct <- subset(qsn, as.Date(DATETIME) > "2017-09-25 18:00:00 EST")
oct2 <- subset(oct, as.Date(DATETIME) < "2017-10-02 00:00:00 EST")

ggplot(oct2, aes(NULL)) +
  geom_line(aes(DATETIME,DischargeCFS)) +
  geom_point(aes(DATETIME,NO3mgL), color = "blue")

jan <- subset(qsn, as.Date(DATETIME) > "2018-01-01 22:00:00 EST")
jan2 <- subset(jan, as.Date(DATETIME) < "2018-01-03 01:15:00 EST")

ggplot(jan2, aes(NULL)) +
  geom_line(aes(DATETIME,DischargeCFS)) +
  geom_point(aes(DATETIME,NO3mgL), color = "blue")


#Fig 2 v1 - hydrographs on left side, hystersis on right

f1 <- ggplot(aug2, aes(Qcms,NO3mgL,color= as.numeric(DATETIME))) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.2,3)) +
  scale_y_continuous(limits = c(0.05,0.45),position = "right") +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cms)") +
  annotate("text",x=2.5,y=0.4,label="Pre",size=6) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

f2 <- ggplot(aug2, aes(DATETIME,Qcms)) +
  geom_point(size = 2, color= "blue") +
  labs(y = "Discharge (cms)",
       x = "Time") +
  theme_bw() +
  annotate("text",x=as.POSIXct("2017-08-15 18:00:00 EDT"),y=2.2,label="Pre",size=6) +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin=unit(c(0.1,0.3,0.1,0.1),"cm"))


f3 <- ggplot(jan2, aes(Qcms,NO3mgL,color= as.numeric(DATETIME))) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.2,3)) +
  scale_y_continuous(limits = c(0.05,0.45),position = "right") +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cms)") +
  annotate("text",x=2.5,y=0.4,label="Post",size=6) +
  theme_bw() +
  #coord_fixed(1.4) +
  theme(axis.text=element_text(size=12), panel.grid.major = element_blank(), axis.title=element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

f4 <- ggplot(jan2, aes(DATETIME,Qcms)) +
  geom_point(size = 2, color= "blue") +
  labs(y = "Discharge (cms)",
       x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M")) +
  annotate("text",x=as.POSIXct("2018-01-02 17:00:00 EST"),y=1.0,label="Post",size=6) +
  theme_bw() +
  theme(axis.text=element_text(size=12),panel.grid.major = element_blank(), axis.title=element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(plot.margin=unit(c(0.1,0.3,0.2,0.1),"cm"))


grid.arrange(f4, f3, f2, f1, ncol = 2, left=textGrob("Discharge (cms)",gp=gpar(fontsize=14),rot=90),
             right=textGrob("Nitrate-N (mg/L)",gp=gpar(fontsize=14),rot=270))


#Fig 2 v2 - hystersis upper, hydrograph lower


n1 <- ggplot(aug2, aes(Qcms,NO3mgL,color= as.numeric(DATETIME))) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.2,3)) +
  scale_y_continuous(limits = c(0.05,0.45)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cms)") +
  annotate("text",x=2.5,y=0.4,label="Pre",size=6) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

n3 <- ggplot(aug2, aes(DATETIME,Qcms)) +
  geom_point(size = 2, color= "blue") +
  labs(y = "Discharge (cms)",
       x = "Time") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin=unit(c(0.1,0.3,0.1,0.1),"cm"))


n2 <- ggplot(jan2, aes(Qcms,NO3mgL,color= as.numeric(DATETIME))) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.2,3)) +
  scale_y_continuous(limits = c(0.05,0.45)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cms)") +
  annotate("text",x=2.5,y=0.4,label="Post",size=6) +
  theme_bw() +
  #coord_fixed(1.4) +
  theme(axis.text=element_text(size=12), axis.title.x=element_text(size=14),axis.title.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

n4 <- ggplot(jan2, aes(DATETIME,Qcms)) +
  geom_point(size = 2, color= "blue") +
  labs(y = "Discharge (cms)",
       x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M")) +
  theme_bw() +
  theme(axis.text=element_text(size=12),axis.title.x=element_text(size=14),axis.title.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin=unit(c(0.1,0.3,0.2,0.1),"cm"))

grid.arrange(n1, n2, n3, n4, ncol = 2)


