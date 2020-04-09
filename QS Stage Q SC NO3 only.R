library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(RColorBrewer)

#pull data - from ODM2 server
QS <- read.csv2("QS_QstageNO3SC.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="UTC"))

###want to round the time to 15 minutes#####
QS$DATETIME2 <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 04:00:00 UTC')


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "NO3mgL"
QS$NO3mgL <- as.numeric(as.character(QS$NO3mgL))
colnames(QS)[5] <- "NO3QC"
colnames(QS)[7] <- "Qcfs"
QS$Qcfs <- as.numeric(as.character(QS$Qcfs)) 
colnames(QS)[8] <- "QcfsQC"
colnames(QS)[10] <- "Stageft"
QS$Stageft <- as.numeric(as.character(QS$Stageft)) 
colnames(QS)[11] <- "StageftQC"
colnames(QS)[13] <- "SpecCondms"
QS$SpecCondms <- as.numeric(as.character(QS$SpecCondms))
colnames(QS)[14] <- "SpecCondmsQC"

##separate out by variable
no <- subset(QS, select = c(NO3mgL,DATETIME2,NO3QC), QS$NO3QC != 'Bad')
q <- subset(QS, select = c(Qcfs,DATETIME2,QcfsQC), QS$QcfsQC != 'Bad')
s <- subset(QS, select = c(Stageft,DATETIME2,StageftQC), QS$StageftQC != 'Bad')
sc <- subset(QS, select = c(SpecCondms,DATETIME2,SpecCondmsQC), QS$SpecCondmsQC != 'Bad')

###bring them back together
qsf <- merge(no,q,by = "DATETIME2")
qsf2 <- merge(qsf,s,by = "DATETIME2")
qsd <- merge(qsf2,sc,by = "DATETIME2")

###ok, now want plots logQ vs NO3 and SC before and after storms

##just plot os log Q vs NO3 & stage
ggplot(qsd, aes(Qcfs, NO3mgL)) +
  geom_point() +
  scale_x_log10() +
  scale_y_continuous(limits = c(0,1.25)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw()

ggplot(qsd, aes(Qcfs, SpecCondms)) +
  geom_point() +
  scale_x_log10() +
  scale_y_continuous() +
  labs(y = "Specific Cond (mS/cm)",
       x = "Discharge (cfs)") +
  theme_bw()


#now look for some storms
ggplot(qsd, aes(DATETIME2,Qcfs)) +
  geom_line()

ggplot(qsd, aes(DATETIME2,NO3mgL)) +
  geom_point()

aug <- subset(qsd, as.POSIXct(DATETIME2) > "2017-08-14 12:00:00 EDT")
aug2 <- subset(aug, as.POSIXct(DATETIME2) < "2017-08-16 00:00:00 EDT")

ggplot(aug2, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")


##ok, nice one
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))

N1 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=aug2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.2,0.1,0.9,0.1),"cm"))


S1 <- qplot(Qcfs, SpecCondms, color=DATETIME2, data=aug2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Spec Cond (mS/cm)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.2,0.1,0.9,0.1),"cm"))

  
##post storm
oct <- subset(qsd, as.Date(DATETIME2) > "2017-09-25 18:00:00 EDT")
oct2 <- subset(oct, as.Date(DATETIME2) < "2017-10-02 00:00:00 EDT")

ggplot(oct2, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")


N2 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=oct2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(8,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
  
S2 <- qplot(Qcfs, SpecCondms, color=DATETIME2, data=oct2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(8,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Spec Cond (mS/cm)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")



###recent storm for both
jan <- subset(qsd, as.Date(DATETIME2) > "2017-12-31 23:00:00 EDT")
jan2 <- subset(jan, as.Date(DATETIME2) < "2018-01-04 01:00:00 EDT")

ggplot(jan2, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")


N3 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=jan2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.2,0.1,0.9,0.1),"cm"))


#need different timer period for this one
jan3 <- subset(qsd, as.Date(DATETIME2) > "2018-01-15 23:00:00 EDT")
jan4 <- subset(jan3, as.Date(DATETIME2) < "2018-01-18 01:00:00 EDT")

ggplot(jan4, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs)) +
  geom_point(aes(DATETIME2,SpecCondms), color = "blue")


S3 <- qplot(Qcfs, SpecCondms, color=DATETIME2, data=jan4) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(8,150)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Spec Cond (mS/cm)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.2,0.1,0.9,0.1),"cm"))



grid.arrange(N1, N2, N3, nrow = 1)

grid.arrange(S1, S2, S3, nrow = 1)


###same version of NO3-Q with y-axis changed

N4 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=aug2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_y_continuous(limits = c(0,0.45)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.2,0.1,0.9,0.1),"cm"))

N5 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=oct2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(8,100)) +
  scale_y_continuous(limits = c(0,0.45)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

N6 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=jan2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_y_continuous(limits = c(0,0.45)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.2,0.1,0.9,0.1),"cm"))

grid.arrange(N4, N5, N6, nrow = 1)



###big messy storm
dec <- subset(qsd, as.Date(DATETIME2) > "2017-12-04 23:00:00 EDT")
dec2 <- subset(dec, as.Date(DATETIME2) < "2017-12-09 01:00:00 EDT")

ggplot(dec2, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")

ggplot(dec2, aes(NULL)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")
  

  
N7 <- ggplot(dec2, aes(DATETIME2,NO3mgL)) +
  geom_point() + 
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
N8 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=dec2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

grid.arrange(N7, N8, nrow = 1)

####one more in Nov###
###do not use because NO3 is weird###
nov <- subset(qsd, as.Date(DATETIME2) > "2017-11-04 23:00:00 EDT")
nov2 <- subset(nov, as.Date(DATETIME2) < "2017-11-07 18:00:00 EDT")

ggplot(nov2, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")

ggplot(nov2, aes(NULL)) +
  geom_point(aes(DATETIME2,NO3mgL), color = "blue")
  

N9 <- ggplot(nov2, aes(NULL)) +
  geom_line(aes(DATETIME2,Qcfs), color = "blue") +
  geom_point(aes(DATETIME2,NO3mgL*60)) + 
  scale_y_continuous(limits = c(0,65), 
                     sec.axis = sec_axis(~./60 , name = "Nitrate-N (mg/L)")) +
  labs(y = "Discharge (cfs)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

N10 <- qplot(Qcfs, NO3mgL, color=DATETIME2, data=nov2) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(5,100)) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")

grid.arrange(N9, N10, nrow = 1)
