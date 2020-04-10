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

#pull data - from ODM2
QS <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QS Maria Final Export.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


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
QSLL <- subset(QSL, as.Date(DATETIME) < "2018-10-01 23:01:00 UTC")


ggplot(NULL, aes(DATETIME, NO3)) +
  geom_line(data = QSL, color = "darkblue", size = 1) +
  scale_y_continuous(limits = c(0,1.2)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3, fill = "darkgreen") +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3, fill = "darkgreen") +
  annotate("text", x = as.POSIXct("2017-08-29 10:00:00 AST"), y = 1.0, label = "Irma", angle = 90, 
           color="black", size = 8) +
  annotate("text", x = as.POSIXct("2017-09-27 10:00:00 AST"), y = 1.0, label = "Maria", angle = 90, 
           color="black", size = 8) +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=18), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())




###Fig 2

aug <- subset(QS, as.POSIXct(DATETIME) > "2017-08-15 07:00:00 EDT")
aug2 <- subset(aug, as.POSIXct(DATETIME) < "2017-08-15 19:00:00 EDT")

ggplot(aug2, aes(NULL)) +
  geom_line(aes(DATETIME,Qcms)) +
  geom_point(aes(DATETIME,NO3), color = "blue")

pre <- subset(QS, as.POSIXct(DATETIME) > "2017-08-10 00:00:00 EDT")
pre2 <- subset(pre, as.POSIXct(DATETIME) < "2017-08-11 00:15:00 EDT")

ggplot(pre2, aes(NULL)) +
  geom_line(aes(DATETIME,Qcms)) +
  geom_point(aes(DATETIME,NO3), color = "blue")


##ok, nice one
library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sn <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))


##post storm
#Oct is good, but right at start of increase in NO3
oct <- subset(QS, as.Date(DATETIME) > "2017-09-30 18:00:00 EST")
oct2 <- subset(oct, as.Date(DATETIME) < "2017-10-02 18:00:00 EST")

ggplot(oct2, aes(NULL)) +
  geom_line(aes(DATETIME,Qcms)) +
  geom_point(aes(DATETIME,NO3), color = "blue")

#this one has weird timing on NO3
jan <- subset(QS, as.Date(DATETIME) > "2018-01-01 22:00:00 EST")
jan2 <- subset(jan, as.Date(DATETIME) < "2018-01-03 01:15:00 EST")

ggplot(jan2, aes(NULL)) +
  geom_line(aes(DATETIME,Qcms)) +
  geom_point(aes(DATETIME,NO3), color = "blue")

#how about 
post <- subset(QS, as.Date(DATETIME) > "2018-01-01 22:00:00 EST")
post2 <- subset(post, as.Date(DATETIME) < "2018-01-30 01:15:00 EST")

ggplot(post2, aes(NULL)) +
  geom_line(aes(DATETIME,Qcms)) +
  geom_point(aes(DATETIME,NO3), color = "blue")


#Fig 2 v1 - hydrographs on left side, hystersis on right

f1 <- ggplot(pre2, aes(Qcms,NO3,color= as.factor(DATETIME))) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.2,1)) +
  scale_y_continuous(limits = c(0.05,0.45),position = "right") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cms)") +
  annotate("text",x=0.9,y=0.4,label="Pre",size=6) +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
  theme(plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))

f2 <- ggplot(pre2, aes(DATETIME,Qcms)) +
  geom_point(size = 2, color= "blue") +
  labs(y = "Discharge (cms)",
       x = "Time") +
  scale_x_datetime(labels = date_format("%H:%M")) +
  theme_bw() +
  annotate("text",x=as.POSIXct("2017-08-10 22:00:00 EDT"),y=1.0,label="Pre",size=6) +
  theme(axis.text=element_text(size=12), axis.title.y=element_blank(), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin=unit(c(0.1,0.3,0.1,0.1),"cm"))


f3 <- ggplot(jan2, aes(Qcms,NO3,color= as.factor(DATETIME))) +
  geom_point(size = 2) +
  scale_x_log10(limits = c(0.2,1.0)) +
  scale_y_continuous(limits = c(0.05,0.45),position = "right") +
  scale_color_viridis(discrete = TRUE, option = "D") +
  labs(y = "Nitrate-N (mg/L)",
       x = "Discharge (cms)") +
  annotate("text",x=0.85,y=0.4,label="Post",size=6) +
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
  annotate("text",x=as.POSIXct("2018-01-02 17:00:00 EST"),y=0.8,label="Post",size=6) +
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


###Fig 3#####

#load RODBC package into 'R'
library(RODBC)

WRRC.db <- odbcConnect("WRRCdatabase", uid="a13579Z", pwd="Jh1188!") 

#pull data
data.df <- sqlFetch(WRRC.db,"CTE McD Query")

#date stuff working
data.df$DATETIME <- as.POSIXct(data.df$Collection_Date, "%Y-%M-%D", tz ="America/Puerto_Rico")
col_idx <- grep("DATETIME", names(data.df))
data.df <- data.df[, c(col_idx, (1:ncol(data.df))[-col_idx])]
col_idx <- grep("Sub_ProjectA", names(data.df))
data.df <- data.df[, c(col_idx, (1:ncol(data.df))[-col_idx])]


#numeric
for(i in 6:ncol(data.df)) {
  data.df[,i] <- as.numeric(as.character(data.df[,i]))
}

colnames(data.df)[18] <- "NO3"
colnames(data.df)[12] <- "NH4"
colnames(data.df)[13] <- "PO4"
colnames(data.df)[15] <- "TDN"
colnames(data.df)[19] <- "DON"
colnames(data.df)[7] <- "SO4"

#remove LYS12 because its shit
data2.df <- subset(data.df, Sample_Name != 'LYS12')

#need to subset and remove missing values
NO3.df <- subset(data2.df, select = c(DATETIME, Sub_ProjectA, NO3, TRTDATE), NO3 > 0)

###all plots since June 2017
mariaNOall <- subset(NO3.df, DATETIME > "2017-06-01")
mariaNOall2 <- subset(mariaNOall, DATETIME < "2018-10-01")
NO3mean <- aggregate(NO3 ~ DATETIME, data = mariaNOall2, FUN = "mean")

###weekly mean NO3 of sensor
senNO3 <- subset(QSLL, NO3 > 0)
xts <- xts(senNO3$NO3, order.by=senNO3$DATETIME, .RECLASS=TRUE)
weekly <- apply.weekly(xts, mean)
weeklydf <- data.frame(Date=index(weekly), coredata(weekly))
colnames(weeklydf)[1] <- "DATETIME"
colnames(weeklydf)[2] <- "NO3"

NO3mean2 <-  subset(NO3mean, DATETIME < "2019-02-01")

ggplot() +
  geom_line(data = weeklydf, aes(DATETIME, NO3, color = "darkblue"), size = 2) +
  geom_line(data = NO3mean2, aes(DATETIME, NO3, color = "darkorange"), size = 2) +
  labs(y="Nitrate (mg N/L)", x="Date") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.text=element_text(size=16)) +
  scale_colour_manual(name = "", values =c('darkblue'='darkblue','darkorange'='darkorange'), 
                      labels = c('Stream','Soil Solution'))


###turb for Bill
ggplot(QSLL, aes(DATETIME, TurbNTU)) +
  geom_point()

