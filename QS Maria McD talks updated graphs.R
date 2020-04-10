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

#pull exported data in
QS <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QS Maria Final Export.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))

#pull recent data in
QSnew <- read.csv2("QS NO3 new.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))
colnames(QSnew)[4] <- "NO3"
QSnew$NO3 <- as.numeric(as.character(QSnew$NO3))


#need to convert data to numeric for certain columns (we need code columns to remain text)
for(i in 3:ncol(QS)) {
  QS[,i] <- as.numeric(as.character(QS[,i]))
}

#convert serial date to POSIX object to allow the timestamp to be compliant with R
colnames(QS)[2] <- "Date.and.Time"
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
QSnew$DATETIME = as.POSIXct(strptime(QSnew$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

#convert discharge cfs to m3s
QS$Qcms <- QS$DischargeCFS_new * 0.028316847

###long term NO3 plot#####
QSL <- subset(QS, as.Date(DATETIME) > "2017-06-01 23:01:00 AST")

QSnew2 <- subset(QSnew, NO3 < 0.3)
QSnew3 <- subset(QSnew2, as.Date(DATETIME) < "2019-10-01 23:01:00 AST")


ggplot(NULL, aes(DATETIME, NO3)) +
  geom_point(data = QSL) +
  geom_point(data = QSnew3) +
  scale_y_continuous(limits = c(0,1.25)) +
  scale_x_datetime(expand = c(0, 0)) +
  labs(y = "Nitrate-N (mg/L)",
       x = "Date") +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3, fill = "darkgreen") +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 1.2,
           alpha = .3, fill = "darkgreen") +
  annotate("text", x = as.POSIXct("2017-08-15 10:00:00 AST"), y = 1.0, label = "Irma", angle = 90, 
           color="red", size = 10) +
  annotate("text", x = as.POSIXct("2017-10-01 10:00:00 AST"), y = 1.0, label = "Maria", angle = 90, 
           color="red", size =10) +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title.y=element_text(size=18), axis.title.x=element_text(size=18))


####weekly sensor NO3 and CTE together#######
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

#remove LYS12 because its shit
data2.df <- subset(data.df, Sample_Name != 'LYS12')

#need to subset and remove missing values
NO3.df <- subset(data2.df, select = c(DATETIME, Sub_ProjectA, NO3, TRTDATE), NO3 > 0)
K <- subset(data2.df, select = c(DATETIME, Sub_ProjectA, K, TRTDATE), K > 0)

###all plots since June 2017
mariaNOall <- subset(NO3.df, DATETIME > "2017-06-01")
mariaKall <- subset(K, DATETIME > "2017-05-01")

NO3mean <- aggregate(NO3 ~ DATETIME, data = mariaNOall, FUN = "mean")
Kmean <- aggregate(K ~ DATETIME, data = mariaKall, FUN = "mean")

ggplot(NO3mean, aes(DATETIME, NO3)) +
  geom_point()

ggplot(Kmean, aes(DATETIME, K)) +
  geom_point()

###weekly mean NO3 of sensor
QSLN <- subset(QSL, select=c("DATETIME","NO3"))
QSnew4 <- subset(QSnew3, select=c("DATETIME","NO3"))
sensorno3 <- rbind(QSLN,QSnew4)
senNO3 <- subset(sensorno3, NO3 > 0)
xts <- xts(senNO3$NO3, order.by=senNO3$DATETIME, .RECLASS=TRUE)
weekly <- apply.weekly(xts, mean)
weeklydf <- data.frame(Date=index(weekly), coredata(weekly))
colnames(weeklydf)[1] <- "DATETIME"
colnames(weeklydf)[2] <- "NO3"

NO3mean2 <-  subset(NO3mean, DATETIME < "2019-08-01")

ggplot() +
  geom_line(data = weeklydf, aes(DATETIME, NO3, color = "blue"), size = 2) +
  geom_line(data = NO3mean2, aes(DATETIME, NO3, color = "darkorange"), size = 2) +
  labs(y="Nitrate (mg N/L)", x="Date") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.text=element_text(size=16)) +
  scale_colour_manual(name = "", values =c('blue'='blue','darkorange'='darkorange'), 
                      labels = c('Stream','Soil Solution'))


###weekly QS K and CTE K####
WRRC.db <- odbcConnect("WRRCdatabase", uid="a13579Z", pwd="Jh1188!") 

#pull data
data <- sqlFetch(WRRC.db,"LCZO Weekly Query")
qs <- subset(data, Sample_Name == "QS")
colnames(qs)[25] <- "K"

ggplot() +
  geom_line(data = qs, aes(Collection_Date, K, color = "blue"), size = 2) +
  geom_line(data = Kmean, aes(DATETIME, K, color = "darkorange"), size = 2) +
  labs(y="Potassium (mg/L)", x="Date") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        legend.text=element_text(size=16)) +
  scale_colour_manual(name = "", values =c('blue'='blue','darkorange'='darkorange'), 
                      labels = c('Stream','Soil Solution'))

####conductance plot

ggplot()

