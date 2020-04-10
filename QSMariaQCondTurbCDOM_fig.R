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
QS <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QSMariaQCondTurbCDOM.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[10] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[13] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS)) 
colnames(QS)[4] <- "DischargeCFS"
QS$DischargeCFS <- as.numeric(as.character(QS$DischargeCFS))
colnames(QS)[7] <- "Stage_adjusted_ft"
QS$Stage_adjusted_ft <- as.numeric(as.character(QS$Stage_adjusted_ft))
colnames(QS)[16] <- "fDOMmV"
QS$fDOMmV <- as.numeric(as.character(QS$fDOMmV))
colnames(QS)[19] <- "TurbmV"
QS$TurbmV <- as.numeric(as.character(QS$TurbmV))
colnames(QS)[11] <- "CondQC"
colnames(QS)[5] <- "DischargeQC"
colnames(QS)[8] <- "StageftQC"
colnames(QS)[17] <- "fDOMQC"
colnames(QS)[20] <- "turbQC"
colnames(QS)[14] <- "SpecCondQC"


#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))


###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UCT')

##separate out by variable
q <- subset(QS, select = c(DischargeCFS,DATETIME,DischargeQC), QS$DischargeQC != 'Bad')
s <- subset(QS, select = c(Stage_adjusted_ft,DATETIME,StageftQC), QS$StageftQC != 'Bad')
c <- subset(QS, select = c(CondmS,DATETIME,CondQC), QS$CondQC != 'Bad')
t <- subset(QS, select = c(TurbmV,DATETIME,turbQC), QS$turbQC != 'Bad')
f <- subset(QS, select = c(fDOMmV,DATETIME,fDOMQC), QS$fDOMQC != 'Bad')
sc <- subset(QS, select = c(SpecCondmS,DATETIME,SpecCondQC), QS$SpecCondQC != 'Bad')

c2 = c[!duplicated(c$DATETIME),]


###bring them back together
qsf <- merge(c2,q,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,s,by = "DATETIME", all.x = TRUE)
qsd <- merge(qss,sc,by = "DATETIME", all.x = TRUE)
qst <- merge(qsd,t,by = "DATETIME", all.x = TRUE)
qsn <- merge(qst,f,by = "DATETIME", all.x = TRUE)


#Check them all
ggplot(NULL, aes(DATETIME, CondmS)) +
  geom_point(data = QS)

ggplot(NULL, aes(DATETIME, DischargeCFS)) +
  geom_point(data = QS)

ggplot(NULL, aes(DATETIME, SpecCondmS)) +
  geom_point(data = QS)

ggplot(NULL, aes(DATETIME, TurbmV)) +
  geom_point(data = QS)

ggplot(NULL, aes(DATETIME, fDOMmV)) +
  geom_point(data = QS) 

##convert fDOMmV to ppb QSU
QS$fDOMppb <- (0.061868*QS$fDOMmV) - 1.60858

ggplot(NULL, aes(DATETIME, fDOMppb)) +
  geom_point(data = QS) 

##looks like July 1 is where good cond data starts, so start there
QS2 <- subset(QS, as.Date(DATETIME) > "2017-08-01 23:01:00 America/Puerto_Rico")
QS3 <- subset(QS2, as.Date(DATETIME) < "2017-12-15 23:01:00 America/Puerto_Rico")

p1 <- ggplot(QS3, aes(DATETIME, DischargeCFS)) +
  geom_point() +
  scale_x_datetime(expand = c(0, 0)) +
  scale_y_log10() +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 5, ymax = 800,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 5, ymax = 800,
           alpha = .3) +
  annotate("text", x = as.POSIXct("2017-09-03 10:00:00 AST"), y = 100, label = "Irma", angle = 90, 
           color="red", size = 6) +
  annotate("text", x = as.POSIXct("2017-09-16 10:00:00 AST"), y = 100, label = "Maria", angle = 90, 
           color="red", size =6) +
  labs(y = "Q (cfs)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())

p2 <- ggplot(QS3, aes(DATETIME, TurbmV)) +
  geom_point() +
  scale_y_log10(limits = c(12,1250)) +
  scale_x_datetime(expand = c(0, 0)) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 15, ymax = 800,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 15, ymax = 800,
           alpha = .3) +
  labs(y = "Turb (FNU)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())

p3 <- ggplot(QS3, aes(DATETIME, CondmS)) +
  geom_point() +
  scale_x_datetime(expand = c(0, 0)) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 0, ymax = 0.11,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 0.11,
           alpha = .3) +
  labs(y = "S.Cond (mS/cm)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_blank())


p4 <- ggplot(QS3, aes(DATETIME, fDOMppb)) +
  geom_point() +
  scale_x_datetime(expand = c(0, 0)) +
  annotate("rect", xmin = as.POSIXct("2017-09-06 00:01:00 AST"), xmax = as.POSIXct("2017-09-09 00:01:00 AST")
           , ymin = 10, ymax = 55,
           alpha = .3) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 10, ymax = 55,
           alpha = .3) +
  labs(y = "fDOM (ppb)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.margin=unit(c(0.1,0.2,0.1,0.5),"cm"))


plot_grid(p1, p2, p3, p4, nrow = 4, rel_heights = c(1, 1, 1, 1.3))




