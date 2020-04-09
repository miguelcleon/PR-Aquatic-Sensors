library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)

#pull data - from ODM2 server
QS <- read.csv2("QS_cond.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "ConduS"
QS$ConduS <- as.numeric(as.character(QS$ConduS)) 
colnames(QS)[13] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[16] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[19] <- "SpecConduS"
QS$SpecConduS <- as.numeric(as.character(QS$SpecConduS))
colnames(QS)[7] <- "Qcfs"
QS$Qcfs <- as.numeric(as.character(QS$Qcfs))
colnames(QS)[10] <- "Stageft"
QS$Stageft <- as.numeric(as.character(QS$Stageft))

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UTC')

scm <- subset(QS, select = c(SpecCondmS,DATETIME,quality.code.4), QS$quality.code.4 != 'Bad')
scu <- subset(QS, select = c(SpecConduS,DATETIME,quality.code.5), QS$quality.code.5 != 'Bad')
cm <- subset(QS, select = c(CondmS,DATETIME,quality.code.3), QS$quality.code.3 != 'Bad')
cu <- subset(QS, select = c(ConduS,DATETIME,quality.code), QS$quality.code != 'Bad')
q <- subset(QS, select = c(Qcfs,DATETIME,quality.code.1), QS$quality.code.1 != 'Bad')


###bring them back together
qsf <- merge(scm,scu,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,cm,by = "DATETIME", all.x = TRUE)
qsq <- merge(qss,cu,by = "DATETIME", all.x = TRUE)
qsd <- merge(qsq,q,by = "DATETIME", all.x = TRUE)

###which one looks best???
ggplot(qsd, aes(DATETIME,CondmS)) +
  geom_point() +
  theme_bw()

ggplot(qsd, aes(DATETIME,ConduS)) +
  geom_point() +
  theme_bw()

ggplot(qsd, aes(DATETIME,SpecCondmS)) +
  geom_point() +
  theme_bw()

ggplot(qsd, aes(DATETIME,SpecConduS)) +
  geom_point() +
  theme_bw()

###the best looks like CondmS
qsdl <- subset(qsd, as.Date(DATETIME) > "2017-08-15 23:01:00 UTC")
qsdll <- subset(qsdl, as.Date(DATETIME) < "2017-12-01 23:01:00 UTC")

ggplot(qsdll, aes(DATETIME,CondmS)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.12)) +
  labs(y = "Specific Conductance (mS/cm)",
      x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 0.115,
           alpha = .3) +
  annotate("text", x = as.POSIXct("2017-09-16 10:00:00 AST"), y = 0.1, label = "Maria", angle = 90, 
           color="red", size = 6)






