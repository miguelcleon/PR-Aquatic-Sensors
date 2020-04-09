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
QS <- read.csv2("QScondturb.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[10] <- "CondmS"
QS$CondmS <- as.numeric(as.character(QS$CondmS)) 
colnames(QS)[13] <- "SpecCondmS"
QS$SpecCondmS <- as.numeric(as.character(QS$SpecCondmS))
colnames(QS)[4] <- "Qcfs"
QS$Qcfs <- as.numeric(as.character(QS$Qcfs))
colnames(QS)[7] <- "Stageft"
QS$Stageft <- as.numeric(as.character(QS$Stageft))
colnames(QS)[16] <- "Turb"
QS$Turb <- as.numeric(as.character(QS$Turb))

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes#####
QS$DATETIME <- as.POSIXct(round(as.numeric(QS$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UTC')

scm <- subset(QS, select = c(SpecCondmS,DATETIME,quality.code.3), QS$quality.code.3 != 'Bad')
cm <- subset(QS, select = c(CondmS,DATETIME,quality.code.2), QS$quality.code.2 != 'Bad')
q <- subset(QS, select = c(Qcfs,DATETIME,quality.code), QS$quality.code != 'Bad')
st <- subset(QS, select = c(Stageft,DATETIME,quality.code.1), QS$quality.code.1 != 'Bad')
tu <- subset(QS, select = c(Turb,DATETIME,quality.code.4), QS$quality.code.4 != 'Bad')


###bring them back together
qsf <- merge(scm,cm,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,q,by = "DATETIME", all.x = TRUE)
qsq <- merge(qss,st,by = "DATETIME", all.x = TRUE)
qsd <- merge(qsq,tu,by = "DATETIME", all.x = TRUE)

##which cond are we using
ggplot(qsd, aes(DATETIME,CondmS)) +
  geom_point() +
  theme_bw()

ggplot(qsd, aes(DATETIME,SpecCondmS)) +
  geom_point() +
  theme_bw()

###cond goes below 0 before the storm, do temporary fix for cond for now
qsd$Cond <- qsd$CondmS + 0.025


###ok 2 panel plot of cond & turb

p1 <- ggplot(qsd, aes(DATETIME,Cond)) +
  geom_point() +
  labs(y = "Conductance (mS/cm)",
       x = "Date") +
  theme_bw() +
  theme(axis.text.y=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x=element_blank()) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 0.14,
           alpha = .3) +
  annotate("text", x = as.POSIXct("2017-09-16 10:00:00 AST"), y = 0.12, label = "Maria", angle = 90, 
           color="red", size = 6) +
  theme(plot.margin=unit(c(0.2,0.2,0.5,0.3),"cm"))

p2 <- ggplot(qsd, aes(DATETIME,Turb)) +
  geom_point() +
  scale_y_continuous(limits = c(0,1250)) +
  labs(y = "Turbidity (FNU)",
       x = "Date") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), axis.title.x=element_text(size=14),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  annotate("rect", xmin = as.POSIXct("2017-09-19 00:01:00 AST"), xmax = as.POSIXct("2017-09-22 00:01:00 AST")
           , ymin = 0, ymax = 1200,
           alpha = .3) +
  theme(plot.margin=unit(c(0.1,0.2,0.2,0.2),"cm"))

grid.arrange(p1,p2,ncol=1)


###now a plot of cond vs Q for chasing ball plot

aug <- subset(qsd, as.POSIXct(DATETIME) > "2017-08-15 07:00:00 EDT")
aug2 <- subset(aug, as.POSIXct(DATETIME) < "2017-08-15 19:00:00 EDT")

oct <- subset(qsd, as.Date(DATETIME) > "2017-09-30 00:15:00 EST")
oct2 <- subset(oct, as.Date(DATETIME) < "2017-10-02 00:00:00 EST")

ggplot(NULL, aes(Qcfs,CondmS)) +
  geom_point(data=aug2,size = 2,color = "green") +
  geom_point(data=oct2,size = 2,color = "darkgoldenrod2") +
  scale_x_log10(limits = c(8,100)) +
  labs(y = "Specific Conductance (mS/cm)",
       x = "Discharge (cfs)") +
  theme_bw() +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=18),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") 





