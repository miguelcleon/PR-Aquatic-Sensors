library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)
library(dataRetrieval)

#pull data - from ODM2 server
RI <- read.csv2("RIcond.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#pull discharge from USGS
qdat <- 
  readNWISuv(
    siteNumbers='50075000', parameterCd=c('00060','00065'), startDate='2017-08-13', 
    endDate='2018-05-16', tz='America/Jamaica') 
renameNWISColumns(qdat)


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(RI)[4] <- "SCmS"
RI$SCmS <- as.numeric(as.character(RI$SCmS)) 
colnames(RI)[7] <- "CondmS"
RI$CondmS <- as.numeric(as.character(RI$CondmS)) 

for(i in 4:ncol(qdat)) {
  qdat[,i] <- as.numeric(as.character(qdat[,i]))
}

#convert serial date to POSIX object to allow the timestamp to be compliant with R
RI$DATETIME = as.POSIXct(strptime(RI$Date.and.Time, "%Y-%m-%d %H:%M", 
                                  tz="America/Puerto_Rico"))


colnames(qdat)[6] <- "Stage_ft"
colnames(qdat)[4] <- "Discharge_cfs"


###plot sc

ggplot(RI, aes(DATETIME,SCmS)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.1))

ggplot(qdat, aes(dateTime, Discharge_cfs)) +
  geom_line(color = "blue") +
  scale_y_continuous(limits = c(0,1000))

##together

f1 <- ggplot(RI, aes(DATETIME,SCmS)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.1))

f2 <- ggplot(qdat, aes(dateTime, Discharge_cfs)) +
  geom_line(color = "blue") +
  scale_y_continuous(limits = c(0,1000))

grid.arrange(f1, f2, nrow = 2)


#zoom
pre <- subset(RI, as.Date(DATETIME) < "2017-09-12 23:01:00 AST")
preq <- subset(qdat, as.Date(dateTime) < "2017-09-12 23:01:00 AST")
post <- subset(RI, as.Date(DATETIME) > "2018-04-16 23:01:00 AST")
postq <- subset(qdat, as.Date(dateTime) > "2018-04-16 23:01:00 AST")

f3 <- ggplot(pre, aes(DATETIME,SCmS)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.1))

f4 <- ggplot(preq, aes(dateTime, Discharge_cfs)) +
  geom_line(color = "blue") +
  scale_y_continuous(limits = c(0,500))

grid.arrange(f3, f4, nrow = 2)

f5 <- ggplot(post, aes(DATETIME,SCmS)) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.1))

f6 <- ggplot(postq, aes(dateTime, Discharge_cfs)) +
  geom_line(color = "blue") +
  scale_y_continuous(limits = c(0,500))

grid.arrange(f5, f6, nrow = 2)


