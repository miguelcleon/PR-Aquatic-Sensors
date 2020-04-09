library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(cowplot)
library(plyr)

##working on just QS here for now####

#pull data - from ODM2 server
QS <- read.csv2("QS_DO.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#grab data
czo <- read.csv2("PR Sensors CSV Query.csv", header = T, fill = TRUE,
                 sep = ",", na.strings=c("","NA"))
QSczo <- subset(czo, Sample.Name == "QS")


#need to convert data to numeric
for(i in 9:ncol(QSczo)) {
  QSczo[,i] <- as.numeric(as.character(QSczo[,i]))
}

colnames(QSczo)[13] <- "DOper"


#need to convert sensor data to numeric for certain columns (we need code columns to remain text)
colnames(QS)[4] <- "DOper"
QS$DOper <- as.numeric(as.character(QS$DOper)) 

colnames(QS)[7] <- "DO_Conc"
QS$DO_Conc <- as.numeric(as.character(QS$DO_Conc)) 

#convert serial date to POSIX object to allow the timestamp to be compliant with R
QS$DATETIME = as.POSIXct(strptime(QS$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
QSczo$DATETIME = as.POSIXct(strptime(QSczo$DateTime, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

###want to round the time to 15 minutes#####
QSczo$DATETIME <- as.POSIXct(round(as.numeric(QSczo$DATETIME)/(15*60))*(15*60),
                             origin='1970-01-01 12:00:00 America/Puerto_Rico', tz="America/Puerto_Rico")


#need to combine these to plot DO vs DO
qsall <- merge(QS, QSczo, by="DATETIME", all.x = TRUE)

#plot
ggplot(NULL, aes(DATETIME,DOper)) +
  geom_point(data = QS) + 
  geom_point(data = QSczo, color = "blue")

#pre Maria
pre <- subset(QS, as.Date(DATETIME) > "2017-08-14 23:01:00 America/Puerto_Rico")
pre2 <- subset(pre, as.Date(DATETIME) < "2017-09-11 23:01:00 America/Puerto_Rico")

ggplot(pre2, aes(DATETIME,DOper)) +
  geom_point() +
  labs(x = "Date",
       y = "DO (%)") +
  theme_bw()


#post Maria
post <- subset(QS, as.Date(DATETIME) > "2018-04-16 23:01:00 America/Puerto_Rico")
post2 <- subset(post, as.Date(DATETIME) < "2018-05-16 23:01:00 America/Puerto_Rico")

ggplot(post2, aes(DATETIME,DOper)) +
  geom_point() +
  labs(x = "Date",
       y = "DO (%)") +
  theme_bw()

#way post Maria
june <- subset(QS, as.Date(DATETIME) > "2018-06-01 23:01:00 America/Puerto_Rico")

ggplot(june, aes(DATETIME,DOper)) +
  geom_point() +
  labs(x = "Date",
       y = "DO (%)") +
  theme_bw()



