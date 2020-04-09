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
sh <- read.csv2("RESSH_cond.csv", header = T, fill = TRUE, sep = ",", na.strings=c("","NA"))


#need to convert data to numeric for certain columns (we need code columns to remain text)
colnames(sh)[4] <- "SCmS"
sh$SCmS <- as.numeric(as.character(sh$SCmS)) 


#convert serial date to POSIX object to allow the timestamp to be compliant with R
sh$DATETIME = as.POSIXct(strptime(sh$Date.and.Time, "%Y-%m-%d %H:%M", 
                                  tz="America/Puerto_Rico"))

###plot sc

ggplot(sh, aes(DATETIME,SCmS)) +
  geom_point()

post <- subset(sh, as.Date(DATETIME) > "2017-07-01 23:01:00 AST")

ggplot(post, aes(DATETIME,SCmS)) +
  geom_point()




