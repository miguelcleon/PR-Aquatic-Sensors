library(xts)
library(ggplot2)
library(gridExtra)
library(reshape)
library(gtable)
library(segmented)
library(zoo)
library(grid)

#USGS data
usgs <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/USGS_Icacos_sensor_data_Sept_2017_Jan_2018.csv", header = T, fill = TRUE,
                  sep = ",")

#pull data - from LCZO server
data <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/Sonadora_WQual.csv", header = T, fill = TRUE,
                  sep = ",")


#need to convert data to numeric
for(i in 2:ncol(usgs)) {
  usgs[,i] <- as.numeric(as.character(usgs[,i]))
}


#convert serial date to POSIX object to allow the timestamp to be compliant with R
usgs$DATETIME = as.POSIXct(strptime(usgs$datetime, "%Y-%m-%d %H:%M", tz="America/Port_of_Spain"))

#quick plots
ggplot(usgs, aes(DATETIME, FDOM..ppb)) +
  geom_point() +
  scale_y_continuous(limits = c(0,50))

ggplot(usgs, aes(DATETIME, Turb..FNU)) +
  geom_point() +
  scale_y_continuous(limits = c(0,2000))

###more to do ...
  
