library(xts)
library(ggplot2)


#pull data
ri <- read.csv2("IcacosPulse07-07-2017Download.csv", header = T, fill = TRUE, sep = ",")

#need to convert data to numeric

for(i in 2:ncol(ri)) {
  ri[,i] <- as.numeric(as.character(ri[,i]))
}

#convert serial date to POSIX object to allow the timestamp to be compliant with R

ri$DATETIME = as.POSIXct(strptime(ri$TIMESTAMP, "%Y-%m-%d %H:%M", tz="EST"))


#look at data
ggplot(ri, aes(DATETIME,PTemp)) +
  geom_point()

ggplot(ri, aes(DATETIME,batt_volt_Min)) +
  geom_point()

ggplot(ri, aes(DATETIME,Nitrate_um)) +
  geom_point()

ggplot(ri, aes(DATETIME,Nitrate_mgL)) +
  geom_point()

ggplot(ri, aes(DATETIME,Absorbance_254)) +
  geom_point()

ggplot(ri, aes(DATETIME,Cond)) +
  geom_point()

ggplot(ri, aes(DATETIME,Ct)) +
  geom_point() +
  scale_y_continuous(limits=c(-10,1))

ggplot(ri, aes(DATETIME,Turb)) +
  geom_point() 

ggplot(ri, aes(DATETIME,fDOM)) +
  geom_point()

#subset date
rijuly <- subset(ri, DATETIME > as.POSIXct('2017-07-04 00:06:00'))

ggplot(rijuly, aes(DATETIME,Turb)) +
  geom_point()




