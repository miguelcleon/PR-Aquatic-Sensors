library(xts)
library(ggplot2)
library(gridExtra)
library(zoo)
library(grid)
library(data.table)
library(lubridate)
library(dplyr)
library(RColorBrewer)
library(dataRetrieval)

##pull in all stage & Q from LTER DB - this is best QS data
##
#load RODBC package into 'R'
library(RODBC)

#Connect 'R' to Access Database
#This "file name" needs to match the DSN in the Access ODBC
#To change this go to Control Panel-Administrative Tools-Data Sources (ODBC)
#     and create a new Access data source under the User DSN tab
PRLTER.db <- odbcConnect("PRLTER database") 

data <- sqlFetch(PRLTER.db,"USGS QS Useable 15Minute Discharge Table")

colnames(data)[5] <- "Sample_Date"

##add the time
data$MINUTE <- as.numeric(as.character(data$MINUTE)) 
data$MINUTE <- sprintf("%04d",data$MINUTE)
data$MINUTE = paste0(substr(data$MINUTE,1,2),":",substr(data$MINUTE,3,4),":", 
                     substr(data$MINUTE,5,6),00)

data$DATETIME <- with(data, ymd(data$Sample_Date) + hms(data$MINUTE))
data$DATETIME = as.POSIXct(strptime(data$DATETIME, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))

col_idx <- grep("DATETIME", names(data))
data <- data[, c(col_idx, (1:ncol(data))[-col_idx])]

#numeric
for(i in 8:ncol(data)) {
  data[,i] <- as.numeric(as.character(data[,i]))
}

qsusgs <- subset(data, as.POSIXct(DATETIME) < "2010-01-01 12:00:00 AST")

ggplot(qsusgs, aes(DATETIME,USGS_cfps)) +
  geom_point()

ggplot(qsusgs, aes(DATETIME,USGS_STAGE)) +
  geom_point()

##stage-Q
ggplot(qsusgs, aes(USGS_STAGE,USGS_cfps)) +
  geom_point()

##where do we think the break is in stage-Q where it is "step backwater"
ggplot(qsusgs, aes(USGS_STAGE,USGS_cfps)) +
  geom_point() +
  scale_x_continuous(limits = c(3.2,7.0)) +
  scale_y_continuous(limits = c(0,500))


#new QS data from ODM2 server
qsczo <- read.csv2("C:/Users/jpotter/Box/PR Sensors Data/ODM2 Output/QS_discharge_curve.csv", header = T, 
                fill = TRUE, sep = ",", na.strings=c("","NA"))

qsczo$DATETIME = as.POSIXct(strptime(qsczo$Date.and.Time, "%Y-%m-%d %H:%M", tz="America/Puerto_Rico"))
colnames(qsczo)[4] <- "Water_temperature_C_L1"
colnames(qsczo)[5] <- "Water_temperature_C_L1QC"
colnames(qsczo)[7] <- "Barometric_pressure_kPa"
colnames(qsczo)[8] <- "Barometric_pressure_kPaQC"
colnames(qsczo)[19] <- "SpCond_uScm"
colnames(qsczo)[20] <- "SpCond_uScmQC"
colnames(qsczo)[10] <- "stage_ft_L0"
colnames(qsczo)[11] <- "stage_ft_L0QC"
colnames(qsczo)[13] <- "stage_manual_ft"
colnames(qsczo)[14] <- "stage_manual_ftQC"
colnames(qsczo)[16] <- "stage_adjusted_ft_L1"
colnames(qsczo)[17] <- "stage_adjusted_ft_L1QC"
colnames(qsczo)[22] <- "stage_CM_L1"
colnames(qsczo)[23] <- "stage_CM_L1QC"
colnames(qsczo)[25] <- "stage_HOBOassistant_m_L0"
colnames(qsczo)[26] <- "stage_HOBOassistant_m_L0QC"
colnames(qsczo)[28] <- "discharge_cfs_L1"
colnames(qsczo)[29] <- "discharge_cfs_L1QC"
colnames(qsczo)[31] <- "Water_pressure_kPa_L1"
colnames(qsczo)[32] <- "Water_pressure_kPa_L1QC"

###want to round the time to 15 minutes#####
qsczo$DATETIME <- as.POSIXct(round(as.numeric(qsczo$DATETIME)/(15*60))*(15*60),origin='1970-01-01 12:00:00 UCT')

##separate out by variable
q <- subset(qsczo, select = c(discharge_cfs_L1,DATETIME,discharge_cfs_L1QC), qsczo$discharge_cfs_L1QC != 'Bad')
s <- subset(qsczo, select = c(stage_ft_L0,DATETIME,stage_ft_L0QC), qsczo$stage_ft_L0QC != 'Bad')
c <- subset(qsczo, select = c(SpCond_uScm,DATETIME,SpCond_uScmQC), qsczo$SpCond_uScmQC != 'Bad')
b <- subset(qsczo, select = c(Barometric_pressure_kPa,DATETIME,Barometric_pressure_kPaQC), qsczo$Barometric_pressure_kPaQC != 'Bad')
f <- subset(qsczo, select = c(stage_manual_ft,DATETIME,stage_manual_ftQC), qsczo$stage_manual_ftQC != 'Bad')
sa <- subset(qsczo, select = c(stage_adjusted_ft_L1,DATETIME,stage_adjusted_ft_L1QC), qsczo$stage_adjusted_ft_L1QC != 'Bad')
scm <- subset(qsczo, select = c(stage_CM_L1,DATETIME,stage_CM_L1QC), qsczo$stage_CM_L1QC != 'Bad')
sho <- subset(qsczo, select = c(stage_HOBOassistant_m_L0,DATETIME,stage_HOBOassistant_m_L0QC), qsczo$stage_HOBOassistant_m_L0QC != 'Bad')
wp <- subset(qsczo, select = c(Water_pressure_kPa_L1,DATETIME,Water_pressure_kPa_L1QC), qsczo$Water_pressure_kPa_L1QC != 'Bad')

s2 = s[!duplicated(c$DATETIME),]


###bring them back together
qsf <- merge(s2,q,by = "DATETIME", all.x = TRUE)
qss <- merge(qsf,c,by = "DATETIME", all.x = TRUE)
qsd <- merge(qss,b,by = "DATETIME", all.x = TRUE)
qst <- merge(qsd,sa,by = "DATETIME", all.x = TRUE)
qsn <- merge(qst,f,by = "DATETIME", all.x = TRUE)
qso <- merge(qsn,scm,by = "DATETIME", all.x = TRUE)
qsp <- merge(qso,sho,by = "DATETIME", all.x = TRUE)
qsq <- merge(qsp,wp,by = "DATETIME", all.x = TRUE)

for(i in 2:ncol(qsq)) {
  qsq[,i] <- as.numeric(as.character(qsq[,i]))
}

ggplot(qsq, aes(DATETIME,stage_manual_ft)) +
  geom_point()


#bring in new manual discharge from Flowtracker measurements
##is it a big enough range for low measurements?
qsflow <- read.csv2("QS_flowtracker.csv", header = T, 
                   fill = TRUE, sep = ",", na.strings=c("","NA"))

for(i in 5:ncol(qsflow)) {
  qsflow[,i] <- as.numeric(as.character(qsflow[,i]))
}

qsflow$discharge_cfps <- qsflow$Discharge..m3.s. * 35.31

ggplot(qsflow, aes(Stage..ft.,discharge_cfps)) +
  geom_point()

###USGS stage-Q color by year

ggplot(qsusgs, aes(USGS_cfps,USGS_STAGE,color=YEAR)) +
  geom_point() +
  scale_y_continuous(limits = c(3.2,7.0)) +
  scale_x_continuous(limits = c(0,500))


###where does flowtracker data fit?
ggplot(NULL) +
  geom_point(data=qsusgs, aes(USGS_cfps,USGS_STAGE,color=YEAR)) +
  geom_point(data=qsflow, aes(discharge_cfps,Stage..ft.), color="red") +
  scale_y_continuous(limits = c(3.2,7.0)) +
  scale_x_continuous(limits = c(0,500))

###hard to tell which year it falls on best

###this separates USGS curves by year
###still need to do 2 things
#2007 data is causing an error, probably because lack of stage data. Can we skip it?
#need to add the flowtracker plot to each one of these

qsusgs2 <- subset(qsusgs, (!is.na(qsusgs$USGS_STAGE)) & (!is.na(qsusgs$USGS_cfps)))

plist = lapply(split(qsusgs2, qsusgs2$YEAR), function(d) {
  ggplot(d, aes(USGS_cfps, USGS_STAGE)) + 
    geom_point() +
    facet_wrap(~ YEAR) +
    geom_point(data=qsflow, aes(discharge_cfps,Stage..ft.), color="red") +
    scale_y_continuous(limits=c(0, max(qsusgs2$USGS_STAGE))) +
    scale_x_continuous(limits=c(0, max(qsusgs2$USGS_cfps))) +
    theme_bw() +
    theme(plot.margin=unit(rep(0.4,4),"lines"),
          axis.title=element_blank())
})


pdf("USGS stage-Q by year.pdf", 7, 5)
for (i in seq(1, length(plist), 6)) {
  grid.arrange(grobs=plist[i:(i+5)], 
               ncol=3)
}
dev.off()

#zoom in to see better

plist2 = lapply(split(qsusgs2, qsusgs2$YEAR), function(d) {
  ggplot(d, aes(USGS_cfps, USGS_STAGE)) + 
    geom_point() +
    facet_wrap(~ YEAR) +
    geom_point(data=qsflow, aes(discharge_cfps,Stage..ft.), color="red") +
    scale_y_continuous(limits=c(2,5)) +
    scale_x_continuous(limits=c(0,80)) +
    theme_bw() +
    theme(plot.margin=unit(rep(0.4,4),"lines"),
          axis.title=element_blank())
})


pdf("USGS stage-Q by year zoomed in.pdf", 7, 5)
for (i in seq(1, length(plist2), 6)) {
  grid.arrange(grobs=plist2[i:(i+5)], 
               ncol=3)
}
dev.off()

###can we get a curve that fits with historical, flowtracker, and higher curve??
###looks like our current flowtracker measurements fit well with 1993 data

usgs93 <- subset(qsusgs2, YEAR == 1993)

ggplot(NULL) +
  geom_point(data=usgs93, aes(USGS_STAGE,USGS_cfps)) +
  geom_point(data=qsflow, aes(Stage..ft.,discharge_cfps), color="red") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18))

###power relationship?
#https://thewetlandblog.wordpress.com/2013/06/17/fitting-rating-curves-with-r/

m <- nls(USGS_cfps ~ I(USGS_STAGE^power), data = usgs93, start=list(power = 1),trace = T)

z <- nls(USGS_cfps ~ a*USGS_STAGE^b, data = usgs93, start=list(a=1,b=1))

# Viewing the model summary and accessing estimated constants
summary(m)
summary(m)$coefficients

summary(z)
summary(z)$coefficients

###use Z, works well
ggplot(data=usgs93, aes(USGS_STAGE,USGS_cfps)) +
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', 
              method.args=list(start = c(a = 1, b = 1)),se=FALSE, color = "blue") +
  ylab("USGS Discharge (cfps)") +
  xlab("USGS Stage (ft)") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18))

###above 6.5 ft is it linear?
ggplot(data=usgs93, aes(USGS_STAGE,USGS_cfps)) +
  geom_point() +
  stat_smooth(method = 'nls', formula = 'y~a*x^b', 
              method.args=list(start = c(a = 1, b = 1)),se=FALSE, color = "blue") +
  geom_smooth (data =  subset(usgs93, USGS_STAGE > 6.5), method='lm',
              se=FALSE, size=2, color = "red") +
  ylab("USGS Discharge (cfps)") +
  xlab("USGS Stage (ft)") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title=element_text(size=18))

usgs93high <- subset(usgs93, USGS_STAGE > 6.5)

lmh=lm(usgs93high$USGS_cfps~usgs93high$USGS_STAGE)
summary(lmh)

##ok how do we apply it to data? 
##use broom package to export these regression results to use later
library(broom)

tidylmh <- tidy(lmh)
tidylmh

write.csv(tidylmh, "tidylmh.csv")

tidypower <- tidy(z)
tidypower

write.csv(tidylmh, "tidylmh.csv")
write.csv(tidypower, "tidypower.csv")
