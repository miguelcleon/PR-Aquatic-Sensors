library(zoo)
library(xts)
library(ggplot2)

#pull data
data.df <- read.table("Sonadora_WQual.DAT", header = F, fill = TRUE,
                      sep = ",")
#remove rows at top
data.df = data.df[-1,]
data.df = data.df[-1,]
data.df = data.df[-1,]
data.df = data.df[-1,]