library(lubridate)
library(ggplot2)
library(openair)

dir <- "D:/school/geog402"
setwd(dir)

# select file to read
file <- "rmAlldata.Rdata"
load(file)

pmdata <- file
file <- NULL

colnames(pmdata) <- c("date","pm25","rpm25")

pmdata$year  <- year(pmdata$date)
pmdata$day   <- yday(pmdata$date)
pmdata$month <- month(pmdata$date,label=T)
pmdata$nmonth <- month(pmdata$date)
pmdata$wday   <- wday(pmdata$date)

allpm <- pmdata
pmdata <- pmdata[pmdata$year == 2004,]


for (i in 1:length(pmdata$date)){
  if((pmdata$nmonth[i] >= 1) & (pmdata$nmonth[i] <= 3)){
    pmdata$season[i] <- "Jan-Mar"
  }else{
    if((pmdata$nmonth[i] >= 4) & (pmdata$nmonth[i] <= 6)){
      pmdata$season[i] <- "Apr-Jun"
    }else{
      if((pmdata$nmonth[i] >= 7) & (pmdata$nmonth[i] <= 9)){
        pmdata$season[i] <-"Jul-Sep"
      }else{
        if((pmdata$nmonth[i] >= 10) & (pmdata$nmonth[i] <= 12))
          pmdata$season[i] <- "Oct-Dec"
      }
    }
  }
}


pmdata$season <- factor(pmdata$season,
                        levels = c("Jan-Mar","Apr-Jun","Jul-Sep","Oct-Dec"))

save(pmdata, file="pmdata.RData")


ggplot(data = pmdata, aes(x=date, y=rpm25)) +
  geom_line(size=1, color = "red") +
  facet_wrap(~ season, scales = "free_x",ncol=1) +
  ylab(expression(paste(PM[2.5]," (µg /",m^3,")", sep=""))) + 
  xlab("Date") +
  theme(plot.title = element_text(color="firebrick4", size=14, face="bold.italic")) +
  theme(plot.title=element_text(size=15, vjust=1.5)) +
  theme(strip.background = element_rect(fill = "cornsilk", color = "grey20")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(colour = "grey80")) +
  ggtitle("24 hour Average PM2.5 in Abbotsford")
  
# try sexy plot

tplot <- timeVariation(pmdata, pollutant = c("pm25"), ylab = "PM2.5",
              difference = F, normalize = F)
allplot <- timeVariation(pmdata, pollutant = c("pm25"), ylab = "PM2.5",
                       difference = F, normalize = F,statistic = "median",
                       ci=T,conf.int = c(0.75, 0.95))
plot(allplot)
plot(tplot,subset="day.hour")
plot(tplot,subset="month")
plot(tplot,subset="day")

timePlot(pmdata, pollutant = "pm25", avg.time = "day")



