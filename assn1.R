
# import libraries
library(openair)
library(ggplot2)
library(lubridate)

dir <- "D:/school/geog402"
setwd(dir)


# writes years worth of data to file with pm25 vales and fixed dates
getTable <- function(dataset){

  # collects data from envista web webpage
  
  # get station info from file
  info <- read.csv(paste("D:/school/geog402/",dataset,".csv",sep=""), header = TRUE)
  info <- as.character(info[1,1])
  
  # get the data from file
  file <- read.csv(paste("D:/school/geog402/",dataset,".csv",sep=""), header = TRUE)
  file <- file[-1]
  file <- file[-1,]
  row.names(file) <- NULL
  
  # fix the dates to posix format
  datefix <- data.frame(as.POSIXlt(file$Date.Time, format ="%m/%d/%Y %H:%M"))
  colnames(datefix) <- "date"
  file$date <- as.POSIXlt(file$Date.Time, format ="%m/%d/%Y %I:%M %p")
  file$date[is.na(file$date)] <- datefix$date[is.na(file$date)]
  datefix <- NULL
  file <- file[!is.na(file$date),]
  
  # adjust numeric values from factors to numers
  file$pm25 <- as.numeric(as.character(file$PM25))
  
  # write the file
  mydata <- file
  return(mydata)
  
}

# get data for abbotsford for three years
ab2003 <- getTable("abb2003")
ab2004 <- getTable("abb2004")
ab2005 <- getTable("abb2005")

# narrow down size of data frames to only contain pm2.5
ab2003 <- data.frame(ab2003$date,ab2003$pm25)
colnames(ab2003) <- c("date","pm25")
ab2004 <- data.frame(ab2004$date,ab2004$pm25)
colnames(ab2004) <- c("date","pm25")
ab2005 <- data.frame(ab2005$date,ab2005$pm25)
colnames(ab2005) <- c("date","pm25")

# create data frame of all three years combined
alldata <- rbind(ab2003,ab2004,ab2005)

# run to save data
###save(alldata, file="alldata.RData")

# create colums for year and day of year for comparison plot
alldata$year  <- year(alldata$date)
alldata$day   <- yday(alldata$date)
alldata$wkday  <- wday(alldata$date,label=T)
alldata$hour  <- hour(alldata$date)
alldata$month <- month(alldata$date)
alldata$wday  <- wday(alldata$date)

# write mydata as primary data for project
# and add the month as category

# get out seasons for 2004
mydata <- alldata[alldata$year == 2004,]
# seasons
winter <- mydata[(mydata$month == 12) | (mydata$month <= 2),]
spring <- mydata[(mydata$month >= 3) | (mydata$month <= 5),]
summer <- mydata[(mydata$month == 6) | (mydata$month <= 8),]
fall   <- mydata[(mydata$month == 9) | (mydata$month <= 11),]

# quick summary of abbotsford 2004 data
# summary(mydata)

seasons <- list(winter, spring, summer, fall)
names <- list("Winter","Spring","Summer","Fall")

# for (i in 1:length(seasons)){
#   ggplot(data = seasons[[i]], aes(factor(hour),pm25)) +
#     geom_boxplot(fill = "grey80", colour = "#3366FF",outlier.colour = "navy") +
#     xlab("Hour") +
#     ylab("PM2.5") + 
#     theme(plot.title = element_text(color="navyblue", size=14, face="bold.italic")) +
#     theme(plot.title=element_text(size=15, vjust=1.5)) +
#     ggtitle(paste(names[i]," PM2.5 in Abbotsford 2004",sep=""))
#   ggsave(paste0(names[i],".png"))
# }

# plot the same thing in a different way
# make sure date column is the right label ie) Date

for (i in 1:length(mydata$date)){
  if((mydata$month[i] == 12) | (mydata$month[i] <= 2)){
    mydata$season[i] <- "Winter"
  }else{
    if((mydata$month[i] >= 3) & (mydata$month[i] <= 5)){
      mydata$season[i] <- "Spring"
    }else{
      if((mydata$month[i] >= 6) & (mydata$month[i] <= 8)){
        mydata$season[i] <-"Summer"
      }else{
        if((mydata$month[i] >= 9) & (mydata$month[i] <= 11)){
          mydata$season[i] <- "Autumn"
        }
      }
    }
  }
}

weekend <- c()
for (i in 1:length(mydata$date)){
  if((mydata$wday[i] == 1) | (mydata$wday[i] == 7)){
    weekend[i] <- "weekend"
  }else{
    weekend[i] <- "weekday"
  }
}

mydata$Days <- weekend


# output graphs with season in order
mydata$season <- factor(mydata$season,
                        levels = c("Spring","Summer","Autumn","Winter"))


# plot the Diurnal values by season
ggplot(data = mydata, aes(factor(hour),pm25)) +
  geom_boxplot(fill = "red", colour = "black",outlier.colour = "firebrick4",alpha = 0.5) +
  facet_wrap(~ season, ncol=2) +
  xlab("Hour") +
  ylab(expression(paste(PM[2.5]," (µg /",m^3,")", sep=""))) + 
  theme(plot.title = element_text(color="firebrick4", size=14, face="bold.italic")) +
  theme(plot.title=element_text(size=15, vjust=1.5)) +
  theme(strip.background = element_rect(fill = "cornsilk", color = "grey20")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(colour = "grey80")) +
  ggtitle("Diurnal Seasonal PM2.5 in Abbotsford 2004")

mydata$wkday <- factor(mydata$wkday,
                        levels = c("Mon","Tues","Wed","Thurs","Fri",
                                   "Sat","Sun"))

# plot the weekly seasonal values 
ggplot(data = mydata, aes(factor(wkday), pm25, fill=Days)) +
  geom_boxplot( colour = "black",outlier.colour = "firebrick4",alpha = 0.5) +
  scale_fill_manual(values = c("red","black")) +
  facet_wrap(~ season, ncol=4) +
  xlab("Day of Week") +
  ylab(expression(paste(PM[2.5]," (µg /",m^3,")", sep=""))) + 
  theme(plot.title = element_text(color="firebrick4", size=14, face="bold.italic")) +
  theme(plot.title=element_text(size=15, vjust=1.5)) +
  theme(strip.background = element_rect(fill = "cornsilk", color = "grey20")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(colour = "grey80")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Weekly Seasonal PM2.5 in Abbotsford 2004")



# addtional plots that Ian provided from open air
timePlot(selectByDate(mydata, year = 2004),pollutant = "pm25")
calendarPlot(mydata, pollutant = "pm25", year = 2004)
