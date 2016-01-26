library(openair)
library(ggplot2)
library(lubridate)

# set working directory
dir <- "D:/school/geog402"
setwd(dir)

# load data
file <- "pmdata.Rdata"
load(file)

file <- "abMet.Rdata"
load(file)

# merge data sets
df <- merge(alld, pmdata, by='date')
df$wd <- df$wd*10

#------------------------------------------------

df$hour <- as.numeric(hour(df$date))

getSeason <- function(xx){
  if((xx == 12) | (xx <= 2)){
    seas <- "Winter"
  }else{
    if((xx >= 3) & (xx <= 5)){
      seas <- "Spring"
    }else{
      if((xx >= 6) & (xx <= 8)){
        seas <-"Summer"
      }else{
        if((xx >= 9) & (xx <= 11)){
          seas <- "Autumn"
        }
      }
    }
  }
}

getLabel <- function(xx){
  # subset the hours to get smaller amount of wind roses
  if(xx %in% c(0,1,2,3,4,5)){
    subs <- "00-05"
  }else{
    if(xx %in% c(6,7,8,9,10,11)){
      subs <- "06-11"
    }else{
      if(xx %in% c(12,13,14,15,16,17)){
        subs <- "12-17"
      }else{
        if(xx %in% c(18,19,20,21,22,23)){
          subs <- "18-23"
        }
      }
    }
  }
  return(subs)
}

seasons  <- lapply(df$nmonth,getSeason)
df$seasons <- as.character(seasons)
subHours <- lapply(df$hour,getLabel) 
df$hours <- subHours 
df$hours <- as.character(df$hours) 
#------------------------------------------------

# make plots
#spring <- df[df$seasons == "Spring",] 
pollutionRose(df, pollutant = "pm25",statistic="prop.mean",type=c("hours","seasons"))

# summer <- df[df$seasons == "Summer",] 
# pollutionRose(summer, pollutant = "pm25",type="hours") 
# 
# autumn <- df[df$seasons == "Autumn",] 
# pollutionRose(autumn, pollutant = "pm25",type="hours") 
# 
# winter <- df[df$seasons == "Winter",] 
# pollutionRose(winter, pollutant = "pm25",type="hours") 

# pollutionRose(mydata, type = c("season"),
#                pollutant = "pm25", col = "Set2", mean.col = "black") 
# 
# polarPlot(mydata, type = c("season"),
#               pollutant = "pm25", mean.col = "black") 



#=================================================
# subset data to get only weather variables
df <- df[as.character(df$weather) != "",] 

# subset so only weather shows up when it has occurred more than
# 100 times throughout all the hours of a year to get rid of
# one hit wonders
occ <- ddply(df, .(weather), summarise, length(weather))
colnames(occ) <- c("weather","freq")

# set the frequency to 100
occ <- occ[occ$freq > 100,]

# subset the data
df <- df[df$weather %in% occ$weather,]

# set order of weather
df$weather <- factor(df$weather,
                        levels = c("Clear", "Mainly Clear", "Mostly Cloudy", "Cloudy",
                                   "Rain", "Rain Showers", "Rain,Fog","Rain Showers,Fog",
                                   "Fog"))

# weather distribution
ggplot(data = df, aes(factor(weather),pm25)) +
  geom_boxplot(fill = "red", colour = "black",outlier.colour = "firebrick4",alpha = 0.5) +
  facet_wrap(~ seasons,ncol=4,scales="free") +
  xlab("Hour") +
  ylab("PM2.5") + 
  theme(plot.title = element_text(color="firebrick4", size=14, face="bold.italic")) +
  theme(plot.title=element_text(size=15, vjust=1.5)) +
  theme(strip.background = element_rect(fill = "cornsilk", color = "grey20")) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.major = element_line(colour = "grey80")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Weather dependent PM2.5 in Abbotsford 2004")


