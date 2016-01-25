# Written by Tim Atkinson in 2016

# Purpose: convert files into 24 rolling averages for PM25
# input:   input a time series data frame
# output:  data frame with rolling average column for specified variable

# eg. produce 24 PM2.5 rolling averages for air quality data

# use library zoo
library(zoo)

# set working directory
dir <- "D:/school/geog402"
setwd(dir)

#

# identify number of consecutive NA values. 
# will be used to fill in some missing values

consecna <- function(x, n=5) {
  # function to identify elements with n or more consecutive NA values
  # n=5 is the default value, but can be changed when included in the funciton call
  y <- rle(is.na(x))
  y$values <- y$lengths > (n - 0.5) & y$values
  inverse.rle(y)
}



# select file to read ( import data file that is already saved from
#                       previous script)
file <- "alldata.Rdata"
load(file)

# get values in numeric format
alldata$PM25 <- as.numeric(as.character(alldata$pm25))
file <- alldata

# removes NA at the beginning or end of file
if(is.na(file$PM25[length(file$PM25)])){
  file$PM25[length(file$PM25)] <- file$PM25[length(file$PM25)-1]
}

# fill in some na values with averages 
row.names(file) <- NULL
file$num <- seq(1:length(file[,1])) 

file1 <- file[!consecna(file$PM25),]
number <- file1[is.na(file1$PM25),]
file1$PM25 <- na.approx(file1$PM25)
file$PM25[is.element(file$num,number$num)] <- file1$PM25[is.element(file1$num,number$num)]
file1  <- NULL
number <- NULL

# apply rolling average for 24 hours
pm <- rollapply(file$PM25,24,mean,fill=NA,align="right")

# set as a new column in data frame
file$rPM25 <- pm

# remove unwanted columns
file <- file[-c(3,4)]

# save the file
save(file, file="rmAlldata.RData")








