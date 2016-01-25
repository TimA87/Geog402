# convert files into 24 rolling averages for PM25

#use library zoo
library(zoo)

# write function to fill in some na values
consecna <- function(x, n=5) {
  # function to identify elements with n or more consecutive NA values
  # n=2 is the default value, but can be changed when included in the funciton call
  y <- rle(is.na(x))
  y$values <- y$lengths > (n - 0.5) & y$values
  inverse.rle(y)
}

# set working directory
dir <- "D:/school/geog402"
setwd(dir)

# select file to read
file <- "alldata.Rdata"

# read in file name from other script
load(file)

alldata$PM25 <- as.numeric(as.character(alldata$pm25))
file <- alldata

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

insert <- rep(NA,23)
PM25<- c(insert,rollapply(file$PM25,24,mean,fill=NA,align="right"))

pm <- rollapply(file$PM25,24,mean,fill=NA,align="right")

file$rPM25 <- pm

file <- file[-c(3,4)]

# save the file
save(file, file="rmAlldata.RData")








