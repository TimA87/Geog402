library(openair)
library(ggplot2)
library(lubridate)
library(plyr)

dir <- "D:/school/geog402"
setwd(dir)

monthList <- list("Jan-ab","Feb-ab","Mar-ab","Apr-ab","May-ab","Jun-ab",
                  "Jul-ab","Aug-ab","Sep-ab","Oct-ab","Nov-ab","Dec-ab")

nameList <- list("Janab","Febab","Marab","Aprab","Mayab","Junab",
                  "Julab","Augab","Sepab","Octab","Novab","Decab")


for(i in 1:length(monthList)){
  # output all months 
  assign(nameList[[i]],read.csv(paste("D:/school/geog402/",
                                         monthList[i],".csv",sep=""),
                                   skip=16,header=T))
}

alld <- rbind(Janab,Febab,Marab,Aprab,Mayab,Junab, 
              Julab,Augab,Sepab,Octab,Novab,Decab)

Janab<-Febab<-Marab<-Aprab<-Mayab<-Junab<-Julab<-Augab<-Sepab<-Octab<-Novab<-Decab <- NULL

alld$Date.Time <- as.POSIXlt(alld$Date.Time,format="%Y-%m-%d %H:%M")

alld <- data.frame(alld$Date.Time,alld[,7],alld[,9],alld[,11],alld[,13],
                   alld[,15],alld[,17],alld[,19],alld[,25])
colnames(alld) <- c("date","temp","dew","humidity","wd","ws",
                   "visibility","pressure","weather")

save(alld, file="abMet.RData")




