#This is a bit of code to grab location and time from jpg files and output an excel table
#the goal is to be able to capture activities and things from phones during hydrophone deployment

#bring in libraries
library(exifr)
library(sf)
library(geosphere)
library(stringr)
library(openxlsx)

#set working directory and read in image files
setwd("/Users/christopherwilcox/Library/CloudStorage/GoogleDrive-chris@wilcoanalytics.org/My Drive/Hydrophones Chile/Photos")
#you need to export these files in "original format" so that they have the full resolutions gps data.  If you convert to jpeg 
#my iphone at least strips most of the gps accuracy.

#use exifr to read in files.  Note we can just read in the fields we want, as there are lots of extras
files <- list.files(path="/Users/christopherwilcox/Library/CloudStorage/GoogleDrive-chris@wilcoanalytics.org/My Drive/Hydrophones Chile/Photos", recursive=TRUE, pattern="*.HEIC", full.names=TRUE) 
exifinfo <- read_exif(files)

#make a smaller dataframe for processing
Data <- exifinfo[,c("GPSPosition","DateTimeOriginal")]

#split out text from gps, and make posix time
Data$Posix <- strptime(Data$DateTimeOriginal,"%Y:%m:%d %H:%M:%S")

#get the time difference for each point pair - note we are storing it at the first position
Data$DtSec <- c(as.numeric(diff(Data$Posix,lag=1)),NA)

#get the distance for each point pair, first we need to locate the white space in each entry so
#we can split the lat and lon
IndexSpace <- str_locate(Data$GPSPosition," ")[,1]
Data$Lat <- as.numeric(stringr::str_sub(Data$GPSPosition, 1, IndexSpace-1))
Data$Lon <- as.numeric(stringr::str_sub(Data$GPSPosition, IndexSpace+1, str_length(Data$GPSPosition)))

#we need to convert the data to points, and to do this it needs to be in list format.  The 
#easiest way is to cycle over the data using a for loop, put it in a list, then use st_point,
#and then calculate the distance.
Data$Dist <- NA
Data$Bearing <- NA
for(i in 1:(dim(Data)[1]-1)){
  p1 <- st_point(c(Data$Lon[i],Data$Lat[i]))
  p2 <- st_point(c(Data$Lon[i+1],Data$Lat[i+1]))
  #get distance, I am pretty sure the original units are km, but I havent been able to find it
  Data$Dist[i] <- st_distance(p1,p2)
  #and the bearing
  Data$Bearing[i] <- bearing(Data[i,c("Lon","Lat")],Data[i+1,c("Lon","Lat")], a=6378137, f=1/298.257223563)
}

#now get the speed
Data$Speed <- Data$Dist/Data$DtSec

#now write this out as a .csv file
write.xlsx(Data,"Trajectory.xlsx",colNames = TRUE)

