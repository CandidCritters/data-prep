########## eMammal Data Cleaning ###########

#############################################################

#### Load Necessary Packages #######
#The below packages are the ones used throughout this code template. Please install
#and load the below packages before proceeding 

#The following code checks if the required packages exist in your library
#If you do not have that package, it will go ahead and install them.
list.of.packages<-c("data.table","dplyr")
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))installed.packages(new.packages)
#Now load the needed packages
library(data.table)   
library(dplyr)

#######Loading and organizing data ##########

#First set the working directory where the data is stored and where
#your output will be saved
setwd("E:/Grad School/Candid Critters Project/Descriptive Stats Code")

#Load the latest dataset downloaded from the website
#Note that the filename will change each time so make sure it is
#edited properly below. ~~~ Please see ingest in CandidCritters Github to learn how to extract data from eMammal 
#Or use the given data csv given with this template to use with this code. 
dat <-fread(file="sianctapi-selected-observations-5a4bf68d007dc.csv")
dat<-as.data.table(dat)
names(dat)#these are the column names
unique(dat$Subproject)#these are the counties in the dataset
head(dat, n=30) #These are the first 30 lines of your data
tail(dat, n=30) #These are the last 30 lines of your data

#Fix the timestamps
dat$Begin <- gsub("T", " ", dat$'Begin Time')  # This will make a new column called "Begin" with just date and time separated
dat$End <- gsub("T", " ", dat$'End Time')
ols.sys.timezone <- Sys.timezone() #Setting timezone
Sys.setenv(TZ = 'EST') #Setting timezone --- "EST" is eastern timezone
dat$Begin <- as.POSIXct(dat$Begin, format = "%Y-%m-%d %H:%M:%S") #This formats the date and time to desired formats
dat$End <- as.POSIXct(dat$End, format = "%Y-%m-%d %H:%M:%S")
head(dat, n=5) #Check dat for new columns formated correctly
dat[ ,c("Begin Time","End Time") :=NULL]


#Fix misspelled county names
data$'Subproject'[data$'Subproject' == 'Allegheny County'] <- 'Alleghany County'

#Determining Deployment Start and End times/dates
SiteInfo <- dat[,c('Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat', 'Begin', 'End')]
#Start Times extraction
StartTime<-SiteInfo[order(Begin)]   #Orders entries by chronological order
StartTime<- StartTime[order(StartTime$'Deployment Name')]  #groups by Deployment name
StartTime<- StartTime[!duplicated(StartTime$'Deployment Name'),] #Removes entries after the first entry for each deployment
StartTime$End <- NULL


#End Times Extraction 
EndTime<- SiteInfo[order(-End)] #Orders entries  by reverse chronological order
EndTime<- EndTime[order(EndTime$'Deployment Name')]  #groups by deployment name
EndTime<- EndTime[!duplicated(EndTime$'Deployment Name'),]
EndTime$Begin <-NULL


#Generating a Site Information Dataframe
SiteInfo <- merge(StartTime, EndTime, by =c('Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat'))
setDT(SiteInfo)[,paste0("Begin.Time",1:2):= tstrsplit(Begin," ")] #Splits the time stamp into Date and Time Columns
setDT(SiteInfo)[,paste0("End.Time",1:2):= tstrsplit(End," ")]
setnames(SiteInfo, old = c('Begin.Time1','Begin.Time2', 'End.Time1', 'End.Time2'), new = c('Deployment.Date','Deployment.Time', 'Retrieval.Date','Retrieval.Time')) #Renames the new columns 
SiteInfo[ ,c("Begin","End") :=NULL]

#Calculate Number of Days Cameras were deployed for
SiteInfo$TrapNights <- difftime((as.Date(SiteInfo$Retrieval.Date)), (as.Date(SiteInfo$Deployment.Date)), units="days")
head(SiteInfo)


#create a .csv for data on camera site information only data
write.csv(SiteInfo, file = "E:/Grad School/Candid Critters Project/Descriptive Stats Code/SiteInfo.csv", row.names = FALSE) 


#Merge Site Infomation with your subsetted dataframe
data<- merge(dat, SiteInfo, by =c('Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat'))




##Use the following table to examine your deployment days to ensure you do not have a 
#possible issue with your time stamps
DeploymentDays <- aggregate(data$TrapNights, list(data$'Deployment Name'), FUN=mean)
names(DeploymentDays)<-c("Deployment", "Camera Nights")
DeploymentDays


#Note: If you have wonky trap nights such as huge number of days when it should only less than 100
#Double check to see if you know the actual time frame and manually fix the time stamps in the data log
# Or remove that deployment completely if you cannot accurately fix the timeframe


###### Removing Incorrect Timestamps ##########

#Clean out rows where camera time not set properly
times <- c("2012-01-01 00:00:00", "2013-01-01 00:00:00", 
           "2014-01-01 00:00:00", "2015-01-01 00:00:00", 
           "2016-01-01 00:00:00", "2017-01-01 00:00:00", 
           "2018-01-01 00:00:00", "2019-01-01 00:00:00", 
           "2020-01-01 00:00:00", "2021-01-01 00:00:00")


#If a deployment has one of the times above, remove any data that has those dates in its deployment
`%notin%` <- function(x,y) !(x %in% y) 
data$TI<-data$Begin %notin% times
p<-data %>%mutate(ifelse(any(TI=="FALSE") & data$Deployment.Date < max(data$Deployment.Date),
                         "remove", "keep"))
p2<-as.data.frame(p)
colnames(p2)[25] <- "RM"    #Number 26 corresponds to the number of columns in you dataframe. 
data<-filter(p2, RM=="keep")
data$TI<-NULL
data$RM<-NULL
head(data)

#Another option is to subset the data based on a min and max allowance of deployment nights
#Remove any sites that have less than 7 days of trap nights or above 250 days
#This is a simple check to remove any sites that did not function correctly or
#have an issue that needs to be reviewed
data<- subset(data, TrapNights>= 7)    
data<- subset(data, !TrapNights>=250)


##Checking Species
#Double check the common names and species names to make sure the data does not have species not present in that area
unique(data$'Common Name')
unique(data$'Species Name')


#Fix any animal names that are incorrectly spelled
data$'Common Name'[data$'Common Name' == 'Elk aka Red Deer'] <- 'Elk_Red Deer'

#Remove animals that you do not want in your data
remove_spp<-("No Animal|Unknown Animal|Vehicle|Calibration Photos|Bicycle|Animal Not on List|Time Lapse")
data<-data[grep(remove_spp, data$'Common Name', invert=T),] 

###### Data Organized #####

#Review your new organized data
names(data)#these are the column names
head(data, n=30) #These are the first 30 lines of your data
tail(data, n=30) #These are the last 30 lines of your data

#Save your saved data in a .csv file to use with other code templates
#To name the file before saving just change the path to the path you want to save the data at.
#Replace where I wrote eMammaldata with your desired name. Make sure .csv follows it. 
write.csv(data, file = "E:/Grad School/Candid Critters Project/Descriptive Stats Code/eMammaldata.csv", row.names = FALSE) 