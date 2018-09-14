##### Data Cleaning and Capture History Generation for Stephanie ########


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
setwd("F:/stephs files")

#Load the latest dataset downloaded from the website
#Note that the filename will change each time so make sure it is
#edited properly below. ~~~ Please see ingest in CandidCritters Github to learn how to extract data from eMammal 
#Or use the given data csv given with this template to use with this code. 
emammal<-fread(file="sianctapi-selected-observations-5b06d937ae87b.csv")
emammaldat<-as.data.table(emammal)
names(emammaldat)#these are the column names
unique(emammaldat$Subproject)#these are the counties in the dataset
head(emammaldat, n=30) #These are the first 30 lines of your data
tail(emammaldat, n=30) #These are the last 30 lines of your data

#Fix the timestamps
emammaldat$Begin <- gsub("T", " ", emammaldat$'Begin Time')  # This will make a new column called "Begin" with just date and time separated
emammaldat$End <- gsub("T", " ", emammaldat$'End Time')

emammaldat$Begin <- as.POSIXct(emammaldat$Begin, format = "%Y-%m-%d %H:%M:%S") #This formats the date and time to desired formats
emammaldat$End <- as.POSIXct(emammaldat$End, format = "%Y-%m-%d %H:%M:%S")
head(emammaldat, n=5) #Check dat for new columns formated correctly
emammaldat[ ,c("Begin Time","End Time") :=NULL]




#Determining Deployment Start and End times/dates
emammalsite <- emammaldat[,c('Project', 'Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat', 'Begin', 'End')]
#Start Times extraction
StartTime<-emammalsite[order(Begin)]   #Orders entries by chronological order
StartTime<- StartTime[order(StartTime$'Deployment Name')]  #groups by Deployment name
StartTime<- StartTime[!duplicated(StartTime$'Deployment Name'),] #Removes entries after the first entry for each deployment
StartTime$End <- NULL


#End Times Extraction 
EndTime<- emammalsite[order(-End)] #Orders entries  by reverse chronological order
EndTime<- EndTime[order(EndTime$'Deployment Name')]  #groups by deployment name
EndTime<- EndTime[!duplicated(EndTime$'Deployment Name'),]
EndTime$Begin <-NULL


#Generating a Site Information Dataframe
emammalsite <- merge(StartTime, EndTime, by =c('Project','Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat'))
setDT(emammalsite)[,paste0("Begin.Time",1:2):= tstrsplit(Begin," ")] #Splits the time stamp into Date and Time Columns
setDT(emammalsite)[,paste0("End.Time",1:2):= tstrsplit(End," ")]
setnames(emammalsite, old = c('Begin.Time1','Begin.Time2', 'End.Time1', 'End.Time2'), new = c('Deployment.Date','Deployment.Time', 'Retrieval.Date','Retrieval.Time')) #Renames the new columns 
emammalsite[ ,c("Begin","End") :=NULL]

#Calculate Number of Days Cameras were deployed for
emammalsite$TrapNights <- difftime((as.Date(emammalsite$Retrieval.Date)), (as.Date(emammalsite$Deployment.Date)), units="days")
head(emammalsite)


#create a .csv for data on camera site information only data
write.csv(emammalsite, file = "emammalSiteInfo.csv", row.names = FALSE) 


#Merge Site Infomation with your subsetted dataframe
emammaldata<- merge(emammaldat, emammalsite, by =c('Project','Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat'))




##Use the following table to examine your deployment days to ensure you do not have a 
#possible issue with your time stamps
DeploymentDays <- aggregate(emammaldata$TrapNights, list(emammaldata$'Deployment Name'), FUN=mean)
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
emammaldata$TI<-emammaldata$Begin %notin% times
p<-emammaldata %>%mutate(ifelse(any(TI=="FALSE") & emammaldata$Deployment.Date < max(emammaldata$Deployment.Date),
                         "remove", "keep"))
p2<-as.data.frame(p)
colnames(p2)[25] <- "RM"    #Number 26 corresponds to the number of columns in you dataframe. 
emammaldata<-filter(p2, RM=="keep")
emammaldata$TI<-NULL
emammaldata$RM<-NULL
head(emammaldata)

#remove any Sites with less than a week of function
emammaldata<-as.data.table(emammaldata)
emammaldata<- emammaldata[emammaldata$TrapNights > 7]

##Checking Species
#Double check the common names and species names to make sure the data does not have species not present in that area
unique(emammaldata$'Common Name')
unique(emammaldata$'Species Name')



#Remove animals / Camera specs / vehicles that you do not want in your data
remove_spp<-("No Animal|Unknown Animal|Vehicle|Calibration Photos|Bicycle|Animal Not on List|Time Lapse|Camera Misfire|Bicycle")
emammaldata<-emammaldata[grep(remove_spp, emammaldata$'Common Name', invert=T),] 
names(emammaldata)


#Save cleaned emammal data and site data
setnames(emammaldata, old = c('Deployment Name','Deploy ID', 'Actual Lon','Actual Lat', 'ID Type', 'Sequence ID', 'Species Name', 'Common Name'), new = c('Deployment.Name','Deploy.ID', 'Actual.Lon','Actual.Lat','ID.Type', 'Sequence.ID', 'Species.Name','Common.Name'))

emammalsiteinfo <- emammaldata[,c('Subproject','Deployment.Name','Actual.Lon', 'Actual.Lat','Deployment.Time','Deployment.Date','Retrieval.Date','Retrieval.Time','TrapNights')]
emammalsite<-emammalsiteinfo[!duplicated(emammalsiteinfo$'Deployment.Name'), ]  #Remove duplicated lines for a single site
write.csv(emammalsite, file="emammalsite.csv") #rewrite the file you had made earlier
write.csv(emammaldata, file="cleaned_emammal_data.csv")



###### Emammal Data Organized #####




#TEAM DATA CLEANING

#Upload the team data to be organized
team <- fread(file="TEAMdata_raw.csv")

#Extract only the columns that you need to match those from the emammal file and for capture histories.
team <- team[,c('Site Name', 'Sampling Unit Name','Latitude','Longitude', 'Photo Date', 'Photo Time', 'Number of Animals', 'Species Name', 'Camera Start Date and Time', 'Camera End Date and Time')]

#Lets rename the columns to match 
setnames(team, old = c('Site Name', 'Sampling Unit Name','Latitude','Longitude', 'Photo Date', 'Photo Time', 'Number of Animals','Camera Start Date and Time', 'Camera End Date and Time', 'Species Name'), new = c('Subproject','Deployment.Name', 'Actual.Lat','Actual.Lon','Begin.Date', 'Begin.Time', 'Count','Deployment', 'Retrieval', 'Species.Name'))
head(team, n= 5)
names(team)



#based on the data. team data needs trap nights and the deployment retrieval times to be seperated and the emammal data needs the begin and end times seperated
#Lets start with the team data

#Seperate Deployment and retrieval times
setDT(team)[,paste0("Deployment.Time",1:2):= tstrsplit(Deployment," ")] #Splits the time stamp into Date and Time Columns
setDT(team)[,paste0("Retrieval.Time",1:2):= tstrsplit(Retrieval," ")]
setnames(team, old = c('Deployment.Time1','Deployment.Time2', 'Retrieval.Time1', 'Retrieval.Time2'), new = c('Deployment.Date','Deployment.Time', 'Retrieval.Date','Retrieval.Time')) #Renames the new columns 
team[ ,c("Deployment","Retrieval") :=NULL]

#Now lets calculate number of trap nights for team data
teamsiteinfo <- team[,c('Subproject','Deployment.Name','Actual.Lon', 'Actual.Lat','Deployment.Time','Deployment.Date','Retrieval.Date','Retrieval.Time')]
teamsite<-teamsiteinfo[!duplicated(teamsiteinfo$'Deployment.Name'), ]  #Remove duplicated lines for a single site



write.csv(teamsite, file="teamsite.csv")

### Stop open the .csv file you just made and check the format on the dates so that they are the same.
# I recommend hitting format cells and go to custom the typing this format: mm/dd/yyyy

teamsite<- fread(file="teamsite.csv")


#Calculate the trap nights for the team data
teamsite$Deployment.Date <- as.Date(teamsite$Deployment.Date , "%m-%d-%Y")
teamsite$Retrieval.Date <- as.Date(teamsite$Retrieval.Date, "%m-%d-%Y")


teamsite$TrapNights <- difftime((teamsite$Retrieval.Date), (teamsite$Deployment.Date), units="days")

#check your site info to make sure everything is correct 
head(teamsite, n=5)



#Merge team data with teamsite 
team$Deployment.Date <- as.Date(team$Deployment.Date, "%m-%d-%Y")
team$Retrieval.Date <-as.Date(team$Retrieval.Date, '%m-%d-%Y')
teamdata<- merge(team, teamsite, by =c('Subproject','Deployment.Name','Actual.Lon', 'Actual.Lat','Deployment.Date', 'Deployment.Time','Retrieval.Date','Retrieval.Time'))

#Check to see no columns were duplicated and the merge was successful
head(teamdata, n=5)


#remove any sites with less than a week of function

teamdata<- teamdata[teamdata$TrapNights > 7]
unique(teamdata$TrapNights)  #This will give you all the trap night values in your dataframe. check to see if you have the correct range of trap nights for your project


#Save your team data and team site data

teamsiteinfo <- teamdata[,c('Subproject','Deployment.Name','Actual.Lon', 'Actual.Lat','Deployment.Time','Deployment.Date','Retrieval.Date','Retrieval.Time','TrapNights')]
teamsite<-teamsiteinfo[!duplicated(teamsiteinfo$'Deployment.Name'), ]  #Remove duplicated lines for a single site
write.csv(teamsite, file="teamsite.csv") #rewrite the file you had made earlier
write.csv(teamdata, file="cleaned_team_data.csv")


#Team data is now cleaned and now it's time to merge the emammal and team data.
#Note for capture histories we need a site file as well as the camera capture data. So we will be making two files
#Lets start with the site information
#Reopen the site information for both team and emammal
emammalsite<-fread(file="emammalsite.csv")
teamsite<-fread(file="teamsite.csv")
head(emammalsite, n=3)
head(teamsite, n=3)

#Make sure the names of the two files match and see if you need to remove columns
names(emammalsite)
names(teamsite)
teamsite[ ,c("V1") :=NULL]
emammalsite[ ,c("V1") :=NULL]

siteinfo<-rbind(teamsite,emammalsite) #your site info is now combined
write.csv(siteinfo, file="combinedsiteinfo.csv")


#Reopen the cleaned master data sets for both emammal and team
emammaldata<- fread(file="cleaned_emammal_data.csv")
teamdata<-fread(file="cleaned_team_data.csv")
head(emammaldata)
head(teamdata)
names(emammaldata)
names(teamdata)
#remove columns that do not match up
teamdata$Begin = paste(teamdata$Begin.Date, teamdata$Begin.Time, sep=" ")
teamdata[ ,c("V1","Begin.Date", 'Begin.Time') :=NULL]
emammaldata[ ,c("V1","Project",'Treatment','Deploy.ID','ID.Type','Sequence.ID','Common.Name','Age','Sex', 'Individual','Fuzzed') :=NULL]


#combine the master data dataframes into one
combinedmaster<-rbind(emammaldata, teamdata)

write.csv(combinedmaster, file="team_emammal_data.csv")

# Your data is now organized and you can make capture histories now.



#####
##### go into your combinedsiteinfo.csv and right-click the column "Deployment.Date" --> go to format cells --> custom --> type "mm/dd/yyyy --> hit okay --> save
##### go into your team_emammal_data.csv and right click the column "Begin" --> go to format cells --> custom --> type "mm/dd/yyyy hh:mm:ss" --> hit okay --> save
####


library(camtrapR)

Siteinfo<- read.csv(file= "combinedsiteinfo.csv", header=T)
data<-read.csv(file="team_emammal_data.csv", header=T)


Sitedata <- cameraOperation(CTtable = Siteinfo,
                            stationCol = "Deployment.Name",
                            setupCol = "Deployment.Date",
                            retrievalCol = "Retrieval.Date",
                            hasProblems = FALSE,
                            dateFormat = "%m/%d/%Y")

detectionHistory(recordTable = data,
                 species = "Myoprocta acouchy",
                 camOp = Sitedata,
                 stationCol = "Deployment.Name",
                 speciesCol = "Species.Name",
                 recordDateTimeCol = "Begin",
                 recordDateTimeFormat = "%m/%d/%Y %H:%M:%S",
                 occasionLength = 1,
                 day1 = "survey",
                 datesAsOccasionNames = T,
                 includeEffort = F,
                 writecsv = TRUE,
                 outDir = "F:/stephs files/capturehistory")

#to change the species type the desired species name on the species line in replace of Myoprocta acouchy
#if you want the capture history condensed by a number other than a day such as condensed by week replace the number in occasionLength with the desired number
#IF you are interested in just the site not and want all of the capture histories to start on day1 replace "survey" with "station
#the file will need to be saved somewhere so fix the outDir with the desired location you would like to save at. 

