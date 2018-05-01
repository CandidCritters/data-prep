########## eMammal Data Cleaning ###########
## NOTE: This code will clean the data so only mammals will be left in the code##
## This code also cleans for species across continents: North America, South America,
## Africa, Asia, and Europe

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
setwd("D:/Grad School/24hrpaper")

#Load the latest dataset downloaded from the website
#Note that the filename will change each time so make sure it is
#edited properly below. ~~~ Please see ingest in CandidCritters Github to learn how to extract data from eMammal 
#Or use the given data csv given with this template to use with this code. 
dat <-fread(file="euro_asian_african_eMammal_data.csv")
dat2<- fread(file="NorthAmerica_All_eMammal.csv")
dat3<- fread(file="SouthAmerica_eMammal.csv")

data<-rbind(dat, dat2, dat3)




data<-as.data.table(data)
names(data)#these are the column names
unique(data$Subproject)#these are the counties in the dataset
head(data, n=30) #These are the first 30 lines of your data
tail(data, n=30) #These are the last 30 lines of your data

#Fix the timestamps
data$Begin <- gsub("T", " ", data$'Begin Time')  # This will make a new column called "Begin" with just date and time separated
data$End <- gsub("T", " ", data$'End Time')
ols.sys.timezone <- Sys.timezone() #Setting timezone
Sys.setenv(TZ = 'EST') #Setting timezone --- "EST" is eastern timezone
data$Begin <- as.POSIXct(data$Begin, format = "%Y-%m-%d %H:%M:%S") #This formats the date and time to desired formats
data$End <- as.POSIXct(data$End, format = "%Y-%m-%d %H:%M:%S")
head(data, n=5) #Check dat for new columns formated correctly
data[ ,c("Begin Time","End Time") :=NULL]

#Determining Deployment Start and End times/dates
SiteInfo <- data[,c('Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat', 'Begin', 'End')]
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


#Merge Site Infomation with your subsetted dataframe
data1<- merge(data, SiteInfo, by =c('Subproject','Deployment Name','Treatment', 'Deploy ID', 'Actual Lon', 'Actual Lat'))




##Use the following table to examine your deployment days to ensure you do not have a 
#possible issue with your time stamps
DeploymentDays <- aggregate(data1$TrapNights, list(data1$'Deployment Name'), FUN=mean)
names(DeploymentDays)<-c("Deployment", "Camera Nights")
DeploymentDays


#Note: If you have wonky trap nights such as huge number of days when it should only less than 100
#Double check to see if you know the actual time frame and manually fix the time stamps in the data log
# Or remove that deployment completely if you cannot accurately fix the timeframe






##Checking Species
#Double check the common names and species names to make sure the data does not have species not present in that area
unique(data1$'Common Name')
unique(data1$'Species Name')


#Fix any animal names that are incorrectly spelled
data1$'Common Name'[data1$'Common Name' == 'Elk aka Red Deer'] <- 'Elk_Red Deer'

#Remove animals that you do not want in your data

#Remove camera issues, humans, non-animals
remove_spp<-("Unknown|Human|Human. non-staff|Project Staff|Person|False trigger|Camera Trappe|Camera Trapper|Human, non-staff|Human non-staff|Camera Trapper|Camera  Trapper|No Animal|Unidentifiable|Re-inspect|Animal Not On List|Unknown Animal|Vehicle|Calibration|Calibration Photos|Bicycle|Animal Not on List|Time Lapse|Camera Misfire|Blank|Setup_Pickup")
data2<-data1[grep(remove_spp, data1$'Common Name', invert=T),] 

#Remove all birds and reptiles
remove_spp<- ("Wood Pigeon|Great Curassow|Barred Owl|Common Raven|Wild turkey|Crestless Fireback|Grey-necked Wood Rail|Northern Bobwhite|Gambel's Quail|Western Scrub-jay|Turkey Vulture|Northern Red-shafted Flicker|Montezuma Quail|Black Vulture|Dark-eyed Junco|Common Grackle|Crow or Raven|Steller's Jay|Bird Species|Northern Flicker|Black-capped Chickdee|American Robin|Northern cardinal|American Woodcock|Downy woodpecker|Rufous Motmot|Blue Jay|American Crow|Cattle Egret|Ruffed Grouse|Great Spotted Woodpecker|Blackbird|Baudo Guan|Scaly-breasted Partridge|Sickle-winged Guan|Andean Guan|European Robin|Helmeted Guineafowl|Vulturine Guineafowl|Crested Francolin|Mistle Thrush|Harlequin Quail|Great Tinamou|Common Woodpigeon|Hawfinch|Song Thrush|Eurasian Blackbird|Nocturnal Curassow|Ruddy Quail-dove|Little Tinamou|Great Black Hawk|Marbled Wood-quail|Setup_Pickup|Salvin's Curassow|Grey Junglefowl|Raptor species|Other Bird Species|Tragopan pheasant|Blue Magpie|Owl species|olden Pheasant|Crested Guineafowl|Coral-billed Ground Cuckoo|Indian Peafowl|Green Peafowl|African Green Ibis|Moorland Francolin|Jackson's Francolin|Spix's Guan|Wild Turkey|Cinereous Tinamou|Red Junglefowl|Siamese Fireback|Other Bird species|Sichuan Partridge|Lady Amherst's Pheasant|Temminck's Tragopan|Eurasian Jay|Reptile species|Large-billed Crow")
data2<-data2[grep(remove_spp, data2$'Common Name', invert=T),]

#Remove domestics
remove_spp<-("Domestic Cattle|Domestic Dog|Domestic Goat|Domestic Horse|Domestic Chicken|Domestic Cat|Domestic Pig|Domestic Cow|Domestic Sheep|Domestic cat|Domestic Donkey|Dog")
data2<-data2[grep(remove_spp, data2$'Common Name', invert=T),]

#Remaining mammal list across all sites
unique(data2$"Common Name")





###### Data Organized #####

#Review your new organized data
names(data2)#these are the column names
head(data2, n=30) #These are the first 30 lines of your data
tail(data2, n=30) #These are the last 30 lines of your data

#Save your saved data in a .csv file to use with other code templates
#To name the file before saving just change the path to the path you want to save the data at.
#Replace where I wrote eMammaldata with your desired name. Make sure .csv follows it. 
write.csv(data2, file="D:/Grad School/24hrpaper/clean_emammal_data.csv", row.names = FALSE)

#create a .csv for data on camera site information only data to use for occupancy or to get deployment info
write.csv(SiteInfo, file = "D:/Grad School/24hrpaper/clean_emammal_Sitedata.csv", row.names = FALSE) 



