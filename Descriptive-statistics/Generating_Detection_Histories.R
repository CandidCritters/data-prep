################# Generating Capture (or Detection) Histories ##################


#First set the working directory where the data is stored and where
#your output will be saved
setwd("F:/Grad School/Candid Critters Project/Decoy Project/Code")

#Load necessary packages
library(data.table)
library(camtrapR)



#Read in both the site .csv and cleaned data file generated in Data_Cleaning_Code_template.R
#Be sure that data is a data.frame not data.table dataframe --> use read.csv to read in data only

AnimalData<- read.csv("eMammaldata.csv")

StationInfo<- read.csv("SiteInfo.csv")



#Check to see if data was read in correctly
head(AnimalData)
head(StationInfo)

unique(StationInfo$Deployment.Name)    #Check count to be sure that # of deployments is the same
unique(AnimalData$Deployment.Name)




#Prepare Site information for capture history
Siteinfo <- cameraOperation(CTtable = StationInfo,
                            stationCol = "Deployment.Name",
                            setupCol = "Deployment.Date",
                            retrievalCol = "Retrieval.Date",
                            hasProblems = F,
                            dateFormat = "%Y-%m-%d")




#Below code generates the detection history. Change species section to desired species
#Occasion length is set to 7 days, adjust based on your needs. This groups the data 
# based on 1 there was an animal in those 7 days or 0 no animal was captured during those 7 days
# If you are interested in per station information set day 1 to "station". If you are interested
# in making a history across a combined time period set day1 = "survey" and the history column
# will be the first date for your first site deployed across all deployments. 
#outDir is where you want your file saved. 

DeerHistory<- detectionHistory(recordTable = AnimalData,
                               species ="White-tailed Deer",
                               camOp = Siteinfo,
                               stationCol = "Deployment.Name",
                               speciesCol = "Common.Name",
                               recordDateTimeCol = "Begin",
                               recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                               occasionLength = 7,
                               day1= "station",
                               datesAsOccasionNames = T,
                               includeEffort = F,
                               writecsv = T,
                               outDir= "F:/Grad School/Candid Critters Project/Decoy Project/Code")