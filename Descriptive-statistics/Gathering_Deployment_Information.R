########### Gathering Site Information ################
#



#######Loading and organizing data ##########

#First set the working directory where the data is stored and where
#your output will be saved
setwd("F:/Grad School/Candid Critters Project/Decoy Project/Code")

#Load packages
library(data.table)


#Load the dataset you want to get descriptive information on. For this example
# load the eMammaldata.csv and SiteInfo.csv file that was found in the same folder as this template
data <-fread(file="eMammaldata.csv")
data<-fread(file="SiteInfo.csv")
names(data)#these are the column names
head(data, n=5) #These are the first 30 lines of your data
tail(data, n=5) #These are the last 30 lines of your data




##Use the following table to examine your deployment days to ensure you do not have a 
#possible issue with your time stamps
DeploymentDays <- aggregate(SiteInfo$TrapNights, list(SiteInfo$'Deployment Name'), FUN=mean)
names(DeploymentDays)<-c("Deployment", "Trap Nights")
DeploymentDays




#################Description of effort in a table #######

#You can subset the data based on a min and max allowance of deployment nights
#Remove any sites that have less than 7 days of trap nights or above 250 days
#This is a simple check to remove any sites that did not function correctly or
#have an issue that needs to be reviewed


data<- subset(data, !TrapNights<=6)
SiteInfo<- subset(SiteInfo, !TrapNights<=6)
data<- subset(data, !TrapNights>=250)
SiteInfo<- subset(SiteInfo, !TrapNights>=250)


#If you wanted to update your files so they do not have the sites without the 
#desired amount of trap nights, use the following code. Just switch you the file 
#location to where you would like to save it. 
write.csv(SiteInfo, file = "F:/Grad School/Candid Critters Project/Decoy Project/Code/SiteInfo.csv", row.names = FALSE) 
write.csv(data, file = "F:/Grad School/Candid Critters Project/Decoy Project/Code/eMammaldata.csv", row.names = FALSE) 


####### Make Camera Night Output Table ############
#Generate a table with the camera deployment effort or trap nights
#This is the # of nights that a specific camera is deployed for
DeploymentNightTable <- aggregate(SiteInfo$TrapNights, list(SiteInfo$'Deployment Name'), FUN=mean)
names(DeploymentNightTable)<-c("Deployment", "Trap Nights")
DeploymentNightTable




#Total and average trap night per subproject
SubprojectTrapNights = SiteInfo[, sum(TrapNights), by='Subproject'] 
names(SubprojectTrapNights)<-c("Subproject", "Total Trap Nights")
SubprojectTrapNights
AverageSubprojectTrapNight<- SiteInfo[, mean(TrapNights), by="Subproject"]
names(SubprojectTrapNights)<-c("Subproject", "Average Trap Nights")
AverageSubprojectTrapNight



#Total Trap Nights across the entire project
TotalTrapNights<- SiteInfo[,sum(TrapNights), by='Project']
names(TotalTrapNights)<-c("Total Trap Nights", "Camera Nights")
TotalTrapNights

