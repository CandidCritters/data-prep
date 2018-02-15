######################### Emammal Descriptive Stats ############################

################################################################################
#### Load Necessary Packages #######
#The below packages are the ones used throughout this code template. Please install
#and load the below packages before proceeding 
#If you do not have one of these packages you can install with the following code:
list.of.packages<-c("data.table","dplyr",'xtable','reshape2',"ggplot2",'ggmap','overlap','activity','camtrapR','rgdal')
new.packages<-list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages))install.packages(new.packages)

library(data.table)
library(xtable)
library(rgdal)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggmap)
library(overlap)
library(activity)
library(camtrapR)


library(lubridate)
library(jpeg)
library(png)


#######Loading and organizing data ##########

#First set the working directory where the data is stored and where
#your output will be saved
setwd("E:/Grad School/Candid Critters Project/Descriptive Stats Code")

#Load the latest dataset downloaded from the website
#Note that the filename will change each time so make sure it is
#edited properly below
data <-fread(file="eMammaldata.csv")
SiteInfo<-fread(file="SiteInfo.csv")
names(data)#these are the column names
levels(data$Subproject)#these are the counties in the dataset
head(data, n=5) #These are the first 30 lines of your data
tail(data, n=5) #These are the last 30 lines of your data




#################Description of effort in a table #######


####### Make Camera Night Output Table ############
#Generate a table with the camera deployment days
DeploymentNightTable <- aggregate(data$TrapNights, list(data$'Deployment Name'), FUN=mean)
names(DeploymentNightTable)<-c("Deployment", "Camera Nights")
DeploymentNightTable

DeploymentNightTable2<-xtable(DeploymentNightTable)

#Saving your camera nights table in a few ways:
print.xtable(DeploymentNightTable2, type="html", file="Summary_Table.html")  #Saves as an HTML File
#Open file in web browser
#Open the generated HTML file in your browser (may I recommend Firefox)
#Saves as a csv file - table can be edited in excel 
write.csv(DeploymentNightTable, file = "E:/Grad School/NSF Grant/Candid critters test R Coding/CameraNightTable.csv", row.names = FALSE) 
#Saves as a txt file - table can be made in word
write.table(DeploymentNightTable, file = "E:/Grad School/NSF Grant/Candid critters test R Coding/CameraNightTable.txt", row.names = FALSE)


#Total and average Trap Night per Subproject
SubprojectTrapNights = SiteInfo[, sum(TrapNights), by='Subproject'] 
names(SubprojectTrapNights)<-c("Subproject", "Camera Nights")
SubprojectTrapNights
AverageSubprojectTrapNight<- SiteInfo[, mean(TrapNights), by="Subproject"]
AverageSubprojectTrapNight



#Total Trap Nights across the entire project
TotalTrapNights<- SiteInfo[,sum(TrapNights), by='Project']
names(TotalTrapNights)<-c("Total Trap Nights", "Camera Nights")
TotalTrapNights




############ Bar graph of relative abundance  ##########
#Make data summary, detection rate for each species for the entire project
duration <- data %>% summarise(duration=mean(TrapNights))
spp<-unique(data$'Common Name')
dur<-rep(as.numeric(duration), length(spp))
count <- data[,list(sum=sum(Count)),by='Common Name']
rate<-count$sum/dur
rate_input<-cbind(count, rate)
remove_spp<-("Camera Trapper|No Animal|Unknown Animal|Vehicle|Unknown Squirrel|Unknown Small Rodent|Unknown Rabbit_Hare|Unknown Flying Squirrel|Unknown Felid|Unknown Coati_Raccoon|Unknown Canid|Unknown Bird|Time Lapse|Reptile species|Raptor species|Owl species|Other Bird species|Northern Bobwhite|Human non-staff|Common Raven|Calibration Photos|Blue Jay|Bicycle|Animal Not on List|American Crow")
rrate<-rate_input[grep(remove_spp, rate_input$'Common Name', invert=T),]   #invert = T shows the species not designated in remove_spp. 
#If you made a list of species you are interested in use invert=F
 #Orders based on which species has the higher Detection rate



#Make graph showing total capture counts
rrate<-rrate[order(-rrate$sum)]
head(rrate)
Countgraph1 <- ggplot(data=rrate, aes(x=reorder(rrate$'Common Name', sum), y=sum)) +
  geom_bar(stat="identity", color="black", 
           fill="steelblue")+
  theme_classic() + 
  labs(x="Species", 
       y = "Total Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))

Countgraph1
ggsave("Countgraph1.png", width = 20, height = 20, units = "cm")




#Make graph showing detection rate
rrate<-rrate[order(-rrate$rate)]
Detectiongraph1<-ggplot(data=rrate, aes(x=reorder(rrate$'Common Name', rate), y=rate)) +
  geom_bar(stat="identity", color="black", 
           fill="steelblue")+
  theme_classic() + 
  labs(x="Species", 
       y = "Detection Rate (count/day)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))
Detectiongraph1
ggsave("Detectiongraph1.png", width = 20, height = 20, units = "cm")



############################################################################################# 
                             ### Subsetting your data ###
#############################################################################################
#Pull the deployments for the protected area you are interested
#in by entering the name in quotes below.

subb <- subset(data, Subproject == "Wake County") #Run this if you are interested in only one name
multiname<-subset(data, Subproject %in% c("Wake County","Ashe County","Dare County")) #Run this if you want multiple names
SubprojectSites <- multiname[,c('Subproject','Deployment Name')] #this gives you a list of all the camera deployments in each Subproject




#If you are interested in subsetting the data in other ways. Here is additional ways to do so:
#example1<- subset(dat, Subproject == "Wake County" & dat$'Common Name' == "White-tailed Deer")  
              #If you have a space in your column names be sure to put the dataframe$ in front of that column in ''
#example2<- subset(dat, Subproject != "Wake County")   #this line gives you all the counties except Wake County
#example3<- subset(dat, Count>= 2) #this gives you entries with the animal count more than equal to 2





############################################################################################# 
                  ### Subsetted Data Bar graphs for specific Species ###
#############################################################################################

############ Bar graph of relative abundance for subset data ##########
#Make data summary, detection rate for each species
duration <- multiname %>% summarise(duration=mean(TrapNights))
spp<-unique(data$'Common Name')
dur<-rep(as.numeric(duration), length(spp))
count <- data[,list(sum=sum(Count)),by='Common Name']
rate<-count$sum/dur
rate_input<-cbind(count, rate)
remove_spp<-("American Black Bear|White-tailed Deer|Coyote|Northern Raccoon")
rrate<-rate_input[grep(remove_spp, rate_input$'Common Name', invert=F),]   #invert = T shows the species not designated in remove_spp. 
#If you made a list of species you are interested in use invert=F
rrate<-rrate[order(-rrate$rate)] #Orders based on which species has the higher Detection rate

#Make graph showing total capture counts
rrate<-rrate[order(-rrate$sum)]
head(rrate)
Countgraph2 <- ggplot(data=rrate, aes(x=reorder(rrate$'Common Name', sum), y=sum)) +
  geom_bar(stat="identity", color="black", 
           fill="steelblue")+
  theme_classic() + 
  labs(x="Species", 
       y = "Total Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))

Countgraph2
ggsave("Countgraph2.png", width = 20, height = 20, units = "cm")


#Make graph showing detection rate

Detectiongraph2<-ggplot(data=rrate, aes(x=reorder(rrate$'Common Name', rate), y=rate)) +
  geom_bar(stat="identity", color="black", 
           fill="steelblue")+
  theme_classic() + 
  labs(x="Species", 
       y = "Detection Rate (count/day)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))
Detectiongraph2
ggsave("Detectiongraph2.png", width = 20, height = 20, units = "cm")





############################################################################################# 
                  ### Comparison Bar graphs data organization ###
#############################################################################################

unique(data$Subproject) #This gives a list of the counties for the park you pulled out at the beginning

#Now use those names to pull out all the data for those counties.
#If multiple counties, separate by |.
counties<-paste(unique(data$Subproject), collapse = "|")
subb_Subproject<-data[grep(counties, data$Subproject),]
unique(subb_Subproject$Subproject)

#Calculate duration
z_co <- subb_Subproject[,c('Deployment Name','ID Type', 'Deploy ID', 'Actual Lon', 'Actual Lat', 'Begin', 'End','TrapNights')]
z_co<-as.data.table(z_co)


#Start Times Extraction
Z_start<-z_co[order(Begin)]   #Orders entries by chronological order
Z_start<- Z_start[order(Z_start$'Deployment Name')]  #groups by Deployment name
Z_start<- Z_start[!duplicated(Z_start$'Deployment Name'),] #Removes entries after the first entry for each deployment
Z_start$End <- NULL


#End Times Extraction 
Z_End <- z_co[order(-End)] #Orders entries  by reverse chronological order
Z_End<- Z_End[order(Z_End$'Deployment Name')]  #groups by deployment name
Z_End<- Z_End[!duplicated(Z_End$'Deployment Name'),]
Z_End[ ,c("Begin","TrapNights") := NULL]



#Generating a Site Information Dataframe
z_times <- merge(Z_start, Z_End, by =c('Deployment Name','ID Type', 'Deploy ID', 'Actual Lon', 'Actual Lat'))
#Extract Start and End Dates 
setDT(z_times)[,paste0("Begin.Time",1:2):= tstrsplit(Begin," ")] #Splits the time stamp into Date and Time Columns
setDT(z_times)[,paste0("End.Time",1:2):= tstrsplit(End," ")]
z_times[ ,c("Begin.Time2","End.Time2") := NULL] #Removes the time column for both end and begin
setnames(z_times, old = c('Begin.Time1','End.Time1', 'Begin', 'End'), new = c('Begin.Date','End.Date', 'Deployment.Start','Deployment.End')) #Renames the new columns 

#Calculate Total Days Samples
z_times$TrapNights <- difftime((as.Date(z_times$End.Date)), (as.Date(z_times$Begin.Date)), units="days")
head(z_times)
z_data<- merge(subb_Subproject, z_times, by =c('Deployment Name','ID Type', 'Deploy ID', 'Actual Lon', 'Actual Lat','TrapNights'))


#Make data summary, rate for each species in other counties
duration_co <- z_data %>% summarise(duration=mean(TrapNights))  #This calcuates the average trap night across all counties
spp_co<-unique(z_data$'Common Name') #Generates a list of common names from the data matrix
dur_co<-rep(as.numeric(duration_co), length(spp_co))
count_co <- z_data[,list(sum=sum(Count)),by=z_data$'Common Name']   #This gives you the sum of counts for each species 

rate_co<-count_co$sum/dur_co   #This calculates the detection rate for each species across all other counties
rate_input_co<-cbind(count_co, rate_co)       #This combines calculated rate with the number of counts
setnames(rate_input_co, old=c("z_data","rate_co"), new=c("Common Name","rate"))
remove_spp_co<-("Camera Trapper|No Animal|Unknown Animal|Vehicle|Unknown Squirrel|Unknown Small Rodent|Unknown Rabbit_Hare|Unknown Flying Squirrel|Unknown Felid|Unknown Coati_Raccoon|Unknown Canid|Unknown Bird|Time Lapse|Reptile species|Raptor species|Owl species|Other Bird species|Northern Bobwhite|Human non-staff|Common Raven|Calibration Photos|Blue Jay|Bicycle|Animal Not on List|American Crow")
rrate_co<-rate_input_co[grep(remove_spp_co, rate_input_co$'Common Name', invert=T),] #This removes stated species from list.
                                                                        #If you want the stated species instead of the others simply write invert=F instead ofinvert=T
rrate_co<-rrate_co[ order(-rrate_co$rate)]

#Pull in the rate for the protected area you selected in the very beginning
rrate<-rrate[order(-rrate$rate)]





############################################################################################# 
                    ### Site Comparison Bar graphs to plot ALL species ###
#############################################################################################
###### Determining Species Detection Rates for both Selected area and all counties #####
#Combine dataframes together
#First add species to rrate that are only in rrate_co to get complete species detection rates
missing_spp<-subset(rrate_co, !(rrate_co$'Common Name' %in% rrate$'Common Name'))   #This shows if any species shows up in other Subproject, but not your selected site
missing_spp$sum[missing_spp$sum>0]  <- 0   #Missing species will have their counts set to 0
missing_spp$rate[missing_spp$rate>0]  <- 0   #Missing species will have their detection rates set to 0
rrate2<-rbind(rrate, missing_spp)   #Combines original subset dataframe with 
missing_spp2<-subset(rrate, !(rrate$'Common Name' %in% rrate_co$'Common Name'))  #This shows species that have showed up in your selected site, but not other counties
missing_spp2$sum[missing_spp2$sum>0]  <- 0   #Missing species will have their counts set to 0
missing_spp2$rate[missing_spp2$rate>0]  <- 0   #Missing species will have their detection rates set to 0
rrate_co<-rbind(rrate_co, missing_spp2)   #Combines original subset dataframe with 


#Then combine the protected area and Subproject data into one
#dataframe
rrate_co<-rrate_co[ order(rrate_co[,1]), ]    #All Subproject data 
rrate2<-rrate2[ order(rrate2[,1]), ]          #Selected area data
rrate3<-cbind(rrate2, rrate_co$rate)
names(rrate3)<-c("Common Name","Count Sum", "Selected Area Rate", "Subproject Rate")
#Order In Alphabetical order
rrate3<-rrate3[ order(rrate3[,1]), ]
rrate3

###### Make a data frame with Detection Rate isolated for selected area and Subproject ####
rrate_co$Area<-rep("Subproject", nrow(rrate_co))
rrate2$Area<-rep("Selected Area", nrow(rrate2))
rrate4<-rbind(rrate2, rrate_co)    #Combines all data frames together.
names(rrate4)<-c("Common Name","Count Sum", "Detection Rate", "Area")

#Order by rate
rrate4<-rrate4[ order(-rrate4$"Detection Rate")]
rrate4


#To plot all the species in the rrate4 data frame
Countgraph3<-ggplot(data=rrate4, aes(x=reorder(rrate4$'Common Name', rrate4$'Count Sum'), y=rrate4$'Count Sum', fill=Area)) +
  geom_bar(stat="identity", color="black", position="dodge")+
  scale_fill_manual(values = c("slateblue2", "burlywood2"))+
  theme_classic() + 
  labs(x="Species", 
       y = "Total Capture Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))
Countgraph3
ggsave("Countgraph3.png", width = 20, height = 20, units = "cm")



#Make graph Detection Rate
#To plot all the species in the rrate4 data frame
Detectiongraph3<-ggplot(data=rrate4, aes(x=reorder(rrate4$'Common Name', rrate4$'Detection Rate'), y=rrate4$'Detection Rate', fill=Area)) +
  geom_bar(stat="identity", color="black", position="dodge")+
  scale_fill_manual(values = c("slateblue2", "burlywood2"))+
  theme_classic() + 
  labs(x="Species", 
       y = "Detection Rate (count/day)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))
Detectiongraph3
ggsave("Detectiongraph3.png", width = 20, height = 20, units = "cm")






#############################################################################################
              ###  Site Comparison Bar graphs to plot specific species ###
#############################################################################################
Desiredspp<-("American Black Bear|Coyote|Virginia Opossum|Northern Raccoon|Eastern Grey Squirrel|White-tailed Deer")
rrate5<-rrate4[grep(Desiredspp, rrate4$'Common Name', invert=F),]
Detectiongraph4<-ggplot(data=rrate5, aes(x=reorder(rrate5$'Common Name', rrate5$'Detection Rate'), y=rrate5$'Detection Rate', fill=Area)) +
        geom_bar(stat="identity", color="black", position="dodge")+
        scale_fill_manual(values = c("slateblue2", "burlywood2"))+
        theme_classic() + 
        labs(x="Species", 
             y = "Detection Rate (count/day)")+
        theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
        theme(axis.text.y = element_text(color="black"))
Detectiongraph4
ggsave("Detectiongraph4.png", width = 20, height = 20, units = "cm")

#To make a box plot of detection rate for the specific animals
Countgraph4<-ggplot(data=rrate5, aes(x=reorder(rrate5$'Common Name', rrate5$'Count Sum'), y=rrate5$'Count Sum', fill=Area)) +
  geom_bar(stat="identity", color="black", position="dodge")+
  scale_fill_manual(values = c("slateblue2", "burlywood2"))+
  theme_classic() + 
  labs(x="Species", 
       y = "Total Capture Count")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, color="black"))+
  theme(axis.text.y = element_text(color="black"))
Countgraph4
ggsave("Countgraph4.png", width = 20, height = 20, units = "cm")









################################################################################
####################### Graphing the data with Boxplots ########################
################################################################################


################################################################################
                     ###  Boxplots using all data ###
################################################################################
# All Species Count/Detection in entire dataset boxplot - by camera Deployment
#Remove species groups that are unknown, humans, or not species
remove_spp_co<-("Camera Trapper|No Animal|Unknown Animal|Vehicle|Unknown Squirrel|Unknown Small Rodent|Unknown Rabbit_Hare|Unknown Flying Squirrel|Unknown Felid|Unknown Coati_Raccoon|Unknown Canid|Unknown Bird|Time Lapse|Reptile species|Raptor species|Owl species|Other Bird species|Northern Bobwhite|Human non-staff|Common Raven|Calibration Photos|Blue Jay|Bicycle|Animal Not on List|American Crow")
boxplotdata<-data[grep(remove_spp_co, data$'Common Name', invert=T),] #This removes stated species from list.
dataCapturecount <- data[,list (Capturecount=sum(Count)), by=c('Common Name','Deployment Name')]
boxplotdata1<-merge(dataCapturecount, boxplotdata, by=c('Common Name','Deployment Name'))
Nights<-data[!duplicated(data$'Deployment Name'),] 
TotalTrapNights<-Nights[,list(TotalNights=sum(TrapNights)), by="Deployment Name"]
boxplotdata1<-merge(TotalTrapNights, boxplotdata1, by="Deployment Name")

# Counts of species per all camera sites in the entire dataset boxplot - For camera Deployment 

Boxplot1<-ggplot(data=boxplotdata1) + geom_boxplot (aes(x=boxplotdata1$'Common Name',y=boxplotdata1$Capturecount),fill="darkseagreen4")
Boxplot1 + labs(title = "Species photographic capture count for all camera sites", x='Species', y='Photographic Capture Count') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))


# Detection Rate of species per all camera sites in the entire dataset boxplot - For camera Deployment 
#Calculate Detection for Total Counts / Total Trap Nights for the entire dataset
boxplotdata1$Detection<- boxplotdata1$Capturecount / boxplotdata1$TotalNights

Boxplot2<-ggplot(data=boxplotdata1) + geom_boxplot (aes(x=boxplotdata1$'Common Name',y=boxplotdata1$Detection),fill="darkseagreen4")
Boxplot2 + labs(title = "Species Detection for all camera sites", x='Species', y='Detection Rate') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))





######## Total Detection of species in entire dataset boxplot by Subproject #################
dataCapturecount <- data[,list (Capturecount=sum(Count)), by=c('Common Name','Subproject')]
boxplotdata2<-merge(dataCapturecount, boxplotdata, by=c('Common Name','Subproject'))
Nights<-data[!duplicated(data$'Subproject'),] 
TotalTrapNights<-Nights[,list(TotalNights=sum(TrapNights)), by="Subproject"]
boxplotdata2<-merge(TotalTrapNights, boxplotdata2, by="Subproject")

# Counts of species per all camera sites in the entire dataset boxplot - by Subproject 

Boxplot3<-ggplot(data=boxplotdata2) + geom_boxplot (aes(x=boxplotdata2$'Common Name',y=boxplotdata2$Capturecount),fill="deepskyblue4")
Boxplot3 + labs(title = "Species photographic capture for all counties", x='Species', y='Photographic Capture Count') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))


# Detection Rate of species per all camera sites in the entire dataset boxplot - by Subproject
#Calculate Detection for Total Counts / Total Trap Nights for the entire dataset
boxplotdata2$Detection<- boxplotdata2$Capturecount / boxplotdata2$TotalNights

Boxplot4<-ggplot(data=boxplotdata2) + geom_boxplot (aes(x=boxplotdata2$'Common Name',y=boxplotdata2$Detection),fill="deepskyblue4")
Boxplot4 + labs(title = "Species Detection Rate for all counties", x='Species', y='Detection Rate') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))



################################################################################
           ###  Boxplots for specific species across all sites  ###
################################################################################
#specify the species that you are interested in using for boxplots
spp_interested<-("White-tailed Deer|Coyote|American Black Bear|Northern Raccoon|Domestic Dog|Domestic Cat|Bobcat|Virginia Opossum")
boxplotdata3<-data[grep(spp_interested, data$'Common Name', invert=F),]


#Make a boxplot for multiple specific species across subsetted camera sites
dataCapturecount <- boxplotdata3[,list (Capturecount=sum(Count)), by=c('Common Name','Deployment Name')]
trapinfo<-  boxplotdata3[,c('TrapNights','Deployment Name','Subproject','Project')]
trapinfo<-trapinfo[!duplicated(trapinfo$'Deployment Name'),] 
boxplotdata3<-merge(dataCapturecount, trapinfo, by='Deployment Name')
boxplotdata3$Detection<- boxplotdata3$Capturecount / boxplotdata3$TrapNights

#Capture Counts for specific species across subsetted camera sites
Boxplot5<-ggplot(data=boxplotdata3) + geom_boxplot (aes(x=boxplotdata3$'Common Name',y=boxplotdata3$Capturecount),fill="seagreen3")
Boxplot5 + labs(title = "Species photographic capture count for subsetted camera sites", x='Species', y='Photographic Capture Count') + theme(axis.text.x=element_text(angle=90, size=8, vjust=0.55))



Boxplot6<-ggplot(data=boxplotdata3) + geom_boxplot (aes(x=boxplotdata3$'Common Name',y=boxplotdata3$Detection),fill="seagreen3")
Boxplot6 + labs(title = "Species Detection Rate for subsetted camera sites", x='Species', y='Detection Rate') + theme(axis.text.x=element_text(angle=90, size=8, vjust=0.55))






######## Make a boxplot for multiple specific species across subsetted  by Subproject #################
dataCapturecount <- boxplotdata3[,list (Subprojectcount=sum(Capturecount)), by=c('Common Name','Subproject')]
trapinfo<-  boxplotdata3[,c('TrapNights','Deployment Name','Subproject','Project')]
trapinfo<-trapinfo[,list(TotalNights=sum(TrapNights)), by="Subproject"]
trapinfo<-trapinfo[!duplicated(trapinfo$'Subproject'),]
boxplotdata4<-merge(dataCapturecount, trapinfo, by=('Subproject'))

boxplotdata4$Detection<- boxplotdata4$Subprojectcount / boxplotdata4$TotalNights

# Counts of specific species per subset camera sites boxplot - by Subproject (Subproject)
Boxplot7<-ggplot(data=boxplotdata4) + geom_boxplot (aes(x=boxplotdata4$'Common Name',y=boxplotdata4$Subprojectcount),fill="deepskyblue4")
Boxplot7 + labs(title = "Species photographic capture count for subsetted counties", x='Species', y='Photographic Capture Count') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))


# Detection Rate of specific species per subset camera sites boxplot - by Subproject (Subproject)
Boxplot8<-ggplot(data=boxplotdata4) + geom_boxplot (aes(x=boxplotdata4$'Common Name',y=boxplotdata4$Detection),fill="deepskyblue4")
Boxplot8 + labs(title = "Species Detection Rate for subsetted counties", x='Species', y='Detection Rate') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))







################################################################################
          ###  Boxplots for subsetted data and specific species ###
################################################################################
#Subset your data for the information you are interested to see as a boxplot
#Note smaller groupings such as one or two species or just a select few counties work best


multiname<-subset(data, Subproject %in% c("Wake County","Ashe County","Dare County")) #Run this if you want multiple names
SubprojectSites <- multiname[,c('Subproject','Deployment Name')] #this gives you a list of all the camera deployments in each Subproject

#specify the species that you are interested in using for boxplots
spp_interested<-("White-tailed Deer|Coyote|American Black Bear|Northern Raccoon|Domestic Dog|Domestic Cat|Bobcat|Virginia Opossum")
boxplotdata5<-multiname[grep(spp_interested, multiname$'Common Name', invert=F),]



#Make a boxplot for multiple specific species across subsetted camera sites
dataCapturecount <- boxplotdata5[,list (Capturecount=sum(Count)), by=c('Common Name','Deployment Name')]
trapinfo<-  boxplotdata5[,c('TrapNights','Deployment Name','Subproject','Project')]
trapinfo<-trapinfo[!duplicated(trapinfo$'Deployment Name'),] 
boxplotdata5<-merge(dataCapturecount, trapinfo, by='Deployment Name')
boxplotdata5$Detection<- boxplotdata5$Capturecount / boxplotdata5$TrapNights

#Capture Counts for specific species across subsetted camera sites
Boxplot9<-ggplot(data=boxplotdata5) + geom_boxplot (aes(x=boxplotdata5$'Common Name',y=boxplotdata5$Capturecount),fill="seagreen3")
Boxplot9 + labs(title = "Species photographic capture count for subsetted camera sites", x='Species', y='Photographic Capture Count') + theme(axis.text.x=element_text(angle=90, size=8, vjust=0.55))


#Detection Rate for specific species across subsetted camera sites
Boxplot10<-ggplot(data=boxplotdata5) + geom_boxplot (aes(x=boxplotdata5$'Common Name',y=boxplotdata5$Detection),fill="seagreen3")
Boxplot10 + labs(title = "Species Detection Rate for subsetted camera sites", x='Species', y='Detection Rate') + theme(axis.text.x=element_text(angle=90, size=8, vjust=0.55))




######## Make a boxplot for multiple specific species across subsetted Subproject #################
dataCapturecount <- boxplotdata5[,list (Subprojectcount=sum(Capturecount)), by=c('Common Name','Subproject')]
trapinfo<-  boxplotdata5[,c('TrapNights','Deployment Name','Subproject','Project')]
trapinfo<-trapinfo[,list(TotalNights=sum(TrapNights)), by="Subproject"]
trapinfo<-trapinfo[!duplicated(trapinfo$'Subproject'),]
boxplotdata6<-merge(dataCapturecount, trapinfo, by=('Subproject'))

boxplotdata6$Detection<- boxplotdata6$Subprojectcount / boxplotdata6$TotalNights

# Counts of specific species per subset  by Subproject (Subproject)
Boxplot11<-ggplot(data=boxplotdata6) + geom_boxplot (aes(x=boxplotdata6$'Common Name',y=boxplotdata6$Subprojectcount),fill="deepskyblue4")
Boxplot11 + labs(title = "Species photographic capture count for subsetted counties", x='Species', y='Photographic Capture Count') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))


# Detection Rate of specific species per subset by Subproject (Subproject)
Boxplot12<-ggplot(data=boxplotdata6) + geom_boxplot (aes(x=boxplotdata6$'Common Name',y=boxplotdata6$Detection),fill="deepskyblue4")
Boxplot12 + labs(title = "Species Detection Rate for subsetted counties", x='Species', y='Detection Rate') + theme(axis.text.x=element_text(angle=90, size=7, vjust=0.55))












################################################################################
#####################  Species of Interest Maps  ###############################
################################################################################


################################################################################
                   #### Map making data organization ###
################################################################################

#Define the species you are interested in such as "Coyote"
#Must match this list:
unique(data$'Common Name')

species<-"Coyote"
data.sp <- subset(data, data$'Common Name' == species)
s<-data.sp[, -c(2:10, 12:14, 18:23)]

#Add list of empty cameras back to species subset
cam<-subset(data, !(data$'Deployment Name' %in% data.sp$'Deployment Name'))
cam$Count[cam$Count>0]  <- 0
cam$'Common Name'[cam$'Common Name'>0]  <- 0
ss<-rbind(data.sp, cam)

#Summarize detection rate on each camera for this species
duration<-aggregate(ss$TrapNights, by=list(ss$'Deployment Name'), FUN=mean)
duration$x<-as.numeric(duration$x)
lat<-aggregate(ss$'Actual Lat', by=list(ss$'Deployment Name'), FUN=mean)
long<-aggregate(ss$'Actual Lon', by=list(ss$'Deployment Name'), FUN=mean)
count<-aggregate(ss$Count, by=list(ss$'Deployment Name'), FUN=sum)
tt<-cbind(count, duration$x, lat$x, long$x)
names(tt)<-c("Deployment Name", "Count", "Duration", "Actual Lat", "Actual Lon")
tt$Rate<-tt$Count/tt$Duration

#Define the map area
latrange = range(tt$'Actual Lat') + c(-0.15,0.15)
lonrange = range(tt$'Actual Lon') + c(-0.15,0.15)
extent = c(lonrange[1],latrange[1],lonrange[2],latrange[2])
m = get_map(extent, maptype="terrain")
ggmap(m)


################################################################################
             #### Detection and Count Map for specific species###
################################################################################
detectmap<-ggmap(m) + geom_point(data= tt, aes(x=tt$'Actual Lon',y=tt$'Actual Lat', size=Rate))+
  scale_colour_gradient2(low="white",mid="purple",high="blue", midpoint=0.2, breaks=c(0,0.2,0.4), labels=c("0","0.2","0.5"), name="Detection Rate\n(count/day)")+
  theme(text=element_text(size=12))+
  labs(x = "Longitude",y = "Latitude", title="Coyote detection rate throughout NC")+
  theme(axis.text = element_text(color="black"))

detectmap

ggsave("CoyotedetectionMap.png", width = 20, height = 20, units = "cm")


countmap<-ggmap(m) + geom_point(data= tt, aes(x=tt$'Actual Lon',y=tt$'Actual Lat', size=Count))+
  scale_colour_gradient2(low="white",mid="purple",high="blue", midpoint=0.2, breaks=c(0,0.2,0.4),labels=c("0","0.2","0.5"), name="Detection Rate\n(count/day)")+
  theme(text=element_text(size=12))+
  labs(x = "Longitude",y = "Latitude", title="Coyote photographic capture count throughout NC")+
  theme(axis.text = element_text(color="black"))

countmap

ggsave("CoyotecountMap.png", width = 20, height = 20, units = "cm")









################################################################################
#####################      Activity Patterns     ###############################
################################################################################


################################################################################
 ### Data organization for plotting Activity Patterns of specific species ###
################################################################################

names(data)
head(data, n=3)


##### Turn the time into Radians 
setDT(data)[,paste0("Begin.Time",1:2):= tstrsplit(Begin," ")] #Splits the time stamp into Date and Time Columns
setnames(data, old = c('Begin.Time1','Begin.Time2'), new = c('Date','Time')) #Renames the new columns 


setDT(data)[,paste0("Times",1:3):= tstrsplit(Time,":")]
data$hours<-as.numeric(data$Times1)
data$mins<-as.numeric(data$Times2)
data$seconds<-as.numeric(data$Times3)

data$hours<-data$hours * 60
data$seconds<-data$seconds / 60
data$totalminutes<-data$hours + data$mins + data$seconds

data[,!c("Times1","Times2","Times3"), with=F]
data$totalminutes<-apply(data, MARGIN=2, 
                             FUN=function(x) (data$totalminutes-min(data$totalminutes))/diff(range(data$totalminutes)))

summary(data$totalminutes)


data$radians <- data$totalminutes * 2 * pi  #This turns total minutes into radians
#Time is now converted into radians






################################################################################
             ### Activity Density Plots for specific species  ###
################################################################################


#Activity patterns can only plot a species at a time so you will need to subset the data
#based on your desired species or grouping

Deer<-data[data$'Common Name' == "White-tailed Deer"]
Coyote<-data[data$'Common Name' =="Coyote"]
B.Bear<- data[data$'Common Name'=="American Black Bear",]


#densityPlot function will give the average density vs. time for the desired species
densityPlot(Deer$radians, xscale = 24, xcenter = "noon", add = FALSE, rug = FALSE, 
            extend = 'lightgrey', n.grid = 100, adjust = 0.2, main = "Density Vs Time",
            xlab = "Time", ylab = "Density")
#NOTE extend=NULL will get rid of the mirror/ light grey areas on the side of the plot


#To add another species into the density plot:
densityPlot(Coyote$radians, add = TRUE, col = 'blue')
densityPlot(B.Bear$radians, add= TRUE, col='red')
legend ('bottomleft', c("White-tailed Deer","Coyote",'American Black Bear'), lty = 1, col = c('black','blue','red'), bg = 'white')



#to draw lines indicating certain areas of time on the plot - the set numbers shows crepescular periods. 
abline (v = c(4.5, 7.0, (17+0/60), (20+0/60)), lty = 3, col='grey')


#To Save graph 
png(filename ="E:/Grad School/Candid Critters Project/Thesis R Code/densityactivity.png" )
dev.copy(jpeg,filename="E:/Grad School/Candid Critters Project/Thesis R Code/temporalmap.jpg");
dev.off ()








################################################################################
             ### Determing species activity pattern overlap  ###
################################################################################


#Plot the density of two species in relation to time while highlighting periods
#of overlap between the two species. 
overlapPlot (Deer$radians, Coyote$radians, main = "Deer and Coyote Activity Pattern Overlap")
abline (v = c(4.5, 7.0, (17+0/60), (20+0/60)), lty = 3)
legend ('top',c("Deer","Coyote"), lty = c(1,2), col = c(1,4), bty = 'n')


dev.copy(jpeg,filename="E:/Grad School/Candid Critters Project/Thesis R Code/temporaloverlapmap.jpg");
dev.off ()

#overlapEST = calculates up to three estimates of activity pattern overlap based
#on times of observation for 2 species 
D.hat.estimates <-overlapEst(Deer$radians, Coyote$radians)
D.hat.estimates

#Check the sample sizes to choose the best Dhat estimate
length(Deer$radians)
length(Coyote$radians)


# Note Use the Dhat4 estimate if the smaller sample has more than 75 observations
#otherwise, use the Dhat1 estimator 

######################## Bootstrapping ########################################

#Bootstrapping fits a kernal density to the original data then draw random 
#simulated observationsfrom this distribution. 
#bootstrap set to 1000 is good for sample data, but for a real analysis use 10000
Deer.bootstrap <- resample (Deer$radians, 1000)
dim(Deer.bootstrap)
coyote.bootstrap <- resample (Coyote$radians, 1000)
dim(coyote.bootstrap)


#Both of the sample species have lengths greater than 75 observations so Dhat4 (NA,1,NA) should be used.
#If less than 75 observations, use Dhat1 (1,NA,NA)
coyote.Deer.boot <- bootEst(coyote.bootstrap, Deer.bootstrap, adjust = c (NA,1,NA))
dim(coyote.Deer.boot)
Boot.mean <- colMeans (coyote.Deer.boot)
Boot.mean

Boot.Bias <- Boot.mean -D.hat.estimates 
Boot.Bias
#the difference between the dhat estimates and the dhat mean represents the
#bootstrap bias 


####################### confidence intervals ##################################

#use bootCI to calculate confidence intervals for D.hat / boot data

Conf.deer.coyote <- coyote.Deer.boot [, 2]
bootCI(D.hat.estimates[2], Conf.deer.coyote, conf = 0.95)

#NOTE if you are using the D.hat1 you will replace the 2 in the above code with 1

#bootCI produces additional confidence intervals other than perc (basic and Norm)
#These values are inteded to use with a bias-corrected estimator. All the confidence
#interval estimators except perc involve additive corrections which migh result in 
#values outside the proper confidence interval range. These corrections can be
#avoided by carrying out the corrections on a logistic scale and backtransforming




######## Calculating difference between 2 species in relation to time ##########

#to compare activity across certain desired parts of the day per species: reps 
# represents bootstrapping amount
fdeer <- fitact (Deer$radians, reps = 1000)
compareTimes(fdeer, c(5.5,6, 0.5, 1))
fcoyote <- fitact (Coyote$radians, reps = 1000)
compareTimes(fcoyote, c(5.5, 6, 0.5, 1))



#To test whether a species has a significantly different activity level from another
#Use the following code to estimate the statistical difference using a Wald Test

compareAct(list(fdeer, fcoyote))


#To calculate the observed overlap index and the probability observed index 


compareCkern(Deer$radians, Coyote$radians, reps = 10, index = "Dhat4") #if using Dhat1, replace Dhat4 with Dhat1


######### Generating kernel density plot w/ CI, SE, and Bootstrapping ##########


#to generate a kernel density plot that incorporates COnfidence intervals, standard
#Error and Bootstrapping

plot(fdeer)
plot(fcoyote)
plot(fdeer, add = T, lcol= 3)














