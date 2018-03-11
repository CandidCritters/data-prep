


################################################################################
#####################      Activity Patterns     ###############################
################################################################################


################################################################################
### Data organization for plotting Activity Patterns of specific species ###
################################################################################
#######Loading and organizing data ##########

#First set the working directory where the data is stored and where
#your output will be saved
setwd("F:/Grad School/Candid Critters Project/Descriptive Stats Code")


#Load the necessary packages
library(data.table)
library(activity)
library(overlap)



#Load the latest dataset downloaded from the website
#Note that the filename will change each time so make sure it is
#edited properly below
data <-fread(file="eMammaldata.csv")
names(data)#these are the column names
levels(data$Subproject)#these are the counties in the dataset
head(data, n=5) #These are the first 30 lines of your data
tail(data, n=5) #These are the last 30 lines of your data


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




################################################################################
### Determing species activity pattern overlap  ###
################################################################################


#Plot the density of two species in relation to time while highlighting periods
#of overlap between the two species. 
overlapPlot (Deer$radians, Coyote$radians, main = "Deer and Coyote Activity Pattern Overlap")
abline (v = c(4.5, 7.0, (17+0/60), (20+0/60)), lty = 3)
legend ('top',c("Deer","Coyote"), lty = c(1,2), col = c(1,4), bty = 'n')


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


compareCkern(Deer$radians, Coyote$radians, reps = 1000, index = "Dhat4") #if using Dhat1, replace Dhat4 with Dhat1


######### Generating kernel density plot w/ CI, SE, and Bootstrapping ##########


#to generate a kernel density plot that incorporates COnfidence intervals, standard
#Error and Bootstrapping

plot(fdeer)
plot(fcoyote)
plot(fdeer, add = T, lcol= 3)





