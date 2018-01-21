######## Fall Fawn Frenzy Data Organization ############

setwd("E:/Grad School/Candid Critters Project/Descriptive Stats Code")
data <-fread(file="eMammaldata.csv")




Unit5<-subset(data, Subproject %in% c("Dare County","Hyde County"))
Unit5$Unit<-"Unit5"
Unit5<- subset(Unit5, Date > as.Date("2017-08-17") )
Unit5<- subset(Unit5, Date < as.Date("2017-09-23") )
Unit4<-subset(dat, Subproject %in% c("Pender County","Craven County"))
Unit4$Unit<-"Unit4"
Unit4<- subset(Unit4, Date > as.Date("2017-09-07") )
Unit4<- subset(Unit4, Date < as.Date("2017-10-14") )
Unit3<-subset(dat, Subproject %in% c("Alamance County","Moore County"))
Unit3$Unit<-"Unit3"
Unit3<- subset(Unit3, Date > as.Date("2017-09-13") )
Unit3<- subset(Unit3, Date < as.Date("2017-10-21") )
Unit2<-subset(dat, Subproject %in% c("Surry County","Cleveland County"))
Unit2$Unit<-"Unit2"
Unit2<- subset(Unit2, Date > as.Date("2017-09-30") )
Unit2<- subset(Unit2, Date < as.Date("2017-11-04") )
Unit1<-subset(dat, Subproject %in% c("Haywood County","Burke County"))
Unit1$Unit<-"Unit1"
Unit2<- subset(Unit2, Date > as.Date("2017-10-20") )
Unit2<- subset(Unit2, Date < as.Date("2017-11-25") )
dat<-rbind(Unit5,Unit4,Unit3,Unit2,Unit1)

write.csv(dat, file = "E:/Grad School/Candid Critters Project/Thesis R Code/FawnFrenzy.csv", row.names = FALSE) 

