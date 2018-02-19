#business data US

businessdata
malebusinessdata <- businessdata[(businessdata$Gender == 'Male'),]
femalebusinessdata <- businessdata[(businessdata$Gender == 'Female'),]
groupsbusinessdata <- businessdata[(businessdata$GroupVFit == 'FIT'),]
linearbusinessdata <- groupsbusinessdata

checkAndRemoveEmptyandNAvalues <- function(data2Check, column2Check){
  data2Check <- data2Check[!(column2Check == ""),]
  return(data2Check)
}
checkAndRemoveEmptyandNAvalues(linearbusinessdata, linearbusinessdata$GroupVFit )
checkAndRemoveEmptyandNAvalues(linearbusinessdata, linearbusinessdata$Gender )


linearbusinessdata 

linearbusinessdata[which((linearbusinessdata$GroupVFit =="FIT") == T),37] <- "1"
linearbusinessdata[which((linearbusinessdata$GroupVFit =="Groups") == T),37] <- "2"
colnames(linearbusinessdata)[37] <- "GroupvsFit1"
linearbusinessdata[which((linearbusinessdata$Gender =="Female") == T),38] <- "1"
linearbusinessdata[which((linearbusinessdata$Gender=="Male") == T),38] <- "2"
colnames(linearbusinessdata)[38] <- "Gender1"
linearbusinessdata[which((linearbusinessdata$Boutique =="Y") == T),39] <- "1"
linearbusinessdata[which((linearbusinessdata$Boutique =="N") == T),39] <- "2"
colnames(linearbusinessdata)[39] <- "Boutique1"
linearbusinessdata[which((linearbusinessdata$Casino =="Y") == T),40] <- "1"
linearbusinessdata[which((linearbusinessdata$Casino =="N") == T),40] <- "2"
colnames(linearbusinessdata)[40] <- "Casino1"
linearbusinessdata[which((linearbusinessdata$Conference =="Y") == T),41] <- "1"
linearbusinessdata[which((linearbusinessdata$Conference =="N") == T),41] <- "2"
colnames(linearbusinessdata)[41] <- "Conference1"
linearbusinessdata[which((linearbusinessdata$Drycleaning =="Y") == T),42] <- "1"
linearbusinessdata[which((linearbusinessdata$Drycleaning =="N") == T),42] <- "2"
colnames(linearbusinessdata)[42] <- "Drycleaning1"
linearbusinessdata[which((linearbusinessdata$Elevators =="Y") == T),43] <- "1"
linearbusinessdata[which((linearbusinessdata$Elevators =="N") == T),43] <- "2"
colnames(linearbusinessdata)[43] <- "Elevators1"
linearbusinessdata[which((linearbusinessdata$Fitnesscenter =="Y") == T),44] <- "1"
linearbusinessdata[which((linearbusinessdata$Fitnesscenter =="N") == T),44] <- "2"
colnames(linearbusinessdata)[44] <- "Fitnesscenter1"
linearbusinessdata[which((linearbusinessdata$Golf =="Y") == T),45] <- "1"
linearbusinessdata[which((linearbusinessdata$Golf =="N") == T),45] <- "2"
colnames(linearbusinessdata)[45] <- "Golf1"
linearbusinessdata[which((linearbusinessdata$Laundry =="Y") == T),46] <- "1"
linearbusinessdata[which((linearbusinessdata$Laundry =="N") == T),46] <- "2"
colnames(linearbusinessdata)[46] <- "Laundry1"
linearbusinessdata[which((linearbusinessdata$Minibar =="Y") == T),47] <- "1"
linearbusinessdata[which((linearbusinessdata$Minibar =="N") == T),47] <- "2"
colnames(linearbusinessdata)[47] <- "Minibar1"
linearbusinessdata[which((linearbusinessdata$Indoorpool =="Y") == T),48] <- "1"
linearbusinessdata[which((linearbusinessdata$Indoorpool =="N") == T),48] <- "2"
colnames(linearbusinessdata)[48] <- "Indoorpool1"
linearbusinessdata[which((linearbusinessdata$Pooloutdoor =="Y") == T),49] <- "1"
linearbusinessdata[which((linearbusinessdata$Pooloutdoor =="N") == T),49] <- "2"
colnames(linearbusinessdata)[49] <- "Pooloutdoor1"
linearbusinessdata[which((linearbusinessdata$Spa =="Y") == T),50] <- "1"
linearbusinessdata[which((linearbusinessdata$Spa =="N") == T),50] <- "2"
colnames(linearbusinessdata)[50] <- "Spa1"
linearbusinessdata[which((linearbusinessdata$Valet =="Y") == T),51] <- "1"
linearbusinessdata[which((linearbusinessdata$Valet =="N") == T),51] <- "2"
colnames(linearbusinessdata)[51] <- "Valet1"
linearbusinessdata[which((linearbusinessdata$Restaurant =="Y") == T),52] <- "1"
linearbusinessdata[which((linearbusinessdata$Restaurant =="N") == T),52] <- "2"
colnames(linearbusinessdata)[52] <- "Restaurant1"
linearbusinessdata[which((linearbusinessdata$Businesscenter =="Y") == T),53] <- "1"
linearbusinessdata[which((linearbusinessdata$Businesscenter =="N") == T),53] <- "2"
colnames(linearbusinessdata)[53] <- "Businesscenter1"
linearbusinessdata[which((linearbusinessdata$Bellstaff =="Y") == T),54] <- "1"
linearbusinessdata[which((linearbusinessdata$Bellstaff =="N") == T),54] <- "2"
colnames(linearbusinessdata)[54] <- "Bellstaff1"

linearbusinessdata$GroupvsFit1 <- as.numeric(linearbusinessdata$GroupvsFit1)
linearbusinessdata$Bellstaff1 <- as.numeric(linearbusinessdata$Bellstaff1)
linearbusinessdata$Gender1 <- as.numeric(linearbusinessdata$Gender1)
linearbusinessdata$Boutique1 <- as.numeric(linearbusinessdata$Boutique1)
linearbusinessdata$Casino1 <- as.numeric(linearbusinessdata$Casino1)
linearbusinessdata$Conference1 <- as.numeric(linearbusinessdata$Conference1)
linearbusinessdata$Businesscenter1 <- as.numeric(linearbusinessdata$Businesscenter1)
linearbusinessdata$Drycleaning1 <- as.numeric(linearbusinessdata$Drycleaning1)
linearbusinessdata$Elevators1 <- as.numeric(linearbusinessdata$Elevators1)
linearbusinessdata$Fitnesscenter1 <- as.numeric(linearbusinessdata$Fitnesscenter1)
linearbusinessdata$Golf1 <- as.numeric(linearbusinessdata$Golf1)
linearbusinessdata$Indoorpool1 <- as.numeric(linearbusinessdata$Indoorpool1)
linearbusinessdata$Pooloutdoor1 <- as.numeric(linearbusinessdata$Pooloutdoor1)
linearbusinessdata$Spa1 <- as.numeric(linearbusinessdata$Spa1)
linearbusinessdata$Valet1 <- as.numeric(linearbusinessdata$Valet1)
linearbusinessdata$Restaurant1 <- as.numeric(linearbusinessdata$Restaurant1)
linearbusinessdata$Laundry1 <- as.numeric(linearbusinessdata$Laundry1)
linearbusinessdata$Gender1 <- as.numeric(linearbusinessdata$Gender1)

linearbusinessdata <- na.omit(linearbusinessdata)


#for all 3
linearmodelbusiness <-lm(Likelihood~Bellstaff1+GroupvsFit1+Gender1+Boutique1+Casino1+Conference1+Businesscenter1+Drycleaning1+Elevators1+Fitnesscenter1+Golf1+Indoorpool1+Pooloutdoor1+Spa1+Restaurant1+Valet1+Laundry1+Minibar1+Roomrating+Hotelcondition+Staffcare+Internetcare+CustomerServ+Tranquility, data=linearbusinessdata) 
summary(linearmodelbusiness)


#For male
linearmodelbusiness <-lm(Likelihood~Bellstaff1+GroupvsFit1+Casino1+Businesscenter1+Elevators1+Pooloutdoor1+Spa1+Laundry1+Roomrating+Hotelcondition+Staffcare+Internetcare+CustomerServ+Tranquility+Minibar1, data=linearbusinessdata) 
summary(linearmodelbusiness)

#For Female
linearmodelbusiness <-lm(Likelihood~GroupvsFit1+Casino1+Spa1+Valet1+Laundry1+Roomrating+Hotelcondition+Staffcare+Internetcare+CustomerServ+Tranquility, data=linearbusinessdata) 
summary(linearmodelbusiness)

#For overall
linearmodelbusiness <-lm(Likelihood~GroupvsFit1+Casino1+Businesscenter1+Pooloutdoor1+Spa1+Laundry1+Roomrating+Hotelcondition+Staffcare+Internetcare+CustomerServ+Tranquility, data=linearbusinessdata) 
summary(linearmodelbusiness)

#Plotting the graph
ggplot(linearbusinessdata, aes(x=linearbusinessdata$Bellstaff1+linearbusinessdata$GroupvsFit1+linearbusinessdata$Gender1+linearbusinessdata$Boutique1+linearbusinessdata$Casino1+linearbusinessdata$Conference1+linearbusinessdata$Businesscenter1+linearbusinessdata$Drycleaning1+linearbusinessdata$Elevators1+linearbusinessdata$Fitnesscenter1+linearbusinessdata$Golf1+linearbusinessdata$Indoorpool1+linearbusinessdata$Pooloutdoor1+linearbusinessdata$Spa1+linearbusinessdata$Restaurant1+linearbusinessdata$Valet1+linearbusinessdata$Roomrating+Hotelcondition+linearbusinessdata$Staffcare+linearbusinessdata$Internetcare+linearbusinessdata$CustomerServ+linearbusinessdata$Tranquility, y=linearbusinessdata$Likelihood, color=linearbusinessdata$NPStype)) + geom_smooth(method = "lm") +  ylab("Recommendation Rating") +  xlab(" Amenities and Ratings metric") +  ggtitle(" Effect of Amenties and rating on Likelihood to recommend on Feale business customers ")

linearmodelbusiness <-lm(Likelihood~GroupvsFit1+Casino1+Businesscenter1+Pooloutdoor1+Elevators1+Spa1+Roomrating+Hotelcondition+Staffcare+Internetcare+CustomerServ+Tranquility, data=linearbusinessdata) 
summary(linearmodelbusiness)
ggplot(linearbusinessdata, aes(x=linearbusinessdata$GroupvsFit1+linearbusinessdata$Casino1+linearbusinessdata$Businesscenter1+linearbusinessdata$Elevators1+linearbusinessdata$Pooloutdoor1+linearbusinessdata$Spa1+linearbusinessdata$Roomrating+Hotelcondition+linearbusinessdata$Staffcare+linearbusinessdata$Internetcare+linearbusinessdata$CustomerServ+linearbusinessdata$Tranquility, y=linearbusinessdata$Likelihood, color=linearbusinessdata$NPStype)) + geom_smooth(method = "lm") +  ylab("Recommendation Rating") +  xlab(" Amenities and Ratings metric") +  ggtitle(" Effect of Amenties and rating on Likelihood to recommend on Male business customers")

linearmodelbusiness <-lm(Likelihood~Roomrating+Hotelcondition+Staffcare+Internetcare+CustomerServ+Tranquility, data=linearbusinessdata) 
summary(linearmodelbusiness)
ggplot(linearbusinessdata, aes(x=linearbusinessdata$Roomrating+Hotelcondition+linearbusinessdata$Staffcare+linearbusinessdata$Internetcare+linearbusinessdata$CustomerServ+linearbusinessdata$Tranquility, y=linearbusinessdata$Likelihood, color=linearbusinessdata$NPStype)) + geom_smooth(method = "lm") +  ylab("Recommendation Rating") +  xlab(" Amenities and Ratings metric") +  ggtitle(" Effect of Amenties and rating on Likelihood to recommend on Male business customers")








