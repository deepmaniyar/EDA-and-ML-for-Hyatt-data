
library(readxl)
library(RCurl)
library(gdata)
library(ggplot2)
library(arules)
library(arulesViz)
library(ggplot2)
library(ggmap)
library(stringr)
library(rworldmap)
library(googleVis)
library(plotrix)
library(kernlab)
library(e1071)

hoteldata1 <- read.csv("~/Desktop/IST687-data/out-201402.csv")
hoteldata1<- hoteldata1[!is.na(hoteldata1$Likelihood_Recommend_H),] 
hoteldata2 <- read.csv("~/Desktop/IST687-data/out-201403.csv")
hoteldata2<- hoteldata2[!is.na(hoteldata2$Likelihood_Recommend_H),]
hoteldata3 <-read.csv("~/Desktop/IST687-data/out-201404.csv")
hoteldata3<- hoteldata3[!is.na(hoteldata3$Likelihood_Recommend_H),]
hoteldata4 <-read.csv("~/Desktop/IST687-data/out-201405.csv")
hoteldata4<- hoteldata4[!is.na(hoteldata4$Likelihood_Recommend_H),]
hoteldata5 <-read.csv("~/Desktop/IST687-data/out-201406.csv")
hoteldata5<- hoteldata5[!is.na(hoteldata5$Likelihood_Recommend_H),]
hoteldata6 <-read.csv("~/Desktop/IST687-data/out-201407.csv")
hoteldata6<- hoteldata6[!is.na(hoteldata6$Likelihood_Recommend_H),]
hoteldata7 <-read.csv("~/Desktop/IST687-data/out-201408.csv")
hoteldata7<- hoteldata7[!is.na(hoteldata7$Likelihood_Recommend_H),]
hoteldata8 <-read.csv("~/Desktop/IST687-data/out-201409.csv")
hoteldata8<- hoteldata8[!is.na(hoteldata8$Likelihood_Recommend_H),]
hoteldata9 <-read.csv("~/Desktop/IST687-data/out-201410.csv")
hoteldata9<- hoteldata9[!is.na(hoteldata9$Likelihood_Recommend_H),]
hoteldata10 <-read.csv("~/Desktop/IST687-data/out-201411.csv")
hoteldata10<- hoteldata10[!is.na(hoteldata10$Likelihood_Recommend_H),]
hoteldata11 <-read.csv("~/Desktop/IST687-data/out-201412.csv")
hoteldata11<- hoteldata11[!is.na(hoteldata11$Likelihood_Recommend_H),]
hoteldata12 <-read.csv("~/Desktop/IST687-data/out-201501.csv")
hoteldata12<- hoteldata12[!is.na(hoteldata12$Likelihood_Recommend_H),]



newdata <- rbind(hoteldata1,hoteldata2,hoteldata3,hoteldata4,hoteldata5,hoteldata6,hoteldata7,hoteldata8,hoteldata9,hoteldata10,hoteldata11,hoteldata12)

#subsetting the original data and renaming the columns
newdata = subset (newdata, select = c(POV_CODE_C,GROUPS_VS_FIT_R, GUEST_COUNTRY_R, Gender_H, Age_Range_H, Likelihood_Recommend_H, Overall_Sat_H, Guest_Room_H, Tranquility_H, Condition_Hotel_H, Customer_SVC_H, Staff_Cared_H, Internet_Sat_H,Bell.Staff_PL, Boutique_PL,Business.Center_PL,Casino_PL,Conference_PL, Dry.Cleaning_PL,Elevators_PL,Fitness.Center_PL,Golf_PL,Laundry_PL,Mini.Bar_PL,Pool.Indoor_PL,Pool.Outdoor_PL,Restaurant_PL,Spa_PL,Valet.Parking_PL,Hotel.Name.Long_PL, City_PL,State_PL, Country_PL, Sub.Continent_PL, Guest.NPS.Goal_PL, NPS_Type ) )
colnames(newdata) <- c("Purpose","GroupVFit", "Gcountry", "Gender", "Age", "Likelihood", "Satisfaction", "Roomrating", "Tranquility", "Hotelcondition", "CustomerServ", "Staffcare", "Internetcare","Bellstaff", "Boutique","Businesscenter", "Casino","Conference", "Drycleaning","Elevators", "Fitnesscenter","Golf","Laundry","Minibar","Indoorpool","Pooloutdoor","Restaurant","Spa","Valet","HotelNameL", "HotelCity","HotelState", "HotelCountry", "HotelSub", "NPSgoal", "NPStype")
View(newdata)
newdata$Age <- as.character(newdata$Age)
newdata$Age <- as.factor(newdata$Age)
nausdata$Age <- as.factor(nausdata$Age)
usdata$Age <- as.factor(usdata$Age)


#checking if likelihood to recommend is directly responsible for NPS type or not
check <- subset(newdata, ((newdata$Likelihood=='7' ||newdata$Likelihood=='8') && newdata$NPStype != 'Passive'))
check


#Making a data of only the hotels in the US
usdata <- subset(newdata, HotelCountry=='United States')

#checking how many oberservations stay if i remove all na's
nausdata <- na.omit(usdata)


# #Removing observations where any of the satisfaction to internet care has zero because there will still be a lot of data to analyse from
# nausdata <- usdata[!is.na(usdata$Satisfaction),]
# nausdata <- nausdata[!is.na(nausdata$Roomrating),]
# nausdata <- nausdata[!is.na(nausdata$Tranquility),]
# nausdata <- nausdata[!is.na(nausdata$Hotelcondition),]
# nausdata <- nausdata[!is.na(nausdata$CustomerServ),]
# nausdata <- nausdata[!is.na(nausdata$Staffcare),]
# nausdata <- nausdata[!is.na(nausdata$Internetcare),]

#changing the rownumber
rownames(nausdata) <- 1:nrow(nausdata)

#Depends more on satisfaction than any other parameter, 80%
namodel1<- lm(Likelihood~ Satisfaction, nausdata)
summary(namodel1)
plot(nausdata$NPStype, nausdata$Satisfaction)
abline(namodel1)

#50%
namodel2<- lm(Likelihood~ Roomrating, nausdata)
summary(namodel2)

#35%
namodel3<- lm(Likelihood~ Tranquility, nausdata)
summary(namodel3)

#45%
namodel4<- lm(Likelihood~ Hotelcondition, nausdata)
summary(namodel4)

#47%
namodel5<- lm(Likelihood~ CustomerServ, nausdata)
summary(namodel5)

#40%
namodel6<- lm(Likelihood~ Staffcare, nausdata)
summary(namodel6)

#8%
namodel7<- lm(Likelihood~ Internetcare, nausdata)
summary(namodel7)
abline(namodel7)


#R 65% all without Satisfaction 
#Since satisfaction isnt something dependent on the hotels to improve, proceeding on what satisfaction depends on 
namodel8 <-  lm(Likelihood~ Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare, nausdata)
summary(namodel8)

#seeing how much do business people care about the internet
businessinternet <- nausdata[((nausdata$Purpose=='BUSINESS') & (nausdata$NPStype !='Promoter')),]


#subetting where purpose of visit is business

businessdata <- nausdata[nausdata$Purpose=='BUSINESS',]

businessdata <-businessdata[!(businessdata$Gcountry==""), ]

#high R sq but of lesser importance #80%
bizmodel1<- lm(Likelihood~ Satisfaction, businessdata)
summary(bizmodel1)
plot(bizmodel1)
abline(bizmodel1)

#50%
bizmodel2<- lm(Likelihood~ Roomrating, businessdata)
summary(bizmodel2)

#35%
bizmodel3<- lm(Likelihood~ Tranquility, businessdata)
summary(bizmodel3)

#45%
bizmodel4<- lm(Likelihood~ Hotelcondition, businessdata)
summary(bizmodel4)

#47%
bizmodel5<- lm(Likelihood~ CustomerServ, businessdata)
summary(bizmodel5)

#39%
bizmodel6<- lm(Likelihood~ Staffcare, businessdata)
summary(bizmodel6)

#8%
bizmodel7<- lm(Likelihood~ Internetcare, businessdata)
summary(bizmodel7)

#65%
bizmodel8 <-  lm(Likelihood~ Roomrating + CustomerServ, businessdata)
summary(bizmodel8)

#64%%
bizmodel9<- lm(Likelihood~ Roomrating  + Hotelcondition + CustomerServ+Staffcare+Internetcare, businessdata)
summary(bizmodel9)

#8%
bizmodel10<- lm(Satisfaction~Internetcare, businessdata)
summary(bizmodel10)

#42%
bizmodel11<- lm(Satisfaction~Staffcare, businessdata)
summary(bizmodel12)

#52%
bizmodel12<- lm(Satisfaction~Roomrating, businessdata)
summary(bizmodel12)

#45%
bizmodel13<- lm(Satisfaction~Hotelcondition, businessdata)
summary(bizmodel13)

#42%
bizmodel14<- lm(Satisfaction~Staffcare, businessdata)
summary(bizmodel14)

#38%
bizmodel14<- lm(Satisfaction~Tranquility, businessdata)
summary(bizmodel14)

#50%
bizmodel14<- lm(Satisfaction~CustomerServ, businessdata)
summary(bizmodel14)

#68% 
bizmodel15<- lm(Satisfaction~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare, businessdata)
summary(bizmodel15)

#67%
bizmodel16<- lm(Satisfaction~Roomrating  + Hotelcondition + CustomerServ+Staffcare, businessdata)
summary(bizmodel16)

#66%
bizmodel17<- lm(Satisfaction~Roomrating+CustomerServ, businessdata)
summary(bizmodel17)

leisuredata <- nausdata[nausdata$Purpose=='LEISURE',]

#high R sq but of lesser importance #80%
leimodel1<- lm(Likelihood~ Satisfaction, leisuredata)
summary(leimodel1)

#48%
leimodel2<- lm(Likelihood~ Roomrating, leisuredata)
summary(leimodel2)

#35%
leimodel3<- lm(Likelihood~ Tranquility, leisuredata)
summary(leimodel3)

#45%
leimodel4<- lm(Likelihood~ Hotelcondition, leisuredata)
summary(leimodel4)

#47%
leimodel5<- lm(Likelihood~ CustomerServ, leisuredata)
summary(leimodel5)

#39%
leimodel6<- lm(Likelihood~ Staffcare, leisuredata)
summary(leimodel6)

#8%
leimodel7<- lm(Likelihood~ Internetcare, leisuredata)
summary(leimodel7)

#65%
leimodel8 <-  lm(Likelihood~ Roomrating + CustomerServ, leisuredata)
summary(leimodel8)

#64%%
leimodel9<- lm(Likelihood~ Roomrating  + Hotelcondition + CustomerServ+Staffcare+Internetcare, leisuredata)
summary(leimodel9)

#8%
leimodel10<- lm(Satisfaction~Internetcare, leisuredata)
summary(leimodel10)

#42%
leimodel11<- lm(Satisfaction~Staffcare, leisuredata)
summary(leimodel12)

#52%
leimodel12<- lm(Satisfaction~Roomrating, leisuredata)
summary(leimodel12)

#45%
leimodel13<- lm(Satisfaction~Hotelcondition, leisuredata)
summary(leimodel13)

#42%
leimodel14<- lm(Satisfaction~Staffcare, leisuredata)
summary(leimodel14)

#38%
leimodel14<- lm(Satisfaction~Tranquility, leisuredata)
summary(leimodel14)

#50%
leimodel14<- lm(Satisfaction~CustomerServ, leisuredata)
summary(leimodel14)

#68% 
leimodel15<- lm(Satisfaction~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare, leisuredata)
summary(leimodel15)

#67%
leimodel16<- lm(Satisfaction~Roomrating  + Hotelcondition + CustomerServ+Staffcare, leisuredata)
summary(leimodel16)

#66%
leimodel17<- lm(Satisfaction~Roomrating+CustomerServ, leisuredata)
summary(leimodel17)

NPS <- function(x){
  return((length(which(x$NPStype=='Promoter')==T)/nrow(x)-length(which(x$NPStype=='Detractor')==T)/nrow(x))*100)
}

NPStapply <- function(x){
  return((length(which(x=='Promoter')==T)/length(x)-length(which(x=='Detractor')==T)/length(x))*100)
}

#Avergae NPS per hotel
a <- tapply(nausdata$NPStype, nausdata$HotelNameL, NPStapply)
a <- data.frame(a)
a <- na.omit(a)
colnames(a) <- c("AverageNPS")
a$HotelName <- rownames(a)
rownames(a) <- 1:nrow(a)

a$latlon <- geocode(a$HotelName)
b <- data.frame(a$latlon)
a <- cbind(a,b)
a$latlon<-NULL
a$NPStype <- NA
a[which(a$AverageNPS<70), 5]<-"Detractor"
a[which(a$AverageNPS>=90), 5]<-"Promoter"
a[which(a$AverageNPS<90 & a$AverageNPS>70), 5]<-"Passive"

d1 <- merge(a,nausdata, by.x = c("HotelName"), by.y = c("HotelNameL"), all.x = TRUE)
d1<-d1[!duplicated(d1$HotelName), ]

hotelwithdetails<- merge(a, d1, by.x = c("HotelName"), by.y = c("HotelName"), all.x=T)

forStatesinA<- subset(hotelwithdetails,select= c(HotelName, HotelState))

a <- merge(a,forStatesinA, by.x = c("HotelName"), by.y = c("HotelName"), all.x = TRUE)
a$state <- tolower(a$HotelState)


us <- get_map("us", zoom = 4)
ggmap(us)

maphotelwise <- ggmap(us) + geom_point(data= a, aes(x=a$lon, y=a$lat, colour= factor(NPStype)))
maphotelwise


us <- map_data("state")
mapus1 <- ggplot(a, aes(map_id = state))  
mapus1 <- mapus1 +geom_map(map = us, fill="white", color="black") 
mapus1 <- mapus1 +expand_limits(x = us$long, y = us$lat)
mapus1 <- mapus1 +coord_map()
mapus1
df <- ggplot(a, aes(map_id = state))  
df <- mapus1 +geom_point(aes(x= a$lon,y=a$lat, color=NPStype))
df <- df +expand_limits(x = us$long, y = us$lat)
df <- df +coord_map() + ggtitle("Hotel wise Average NPS")
df


#City wise
x <- tapply(nausdata$NPStype, nausdata$HotelCity, NPStapply)
x <- data.frame(x)
x <- na.omit(x)
colnames(x) <- c("AverageNPS")
x$Hotelcity <- rownames(x)
rownames(x) <- 1:nrow(x)

x$latlon <- geocode(x$Hotelcity)
y <- data.frame(x$latlon)
x <- cbind(x,y)
x$latlon<-NULL
x$NPStype <- NA
x[which(x$AverageNPS<70), 5]<-"Detractor"
x[which(x$AverageNPS>=90), 5]<-"Promoter"
x[which(x$AverageNPS<90 & x$AverageNPS>70), 5]<-"Passive"

d2 <- merge(x,nausdata, by.x = c("Hotelcity"), by.y = c("HotelCity"), all.x = TRUE)
d2<-d2[!duplicated(d2$Hotelcity), ]

citywithdetails<- merge(x, d2, by.x = c("Hotelcity"), by.y = c("Hotelcity"), all.x=T)

forStatesinA<- subset(citywithdetails,select= c(Hotelcity, HotelState))

x <- merge(x,forStatesinA, by.x = c("Hotelcity"), by.y = c("Hotelcity"), all.x = TRUE)
x$state <- tolower(x$HotelState)


us <- get_map("us")

mapcitywise <- ggmap(us) + geom_point(data= x, aes(x=x$lon, y=x$lat, colour= factor(NPStype)))
mapcitywise


us <- map_data("state")
mapus1 <- ggplot(x, aes(map_id = state))  
mapus1 <- mapus1 +geom_map(map = us, fill="white", color="black") 
mapus1 <- mapus1 +expand_limits(x = us$long, y = us$lat)
mapus1 <- mapus1 +coord_map()
mapus1

df1 <- ggplot(x, aes(map_id = state))  
df1 <- mapus1 +geom_point(aes(x= x$lon,y=x$lat, color=NPStype))
df1 <- df1 +expand_limits(x = us$long, y = us$lat)
df1 <- df1 +coord_map() + ggtitle("City wise Average NPS")
df1

#Average NPS state
c <- tapply(nausdata$NPStype, nausdata$HotelState, NPStapply)
c <- data.frame(c)
c <- na.omit(c)
colnames(c) <- c("AverageNPS")
c$State <- rownames(c)
rownames(c) <- 1:nrow(c)
c$state <- tolower(c$State)


c[which(c$AverageNPS<70), 3]<-"Detractor"
c[which(c$AverageNPS>=90), 3]<-"Promoter"
c[which(c$AverageNPS<90 & c$AverageNPS>70), 3]<-"Passive"
colnames(c)[3] <- "NPS"

us <- map_data("state")
mapus<- ggplot(a, aes(map_id = state))  
mapus <- mapus +geom_map(map = us, fill="white", color="black") 
mapus <- mapus +expand_limits(x = us$long, y = us$lat)
mapus <- mapus +coord_map()
mapus


mapstatewise <- ggplot(c, aes(map_id = state))  
mapstatewise <- mapstatewise +geom_map(map = us, aes(fill=AverageNPS))
mapstatewise <- mapstatewise +expand_limits(x = us$long, y = us$lat)
mapstatewise <- mapstatewise +coord_map() + ggtitle("state wise Average NPS")
mapstatewise

# 



usrules1 <- apriori(subset(usdata,select=c("Roomdescription", "Purpose", "Gcountry","Gender","HotelNameS","NPStype")), parameter = list(support=0.005, confidence=0.5), appearance= list(rhs=c("NPStype=Promoter","NPStype=Detractor","NPStype=Passive"), default="lhs"))
usrules1.sorted <- sort(rules1, by="lift")
inspect(usrules1)
plot(usrules1)

usrules2 <- apriori(subset(usdata,select=c("Roomdescription", "Purpose", "Gcountry","Gender","HotelNameS", "NPStype")), parameter = list(support=0.005,confidence=0.05), appearance= list(rhs=c("NPStype=Detractor"), default="lhs"))
usrules2.sorted <- sort(usrules2, by="lift")
inspect(usrules2)
plot(usrules2)
#10% of the business POV are detractors

usrules3 <- apriori(subset(usdata,select=c("Roomdescription", "Purpose", "Gcountry","Gender","HotelNameS","NPStype")), parameter = list(support=0.005,confidence=0.05), appearance= list(rhs=c("NPStype=Passive"), default="lhs"))
usrules3.sorted <- sort(usrules2, by="lift")
inspect(usrules3)
plot(usrules3)
#16% of the business POV are Passive
#50% sex ratio

usrules4 <- apriori(subset(usdata,select=c("Roomdescription", "Purpose", "Gcountry","Gender","HotelNameS","NPStype")), parameter = list(support=0.005,confidence=0.05), appearance= list(rhs=c("NPStype=Promoter"), default="lhs"))
usrules4.sorted <- sort(usrules4, by="lift")
inspect(usrules4)
plot(usrules4)

usnps <- sum(usdata$Likelihood)/nrow(usdata)
usnps

#Average NPStype per purpose of visit
data <- matrix(c(length(which(businessdata$NPStype=='Promoter')==T),length(which(leisuredata$NPStype=='Promoter')==T),length(which(businessdata$NPStype=='Passive')==T),length(which(leisuredata$NPStype=='Passive')==T),length(which(businessdata$NPStype=='Detractor')==T),length(which(leisuredata$NPStype=='Detractor')==T)), nrow=2)
colnames(data) <- c("Promoter","Passive", "Detractor")
rownames(data) <- c("Business","Leisure")
barplot(data, beside = T)

#Percentage of above data
percentdata <- matrix(c((length(which(businessdata$NPStype=='Promoter')==T)/nrow(businessdata)),(length(which(leisuredata$NPStype=='Promoter')==T)/nrow(leisuredata)),(length(which(businessdata$NPStype=='Passive')==T)/nrow(businessdata)),(length(which(leisuredata$NPStype=='Passive')==T)/nrow(leisuredata)),(length(which(businessdata$NPStype=='Detractor')==T)/nrow(businessdata)),(length(which(leisuredata$NPStype=='Detractor')==T))/nrow(leisuredata)), nrow=2)
colnames(percentdata) <- c("Promoter","Passive", "Detractor")
rownames(percentdata) <- c("Business","Leisure")
barplot(percentdata, beside = T)

sexpercentdata <- matrix(c((length(which(businessdata$Gender=='Female')==T)/nrow(businessdata)),(length(which(leisuredata$Gender=='Female')==T)/nrow(leisuredata)),(length(which(businessdata$Gender=='Male')==T)/nrow(businessdata)),(length(which(leisuredata$Gender=='Male')==T)/nrow(leisuredata))), nrow=2)
colnames(sexpercentdata) <- c("Female","Male")
rownames(sexpercentdata) <- c("Business","Leisure")
barplot(sexpercentdata, beside = T)

#Gender distribution of male and female in business with their NPStype
#%female promoters
(length(which(businessdata$Gender=='Female'& businessdata$NPStype=='Promoter')==T))/length(which(businessdata$Gender=="Female")==T)
#%female detractor
(length(which(businessdata$Gender=='Female'& businessdata$NPStype=='Detractor')==T))/length(which(businessdata$Gender=="Female")==T)
#%Female passive
(length(which(businessdata$Gender=='Female'& businessdata$NPStype=='Passive')==T))/length(which(businessdata$Gender=="Female")==T)
#% male detractor
(length(which(businessdata$Gender=='Male'& businessdata$NPStype=='Detractor')==T))/length(which(businessdata$Gender=="Male")==T)
#%male promoter
(length(which(businessdata$Gender=='Male'& businessdata$NPStype=='Promoter')==T))/length(which(businessdata$Gender=="Male")==T)
#%male passive
(length(which(businessdata$Gender=='Male'& businessdata$NPStype=='Passive')==T))/length(which(businessdata$Gender=="Male")==T)

#Gender distribution of male and female in leisure with their NPStype
(length(which(leisuredata$Gender=='Female'& leisuredata$NPStype=='Promoter')==T))/length(which(leisuredata$Gender=="Female")==T)

(length(which(leisuredata$Gender=='Female'& leisuredata$NPStype=='Passive')==T))/length(which(leisuredata$Gender=="Female")==T)

(length(which(leisuredata$Gender=='Female'& leisuredata$NPStype=='Detractor')==T))/length(which(leisuredata$Gender=="Female")==T)

(length(which(leisuredata$Gender=='Male'& leisuredata$NPStype=='Promoter')==T))/length(which(leisuredata$Gender=="Male")==T)

(length(which(leisuredata$Gender=='Male'& leisuredata$NPStype=='Passive')==T))/length(which(leisuredata$Gender=="Male")==T)

(length(which(leisuredata$Gender=='Male'& leisuredata$NPStype=='Detractor')==T))/length(which(leisuredata$Gender=="Male")==T)


#Taking a sample and seeing for full data

#Taking a sample and seeing for full us data
mean(replicate(1000, NPS(usdata[sample(nrow(usdata), size= 10000, replace = T), ])))

hist(replicate(1000, NPS(usdata[sample(nrow(usdata), size= 10000, replace = T), ])), main="NPS of all the customers in US",  xlab="NPS")

#Taking a sample and seeing for full na us data
mean(replicate(1000, NPS(nausdata[sample(nrow(nausdata), size= 100000, replace = T), ])))

hist((replicate(1000, NPS(nausdata[sample(nrow(nausdata), size= 10000, replace = T), ]))), main="NPS of all the customers in US",  xlab="NPS")

#Association rules for US data
rules1 <- apriori(subset(usdata,select=c("Purpose", "Gcountry","Gender","HotelNameL", "Age","NPStype")), parameter = list(support=0.005, confidence=0.5), appearance= list(rhs=c("NPStype=Promoter","NPStype=Detractor","NPStype=Passive"), default="lhs"))
rules1.sorted <- sort(rules1, by="lift")
inspect(rules1)
plot(rules1)


rules2 <- apriori(subset(usdata,select=c( "Purpose", "Gcountry","Gender","HotelNameL","Age","NPStype")), parameter = list(support=0.005,confidence=0.05), appearance= list(rhs=c("NPStype=Detractor"), default="lhs"))
rules2.sorted <- sort(rules2, by="support", decreasing = T)
inspect(rules2.sorted)
plot(rules2)


rules3 <- apriori(subset(usdata,select=c("Roomdescription", "Purpose", "Gcountry","Gender","HotelNameL","Age","NPStype")), parameter = list(support=0.005,confidence=0.05), appearance= list(rhs=c("NPStype=Passive"), default="lhs"))
rules3.sorted <- sort(rules3, by="support")
inspect(rules3)
plot(rules3)


#Rule related to amenities
rules4 <- apriori(subset(usdata,select=c("Purpose", "Gcountry","Gender","Age","Businesscenter", "Conference","Golf","Minibar","NPStype")), parameter = list(support=0.05,confidence=0.5), appearance= list(rhs=c("NPStype=Promoter","NPStype=Passive","NPStype=Detractor"), default="lhs"))
rules4.sorted <- sort(rules4, by="support")
inspect(head(rules4.sorted,100))
inspect(rules4.sorted)
plot(rules4.sorted)

#Business rules
rules5 <- apriori(subset(usdata,select=c("Purpose", "Gcountry","Gender","Age","Businesscenter","Drycleaning", "Conference","Golf","Minibar","NPStype")), parameter = list(support=0.05,confidence=0.05), appearance= list(rhs=c("NPStype=Detractor"), default="lhs"))
rules5.sorted <- sort(rules5, by="lift")
inspect(head(rules5.sorted,10))
inspect(rules5.sorted)
plot(rules5.sorted)

#Leisure rules 
rules6 <- apriori(subset(usdata,select=c("Purpose", "Gcountry","Gender","Age","Drycleaning","Indoorpool","Pooloutdoor","Casino", "Fitnesscenter","Restaurant","Spa","Golf","Minibar","NPStype")), parameter = list(support=0.05,confidence=0.05), appearance= list(rhs=c("Purpose==BUSINESS", "NPStype=Detractor"), default="lhs"))
rules6.sorted <- sort(rules6, by="lift")
inspect(head(rules6.sorted,100))
inspect(rules6.sorted)
plot(rules6.sorted)

plot((head(rules6.sorted,50)), method="paracoord", control=list(reorder=TRUE))

#plot(rules5.sorted, method="graph", control=list(type="items"))

#women age wise detractors
uswomendetractors <- usdata[(usdata$Gender=='Female' & usdata$NPStype=='Detractor'),]
ageuswomendetractors <- uswomendetractors[uswomendetractors!=" ",]

agewomendetractors3D <- data.frame(tapply(ageuswomendetractors$Age, ageuswomendetractors$Age, length))
colnames(agewomendetractors3D) <- c("Count")
agewomendetractors3D$Age <- rownames(agewomendetractors3D)
rownames(agewomendetractors3D) <- 1:nrow(agewomendetractors3D)

pie(agewomendetractors3D$Count, labels = agewomendetractors3D$Age,explode=0.1, col=rainbow(length(agewomendetractors3D$Age)))

#men age wise detractor 
usmendetractors <- usdata[(usdata$Gender=='Male' & usdata$NPStype=='Detractor'),]
ageusmendetractors <- usmendetractors[usmendetractors!=" ",]

agemendetractors3D <- data.frame(tapply(ageusmendetractors$Age, ageusmendetractors$Age, length))
colnames(agemendetractors3D) <- c("Count")
agemendetractors3D$Age <- rownames(agemendetractors3D)
rownames(agemendetractors3D) <- 1:nrow(agemendetractors3D)

pie(agemendetractors3D$Count, labels = agemendetractors3D$Age,explode=0.1, col=rainbow(length(agemendetractors3D$Age)))

#Overall agewise detractors
usdetractors <- usdata[(usdata$NPStype=='Detractor'),]
ageusdetractors <- usdetractors[usdetractors!=" ",]
agedetractors3D <- data.frame(tapply(ageusdetractors$Age, ageusdetractors$Age, length))
colnames(agedetractors3D) <- c("Count")
agedetractors3D$Age <- rownames(agedetractors3D)
rownames(agedetractors3D) <- 1:nrow(agedetractors3D)

pie3D(agedetractors3D$Count, labels = agedetractors3D$Age,explode=0.1, col=rainbow(length(agedetractors3D$Age)),labels = paste(round(prop.table(table(agedetractors3D$Age))*100)))
