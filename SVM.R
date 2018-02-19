
#SVM

svmdata <-  newdata
svmdata1 <- svmdata[(svmdata$HotelCountry == 'United States' & svmdata$Purpose!="LEISURE" & svmdata$Gender!='Male') ,]


svmdata1 <- data.frame(svmdata1)
svmdata1$NPSgoal <- NULL
svmdata1$HotelSub <- NULL
svmdata1$HotelCountry <- NULL
svmdata1$HotelState <- NULL
svmdata1$HotelCity <- NULL
svmdata1$HotelNameL <- NULL
svmdata1$Satisfaction <- NULL
svmdata1 <- na.omit(svmdata1)
rownames(svmdata1) <- 1:nrow(svmdata1)

svmdata1$Likelihood <- as.character(svmdata1$Likelihood)
svmdata1$Roomrating <- as.character(svmdata1$Roomrating)
svmdata1$Tranquility <- as.character(svmdata1$Tranquility)
svmdata1$Hotelcondition <- as.character(svmdata1$Hotelcondition)
svmdata1$CustomerServ <- as.character(svmdata1$CustomerServ)
svmdata1$Internetcare <- as.character(svmdata1$Internetcare)

svmdata1[which(svmdata1$Likelihood=="1" | svmdata1$Likelihood=="2" | svmdata1$Likelihood=="3" | svmdata1$Likelihood=="4"), 6] <- "Low"
svmdata1[which(svmdata1$Likelihood=="5" | svmdata1$Likelihood=="6" | svmdata1$Likelihood=="7"), 6] <- "Med"
svmdata1[which(svmdata1$Likelihood=="8" | svmdata1$Likelihood=="9" | svmdata1$Likelihood=="10"), 6]<- "High"

svmdata1[which(svmdata1$Roomrating=="1" | svmdata1$Roomrating=="2" | svmdata1$Roomrating=="3" | svmdata1$Roomrating=="4"), 7] <- "Low"
svmdata1[which(svmdata1$Roomrating=="5" | svmdata1$Roomrating=="6" | svmdata1$Roomrating=="7"), 7] <- "Med"
svmdata1[which(svmdata1$Roomrating=="8" | svmdata1$Roomrating=="9" | svmdata1$Roomrating=="10"), 7]<- "High"

svmdata1[which(svmdata1$Tranquility=="1" | svmdata1$Tranquility=="2" | svmdata1$Tranquility=="3" | svmdata1$Tranquility=="4"), 8] <- "Low"
svmdata1[which(svmdata1$Tranquility=="5" | svmdata1$Tranquility=="6" | svmdata1$Tranquility=="7"), 8] <- "Med"
svmdata1[which(svmdata1$Tranquility=="8" | svmdata1$Tranquility=="9" | svmdata1$Tranquility=="10"), 8]<- "High"

svmdata1[which(svmdata1$Hotelcondition=="1" | svmdata1$Hotelcondition=="2" | svmdata1$Hotelcondition=="3" | svmdata1$Hotelcondition=="4"), 9] <- "Low"
svmdata1[which(svmdata1$Hotelcondition=="5" | svmdata1$Hotelcondition=="6" | svmdata1$Hotelcondition=="7"), 9] <- "Med"
svmdata1[which(svmdata1$Hotelcondition=="8" | svmdata1$Hotelcondition=="9" | svmdata1$Hotelcondition=="10"), 9]<- "High"

svmdata1[which(svmdata1$CustomerServ=="1" | svmdata1$CustomerServ=="2" | svmdata1$CustomerServ=="3" | svmdata1$CustomerServ=="4"), 10] <- "Low"
svmdata1[which(svmdata1$CustomerServ=="5" | svmdata1$CustomerServ=="6" | svmdata1$CustomerServ=="7"), 10] <- "Med"
svmdata1[which(svmdata1$CustomerServ=="8" | svmdata1$CustomerServ=="9" | svmdata1$CustomerServ=="10"), 10]<- "High"

svmdata1[which(svmdata1$Staffcare=="1" | svmdata1$Staffcare=="2" | svmdata1$Staffcare=="3" | svmdata1$Staffcare=="4"), 11] <- "Low"
svmdata1[which(svmdata1$Staffcare=="5" | svmdata1$Staffcare=="6" | svmdata1$Staffcare=="7"), 11] <- "Med"
svmdata1[which(svmdata1$Staffcare=="8" | svmdata1$Staffcare=="9" | svmdata1$Staffcare=="10"), 11]<- "High"

svmdata1[which(svmdata1$Internetcare=="1" | svmdata1$Internetcare=="2" | svmdata1$Internetcare=="3" | svmdata1$Internetcare=="4"), 12] <- "Low"
svmdata1[which(svmdata1$Internetcare=="5" | svmdata1$Internetcare=="6" | svmdata1$Internetcare=="7"), 12] <- "Med"
svmdata1[which(svmdata1$Internetcare=="8" | svmdata1$Internetcare=="9" | svmdata1$Internetcare=="10"), 12]<- "High"

svmdata1 <-svmdata1[!(svmdata1$GroupVFit==""), ]
svmdata1 <-svmdata1[!(svmdata1$Gender==""), ]
svmdata1 <-svmdata1[!(svmdata1$Age==""), ]
svmdata1 <-svmdata1[!(svmdata1$Valet==""), ]
svmdata1 <-svmdata1[!(svmdata1$Spa==""), ]
svmdata1 <-svmdata1[!(svmdata1$Restaurant==""), ]
svmdata1 <-svmdata1[!(svmdata1$Pooloutdoor==""), ]
svmdata1 <-svmdata1[!(svmdata1$Indoorpool==""), ]
svmdata1 <-svmdata1[!(svmdata1$Minibar==""), ]
svmdata1 <-svmdata1[!(svmdata1$Laundry==""), ]
svmdata1 <-svmdata1[!(svmdata1$Golf==""), ]
svmdata1 <-svmdata1[!(svmdata1$Fitnesscenter==""), ]
svmdata1 <-svmdata1[!(svmdata1$Bellstaff==""), ]
svmdata1 <-svmdata1[!(svmdata1$Boutique==""), ]
svmdata1 <-svmdata1[!(svmdata1$Businesscenter==""), ]
svmdata1 <-svmdata1[!(svmdata1$Casino==""), ]
svmdata1 <-svmdata1[!(svmdata1$Conference==""), ]
svmdata1 <-svmdata1[!(svmdata1$Drycleaning==""), ]
svmdata1 <-svmdata1[!(svmdata1$Elevators==""), ]
svmdata1 <-svmdata1[!(svmdata1$Internetcare==""), ]
svmdata1 <-svmdata1[!(svmdata1$Purpose==""), ]
svmdata1 <-svmdata1[!(svmdata1$Gcountry==""), ]
svmdata1 <-svmdata1[!(svmdata1$Gender==""), ]
svmdata1 <-svmdata1[!(svmdata1$Likelihood==""), ]
svmdata1 <-svmdata1[!(svmdata1$Roomrating==""), ]
svmdata1 <-svmdata1[!(svmdata1$Tranquility==""), ]
svmdata1 <-svmdata1[!(svmdata1$Hotelcondition==""), ]
svmdata1 <-svmdata1[!(svmdata1$CustomerServ==""), ]
svmdata1 <-svmdata1[!(svmdata1$Staffcare==""), ]


svmdata1 <- svmdata1[(svmdata1$NPStype == 'Promoter' | svmdata1$NPStype == "Detractor" ),]
svmdata1 <-svmdata1[!(svmdata1$NPStype==""), ]
svmdata1$NPStype <- as.factor(as.character(svmdata1$NPStype))
svmdata1$Internetcare <- as.factor(as.character(svmdata1$Internetcare))
svmdata1$Likelihood <- as.factor(as.character(svmdata1$Likelihood))
svmdata1$Roomrating <- as.factor(as.character(svmdata1$Roomrating))
svmdata1$Tranquility <- as.factor(as.character(svmdata1$Tranquility))
svmdata1$Hotelcondition <- as.factor(as.character(svmdata1$Hotelcondition))
svmdata1$CustomerServ <- as.factor(as.character(svmdata1$CustomerServ))
svmdata1$Staffcare <- as.factor(as.character(svmdata1$Staffcare))




svmdata1 <- svmdata1[sample(nrow(svmdata1), 30000), ]

randIndex <- sample(1:dim(svmdata1)[1])
cutPoint2_3 <- floor(2 * dim(svmdata1)[1]/3)
cutPoint2_3

trainData <- svmdata1[randIndex[1:cutPoint2_3],]
testData <- svmdata1[randIndex[(cutPoint2_3+1):dim(svmdata1)[1]],]

accuracysvm <- function(abc) {
  a=0
  i=1
  for(i in 1:nrow(abc)){
    ifelse((abc$testData.NPStype[i]==abc$testData.svmpred[i]),(a=a+1),(a=a+0))
    i=i+1
  }
  return(a/nrow(abc))
}
accuracyksvm <- function(abc) {
  a=0
  i=1
  for(i in 1:nrow(abc)){
    ifelse((abc$testData.NPStype[i]==abc$testData.ksvmpred[i]),(a=a+1),(a=a+0))
    i=i+1
  }
  return(a/nrow(abc))
}
accuracynb <- function(abc) {
  a=0
  i=1
  for(i in 1:nrow(abc)){
    ifelse((abc$testData.NPStype[i]==abc$testData.nbpred[i]),(a=a+1),(a=a+0))
    i=i+1
  }
  return(a/nrow(abc))
}

#For all SVM
svmoutput <- svm(NPStype~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare + Bellstaff+ Boutique + Casino+Conference+Businesscenter+Drycleaning+Elevators+Fitnesscenter+Golf+Indoorpool+Pooloutdoor+Spa+Restaurant+Valet+Laundry+Minibar, data=trainData, kernel = "radial",kpar="automatic",C=5)
svmoutput
svmpred <- predict(svmoutput, testData)

testData$svmpred <- svmpred

comparisonTable <- data.frame(testData$NPStype,testData$svmpred)
table(comparisonTable)
accuracysvm(comparisonTable)

#For all selected
svmoutput <- svm(NPStype~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare  + Casino+Businesscenter+Indoorpool+Pooloutdoor+Spa+Laundry, data=trainData, kernel = "radial",kpar="automatic",C=5)
svmoutput
svmpred <- predict(svmoutput, testData)

testData$svmpred <- svmpred

comparisonTable <- data.frame(testData$NPStype,testData$svmpred)
table(comparisonTable)
accuracysvm(comparisonTable)


#for male  selected
svmoutput <- svm(NPStype~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare + Bellstaff+ Casino+Businesscenter+Elevators+Pooloutdoor+Spa+Laundry, data=trainData, kernel = "radial",kpar="automatic",C=5)
svmoutput
svmpred <- predict(svmoutput, testData)

testData$svmpred <- svmpred

comparisonTable <- data.frame(testData$NPStype,testData$svmpred)
table(comparisonTable)
accuracysvm(comparisonTable)

#For female selected
svmoutput <- svm(NPStype~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare+Valet + Casino+Spa+Laundry, data=trainData, kernel = "radial",kpar="automatic",C=5)
svmoutput
svmpred <- predict(svmoutput, testData)

testData$svmpred <- svmpred

comparisonTable <- data.frame(testData$NPStype,testData$svmpred)
table(comparisonTable)
accuracysvm(comparisonTable)

#1 20%error Business - 8% Leisure- 20%
svmoutput <- svm(NPStype~Hotelcondition+Tranquility+Roomrating+Staffcare, data=trainData, kernel = "radial",kpar="automatic",C=5)
svmoutput
svmpred <- predict(svmoutput, testData)

testData$svmpred <- svmpred

comparisonTable <- data.frame(testData$NPStype,testData$svmpred)
table(comparisonTable)
accuracysvm(comparisonTable)

#For all
ksvmoutput <- ksvm(NPStype~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare + Bellstaff+ Boutique + Casino+Conference+Businesscenter+Drycleaning+Elevators+Fitnesscenter+Golf+Indoorpool+Pooloutdoor+Spa+Restaurant+Valet+Laundry+Minibar, data=trainData, kernel = "rbfdot",kpar="automatic",C=5,cross=2, prob.model=T)
ksvmoutput
ksvmpred <- predict(ksvmoutput, testData)

testData$ksvmpred <- ksvmpred

comparisonTable1 <- data.frame(testData$NPStype,testData$ksvmpred)
table(comparisonTable1)
accuracyksvm(comparisonTable1)

#For all
nboutput <- naiveBayes(NPStype~Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare + Bellstaff+ Boutique + Casino+Conference+Businesscenter+Drycleaning+Elevators+Fitnesscenter+Golf+Indoorpool+Pooloutdoor+Spa+Restaurant+Valet+Laundry+Minibar, data=trainData)
nbpred <- predict(nboutput, testData)

testData$nbpred <- nbpred

comparisonTable <- data.frame(testData$NPStype,testData$nbpred)
table(comparisonTable)
accuracynb(comparisonTable)



rf_Model <- randomForest(NPStype ~ Hotelcondition+Tranquility+Roomrating+Staffcare, data=trainData)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

#2 

svmoutput <- svm(NPStype~GroupVFit+Businesscenter+Pooloutdoor+Drycleaning+Elevators+Casino+Spa+Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare, data=trainData, kernel = "radial",kpar="automatic",C=5)
svmoutput
svmpred <- predict(svmoutput, testData)

testData$svmpred <- svmpred

comparisonTable <- data.frame(testData$NPStype,testData$svmpred)
table(comparisonTable)
accuracysvm(comparisonTable)


ksvmoutput
ksvmpred <- predict(ksvmoutput, testData)

testData$ksvmpred <- ksvmpred

comparisonTable1 <- data.frame(testData$NPStype,testData$ksvmpred)
table(comparisonTable1)
accuracyksvm(comparisonTable1)


nboutput <- naiveBayes(NPStype~Hotelcondition+Tranquility+Roomrating+Staffcare, data=trainData)
nbpred <- predict(nboutput, testData)
testData$nbpred <- nbpred
comparisonTable <- data.frame(testData$NPStype,testData$nbpred)
table(comparisonTable)
accuracynb(comparisonTable)

rf_Model <- randomForest(NPStype ~ Bellstaff+Gender+Boutique+GroupVFit+Businesscenter+Pooloutdoor+Drycleaning+Elevators+Casino+Spa+Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare, data=trainData)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

  
rf_Model <- randomForest(NPStype ~ Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare, data=svmdata1)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip()


#RF for All
rf_Model <- randomForest(NPStype ~ Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare + Bellstaff+ Boutique + Casino+Conference+Businesscenter+Drycleaning+Elevators+Fitnesscenter+Golf+Indoorpool+Pooloutdoor+Spa+Restaurant+Valet+Laundry+Minibar, data=svmdata1)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip()


#For male
rf_Model <- randomForest(NPStype ~ Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare + Bellstaff+ Casino+Businesscenter+Elevators+Pooloutdoor+Spa+Laundry, data=svmdata1)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip()


#For female
rf_Model <- randomForest(NPStype ~ Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare+Valet + Casino+Spa+Laundry, data=svmdata1)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip()

#For overall busnesss
rf_Model <- randomForest(NPStype ~ Roomrating + Tranquility + Hotelcondition + CustomerServ+Staffcare+Internetcare  + Casino+Businesscenter+Indoorpool+Pooloutdoor+Spa+Laundry, data=svmdata1)

plot(rf_Model, ylim=c(0,0.69))
legend('topright', colnames(rf_Model$err.rate), col=1:3, fill=1:3)

importance    <- importance(rf_Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))


ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') + coord_flip()

