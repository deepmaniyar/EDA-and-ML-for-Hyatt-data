
#Overall NPS type distribution
overallpromoter <- ((length(which(newdata$NPStype=='Promoter')==T))/nrow(newdata))*100
overalldetractor <- ((length(which(newdata$NPStype=='Detractor')==T))/nrow(newdata))*100
overallpassive <- ((length(which(newdata$NPStype=='Passive')==T))/nrow(newdata))*100
overall <- t(overall)
overall <- as.matrix(c(overallpromoter,overallpassive,overalldetractor))
colnames(overall) <- c("Promoter", "Passive","Detractor")

barplot(overall,beside = T, main="Distribution of NPS type in the world", xlab = "NPS type", ylab= "Percentage",col = c("red", "orange","green"))

overallpromoter <- ((length(which(usdata$NPStype=='Promoter')==T))/nrow(usdata))*100
overalldetractor <- ((length(which(usdata$NPStype=='Detractor')==T))/nrow(usdata))*100
overallpassive <- ((length(which(usdata$NPStype=='Passive')==T))/nrow(usdata))*100

overall <- as.matrix(c(overallpromoter,overallpassive,overalldetractor))
overall <- t(overall)
colnames(overall) <- c("Promoter", "Passive","Detractor")

barplot(overall,beside = T, main="Distribution of NPS type in the US", xlab = "NPS type", ylab= "Percentage",col = c("red", "orange","green"))


usbusinesscustdist <- ((length(which(usdata$Purpose=='BUSINESS')==T))/nrow(usdata))*100
usleisurecustdist <- ((length(which(usdata$Purpose=='LEISURE')==T))/nrow(usdata))*100

uscustdist <- as.matrix(c(usbusinesscustdist,usleisurecustdist))
uscustdist <- t(uscustdist)
colnames(uscustdist) <- c("Business Customers", "Leisure Customers")

barplot(uscustdist,beside = T, main="Purpose distribution of US customer", xlab = "Purpose", ylab= "Percentage",col = c("red", "orange"))

malenpsdistpromoter <- ((length(which(usdata$Purpose=='LEISURE'& usdata$NPStype=='Promoter')==T))/(length(which(usdata$Purpose=='LEISURE')==T)))*100
malenpsdistpassive <- ((length(which(usdata$Purpose=='LEISURE'& usdata$NPStype=='Passive')==T))/(length(which(usdata$Purpose=='LEISURE')==T)))*100
malenpsdistdetractor <- ((length(which(usdata$Purpose=='LEISURE'& usdata$NPStype=='LEISURE')==T))/(length(which(usdata$Purpose=='BUSINESS')==T)))*100


malenpsdist <- as.matrix(c(malenpsdistpromoter,malenpsdistpassive,malenpsdistdetractor))
malenpsdist <- t(malenpsdist)
colnames(malenpsdist) <- c("Promoter", "Passive","Detractor")

barplot(malenpsdist,beside = T, main="Distribution of Leisure customer based on NPS type in the US", xlab = "NPS type", ylab= "Percentage",col = c("red", "orange","green"))

femalenpsdistpromoter <- ((length(which(usdata$Gender=='Female'& usdata$NPStype=='Promoter')==T))/(length(which(usdata$Gender=='Female')==T)))*100
femalenpsdistpassive <- ((length(which(usdata$Gender=='Female'& usdata$NPStype=='Passive')==T))/(length(which(usdata$Gender=='Female')==T)))*100
femalenpsdistdetractor <- ((length(which(usdata$Gender=='Female'& usdata$NPStype=='Detractor')==T))/(length(which(usdata$Gender=='Female')==T)))*100


femalenpsdist <- as.matrix(c(femalenpsdistpromoter,femalenpsdistpassive,femalenpsdistdetractor))
femalenpsdist <- t(femalenpsdist)
colnames(femalenpsdist) <- c("Promoter", "Passive","Detractor")

barplot(femalenpsdist,beside = T, main="Distribution of female customers NPS type in the US", xlab = "NPS type", ylab= "Percentage",col = c("red", "orange","green"))


#Gender distribution of male and female in business with their NPStype
#%female promoters
femalebusinesspromoter <- (length(which(businessdata$Gender=='Female'& businessdata$NPStype=='Promoter')==T))/length(which(businessdata$Gender=="Female")==T)*100
#%female detractor
femalebusinessdetractor <- (length(which(businessdata$Gender=='Female'& businessdata$NPStype=='Detractor')==T))/length(which(businessdata$Gender=="Female")==T)*100
#%Female passive
femalebusinesspassive <- (length(which(businessdata$Gender=='Female'& businessdata$NPStype=='Passive')==T))/length(which(businessdata$Gender=="Female")==T)*100
#% male detractor
malebusinesdetractor <- (length(which(businessdata$Gender=='Male'& businessdata$NPStype=='Detractor')==T))/length(which(businessdata$Gender=="Male")==T)*100
#%male promoter
malebusinesspromoter  <- (length(which(businessdata$Gender=='Male'& businessdata$NPStype=='Promoter')==T))/length(which(businessdata$Gender=="Male")==T)*100
#%male passive
malebusinesspassive  <- (length(which(businessdata$Gender=='Male'& businessdata$NPStype=='Passive')==T))/length(which(businessdata$Gender=="Male")==T)*100

businessnpstype <- matrix(c(femalebusinesspromoter,malebusinesspromoter,femalebusinessdetractor, malebusinesdetractor, femalebusinesspassive,malebusinesspassive), nrow=2)
colnames(businessnpstype) <- c("Promoter", "Detractor","Passive")
rownames(businessnpstype) <- c("Female", "Male")

barplot(businessnpstype,beside = T, main="Genderwise NPStype distribution for US business customers", xlab = "NPS type", ylab= "Percentage",col = c("darkblue","red"),legend = rownames(businessnpstype))
