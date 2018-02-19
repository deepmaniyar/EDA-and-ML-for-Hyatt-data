#Plotting NPS of each coutry on the worldmap
worldnps <- tapply(newdata$NPStype, newdata$HotelCountry, NPStapply)
worldnps<- data.frame(worldnps)

colnames(worldnps) <- c("AverageNPS")
worldnps$Hotelcountry <- rownames(worldnps)
rownames(worldnps) <- 1:nrow(worldnps)
worldnps$NPStype <- NA
worldnps[which(worldnps$AverageNPS<70), 3]<-"Detractor"
worldnps[which(worldnps$AverageNPS>=90), 3]<-"Promoter"
worldnps[which(worldnps$AverageNPS<90 & worldnps$AverageNPS>=70), 3]<-"Passive"

worldnps$latlon <- geocode(worldnps$Hotelcountry)
worldlatlon <- data.frame(worldnps$latlon)
worldnps <- cbind(worldnps,worldlatlon)
worldnps$latlon <- NULL

mp <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot(data = worldnps,aes(x=worldnps$lon, y=worldnps$lat, color=worldnps$NPStype, label=Hotelcountry)) + mapWorld

#Now Layer the cities on top
mp <- mp+ geom_point()
mp <- mp +  geom_text(aes(label=Hotelcountry),hjust=0,vjust=0)
mp


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
a <- na.omit(a)
a$NPStype <- NA
a[which(a$AverageNPS<70), 6]<-"Detractor"
a[which(a$AverageNPS>=90), 6]<-"Promoter"
a[which(a$AverageNPS<90 & a$AverageNPS>70), 6]<-"Passive"

d1 <- merge(a,nausdata, by.x = c("HotelName"), by.y = c("HotelNameL"), all.x = TRUE)
d1<-d1[!duplicated(d1$HotelName), ]

hotelwithdetails<- merge(a, d1, by.x = c("HotelName"), by.y = c("HotelName"), all.x=T)

forStatesinA<- subset(hotelwithdetails,select= c(HotelName, HotelState))

a <- merge(a,forStatesinA, by.x = c("HotelName"), by.y = c("HotelName"), all.x = TRUE)
a$state <- tolower(a$HotelState)

a <- a[order(-a$AverageNPS),]

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
df <- mapus1 +geom_point(aes(x= a$lon,y=a$lat, colour= AverageNPS))
df <- df +expand_limits(x = us$long, y = us$lat)
df <- df +coord_map() + ggtitle("Hotel wise Average NPStype ")
df

proplot <- ggplot(data=tail(a),aes(x=reorder(HotelName,AverageNPS),y=AverageNPS)) + geom_bar(stat='identity',fill='cornflowerblue')
proplot <- proplot + ggtitle("NPS of each hotel ")
proplot <- proplot + labs(x="NPS", y= "Hotel") + coord_flip() 
proplot


#City wise
x <-tapply(nausdata$NPStype, nausdata$HotelCity, NPStapply)
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
x<- na.omit(x)
x <- x[!(x$lon>40),]
x <- x[!(x$lon<=(-123)),]
x <- x[!(x$lon<=(-123)),]
x <- x[!(x$lon>=(-67)),]

x <- x[order(-x$AverageNPS),]
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
df1 <- mapus1 +geom_point(aes(x= x$lon,y=x$lat, color=AverageNPS))
df1 <- df1 +expand_limits(x = us$long, y = us$lat)
df1 <- df1 +coord_map() + ggtitle("City wise Average NPS")
df1
proplot <- ggplot(data=(x),aes(x=reorder(Hotelcity,AverageNPS),y=AverageNPS)) + geom_bar(stat='identity',fill='cornflowerblue')
proplot <- proplot + ggtitle("NPS of each city ")
proplot <- proplot + labs(x="NPS", y= "City") + coord_flip() 
proplot

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

