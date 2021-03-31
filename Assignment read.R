library(tidyverse)
library(ggplot2)
library(lubridate)
library(corrplot)

#set working directory
setwd("C:/Users/wayne/Desktop/Assignment")
#Load dataset
prop = read.csv("REALIS_Oct17toOct20.csv",header = TRUE, nrows=1)

#Custom Data Type 
dataClasses = sapply(prop, class)
dataClasses[1]="factor"
dataClasses[7:8]="factor"
dataClasses[9]="character"
dataClasses[12]="factor"
dataClasses[13]="numeric"
dataClasses[14:16]="factor"
dataClasses[18:21]="factor"

#Load dataset with new dataclass
prop = read.csv("REALIS_Oct17toOct20.csv", header = TRUE, dec=".", colClasses = dataClasses, stringsAsFactors = TRUE)

#Convert String to Numeric
prop$Transacted.Price....<- as.numeric(gsub(",", "",as.character(prop$Transacted.Price....)))
prop$Area..SQFT.<- as.numeric(gsub(",", "",as.character(prop$Area..SQFT.)))
prop$Unit.Price....PSF.<- as.numeric(gsub(",", "",as.character(prop$Unit.Price....PSF.)))
prop$Area..SQM.<- as.numeric(gsub(",", "",as.character(prop$Area..SQM.)))
prop$Unit.Price....PSM.<- as.numeric(gsub(",", "",as.character(prop$Unit.Price....PSM.)))
prop$Nett.Price...<- as.numeric(gsub(",", "",as.character(prop$Nett.Price...)))
prop$Number.of.Units<- as.numeric(gsub(",", "",as.character(prop$Number.of.Units)))
prop$Sale.Date <- dmy(prop$Sale.Date)

#Convert to NA
prop$Purchaser.Address.Indicator<-gsub('N.A','Did not disclose',prop$Purchaser.Address.Indicator)

#Extract Year of sales
prop$YearofSale <- gsub(".*/","",prop$Sale.Date)
prop$YearofSale<-lubridate::year(prop$YearofSale)


#Extract date
prop$Date.Tenure <- gsub( ".*(\\d{2}/\\d{1,2}/\\d{4}).*", "\\1", prop$Tenure)
#Extract lease period
prop$Tenure <- gsub("(.*)y.*","\\1",prop$Tenure)
prop$Tenure <- gsub("(.*)Y.*","\\1",prop$Tenure)
#need this statement to remove the blank space before 'y' and 'Y'
prop$Tenure <- gsub(" ","",prop$Tenure)

#Extract date without freehold
prop$YearsRemaining <- gsub("Freehold","NA",prop$Date.Tenure)
#Convert to Date Format
prop$YearsRemaining <- dmy(prop$YearsRemaining)
#Original lease 
prop$leaseduration<-as.numeric(gsub("Freehold","9999",prop$Tenure))
#Create function to calculate remaining lease
addYear = function(date, nyear){year(date)=year(date)+nyear; date}
prop$Year.lease.end<-addYear(prop$YearsRemaining,prop$leaseduration)
#to calculate remaining years of lease when property is bought
prop$lease.remainingyears<-round(interval(prop$Sale.Date,prop$Year.lease.end ) / years(1))
#To include freehold lease.remaining years as 9999
prop$lease.remainingyears<-ifelse(is.na(prop$lease.remainingyears),9999,prop$lease.remainingyears)

#Remove 3 columns 
prop[23:26]<-list(NULL)
#Remove net price as it has over 66539 NA
prop[11]<-NULL

#find relationship between region and price
boxplot(prop$Unit.Price....PSM.~prop$Planning.Region,xlab ="Planning Region",ylab="Unit Price PSM")

#find relationship between property type and price
boxplot(prop$Unit.Price....PSM.~prop$Property.Type, xlab ="Property Type",ylab="Unit Price PSM" )

#find relationship between property type and price
boxplot(prop$Unit.Price....PSM.~prop$Type.of.Sale , xlab ="Type of sale",ylab="Unit Price PSM")


df1 <- prop %>% 
  dplyr::select(YearofSale,Unit.Price....PSM.,Property.Type) %>% 
  group_by (YearofSale,Property.Type) %>% 
  summarise(MedianUnitPricePSM = median(Unit.Price....PSM.)) 


p<-ggplot(data = df1,aes(x=YearofSale,y=MedianUnitPricePSM))

p + geom_line(aes(color=Property.Type)) +
  geom_text(aes(label = MedianUnitPricePSM), vjust = -0.95) + 
  geom_point(aes(color=Property.Type))

#Corelation Plot
corrplot(cor(data.matrix(prop[])), method="number", mar= c(1,1,1,3), number.cex = 0.48, tl.cex =0.55)

#dataset for clustering with quantitative data
propcluster <-prop %>%
  dplyr::select(Transacted.Price....,Area..SQM.,Unit.Price....PSM.,Number.of.Units,lease.remainingyears)

#scale for cluster
propclusterscale <-prop %>%
  dplyr::select(Transacted.Price....,Area..SQM.,Unit.Price....PSM.,Number.of.Units,lease.remainingyears)%>%
  scale()

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.maximum = 15
wss <- sapply(1:k.maximum,function(k)(kmeans(propclusterscale, k, nstart=50,iter.max = 15 )$tot.withinss))
wss
#plot elbow plot
plot(1:k.maximum, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

#kmeans
kmc<-kmeans(propclusterscale,3)
kmc

kmc$cluster

#Cluster scatter plot 1
plot(Unit.Price....PSM.~Transacted.Price...., propclusterscale, col=kmc$cluster)


#Cluster scatter plot 2
plot(Unit.Price....PSM.~lease.remainingyears,
     propclusterscale, 
     col=kmc$cluster)


#Cluster scatter plot 3
plot(Unit.Price....PSM.~Area..SQM.,propclusterscale,col=kmc$cluster)

#Cluster scatter plot 4
plot(Unit.Price....PSM.~Number.of.Units, propclusterscale, col=kmc$cluster)

#Liner Regression model 1
model1<-lm(prop$Unit.Price....PSM.~prop$lease.remainingyears +prop$Postal.Code+prop$Transacted.Price....+prop$Area..SQM.+prop$Number.of.Units )
summary(model1)

#linear regression model 2
model2<-lm(prop$Transacted.Price....~prop$Postal.Code+prop$Unit.Price....PSM.+prop$Area..SQM )
summary(model2)

