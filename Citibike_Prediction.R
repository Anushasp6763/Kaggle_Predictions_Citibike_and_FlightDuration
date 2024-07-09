dataset <- citybike.train
View(dataset)

library(rpart)
library(lubridate)
library(geosphere)
library(dplyr)


dataset$dayofstart <- wday(dataset$starttime) #Finding the days of the week 
dataset <- mutate(dataset, Distance = distHaversine(cbind(start.station.longitude, start.station.latitude),cbind(end.station.longitude, end.station.latitude)))#Finding the distance using the latitudes and longitudes

Reg <- lm(tripduration ~ Distance + usertype + birth.year + I(gender) + I(gender^2) + I(dayofstart) + I(dayofstart^2), data = dataset) #Creating the model


#Testing on data sets:
#----------------------100------------------------------------------------------------------------------------------------------------------------------------------------------------------
citybike.test100 <- mutate(citybike.test100, Distance = distHaversine(cbind(start.station.longitude, start.station.latitude),cbind(end.station.longitude, end.station.latitude)))
citybike.test100$dayofstart <- wday(citybike.test100$starttime)

prediction100 <- predict(Reg,citybike.test100)
regr.error(prediction100,citybike.test100withduration$tripduration)

#----------------------10000A-------------------------------------------------------------------------------------------------------------------------------------------------
citybike.test10000A <- mutate(citybike.test10000B, Distance = distHaversine(cbind(start.station.longitude, start.station.latitude),cbind(end.station.longitude, end.station.latitude)))
citybike.test10000A$dayofstart <- wday(citybike.test10000A$starttime)

prediction10000A <- predict(Reg,citybike.test10000A)
regr.error(prediction10000A,citybike.test10000Awithduration$tripduration)


#  Predicting on the test set for Kaggle:
#------------------------20000--------------------------------------------------------------------------------------------------------------------------------------------------------------
citybike.test20000 <- mutate(citybike.test20000, Distance = distHaversine(cbind(start.station.longitude, start.station.latitude),cbind(end.station.longitude, end.station.latitude)))
citybike.test20000$dayofstart <- wday(citybike.test20000$starttime)

prediction20000 <- predict(Reg,citybike.test20000)

#-------Creating a csv file----------------------------------------------------------------------------------------------------------
Submission20000 <- data.frame(1:20000,prediction20000)
colnames(Submission20000)=c("Id","Predicted")
write.csv(Submission20000,file="~/Assignment 12/Kaggle Submission 6/Submission20000.csv",row.names=FALSE)

