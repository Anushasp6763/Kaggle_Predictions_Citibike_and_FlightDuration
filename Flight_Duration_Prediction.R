dataset <- Nov18EastWestCoastFlights.train
View(dataset)

Reg <- lm(ARR_DELAY_NEW ~ CRS_ARR_TIME + CRS_DEP_TIME + I(DISTANCE) + I(DISTANCE^2), data = dataset[dataset$DAY_OF_WEEK>5,]) #Creating the model considering greater air traffic on weekends.


#Testing on data sets:
#----------------------A------------------------------------------------------------------------------------------------------------------------------------------------------------------

predictionA <- predict(Reg,Nov19EastWestCoastFlights.testA)
regr.error(predictionA,Nov19EastWestCoastFlights.testAwithsolutions$ARR_DELAY_NEW)


#  Predicting on the test set for Kaggle:
#------------------------B--------------------------------------------------------------------------------------------------------------------------------------------------------------

predictionB <- predict(Reg,Nov19EastWestCoastFlights.testB)

#-------Creating a csv file----------------------------------------------------------------------------------------------------------
SubmissionB <- data.frame(1:2496,predictionB)
colnames(SubmissionB)=c("Id","Predicted")
write.csv(SubmissionB,file="~/Assignment 13/Sub1/SubmissionB.csv",row.names=FALSE)

