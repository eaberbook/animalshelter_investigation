setwd("~/Desktop/RData")
animals <- read.csv("finaltrain.csv")
real_test <- read.csv("finaltest.csv")

# Cleaning the Outcome category variable.
animals$OutCatg <- as.factor(animals$OutCatg)
library(plyr)
animals$OutCatg<- revalue(animals$OutCatg, c("FOSTER"="OTHER","INVENTORY"="OTHER","RTO"="OTHER",
                                             "TRANSFER"="OTHER","UNKNOWN"="OTHER"))
# Cleaning data by putting into function

animals <- clean_data(animals)
real_test <- clean_data(real_test)

# Testing out some breed stuff:

animal_breed <- cbind(as.character(animals$Primary.Breed),as.character(animals$OutCatg))
a<-melt(animal_breed, id.vars="OutCatg")



# Is having a microchip a significant predictor?
table(animals$Microchip.Status, animals$OutCatg)

# Is having a license a significant predictor?
table(animals$License.Status, animals$OutCatg)

# Trying to turn species into something significant



# SPLITTING INTO TEST AND TRAIN
set.seed(101)
index <- 1:nrow(animals)
trainindex <- sample(index,trunc(length(index)/2))
train <- animals[trainindex,]
test <- animals[-trainindex,]


# MULTINOMIAL MODEL

library(nnet)
multinomial_model <- multinom(OutCatg~Primary.Breed+Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Microchip.Status
                              +Shelter+License.Status+Month,data=train)

multinomial_preds <- predict(multinomial_model,newdata=real_test,type="class")

final_multinomial_probs <- predict(multinomial_model,newdata=real_test,type="prob")
final_frame<-cbind(ARN,final_multinomial_probs)
write.csv(final_frame,file="finalpredictions")

table(multinomial_preds,test$OutCatg)

# RANDOM FOREST MODEL

library(randomForest)
improved_rf <- randomForest(OutCatg~Days.In.Shelter+Sex+Intake.Age+Month+Neuter+Intake.Type+License.Status+Species+Microchip.Status+Outcome.Age+Outcome.Weekday+LifeStage+SN.Status+No_Breed+Shelter+Year+No_Name,
                            data=animals,type="classification",
                            ntree=1000,mtry=4,nodesize=12)

varImpPlot(improved_rf)

relevant_names <- c("Days.In.Shelter","Sex","Neuter","License.Status","Intake.Age","Intake.Type","Species","Microchip.Status","Shelter","Year",
                    "No_Name","Outcome.Weekday","LifeStage","No_Breed","Month","SN.Status")
new_frame<-animals[,relevant_names]
response <- animals$OutCatg
tuned_model <- tuneRF(new_frame,response,mtryStart=4,method="rf",plot=TRUE)



test_preds <- predict(improved_rf,newdata=real_test,type="prob")
table(test_preds,animals$OutCatg)

final_frame <- data.frame(ARN,test_preds)


# Trying to beat 83.96% testing classification rate.


# Trying gbm 
gbm_model <- gbm(OutCatg~Days.In.Shelter+Neuter+Intake.Age+SN.Status+Sex+Intake.Type+License.Status+Species+Microchip.Status+Shelter+Year+No_Name,
                 data=train,n.trees=500,distribution="multinomial")

table(gbm_pred,test$OutCatg)


varImpPlot(improved_rf)




relevant_names <- c("Days.In.Shelter","Neuter","Month","License.Status","Intake.Age","SN.Status","Intake.Type","Species","Microchip.Status","Shelter","Year",
                    "No_Name")
new_frame<-animals[,relevant_names]
response <- animals$OutCatg
tuned_model <- tuneRF(new_frame,response,mtryStart=4,method="rf",plot=TRUE)



# Calculation of variable importance, trying the varImp function.



improved_rf_preds <- predict(improved_rf,newdata=real_test,type="")


rf_table<-data.frame(improved_rf_preds,animals$OutCatg)
class_rate(rf_table)

final_frame <- data.frame(ARN,improved_rf_preds)
write.csv(final_frame,file="finalpredictions.csv")


# This is how I bound random forest for export
final_rf_probs <- predict(improved_rf,newdata=real_test,type="prob")
final_rf_probs <- as.data.frame(final_rf_probs)
ARN<-as.character(real_test$ARN)
final_frame<-rbind(ARN,final_rf_probs)
final_frame<-rbind(real_test$ARN,improved_rf_preds)
row.names(final_frame)<-NULL

# Using the caret library for some stuff

library(caret)

av_nnet <- avNNet(OutCatg~Days.In.Shelter+Sex+Intake.Age+Neuter+SN.Status+Intake.Type+License.Status+Species+Microchip.Status+Shelter+Year+No_Name,size=5,data=train,type="classification")

av_nnet_predictions <- predict(av_nnet,test,type="class")
table(av_nnet_predictions,test$OutCatg)


# Neural network here is giving me an 79.5% prediction.

av_nnet_predictions2 <- predict(av_nnet,animals,type="class")
table(av_nnet_predictions2,animals$OutCatg)



ARN <-real_test$ARN

final_frame <- data.frame(ARN,av_nnet_predictions)

# Binding av_nnet for export

final_frame <- cbind(ARN,av_nnet_predictions)
write.csv(final_frame,file="finalpredictions.csv")

# Train Function

animals$Catg_Pred <- as.factor(animals$Catg_Pred)
table(animals$Catg_Pred,animals$OutCatg)

# CLEANING DATA FUNCTION
clean_data<-function(animals){
  
  animals$ARN <- as.character(animals$ARN)
  animals$DOB <- as.Date(animals$DOB)
  animals$S.N.Date <- as.Date(animals$S.N.Date)
  animals$Intake.Date <- as.Date(animals$Intake.Date)
  animals$Outcome.Date <- as.Date(animals$Outcome.Date)
  animals$Microchip.Date <- as.Date(animals$Microchip.Date)
  
  #### Start to create some meaningful dates.
  
  # Days animal spent in shelter
  animals$Days.In.Shelter <- animals$Outcome.Date - animals$Intake.Date
  animals$Days.In.Shelter <- as.numeric(animals$Days.In.Shelter)
  
  # Age at Intake
  animals$Intake.Age <- animals$Intake.Date - animals$DOB
  animals$Intake.Age <- as.numeric(animals$Intake.Age)
  
  # Outcome Age
  animals$Outcome.Age <- animals$Outcome.Date - animals$DOB
  animals$Outcome.Age <- as.numeric(animals$Outcome.Age)
  
  
  # Turning strings into categorical variables

  animals$Shelter <- as.factor(animals$Shelter)
  animals$Intake.Type <- as.factor(animals$Intake.Type)
  
  # Microchip status to Yes or No
  levels(animals$Microchip.Status) <- "Yes"
  animals$Microchip.Status <- as.character(animals$Microchip.Status)
  animals$Microchip.Status[which(is.na(animals$Microchip.Status)==T)] <- "No"
  animals$Microchip.Status <- as.factor(animals$Microchip.Status)

  # License status to Yes or No
  animals$License.Status[which(is.na(animals$License.Date)==T)]<- "No"
  animals$License.Status[is.na(animals$License.Status)]<-"Yes"
  animals$License.Status <- as.factor(animals$License.Status)
  
  # S.N. status to Yes or No
  
  animals$SN.Status[which(is.na(animals$S.N.Date)==T)]<-"No"
  animals$SN.Status[is.na(animals$SN.Status)]<-"Yes"
  animals$SN.Status <- as.factor(animals$SN.Status)
  
  # Does the animal have a name
  animals$No_Name <- as.character(animals$NAME)
  animals$No_Name[which(is.na(animals$No_Name))]<-"Yes"
  animals$No_Name[which(animals$No_Name!="Yes")]<-"No"
  animals$No_Name <- as.factor(animals$No_Name)
  
  # Does the animal have a defined color
  animals$No_Color <- as.character(animals$Color.Markings)
  animals$No_Color[which(is.na(animals$No_Color))]<-"Yes"
  animals$No_Color[which(animals$No_Color!="Yes")]<-"No"
  animals$No_Color <- as.factor(animals$No_Color)
  
  # Does the animal have a breed
  animals$No_Breed <- as.character(animals$Primary.Breed)
  animals$No_Breed[which(is.na(animals$No_Breed))]<-"Yes"
  animals$No_Breed[which(animals$No_Breed!="Yes")]<-"No"
  animals$No_Breed <- as.factor(animals$No_Breed)
  
  animals$Outcome.Weekday <- wday(animals$Outcome.Date)
  animals$Outcome.Weekday <- as.factor(animals$Outcome.Weekday)
  


  
  # Creating a year variable to see if significant
  animals$Year <- format(animals$Outcome.Date,'%Y')
  animals$Year <- as.factor(animals$Year)
  
  #Creating a month variable to see if significant
  animals$Month <- format(animals$Outcome.Date,'%m')
  animals$Month <- as.numeric(animals$Month)
  
  # Creating a neuter variable
  animals$Neuter <- as.factor(animals$Sex)
  levels(animals$Neuter)<- c("NO","NO","YES","YES","U")
  
  # Now that we have extracted neutered, let's
  # Switch neutered back to male, and spayed back to female
  levels(animals$Sex) <- c("F","M","M","F","U")

  
  # Turning NA's to 0's in some of the columns.
  # Days.In.Shelter. Replace NA's with 0's.
  animals$Days.In.Shelter[is.na(animals$Days.In.Shelter)]<-0
  # Intake.Age. For this we replace NA's with 
  animals$Intake.Age[is.na(animals$Intake.Age)]<-mean(animals$Intake.Age,na.rm=TRUE)
  # Year
  animals$Outcome.Age[is.na(animals$Outcome.Age)] <- mean(animals$Outcome.Age,na.rm=TRUE)
  animals$Year[is.na(animals$Year)]<-2011
  animals$Month[is.na(animals$Month)]<-7
  animals$Month <- as.factor(animals$Month)
  animals$Outcome.Weekday[is.na(animals$Outcome.Weekday)] <- 4
  
  
  animals$LifeStage[animals$Intake.Age < 365] <- "young"
  animals$LifeStage[animals$Intake.Age >= 365] <- "adult"
  animals$LifeStage[animals$Intake.Age >= 5110] <- "old"
  animals$LifeStage <- as.factor(animals$LifeStage)
  
  # Creating an Intake.Area Variable
  # The break points were chosen from quantiles to maximize predictive power.
  animals$Intake.Area <- cut(animals$Intake.Zip.Code, breaks=c(-1,90063,90247,91042,99999))
  levels(animals$Intake.Area) <- c("Area1","Area2","Area3","Area2")
  
  return(animals)
}



