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

# BASIC DATA EXPLORATION

# Is Shelter a significant predictor?
prop.table(table(animals$Shelter, animals$OutCatg), margin=1)
data.frame(prop.table(table(animals$Shelter,animals$OutCatg),margin=1))

# Summarizing data by primary breed

reference_table <- table(animals$Primary.Breed,animals$OutCatg)
reference_table <- data.frame(reference_table)
#reference_table$total <- rowSums(reference_table)
summary <- data.frame(prop.table(table(animals$Primary.Breed,animals$OutCatg),margin=1))



# Is having a microchip a significant predictor?
table(animals$Microchip.Status, animals$OutCatg)

# Is having a license a significant predictor?
table(animals$License.Status, animals$OutCatg)

# Trying to turn species into something significant



# SPLITTING INTO TEST AND TRAIN
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
improved_rf <- randomForest(OutCatg~Days.In.Shelter+Intake.Age+SN.Status+Sex+Intake.Type+Species+Microchip.Status+Shelter+Year+License.Status,data=animals,type="classification",
                            ntree=1000,mtry=5)

# Calculation of variable importance, trying the varImp function.
varImp(improved_rf)


print(improved_rf)
improved_rf_preds <- predict(improved_rf,newdata=real_test,type="prob")
rf_table<-table(improved_rf_preds,test$OutCatg)
class_rate(rf_table)

final_frame <- cbind(ARN,improved_rf_preds)
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


av_nnet <- avNNet(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Microchip.Status+Shelter+Year+License.Status,data=animals,type="classification",size=12)
av_nnet_predictions <- predict(av_nnet,real_test)

# Neural network here is giving me an 79.5% prediction.

av_nnet_predictions2 <- predict(av_nnet,test,type="class")
table(av_nnet_predictions2,test$OutCatg)


ARN <-real_test$ARN

final_frame <- cbind(ARN,bag_fda_preds)


# Binding av_nnet for export
final_frame <- cbind(ARN,av_nnet_predictions)
write.csv(final_frame,file="finalpredictions.csv")

breed_list <- as.character(significant_breeds$Var1)

significant_breeds <- summary[which(summary$Freq>0.40),]

significant_breeds <- significant_breeds[which(reference_table$Freq>20),]
significant_breeds <- na.omit(significant_breeds)


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
  
  # Age at Microchip
  animals$Microchip.Age <- animals$Microchip.Date - animals$DOB
  animals$Microchip.Age <- as.numeric(animals$Microchip.Age)
  
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
  
  animals$SN.Status[which(is.na(animals$S.N.Date)==T)]<-"No"
  animals$SN.Status[is.na(animals$SN.Status)]<-"Yes"
  animals$SN.Status <- as.factor(animals$SN.Status)
  
  # S.N. status to Yes or No
  
  
  # Creating a year variable to see if significant
  animals$Year <- format(animals$Outcome.Date,'%Y')
  animals$Year <- as.factor(animals$Year)
  
  #Creating a month variable to see if significant
  animals$Month <- format(animals$Outcome.Date,'%m')
  animals$Month <- as.numeric(animals$Month)

  
  # Turning NA's to 0's in some of the columns.
  # Days.In.Shelter. Replace NA's with 0's.
  animals$Days.In.Shelter[is.na(animals$Days.In.Shelter)]<-0
  # Intake.Age. For this we replace NA's with 
  animals$Intake.Age[is.na(animals$Intake.Age)]<-mean(animals$Intake.Age,na.rm=TRUE)
  # Year
  animals$Year[is.na(animals$Year)]<-2011
  animals$Month[is.na(animals$Month)]<-7
  animals$Month <- as.factor(animals$Month)
  return(animals)
}



