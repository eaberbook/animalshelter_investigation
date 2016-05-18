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





# BASIC DATA EXPLORATION

# Is Shelter a significant predictor?
prop.table(table(animals$Shelter, animals$OutCatg), margin=1)


# Is having a microchip a significant predictor?
table(animals$Microchip.Status, animals$OutCatg)

# Is having a license a significant predictor?
table(animals$License.Status, animals$OutCatg)


# SPLITTING INTO TEST AND TRAIN
index <- 1:nrow(animals)
trainindex <- sample(index,trunc(length(index)/2))
train <- animals[trainindex,]
test <- animals[-trainindex,]


# MULTINOMIAL MODEL

multinomial_model <- multinom(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Microchip.Status
                              +Shelter,data=train)

multinomial_preds <- predict(multinomial_model,newdata=test,type="class")

final_multinomial_probs <- predict(multinomial_model,newdata=real_test,type="prob")
final_frame<-cbind(ARN,final_multinomial_probs)
write.csv(final_frame,file="finalpredictions")

table(multinomial_preds,test$OutCatg)


# RANDOM FOREST MODEL

improved_rf <- randomForest(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Microchip.Status+Shelter+Year,data=train,type="classification",importance=TRUE,na.action=na.exclude)
print(improved_rf)
improved_rf_preds <- predict(improved_rf,newdata=test,type="response")
rf_table<-table(improved_rf_preds,test$OutCatg)
class_rate(rf_table)

final_rf_probs <- predict(improved_rf,newdata=real_test,type="prob")
final_rf_probs <- as.data.frame(final_rf_probs)
ARN<-real_test$ARN
final_frame<-cbind(ARN,final_rf_probs)
row.names(final_frame)<-NULL
write.csv(final_frame,file="finalpredictions")

# CLASSIFICATION RATE FUNCTION

class_rate<-function(table){
  total = 0;
  classification=0
  for(i in 1:3){
    for(j in 1:3){
      if(i==j){
       classification = classification + table[i,j] 
      }
      total = total + table[i,j]
    }
  }
  return (classification/total)
}

# CLEANING DATA FUNCTION
clean_data<-function(animals){
  
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
  
  # S.N. status to Yes or No
  
  
  # Creating a year variable to see if significant
  animals$Year <- format(animals$Outcome.Date,'%Y')
  animals$Year <- as.numeric(animals$Year)
    
  # Turning NA's to 0's in some of the columns.
  # Days.In.Shelter. Replace NA's with 0's.
  animals$Days.In.Shelter[is.na(animals$Days.In.Shelter)]<-0
  # Intake.Age. For this we replace NA's with 
  animals$Intake.Age[is.na(animals$Intake.Age)]<-mean(animals$Intake.Age,na.rm=TRUE)
  # Year
  animals$Year[is.na(animals$Year)]<-mean(animals$Year,na.rm=TRUE)
  
  return(animals)
  
}



