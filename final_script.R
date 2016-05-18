setwd("~/Desktop/RData")
animals <- read.csv("finaltrain.csv")
View(animals)

# Turning date strings into dates.
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

# Age at Outcome
# May be redundant information, as it is a linear combination of the above 2.
## animals$Outcome.Age <- animals$Outcome.Date - animals$DOB
## animals$Outcome.Age <- as.numeric(animals$Outcome.Age)

# Age at Microchip
animals$Microchip.Age <- animals$Microchip.Date - animals$DOB
animals$Microchip.Age <- as.numeric(animals$Microchip.Age)

# Create column to denote whether the animal has a serial number


# Attempt 1.
animals$OutCatg <- as.factor(animals$OutCatg)
animals$Shelter <- as.factor(animals$Shelter)

library(plyr)
animals$OutCatg<- revalue(animals$OutCatg, c("FOSTER"="OTHER","INVENTORY"="OTHER","RTO"="OTHER",
                           "TRANSFER"="OTHER","UNKNOWN"="OTHER"))

animals$Intake.Zip.Code <- as.factor(animals$Intake.Zip.Code)

# Using the zipcode package

library(zipcode)
attach(zipcode)

#for(i in 1:nrow(animals)){
  
#if((animals$Intake.Zip.Code[i]!=0 && (dim(zipcode[which(zipcode$zip==animals$Intake.Zip.Code[i]),])[1]!=0))){


#animal_city <- zipcode[which(zipcode$zip==animals$Intake.Zip.Code[i]),]$city  
#animals$city[i] <- animal_city

#animal_state <- zipcode[which(zipcode$zip==animals$Intake.Zip.Code[i]),]$state
#animals$state[i] <- animal_state

#animal_latitude <- zipcode[which(zipcode$zip==animals$Intake.Zip.Code[i]),]$latitude
#animals$latitude[i] <- animal_latitude

#animal_longitude <- zipcode[which(zipcode$zip==animals$Intake.Zip.Code[i]),]$longitude
#animals$longitude[i] <- animal_longitude

#}

#}
#animals$latitude <- as.factor(animals$latitude)
#animals$city <- as.factor(animals$city)

animals$Intake.Type <- as.factor(animals$Intake.Type)


# SPLITTING INTO TEST AND TRAIN
index <- 1:nrow(animals)
trainindex <- sample(index,trunc(length(index)/2))
train <- animals[trainindex,]
test <- animals[-trainindex,]

# Microchip status to Yes or No
levels(animals$Microchip.Status) <- "Yes"
animals$Microchip.Status <- as.character(animals$Microchip.Status)
animals$Microchip.Status[which(is.na(animals$Microchip.Status)==T)] <- "No"
animals$Microchip.Status <- as.factor(animals$Microchip.Status)


# Creating a year variable to see if significant
animals$Year <- format(animals$Outcome.Date,'%Y')
animals$Year <- as.numeric(animals$Year)
table(animals$Year,animals$OutCatg)

# Is Shelter a significant predictor?
prop.table(table(animals$Shelter, animals$OutCatg), margin=1)


# Is having a microchip a significant predictor?
table(animals$Microchip.Status, animals$OutCatg)


# Multinomial model

multinomial_model <- multinom(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Microchip.Status
                              +Shelter,data=train)

multinomial_preds <- predict(multinomial_model,newdata=test,type="class")

table(multinomial_preds,test$OutCatg)


# Trying an improved random forest with year as a categorical predictor as well

improved_rf <- randomForest(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Microchip.Status+Shelter+Year,data=train,type="classification",na.action=na.omit)

improved_rf_preds <- predict(improved_rf,newdata=test,type="response")
table(improved_rf_preds,test$OutCatg)

# Test Percentages : 
# Current percentage to beat : 75.2%. Random Forest Classification Rate
# Multinomial Model : 70.7% Classification rate

# Next : Try principal components regression
