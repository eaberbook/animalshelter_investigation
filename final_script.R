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

animals$Intake.Type <- as.factor(animals$Intake.Type)

first_model <- randomForest(OutCatg~Days.In.Shelter +Intake.Age+Sex+Intake.Type+Species,data=animals,type="classification",na.action=na.omit)

multinomial_model <- multinom(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species,data=animals)

multinomial_preds <- predict(multinomial_model,newdata=animals,type="class")

rf_preds <- predict(first_model,newdata=animals,type="prob")

table(multinomial_preds,animals$OutCatg)
table(rf_preds,animals$OutCatg)

prop.table(table(animals$Shelter, animals$OutCatg), margin=1)
# Shelter is definitely a significant predictor

# Microchip status to Yes or No
levels(animals$Microchip.Status) <- "Yes"
animals$Microchip.Status <- as.character(animals$Microchip.Status)
animals$Microchip.Status[which(is.na(animals$Microchip.Status)==T)] <- "No"
animals$Microchip.Status <- as.factor(animals$Microchip.Status)

table(animals$Microchip.Status, animals$OutCatg)
# Whether or not they have microchip is a significant predictor

#trying an improved random forest with microchip status and shelter.

improved_rf <- randomForest(OutCatg~Days.In.Shelter+Intake.Age+Sex+Intake.Type+Species+Shelter+Microchip.Status+Shelter,data=animals,type="classification",na.action=na.omit)

improved_rf_preds <- predict(improved_rf,newdata=animals,type="response")
table(improved_rf_preds,animals$OutCatg)



