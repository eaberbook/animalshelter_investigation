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
