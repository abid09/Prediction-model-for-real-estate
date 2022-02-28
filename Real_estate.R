library(readr)
Real_estate <- read_csv("~/Real estate.csv")
View(Real_estate)
summary(Real_estate)
plot(Real_estate)
# Splitting the data into training and test data
set.seed(2)
install.packages("caTools")
library(caTools)
split <- sample.split(Real_estate, SplitRatio = 0.7)
split
train <- subset(Real_estate, split = "False")
train
Test <- subset(Real_estate, split = "True")
#create the model
Model <- lm(house_price_of_unit_area~.,data = train)
summary(Model)
#Prediction
pred <- predict(Model,Test)
pred
#comparing predicted vs actual
plot(Test$house_price_of_unit_area,type = "l",lty = 1.8,col="red")
lines(pred, type = "l",col = "blue")
#Finding accuracy
rmse <- sqrt(mean(pred-Real_estate$house_price_of_unit_area)^2)
rmse