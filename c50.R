library(C50)
data <- read.csv(file="titanic.csv")

#split dataset into training and testing, 70% 30%
train = data[1: floor(nrow(data) * 0.7), ]
test = data[(floor(nrow(data) * 0.7) + 1) : nrow(data), ]

#create array of used attributes
features = c("Pclass", "Sex", "Age", "FamilySize" , "Fare", "Embarked")
#set options
ctrl = C5.0Control(minCases =  100)
#build the model
model <- C5.0(train[, features], train[, "Survived"]) #, control = ctrl
#output the result
summary(model)
plot(model, type="s", main="C5.0 Decision Tree - Default parameters")
as.party.C5.0(model)
#test the model on testing dataset
results = predict(model, test[, features])
#calulate accuracy on testing dataset
bools = results == test$Survived
t = length(which(bools)) # number of trues value
accuracy = t / length(bools)
sprintf("Accuracy = %.2f ", accuracy)

