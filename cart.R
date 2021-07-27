library(rpart)
library(rpart.plot)
data <- read.csv(file="titanic.csv")
train = data[1: floor(nrow(data) * 0.7), ]
test = data[(floor(nrow(data) * 0.7) + 1) : nrow(data), ]

features = c("Pclass", "Sex", "Age", "FamilySize" , "Fare", "Embarked")
formula = Survived ~ Pclass + Sex + Age + FamilySize + Fare + Embarked
#build the model
cart_model <- rpart (formula, train) #control = ctrl
#print results
print(cart_model)
# summarize the model
summary(cart_model)
#prints a tree as a set of rules
rpart.rules(cart_model)
#display cp table
printcp(cart_model)
#plot cross-validation results
plotcp(cart_model)
#plot the tree
rpart.plot (cart_model, tweak = 1.25, type = 5, extra = 101)
training <- predict(cart_model, train[, features], type="class")
table(training, train$Survived)
# make predictions
predictions <- predict(cart_model, test[, features], type="class")
# summarize accuracy
table(predictions, test$Survived)

#prune tree
#value of CP that minimize the cross-validated error, (xerror)
prune_cart <- prune(cart_model, cp= 0.02)
rpart.plot (prune_cart, tweak = 1.25, type = 5, extra = 101)

training <- predict(prune_cart, train[, features], type="class")
table(training, train$Survived)

# make predictions after pruning
predictions <- predict(prune_cart, test[, features], type="class")
# summarize accuracy
table(predictions, test$Survived)

#minsplit, minimum number of observations in a node
ctrl = rpart.control(minsplit=50)
cart_model <- rpart (formula, train, control = ctrl)
rpart.plot (cart_model, tweak = 1.25, type = 5, extra = 101)
