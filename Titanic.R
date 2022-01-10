#Part1: Booting up R

#Set working directory and import datafiles#
setwd("C:/Users/sivak/OneDrive/Documents/Datasets/New folder")

train <- read.csv("~/Datasets/New folder/train.csv")
View(train)

test <- read.csv("~/Datasets/New folder/test.csv")
View(test)

# Examine structure of dataframe
str(train)

# Look at number of people who survived
table(train$Survived)
prop.table(table(train$Survived))

# Create new column in test set with our prediction that everyone dies
test$Survived <- rep(0, 418)

# Create dataframe and output to file
# write file to store prediction
pred <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(pred, file = "theyallperish.csv", row.names = FALSE)


#Part2:The Gender-Class Model
#Look at gender pattern
table(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1

# Create submission dataframe and output to file
pred <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(pred, file = "gendermodel.csv", row.names = FALSE)

# Look at age patterns
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived~Child+Sex, data = train, FUN=sum)
aggregate(Survived~Child+Sex, data = train, FUN=length)
aggregate(Survived~Child+Sex, data = train, FUN=function(x) {sum(x)/length(x)})

# Look at class and fare patterns
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >=20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >=10] <- '10-20'
train$Fare2[train$Fare < 10] <- '10'
aggregate(Survived~Fare2+Pclass+Sex, data=train, FUN = function(x) {sum(x)/length(x)})

# Create new column in test set with our prediction that everyone dies
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20]<- 0

# Create dataframe and output to file
pred <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(pred, file = "genderclassmodel.csv", row.names = FALSE)

#Part3: Decision Trees
# Build a deeper tree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, method = 'class')

# Plot it with base-R
plot(fit)
text(fit)

# Install and load required packages for fancy decision tree plotting
install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# And then make it look better with fancyRpartPlot!
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
prediction <- predict(fit, test, type = 'class')
sub <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(sub, file='myfirstdtree.csv', row.names = FALSE)

# Let's unleash the decision tree and let it grow to the max
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, method = 'class', control = rpart.control(minsplit = 2, cp=0))
fancyRpartPlot(fit)

# Now let's make a prediction and write a submission file
prediction <- predict(fit, test, type = 'class')
sub <- data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(sub, file='fullgrowthtree.csv', row.names = FALSE)

# Manually trim a decision tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data = train, method = 'class',
             control = rpart.control(minsplit = 2, cp = 0.005))
new.fit <- prp(fit, snip = TRUE)$obj
fancyRpartPlot(new.fit)

#Part4: Feature Engineering
#reload the data
train <- read.csv("~/Datasets/New folder/train.csv")
View(train)
test <- read.csv("~/Datasets/New folder/test.csv")
View(test)

# What's in a name?
train$Name[1]

# Join together the test and train sets for easier feature engineering
test$Survived <- NA
combi <- rbind(train, test)

# Convert to a string
combi$Name <- as.character(combi$Name)
combi$Name[1]
typeof(combi$Name)

# Find the indexes for the tile piece of the name
strsplit(combi$Name[1], split = '[,.]')
strsplit(combi$Name[1], split = '[,.]')[[1]]
strsplit(combi$Name[1], split = '[,.]')[[1]][2]
strsplit(combi$Name[1], split = '[,.]')[[1]][1]

# Engineered variable: Title
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)

# Inspect new feature
table(combi$Title)

# Combine small title groups
combi$Title[combi$Title %in% c('Mme','Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')]<- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')]<- 'Lady'

# Convert to a factor
combi$Title <- factor(combi$Title)

# Engineered variable: Family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Engineered variable: Family
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

# Inspect new feature
table(combi$FamilyID)
# Delete erroneous family IDs
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

# Convert to a factor
combi$FamilyID <- factor(combi$FamilyID)
