
#  Q1.

# Node 9/7
1 - (9/16) ^2 - (7/16) ^ 2

# Node 5/2
1 - (5/7)^2 - (2/7)^2 

# Node 1/5
1 - (1/5)^2 - (4/5)^2 


# Nodes 1/4, 2/1, 1/1
p1 = 1 - (1/5)^2 - (4/5)^2
p2 = 1 - (2/3)^2 - (1/3)^2
p3 = 1 - (1/2)^2 - (1/2)^2 

# wieghted sum of gini values
gini = (5/10)*p1 + (3/10)*p2 + (2/10)* p3
gini


#########################################
# Q3

#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)

help(C5.0)

data(iris)
head(iris)

set.seed(12345)
irisRand <- iris[order(runif(150)), ]

iris_train <- irisRand[1:125, ]
iris_test  <- irisRand[126:150, ]

prop.table(table(iris_train$Species))
prop.table(table(iris_test$Species))

#iris_model <- C5.0(iris_train[-5], iris_train$Species)
irisTreeModel <- C5.0(Species ~ ., data = iris_train)

plot(irisTreeModel)
summary(irisTreeModel)

iris_predict <- predict(irisTreeModel, iris_test)

library(gmodels)
CrossTable(iris_predict, iris_test$Species, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

##############################################################
Q3. 

# Machine Learning with R - Brett Lantz, credit example
# install.packages("C50")
# install.packages("gmodels")
library(C50)
library(gmodels)


###############
# read the data
credit <- read.csv("data/credit.csv", stringsAsFactors = TRUE)
summary(credit)
# breaks plot(model)
credit$job <- NULL
str(credit)

# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)
help(table)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)
hist(credit$amount)

# look at the class variable
table(credit$default)

#########################
# training and test split

# create a random sample for training and test data
set.seed(1)
credit_rand <- credit[order(runif(1000)), ]

credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


#################
## Build the model

library(C50)
model <- C5.0(default ~ ., data = credit_train)

# display simple facts about the tree
model

# only works if credit$job is dropped. 
plot(model)

# display detailed information about the tree
summary(model)

###########
## Evaluate

predictions <- predict(model, credit_test)

library(gmodels)
CrossTable(predictions, credit_test$default,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default'))

help(CrossTable)





#######################################################
# Q4. Credit with Cost Matrix

#install.packages("C50")
#install.packages("gmodels")
library(C50)
library(gmodels)

credit <- read.csv("data/credit.csv")

# create a random sample for training and test data
set.seed(1)
credit_rand <- credit[order(runif(1000)), ]
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

## Train
credit_model <- C5.0(default ~ ., data = credit_train)

## Evaluation
credit_predictions <- predict(credit_model, credit_test)

CrossTable(credit_predictions, credit_test$default,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(default ~ ., data = credit_train,
                    costs = error_cost)

#credit_cost <- C5.0(credit_train[-17], credit_train$default,
#                    costs = error_cost)

credit_cost_predictions <- predict(credit_cost, credit_test)

CrossTable(credit_cost_predictions, credit_test$default,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


##################################################
# Q5. Credit with Boosting

# install.packages("C50")
library(C50)
library(gmodels)

credit <- read.csv("data/credit.csv")

set.seed(1)
credit_rand <- credit[order(runif(1000)), ]

# split the data frames
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

model <- C5.0(default ~ ., data = credit_train)

# create a factor vector of predictions on test data
credit_predictions <- predict(model, credit_test)

CrossTable(credit_predictions, credit_test$default,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

##################
# boosted decision tree with 10 trials
model10 <- C5.0(default ~ ., data = credit_train,
                trials = 10)
model10
# summary(credit_boost10)

predictions10 <- predict(model10, credit_test)
CrossTable(predictions10, credit_test$default,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

######################
# boosted decision tree with 100 trials (not shown in text)
model100 <- C5.0(default ~ ., data=credit_train,
                 trials = 100)
predictions100 <- predict(model100, credit_test)
CrossTable(predictions100, credit_test$default,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))


########################################################
# Q6. Mushrooms

library(C50)

mushrooms <- read.csv("data/mushrooms.csv", stringsAsFactors = TRUE)
summary(mushrooms)

# drop the veil_type feature as it has only only one value
table(mushrooms$veil_type)
mushrooms$veil_type <- NULL

# examine the class distribution
table(mushrooms$type)

###################
# split into training and test data

mushrooms <- mushrooms[order(runif(8124)), ]
mushrooms_train <- mushrooms[1:5000, ]
mushrooms_test  <- mushrooms[5001:8124, ]


###################
# build model

model <- C5.0(type ~ ., data = mushrooms_train)
summary(model)
plot(model)


######################
# check on test data

predictions <- predict(model, mushrooms_test)

library(gmodels)
CrossTable(predictions, mushrooms_test$type, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))