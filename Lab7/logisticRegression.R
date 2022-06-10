
# Q1 Sigmoid function

z = seq(from = -5, to = 5, by = 0.1)
y = 1/(1+exp(-z))
plot(z,y, type="l", main="Sigmoid Function")


########################################
# Q2.  Iris - predict isSetosa

library(gmodels)
data(iris)

iris$isSetosa <- iris$Species == 'setosa'
iris$Species <- NULL

 set.seed(1)
irisRand <- iris[order(runif(150)), ]

iris_train <- irisRand[1:125, ]
iris_test  <- irisRand[126:150, ]

model <- glm(isSetosa ~ ., data = iris_train, family="binomial")

predictions <- predict(model, iris_test, type="response")
classifications <- predictions > 0.5

library(gmodels)
CrossTable(classifications, iris_test$isSetosa, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

##########################################################
# Q3
credit <- read.csv("data/credit.csv")

# create a random sample for training and test data
set.seed(1)
credit_rand <- credit[order(runif(1000)), ]

credit_train <- credit_rand[1:800, ]
credit_test  <- credit_rand[801:1000, ]

# Q3.1
model <- glm(default ~ months_loan_duration + amount + age + dependents, 
             data = credit_train, family="binomial" )
summary(model)

# Prediction
newdata = data.frame(months_loan_duration=6,
                     amount = 1169,
                     age = 67,
                     dependents = 2) 

prediction = predict(model, newdata, type="response")
prediction

# Q3.2
model <- glm(default ~ months_loan_duration + amount + percent_of_income + 
               years_at_residence + age + existing_loans_count + dependents, data = credit_train, family="binomial" )

# display detailed information about the tree
summary(model)

## Prediction
newdata = data.frame(months_loan_duration=6,
                     amount = 1169,
                     percent_of_income = 4, 
                     years_at_residence = 4,
                     age = 67,
                     existing_loans_count = 1,
                     dependents = 2) 

prediction = predict(model, newdata, type="response")
prediction

# Q3.3
model <- glm(default ~ ., data = credit_train, family="binomial" )
summary(model)