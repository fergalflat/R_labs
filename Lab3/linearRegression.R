#1
x <- c(1,2,3,4,5,6)
y <- c(6,1,9,5,17,12)
values <- data.frame(x, y)
plot(values, xlab="X", ylab="Y")
cor(x,y)
model <- lm(y ~ x, data=values)
model
model$coefficients
model$residuals
abline(model)
predict(model, values)
summary(model)

n <- length(x)
a <- ( n * sum (x*y) - sum(x) * sum(y)) / (n * sum(x*x) - sum(x)^2)
b <- (sum(y) - a * sum(x)) / n

#2
help(faithful)
data(faithful)
faithful <- faithful[c(2,1)]
summary(faithful)
plot(faithful$waiting, faithful$eruptions)
cor(faithful$waiting, faithful$eruptions)

model = lm(eruptions ~ waiting, data=faithful) 
summary(model)
abline(model)

newData = data.frame(waiting = c(50, 60, 70, 80, 90))
  predict(model, newData)



# Q3

help(stackloss)
data(stackloss)
stackloss
pairs(stackloss)

#build multiple linear regression model
model = lm(stack.loss ~  Air.Flow + Water.Temp + Acid.Conc., data=stackloss) 
summary(model)
# 0.9136 / 0.8983
model$coefficients

# make a prediction
newdata = data.frame(Air.Flow=72, Water.Temp=20, Acid.Conc.=85) 
predict(model, newdata) 

predict(model, newdata, interval="confidence")
predict(model, newdata, interval="predict")

#build multiple linear regression model
model2 = lm(stack.loss ~  Air.Flow + Water.Temp, data=stackloss) 
summary(model2)
# 0.9088 / 0.8986 
model2$coefficients

anova(model2, model)

# Q4

help(iris)
data(iris)
summary(iris)
pairs(iris)

plot(Petal.Length ~ Petal.Width,  data = iris)
model1 <- lm(Petal.Length ~ Petal.Width, data = iris)
summary(model1)
# 0.9271 / 0.9266
abline(model1)

plot(Petal.Length ~ Sepal.Width,  data = iris)
model2 <- lm(Petal.Length ~ Sepal.Width, data = iris)
summary(model2)
# 0.1836 / 0.178
abline(model2)


#build multiple linear regression model
model3 = lm(Petal.Length ~  Sepal.Length + Sepal.Width + Petal.Width, data=iris) 
summary(model3)
# 0.968 / 0.9674

newdata = data.frame(Sepal.Length=6.3, Sepal.Width=2.8, Petal.Width=0.3) 
predict(model3, newdata) 
predict(model3, newdata, interval="confidence")
predict(model3, newdata, interval="predict")


model4 = lm(Petal.Length ~  Sepal.Length + Sepal.Width + Petal.Width + Species, data=iris) 
# iris.lm = lm(Petal.Width ~ , data=iris) 
summary(model4)
# 0.9784 / 0.9778