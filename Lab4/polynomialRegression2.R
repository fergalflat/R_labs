
# Q1. Orthogonal Polynomials.

x <- seq(-1, 1, 0.1)

 #polynomials
 X1 <- x
 X2 <- x^2
 X3 <- x^3
 X4 <- x^4
 df <- data.frame(X1, X2, X3, X4)

df <- data.frame(X1=x, X2= x*x, X3=x^3, X4=x^4)

cor(df$X1, df$X2)
cor(df$X1, df$X3)
cor(df$X2, df$X4)
df

plot(df$X1)
plot(df$X3)
plot(df$X1, df$X3)

plot(df$X2)
plot(df$X4)
plot(df$X2, df$X4)

# Chebyshev polynomials of first kind
df2 <- data.frame(X1=x, X2= 2*x*x -1, 
                  X3=4*x^3 -3*x, X4=8*x^4 -8 * x^2 +1)
cor(df2$X1, df2$X2)
cor(df2$X1, df2$X3)
cor(df2$X2, df2$X4)

plot(df2$X1)
plot(df2$X3)
plot(df2$X2)
plot(df2$X4)

# Polynomials of degree 4
poly(x, degree=4)


##########################
# Q2 Orthogonal Polynomials - stackloss dataset

data(stackloss)

poly(stackloss$Air.Flow, degree=3)

# df <- as.data.frame(poly(stackloss$Air.Flow, degree=3))
# df$`1`

polym(stackloss$Air.Flow, stackloss$Water.Temp, degree=2)
# 1.0 - AF
# 2.0 - AF^2
# 0.1 - WT
# 0.2 - WT^2
# 1.1 - Af * WT
polym(stackloss$Air.Flow, stackloss$Water.Temp, stackloss$Acid.Conc., degree=2)

######################################
# Q3 
# install.packages("ggplot2")
library('ggplot2')

# polynomial fitting

set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)
ggplot(df, aes(x = X, y = Y)) +
  geom_point()

model <- lm(Y ~ poly(X, degree = 25), data = df)
summary(model)

# add predictions to the data frame 
# df <- transform(df, PredictedY = predict(model))
# simpler
df$PredictedY <- predict(model)

# plot predicted values
ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()


### cross validation, this needs a training set and a validation set 

# set x and y again
set.seed(1)
x <- seq(0, 1, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# get a random sample of 50 indicies from 101
n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

# training data
training.x <- x[indices]
training.y <- y[indices]
training.df <- data.frame(X = training.x, Y = training.y)

# validation data is the other indicies
validation.x <- x[-indices]
validation.y <- y[-indices]
validation.df <- data.frame(X = validation.x, Y = validation.y)

# define the function
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# a dataframe to store performance data
performance <- data.frame()

for (d in 1:12)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
  
  # add a row of three values, including
  # rmse of actual and predicted values of y (training data)  
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))
  
  # same for validation data
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Validation',
                                  RMSE = rmse(validation.y, predict(poly.fit,
                                                                    newdata = validation.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()

#####################################
# Q4
# install.packages("ggplot2")
library('ggplot2')

set.seed(1)
x <- seq(0, 2, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)
df <- data.frame(X=x, Y=y)
ggplot(df, aes(x = X, y = Y)) +
  geom_point()

for (d in 1:10)
{
  model <- lm(Y ~ poly(X, degree = d), data = df)
  df$predictedY <- predict(model)
  title <- paste("degree", d)
  print(ggplot(df, aes(x = X, y = predictedY)) + geom_point() + ggtitle(title))
}

#################################
# Q5.

# install.packages("ggplot2")
library('ggplot2')

# polynomial fitting

set.seed(1)
x <- seq(0, 2, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

df <- data.frame(X = x, Y = y)
ggplot(df, aes(x = X, y = Y)) +
  geom_point()

model <- lm(Y ~ poly(X, degree = 25), data = df)
summary(model)

# add predictions to the data frame 
# df <- transform(df, PredictedY = predict(model))
# simpler
df$PredictedY <- predict(model)

# plot predicted values
ggplot(df, aes(x = X, y = PredictedY)) +
  geom_point() +
  geom_line()

### cross validation, this needs a training set and a validation set 

# set x and y again
set.seed(1)
x <- seq(0, 2, by = 0.01)
y <- sin(2 * pi * x) + rnorm(length(x), 0, 0.1)

# get a random sample of indicies
n <- length(x)
indices <- sort(sample(1:n, round(0.5 * n)))

# training data
training.x <- x[indices]
training.y <- y[indices]
training.df <- data.frame(X = training.x, Y = training.y)

# validation data is the other indicies
validation.x <- x[-indices]
validation.y <- y[-indices]
validation.df <- data.frame(X = validation.x, Y = validation.y)

# define the function
rmse <- function(y, h)
{
  return(sqrt(mean((y - h) ^ 2)))
}

# a dataframe to store performance data
performance <- data.frame()

for (d in 1:25)
{
  poly.fit <- lm(Y ~ poly(X, degree = d), data = training.df)
  
  # add a row of three values, including
  # rmse of actual and predicted values of y (training data)  
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'Training',
                                  RMSE = rmse(training.y, predict(poly.fit))))
  
  # same for validation data
  performance <- rbind(performance,
                       data.frame(Degree = d,
                                  Data = 'validation',
                                  RMSE = rmse(validation.y, predict(poly.fit,
                                                                    newdata = validation.df))))
}

ggplot(performance, aes(x = Degree, y = RMSE, linetype = Data)) +
  geom_point() +
  geom_line()


##########################################
# Q6.  Stackloss models

data(stackloss)

model1 <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data=stackloss)
summary(model1)
# 0.9136 / 0.8983

model2 <- lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc. + I(Acid.Conc.^2), data=stackloss)
summary(model2)
# 0.9187/ 0.8984

model3 <- lm(stack.loss ~ Air.Flow + + Water.Temp + I(Water.Temp^2) + Acid.Conc., data=stackloss)
summary(model3)
# 0.9334/ 0.9167

model5 <- lm(stack.loss ~ polym(Air.Flow, Water.Temp, Acid.Conc., degree=2), data=stackloss)
summary(model5)
# 0.957 / 0.9218
