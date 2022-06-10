
install.packages("ggplot2")
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
