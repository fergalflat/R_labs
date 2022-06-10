
# An introduction to ROC analysis - Tom Fawcett
# Reads a data frame
# 
# Instance 1:20
# Class y/n
# Score (probability)

rocData <- read.csv("data/rocExample.csv",header=TRUE)
YN <- rep(NA, length(rocData$Instance))
points <- matrix(rep(NA, 2 * length(rocData$Instance)), ncol=2)

P <- sum(rocData$Class == 'p')
N <- sum(rocData$Class == 'n')

length(YN) # 20
for (i in 1:length(YN)) {
  
  # choose threshold
  threshold <- rocData$Score[i]
  cat ('threshold=', threshold)
  # get predicted class based on Score (p) and threshold
  YN[rocData$Score >= threshold] = 'Y'
  YN[rocData$Score < threshold] = 'N'
  YN = as.factor(YN)
  
  TPVector <- (rocData$Class == 'p') & (YN == 'Y')
  TPR <- sum(TPVector) / P
  FPVector <- (rocData$Class == 'n') & (YN == 'Y')
  FPR <- sum(FPVector) / N
  point = c(FPR,TPR)
  cat(' point = ', point, '\n')
  
  points[i,1] <- FPR
  points[i,2] <- TPR
}

heading = paste("ROC curve")
plot(points[,1], points[,2], main=heading, xlab='FPR', ylab='TPR') 
lines(points[,1], points[,2], type='S')




# Q1
# An introduction to ROC analysis - Tom Fawcett
# Reads a data frame
# 
# Instance 1:20
# Class y/n
# Score (probability)

rocData <- read.csv("data/rocExample.csv",header=TRUE)
YN <- rep(NA, length(rocData$Instance))
points <- matrix(rep(NA, 2 * length(rocData$Instance)), ncol=2)

P <- sum(rocData$Class == 'p')
N <- sum(rocData$Class == 'n')

length(YN) # 20
for (i in 1:length(YN)) {
  # choose threshold
  threshold <- rocData$Score[i]
  cat ('threshold=', threshold)
  # get predicted class based on Score (p) and threshold
  YN[rocData$Score >= threshold] = 'Y'
  YN[rocData$Score < threshold] = 'N'
  YN = as.factor(YN)
  
  TPVector <- (rocData$Class == 'p') & (YN == 'Y')
  TPR <- sum(TPVector) / P
  FPVector <- (rocData$Class == 'n') & (YN == 'Y')
  FPR <- sum(FPVector) / N
  point = c(FPR,TPR)
  cat(' point = ', point, '\n')
  
  points[i,1] <- FPR
  points[i,2] <- TPR
}

heading = paste("ROC curve")
plot(points[,1], points[,2], main=heading, xlab='FPR', ylab='TPR') 
lines(points[,1], points[,2], type='S')



##########################################
# Q2

# An introduction to ROC analysis - Tom Fawcett
# Calculates the Accuracy for every value of p. 

rocData <- read.csv("data/rocExample.csv",header=TRUE)
YN <- rep(NA, length(rocData$Instance))
points <- matrix(rep(NA, 2 * length(rocData$Instance)), ncol=2)

P <- sum(rocData$Class == 'p')
N <- sum(rocData$Class == 'n')

bestAccuracy = 0
keepTPR = 0
keepFPR = 0
for (i in 1:length(YN)) {
  threshold <- rocData$Score[i]
  cat ('threshold=', threshold)
  YN[rocData$Score >= threshold] = 'Y'
  YN[rocData$Score < threshold] = 'N'
  YN = as.factor(YN)
  rocData
  
  TPVector <- (rocData$Class == 'p') & (YN == 'Y')
  FPVector <- (rocData$Class == 'n') & (YN == 'Y')
  TNVector <- (rocData$Class == 'n') & (YN == 'N')
  FNVector <- (rocData$Class == 'p') & (YN == 'N')
  
  TPR <- sum(TPVector) / P
  FPR <- sum(FPVector) / N
  point = c(FPR,TPR)
  cat(' point is (' , point[1] , ',' , point[2], ')\n')
  
  TP <- sum(TPVector)
  TN <- sum(TNVector)
  
  accuracy <- (TP + TN) / (P + N)
  cat( 'TP=', TP, 'TN=', TN, 'accuracy=', accuracy, '\n')
  
  if(accuracy > bestAccuracy) {
    bestAccuracy = accuracy
    bestThreshold = threshold
    keepTPR = TPR
    keepFPR = FPR
  }
  
  points[i,1] <- FPR
  points[i,2] <- TPR
}

cat('Best accuracy is ', bestAccuracy, ' for threshold  ', bestThreshold,
    'FPR' , keepFPR , 'TPR', keepTPR )

######################################################
# Q3

library(crossval)
credit <- read.csv("data/credit.csv")
set.seed(1)
credit_rand <- credit[order(runif(1000)), ]
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

## Train
model <- glm(default ~ ., data = credit_train, family="binomial" )
# summary(credit_model)

# Prediction
prediction <- predict(model, credit_test, type="response")

# set a threshold
predictionYN <- rep(NA, length(prediction))
threshold <- 0.5
predictionYN[prediction >= threshold] = 'yes'
predictionYN[prediction < threshold] = 'no'

# confusion matrix and accuracy
cm <- confusionMatrix(predicted=predictionYN, actual=credit_test$default,  negative="no")
cm
diagnosticErrors(cm)['acc']

#####################################
# Q4

library(crossval)
credit <- read.csv("data/credit.csv")
set.seed(1)
credit_rand <- credit[order(runif(1000)), ]
credit_train <- credit_rand[1:900, ]
credit_test  <- credit_rand[901:1000, ]

## Train
model <- glm(default ~ ., data = credit_train, family="binomial" )
# summary(credit_model)

# prediction
prediction <- predict(model, credit_test, type="response")

predictionYN <- rep(NA, length(prediction))
for(threshold in seq(from=0, to=1, by=0.1)) {
  predictionYN[prediction >= threshold] = 'yes'
  predictionYN[prediction < threshold] = 'no'
  cm <- confusionMatrix(predicted=predictionYN, 
                        actual=credit_test$default,  negative="no")
  cm
  cat('threshold ', threshold, 'accuracy ', diagnosticErrors(cm)['acc'], '\n')
}
