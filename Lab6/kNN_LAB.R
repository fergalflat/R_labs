
# Q1

library(gmodels)
library(class)

wbcd <- read.csv("data/wisc_bc_data.csv", stringsAsFactors = F)
# drop the id feature
wbcd$id <- NULL

# table of diagnosis
table(wbcd$diagnosis)

# redefine the factor with labels
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))
class(wbcd)

# table of proportions 
prop.table(table(wbcd$diagnosis))

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

## create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data using list apply
# does not include the labels
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# help(as.data.frame)
# help(lapply)
# test <- lapply(wbcd[2:31], normalize)
# str(test)
# str(wbcd)

# confirm that normalization worked
summary(wbcd_n$area_mean)
hist(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

# get labels for training and test data
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

# help(knn)
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=21)

CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

## Try Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
predictions <- knn(train = wbcd_train, test = wbcd_test,
                   cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(predictions, wbcd_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

##################
# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# k=1
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=1)
CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=5
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=5)
CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=11
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=11)
CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=15
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=15)
CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=21
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=21)
CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

# k=27
predictions <- knn(train = wbcd_train, test = wbcd_test, 
                   cl = wbcd_train_labels, k=27)
CrossTable(predictions, wbcd_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)


##############################################################
# Q2

library(class)
library(gmodels)

data(iris)

set.seed(1)
iris_rand <- iris[order(runif(150)), ]
iris_z <- as.data.frame(scale(iris_rand[-5]))

iris_train <- iris_z[1:100,]
iris_test <- iris_z[101:150,]

iris_train_species <- iris_rand[1:100, 5]
iris_test_species <- iris_rand[101:150, 5]

?knn
predictions = knn(iris_train, iris_test, cl=iris_train_species , k = 3, prob=TRUE)

CrossTable(predictions, iris_test_species, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)


cm <- table(predictions, iris_test_species)
sum(diag(cm))/sum(cm)


########################################################
# Q3

library(class)
library(gmodels)
library(psych)
##########################
# read the data
credit <- read.csv("data/credit.csv")
summary(credit)

credit <- credit[c("amount", "months_loan_duration", "age", "credit_history", "default")]

credit_history <- dummy.code(credit$credit_history)
credit$credit_history <- NULL
credit <- data.frame(credit, credit_history)

###########################
# training and test split

# create a random sample for training and test data
set.seed(1)
credit_rand <- credit[order(runif(1000)), ]
credit_z <- scale(credit_rand[-4])

credit_train <- credit_z[1:500, ]
credit_test  <- credit_z[501:1000, ]

#######################
## Evaluate

credit_train_labels <- credit_rand[1:500, "default"]
credit_test_labels <- credit_rand[501:1000, "default"]

predictions <- knn(train = credit_train, test = credit_test, 
                   cl = credit_train_labels, k=5)

library(gmodels)
CrossTable(predictions, credit_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)


