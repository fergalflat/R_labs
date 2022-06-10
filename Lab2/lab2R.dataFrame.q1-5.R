
# Exercise 1 
Name <- c("Alex", "Lilly", "Mark", "Oliver", "Martha", "Lucas", "Caroline")
Age <- c(25, 31, 23, 52, 76, 49, 26)
Height <- c(177, 163, 190, 179, 163, 183, 164)
Weight <- c(57, 69, 83, 75, 70, 83, 53)
Sex <- as.factor(c("F", "F", "M", "M", "F", "M", "F"))

df1 <- data.frame (Name, Age, Height, Weight, Sex, 
                  stringsAsFactors = FALSE)

# df1 <- data.frame (Name, Age, Height, Weight, Sex)

# df1 <- data.frame (Name, Age, Height, Weight, Sex)
help(data.frame)

# Exercise 2 

sex2 <- c("F", "F", "M", "M", "F", "M", "F")
df2 <- data.frame (Age, Height, Weight, sex2)
row.names(df2) <- Name

cbind



# Exercise 3

Name <- c("Alex", "Lilly", "Mark", "Oliver", "Martha", "Lucas", "Caroline")
Working <- as.factor(c("Yes", "No", "No", "Yes", "Yes", "No", "Yes"))

dfT <- data.frame(Name, Working)

df3 <- data.frame (df1,dfT)
# or
df3 <- cbind (df1,dfT)

# this gets a sub dataframe
df3[2]

# these get a column
df3[,2]
df3[[2]]
df3$Age

# drop a column
df3 <- df3[-6]
# or
df3[6] <- NULL
df3$Name.1 <- NULL

# Exercise 4
help("state.center")
class (state.center)
# gives an error
# data(state.center)
data(state)
df <- as.data.frame(state.center)
#or
df <- data.frame(state.center)

row.names(df) <- state.name
names(df) <- c("Longitude", "Latitude")

# Exercise 5
help(rnorm)
set.seed(5)

# Define vectors
v <- c(45:41, 30:33)
b <- rep(c("A", "B", "C"), 3)
n <- round(rnorm(n=9, mean=65, sd=5))

df <- data.frame(v, b, n)
names(df) <- c("Age", "Class", "Grade")
# or
df <- data.frame(Age = v, Class = b, Grade = n)

# column
df$Age
help(order)
#sort order of column
order(df$Age)
# use sort order to get sorted vector
df$Age[order(df$Age)]

# sort the df on increasing values of Age
df[order(df$Age), ]  

df$newCol = 21:29
