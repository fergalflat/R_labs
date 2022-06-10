#Q1

Name = c("Alex", "Lilly", "Mark", "Oliver", "Martha", "Lucas", "Caroline")
Age = c(25, 31, 23, 52, 76, 49, 26)
Height = c(177, 163, 190, 179, 163, 183, 164)
Weight = c(57, 69, 83, 75, 70, 83, 53)
Sex <-as.factor(c("F", "F", "M", "M", "F", "M", "F"))
f=data.frame(Name, Age, Height, Weight, Sex, stringsAsFactors = FALSE)

#Q2

data.frame(row.names(f) <- Name)
df <- f[, -c(1)]

#Q3

Name <- f[, -c(2:5)]
newColDF <- cbind(Name, Working = c("Yes", "No", "No", "Yes", "Yes", "No", "Yes"))

df2 <- cbind(f, newColDF)
df2 <- df2[, -c(6)]

#Q4
state.center
class(state.center)
data.state <- as.data.frame(state.center)

#5
Age <- c(45:41, 30:33)
Class <- rep(c("A", "B", "C"), times = 3)
Grade <-  round(rnorm(9, 65, 5))
df = data.frame(Age, Class, Grade)
df$Age
order(df$Age)
df[order(df$Age), ]

#6
class(VADeaths)
df3 <- as.data.frame(VADeaths)    
df3$Total <- rowSums(df3[1:4]) 
df3 <- df3[, c(5, 1:4)]

#7
class(state.x77)
df4 <- as.data.frame(state.x77)
nrow(subset(df4, df4$Income < 4300))
row.names(df4)[(which(max(df4$Income) == df4$Income))]

#8
df <- swiss[c(1:3, 10:13), c("Examination", "Education", "Infant.Mortality")]
df[df == "24.4"] <- "NA"
df["Total",] <- c(sum(df$Examination), sum(df$Education), sum(df$Infant.Mortality, na.rm = TRUE))

