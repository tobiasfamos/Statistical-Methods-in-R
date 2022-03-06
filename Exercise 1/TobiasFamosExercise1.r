# Exercise 1.1
createOverview <- function (table){
  means <- c(mean(table$Education),mean(table$Gender),mean(table$Wage))
  medians <- c(median(table$Education), median(table$Gender), median(table$Wage))
  standardDeviations <- c(sd(table$Education), sd(table$Gender), sd(table$Wage))
  min <- c(min(table$Education), min(table$Gender), min(table$Wage))
  max <- c(max(table$Education), max(table$Gender), max(table$Wage))
  names <- c("Education", "Gender", "Wage")
  overview <- data.frame(Name=names, Mean=means, Median=medians, StandardDeviation=standardDeviations, Minimum=min, Maximum=max)
  return(overview)
}

educationDataSet <- read.table("Exercise 1/Education.txt", header = T, sep="\t")
overview <- createOverview(educationDataSet)
print("Exercise 1.1:")
print(overview)


# Exercise 1.2
males <- subset(educationDataSet, Gender == 1)
females <- subset(educationDataSet, Gender == 2)
print("Exercise 1.2")
print("Males Overview")
malesOverview <- createOverview(males)
print(malesOverview)

print("Females Overview")
femalesOverview <- createOverview(females)
print(femalesOverview)
# We can infer from those numbers, that the Media and Mean Wage is slightly higher for Males than for females.
# Also can we see, that the median and mean education is about the same for both genders.
# But the standard deviation is higher on the Education for Females than males.
# This means that the distribution of Education on female Education is further apart than the male one.

# Exercise 1.3
summary(educationDataSet)
# There is a inconsistency in the Data set.
# There is one observation (ID: 234) that has a negative Edcuation.
# This might be a typing error. I would strike it out of the data set before doing further calculations

# Exercise 1.4
cor(educationDataSet$Gender, educationDataSet$Wage)
cor(educationDataSet$Education, educationDataSet$Wage)

# We have a quite strong correlation between Educaiton and Wage, but not a strong correlation between Gender and Wage
# Thus we could create a predictor for Wage by Education, but not by Gender.