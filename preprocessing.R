rm(list = ls())
library(dplyr)
setwd("Documents/opiskelu/multivariate/project")
df <- read.csv("full.csv", header = TRUE)

# Show the first few rows of the dataframe
head(df)
colnames(df)

# Remove unnecessary columns
df$Name = NULL
df$Name_wiki = NULL
df$PassengerId = NULL
df$WikiId = NULL
df$Ticket = NULL
df$Boarded = NULL # same as Embarked
df$Lifeboat = NULL # only for survivors
df$Body = NULL
df$Cabin = NULL # so many missing values

# Remove rows where Survived is missing
nrow(df)
df = df[complete.cases(df$Survived), ]
print(length(df$Survived))

# Count the number of missing values for each column
missing_values = function(df) {
  na_count = sapply(df, function(x) sum(is.na(x)))
  na_count
}
print(missing_values(df))
nrow(df)

# Remove columns based on missing values
df$Age = NULL # Age_wiki is better
df$Class = NULL # Pclass is better
print(missing_values(df))

# Remove 4 instances where age is missing
df = df[complete.cases(df$Age_wiki), ]
print(missing_values(df))
nrow(df)

# Remove 2 instances where Embarked is missing
nrow(df)
table(df$Embarked)
df = df[-which(df$Embarked != "C" & df$Embarked != "Q" & df$Embarked != "S"),]
table(df$Embarked)
nrow(df)

# Rename and rearrange columns
colnames(df)
colnames(df) = c("survived","class","sex","sibsp","parch","fare","embarked","age","hometown","destination")
df = select(df, survived, sex, age, class, sibsp, parch, fare, hometown, embarked, destination)
colnames(df)
head(df)

# Write to csv for inspection
#write.table(df, file = "cleaned.tsv", sep = "\t", row.names = FALSE)

# Change hometown to country, same for destination
for (i in 1:nrow(df)) {
  df$hometown[i] = tail(strsplit(df$hometown[i], ",")[[1]],1)
  df$destination[i] = tail(strsplit(df$destination[i], ",")[[1]],1)
}
names(df)[names(df) == "hometown"] <- "homecountry"
head(df)

# Change male female to M F
df$sex = gsub("male", "M", df$sex)
df$sex = gsub("feM", "F", df$sex)
head(df)

# Other specific changes
df$homecountry = gsub(".*Ireland.*", "Ireland", df$homecountry)
df$homecountry = gsub(".*Russia.*", "Russia", df$homecountry)
df$homecountry = gsub(".*Ottoman Empire.*", "Ottoman Empire", df$homecountry)
df$homecountry = gsub(".*UK.*", "UK", df$homecountry)
df$homecountry = gsub(".*England.*", "England", df$homecountry)
df$homecountry = gsub(".*Lithuania.*", "Lithuania", df$homecountry)
df$homecountry = gsub(".*Syria.*", "Syria", df$homecountry)

head(df, 10)

BLUE = "#3574e2"
RED = "#cd34b5"

# Begin univariate analysis
dfa = df

# SURVIVORS
survivor_counts = table(df$survived)
barplot(survivor_counts, main="Survived (1) and deceased (0) passengers", col = c('grey', BLUE))

# SEX
sex_counts = table(df$sex)
barplot(sex_counts, main="Sex of passengers", col = c(RED,BLUE))

# AGE
# Regroup age variable to toddler 0-5, child 5-15, young adult 15-25, adult 25-50, senior 25-50
bins <- c(0, 5, 15, 25, 50, Inf)
age_groups <- cut(df$age, breaks = bins, labels = c("toddler", "child", "teen", "adult", "senior"))
dfa$age = age_groups

age_counts = table(dfa$age)
hist(df$age, main="Age of passengers", col=BLUE)
barplot(age_counts, main="Age of passengers", col = BLUE)
table(dfa$age)

# TICKET CLASS
class_counts = table(df$class)
barplot(class_counts, main="Ticket class of passengers", col = BLUE)

# SIBLINGS & SPOUSES
sibsp_counts = table(df$sibsp)
sibsp_groups = cut(df$sibsp, breaks = c(-0.5, 0.5, 1.5, 2.5, Inf), labels = c("0", "1", "2", "3+"))
dfa$sibsp = sibsp_groups
sibsp_counts_ = table(dfa$sibsp)
barplot(sibsp_counts, main="Number of siblings / spouses on board", col = BLUE)
barplot(sibsp_counts_, main="Number of siblings / spouses on board", col = BLUE)
table(df$sibsp)
table(dfa$sibsp)

# PARENTS AND CHILDREN
parch_counts = table(df$parch)
parch_groups = cut(df$parch, breaks = c(-0.5, 0.5, 1.5, 2.5, Inf), labels = c("0", "1", "2", "3+"))
dfa$parch = parch_groups
parch_counts_ = table(dfa$parch)
barplot(parch_counts, main="Number of children / parents on board", col = BLUE)
barplot(parch_counts_, main="Number of children / parents on board", col = BLUE)
table(df$parch)
table(dfa$parch)

# TICKET PRICE
median(df$fare)
mean(df$fare)
fare_groups = cut(df$fare, breaks = c(-Inf, 20.0, 100.0, 200.0, Inf), labels = c("0-20", "20-100", "100-200", "200+"))
dfa$fare = fare_groups
fare_counts_ = table(dfa$fare)
hist(df$fare, main="Price of ticket", col=BLUE, breaks=50) 
barplot(fare_counts_, main="Price of ticket", col = BLUE)
table(dfa$fare)

# HOMECOUNTRY
dfa$homecountry = gsub("Argentina", "South America", dfa$homecountry)
dfa$homecountry = gsub("Peru", "South America", dfa$homecountry)
dfa$homecountry = gsub("Uruguay", "South America", dfa$homecountry)
dfa$homecountry = gsub("Mexico", "South America", dfa$homecountry)
dfa$homecountry = gsub("Belgium", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Bosnia", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Bulgaria", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Croatia", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Denmark", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Finland", "Europe", dfa$homecountry)
dfa$homecountry = gsub("France", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Germany", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Greece", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Italy", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Slovenia", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Spain", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Sweden", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Switzerland", "Europe", dfa$homecountry)
dfa$homecountry = gsub("The Netherlands", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Turkey", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Lithuania", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Russia", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Syria", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Norway", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Poland", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Portugal", "Europe", dfa$homecountry)
dfa$homecountry = gsub("Scotland", "UK", dfa$homecountry)
dfa$homecountry = gsub("Wales", "UK", dfa$homecountry)
dfa$homecountry = gsub("England", "UK", dfa$homecountry)
dfa$homecountry = gsub("Ireland", "UK", dfa$homecountry)
dfa$homecountry = gsub("British India", "Asia", dfa$homecountry)
dfa$homecountry = gsub("China", "Asia", dfa$homecountry)
dfa$homecountry = gsub("Japan", "Asia", dfa$homecountry)
dfa$homecountry = gsub("Egypt", "Africa", dfa$homecountry)
dfa$homecountry = gsub("South Africa", "Africa", dfa$homecountry)
dfa$homecountry = gsub("Lebanon", "Africa", dfa$homecountry)
dfa$homecountry = gsub("Ottoman Empire", "Africa", dfa$homecountry)
dfa$homecountry = gsub("Channel Islands", "Other", dfa$homecountry)
dfa$homecountry = gsub("Siam", "Other", dfa$homecountry)
dfa$homecountry = gsub("Unknown", "Other", dfa$homecountry)
dfa$homecountry = gsub("Australia", "Other", dfa$homecountry)
dfa$homecountry = gsub("New York City", "US", dfa$homecountry)
dfa$homecountry = gsub("Canada", "US", dfa$homecountry)
dfa$homecountry = gsub(".*US.*", "US", dfa$homecountry)
dfa$homecountry = gsub(" UK", "UK", dfa$homecountry)
dfa$homecountry = gsub(" Europe", "Europe", dfa$homecountry)
dfa$homecountry = gsub(" Africa", "Africa", dfa$homecountry)
dfa$homecountry = gsub("Asia", "Other", dfa$homecountry)
dfa$homecountry = gsub("South America", "Other", dfa$homecountry)
table(df$homecountry)
table(dfa$homecountry)
hc_counts = table(df$homecountry)
hc_counts = sort(hc_counts, decreasing = TRUE)
hc_counts_ = table(dfa$homecountry)
hc_counts_ = sort(hc_counts_, decreasing = TRUE)
par(mar=c(6, 4, 4, 2) + 0.1)
barplot(hc_counts, main="Homecountry of passengers", col = BLUE, names.arg = names(hc_counts), las=2, cex.names = 0.73)
par(mar=c(5, 4, 4, 2) + 0.1)
barplot(hc_counts_, main="Homecountry of passengers", col = BLUE, names.arg = names(hc_counts_))

# EMBARKED
emb_counts = table(df$embarked)
table(df$embarked)
barplot(emb_counts, main="Embarked from port", col = BLUE)
sum(df$embarked == '')

# DESTINATION
dfa$destination = gsub("Mexico", "Other", dfa$destination)
dfa$destination = gsub("Japan", "Other", dfa$destination)
dfa$destination = gsub("Peru", "Other", dfa$destination)
dfa$destination = gsub("Cuba", "Other", dfa$destination)
dfa$destination = gsub("Uruguay", "Other", dfa$destination)
dfa$destination = gsub("Connecticut US", "US", dfa$destination)
dfa$destination = gsub("DC US", "US", dfa$destination)
dfa$destination = gsub("Haiti", "US", dfa$destination)
dfa$destination = gsub("Massachusetts", "US", dfa$destination)
dfa$destination = gsub("New York City", "NYC", dfa$destination)
table(dfa$destination)
table(df$destination)
dest_counts = table(df$destination)
dest_counts = sort(dest_counts, decreasing = TRUE)
dest_counts_ = table(dfa$destination)
dest_counts_ = sort(dest_counts_, decreasing = TRUE)
par(mar=c(6, 4, 4, 2) + 0.1)
barplot(dest_counts, main="Destination of passengers", col = BLUE, las = 2, cex.names = 0.8)
par(mar=c(5, 4, 4, 2) + 0.1)
barplot(dest_counts_, main="Destination of passengers", col = BLUE)

head(dfa)
# Write to csv for inspection
write.csv(dfa, file = "cleaned.csv", row.names = FALSE)

