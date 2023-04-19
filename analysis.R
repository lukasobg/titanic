rm(list = ls())
library(dplyr)
library(ggplot2)
library(corrplot)
library(vcd)
library(caret)
library(gridExtra)
library(FactoMineR)
library(factoextra)
setwd("Documents/opiskelu/multivariate/project")
dfa <- read.csv("cleaned.csv", header = TRUE)

# Show the first few rows of the dataframe
head(dfa)
colnames(dfa)
nrow(dfa)

BLUE = "#3574e2"
RED = "#cd34b5"

#### PLOT DATA ####
# SURVIVORS
survivor_counts = table(dfa$survived)
print(survivor_counts)
barplot(survivor_counts, main="Survived (1) and deceased (0) passengers", col = c('grey', BLUE))

# SEX
sex_counts = table(dfa$sex)
barplot(sex_counts, main="Sex of passengers", col = c(RED,BLUE))

# AGE
age_counts = table(dfa$age)
age_counts = age_counts[order(match(names(age_counts), c("toddler", "child", "teen", "adult", "senior")))]
print(age_counts)
barplot(age_counts, main="Age of passengers", col = BLUE)

# TICKET CLASS
class_counts = table(dfa$class)
barplot(class_counts, main="Ticket class of passengers", col = BLUE)

# SIBLINGS & PARENTS
sibsp_counts_ = table(dfa$sibsp)
barplot(sibsp_counts_, main="Number of siblings / spouses on board", col = BLUE)

# PARENTS AND CHILDREN
parch_counts_ = table(dfa$parch)
barplot(parch_counts_, main="Number of children / parents on board", col = BLUE)

# TICKET PRICE
fare_counts_ = table(dfa$fare)
fare_counts_ = sort(fare_counts_, decreasing = TRUE)
barplot(fare_counts_, main="Price of ticket", col = BLUE)

# HOMECOUNTRY
hc_counts_ = table(dfa$homecountry)
hc_counts_ = sort(hc_counts_, decreasing = TRUE)
barplot(hc_counts_, main="Homecountry of passengers", col = BLUE, names.arg = names(hc_counts_))

# EMBARKED
emb_counts = table(dfa$embarked)
barplot(emb_counts, main="Embarked from port", col = BLUE)

# DESTINATION
dest_counts_ = table(dfa$destination)
dest_counts_ = sort(dest_counts_, decreasing = TRUE)
barplot(dest_counts_, main="Destination of passengers", col = BLUE)

head(dfa)

# INDEPENDENCE TESTS
dfchi = data.frame(var = character(), statistic = numeric(), p.value = numeric())
dflog = data.frame(var = character(), coefficient = numeric(), p.value = numeric())
for (col in names(dfa)[-1]) {
  table = table(dfa$survived, dfa[, col])
  dfchi[nrow(dfchi) + 1, ] = c(col, chisq.test(table)$statistic, chisq.test(table)$p.value)
  
  formula = as.formula(paste("survived ~", col))
  model = glm(formula = formula, data = dfa, family = binomial)
  coefficient = coef(model)[2]
  p.value = summary(model)$coefficients[2,4]
  dflog[nrow(dflog) + 1, ] = c(col, coefficient, p.value)
}
print(dfchi)
print(dflog)

barplot(as.numeric(dfchi$statistic), names.arg = dfchi$var, main = "Chi-Square Statistic", las=2, col = BLUE)
barplot(as.numeric(dfchi$p.value), names.arg = dfchi$var, main = "Chi-Square p-value", las=2, col = BLUE)
barplot(as.numeric(dflog$coefficient), names.arg = dfchi$var, main = "LogReg Coefficient", las=2, col = BLUE)
barplot(as.numeric(dflog$p.value), names.arg = dfchi$var, main = "LogReg pvalue", las=2, col = BLUE)

full_model <- glm(survived ~ age + class + destination + embarked + fare + homecountry + parch + sex + sibsp, data = dfa, family = "binomial")
backwards_model <- step(full_model, direction = "backward", k = log(nrow(dfa)), trace = 0)
summary(full_model)
summary(backwards_model)

# CHOICE OF VARIABLES
df = dfa
df$homecountry = NULL
df$embarked = NULL
df$destination = NULL
head(df)

# tables
table(df$survived)
table(df$sex)
table(df$age)
table(df$parch)
table(df$sibsp)
table(df$fare)
table(df$class)

# REORDER some modalitied
age_order <- c("toddler", "child", "teen", "adult", "senior")
fare_order <- c("0-20", "20-100", "100-200", "200+")
df$age <- factor(df$age, levels = age_order)
df$fare <- factor(df$fare, levels = fare_order)

# BIVARIATE 
# Create dummy variables for categorical variables
df$class <- as.factor(df$class)
df$survived <- as.factor(df$survived)
dummy_data <- dummyVars(" ~ .", data = df)
df_numeric <- predict(dummy_data, newdata = df)
corr_matrix <- cor(df_numeric)
corr_matrix[upper.tri(corr_matrix)] <- NA
diag(corr_matrix) <- NA

corr_matrix["sexM","sexF"] <- NA
corr_matrix["age.child","age.toddler"] <- NA
corr_matrix["age.teen","age.toddler"] <- NA
corr_matrix["age.teen","age.child"] <- NA
corr_matrix["age.adult","age.toddler"] <- NA
corr_matrix["age.adult","age.child"] <- NA
corr_matrix["age.adult","age.teen"] <- NA
corr_matrix["age.senior","age.toddler"] <- NA
corr_matrix["age.senior","age.child"] <- NA
corr_matrix["age.senior","age.teen"] <- NA
corr_matrix["age.senior","age.adult"] <- NA
corr_matrix["class.2","class.1"] <- NA
corr_matrix["class.3","class.1"] <- NA
corr_matrix["class.3","class.2"] <- NA
corr_matrix["sibsp1","sibsp0"] <- NA
corr_matrix["sibsp2","sibsp0"] <- NA
corr_matrix["sibsp2","sibsp1"] <- NA
corr_matrix["sibsp3+","sibsp0"] <- NA
corr_matrix["sibsp3+","sibsp1"] <- NA
corr_matrix["sibsp3+","sibsp2"] <- NA
corr_matrix["parch1","parch0"] <- NA
corr_matrix["parch2","parch0"] <- NA
corr_matrix["parch2","parch1"] <- NA
corr_matrix["parch3+","parch0"] <- NA
corr_matrix["parch3+","parch1"] <- NA
corr_matrix["parch3+","parch2"] <- NA
corr_matrix["fare.20-100","fare.0-20"] <- NA
corr_matrix["fare.100-200","fare.0-20"] <- NA
corr_matrix["fare.100-200","fare.20-100"] <- NA
corr_matrix["fare.200+","fare.0-20"] <- NA
corr_matrix["fare.200+","fare.20-100"] <- NA
corr_matrix["fare.200+","fare.100-200"] <- NA
corr_matrix[,"survived.0"] <- NA

ggplot(data = melt(corr_matrix, na.rm = TRUE), aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1,1), name = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_rect(fill = "#F5F5F5"))

# BAR PLOTS
table(df$survived, df$age)
table(df$survived, df$sex)
table(df$survived, df$class)
table(df$survived, df$fare)
table(df$survived, df$sibsp)
table(df$survived, df$parch)
# Define the order of levels for the age variable
plot1 = ggplot(data = df, aes(x = age, fill = factor(survived))) +
  geom_bar(position = position_stack(), color = "black") +
  labs(title = "Survival by Age", x = "Gender", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
plot2 = ggplot(data = df, aes(x = sex, fill = factor(survived))) +
  geom_bar(position = position_stack(), color = "black") +
  labs(title = "Survival by Sex", x = "Sex", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
plot3 = ggplot(data = df, aes(x = class, fill = factor(survived))) +
  geom_bar(position = position_stack(), color = "black") +
  labs(title = "Survival by Class", x = "Class", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
plot4 = ggplot(data = df, aes(x = fare, fill = factor(survived))) +
  geom_bar(position = position_stack(), color = "black") +
  labs(title = "Survival by Fare", x = "Fare", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
plot5 = ggplot(data = df, aes(x = sibsp, fill = factor(survived))) +
  geom_bar(position = position_stack(), color = "black") +
  labs(title = "Survival by Sibsp", x = "Sibsp", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))
plot6 = ggplot(data = df, aes(x = parch, fill = factor(survived))) +
  geom_bar(position = position_stack(), color = "black") +
  labs(title = "Survival by Parch", x = "Parch", y = "Count") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes"))

grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 2, nrow = 3)


# MULTIVARIATE ANALYSIS
df_subset <- dfa %>% select(survived, age, sex)
proportions <- df_subset %>% group_by(age, sex) %>%
  summarize(survival_rate = mean(survived))
# create the heatmap with sex as a color scale
ggplot(proportions, aes(x = age, y = sex, fill = survival_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Age group", y = "Sex", fill = "Survival rate")

df_subset <- dfa %>% select(survived, class, sex)
proportions <- df_subset %>% group_by(class, sex) %>%
  summarize(survival_rate = mean(survived))
# create the heatmap with sex as a color scale
ggplot(proportions, aes(x = class, y = sex, fill = survival_rate)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Class group", y = "Sex", fill = "Survival rate")


# MCA
mca <- MCA(df, graph = FALSE)
summary(mca)

# Bar plot of variable and component correlation
p <- fviz_mca_var(mca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  title = "Variable Factor Map (cos2)", xlab="Principal Component", 
                  shape.var = 19)
data <- ggplot_build(p)$data[[2]]
p = p + geom_segment(data=data, aes(x=0, y=0, xend=x, yend=y), 
                 linestyle= "dashed",color="gray40", size=0.2)
p

# BIPLOT
fviz_mca_biplot(mca, habillage = df$survived, col.var = "black", axes=c(1,2))

# BIPLOT OBservations
scores <- as.data.frame(mca$ind$coord)
fviz_mca_ind(mca, geom.ind = "point", col.ind = df$survived, 
             addEllipses = FALSE, 
             title = "Biplot of Observations",
             legend.title = "Survival Status")

# Contribution plot of principal components
fviz_contrib(mca, choice = "var", axes = 1:2, 
             title = "Contribution of modalities")


# Heatmap of individuals and component scores
fviz_mca_ind(mca, col.ind = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             title= "MCA - Individuals")
