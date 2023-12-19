#maxGen mistery
refpop <- read_excel("refpop.xlsx")
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(GGally)

# Convert yob to numeric
refpop$yob <- as.numeric(refpop$yob)

# Exploratory Data Analysis (EDA)
summary(refpop)
str(refpop)

# Correlation Analysis
cor_matrix <- cor(refpop[, c("yob", "maxgen")], use = "complete.obs")
print(cor_matrix)

# ANOVA or t-tests
anova_results_sex <- aov(maxgen ~ Sex, data = refpop)
print(summary(anova_results_sex))

# Perform t-tests or ANOVA for other categorical variables (country, damline, sireline)

# Multiple Regression Analysis
reg_model <- lm(maxgen ~ Sex + country + damline + sireline, data = refpop)
summary(reg_model)

# Visualizations
# Scatter plot with regression line for yob vs. maxgen
ggplot(refpop, aes(x = yob, y = maxgen, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year of Birth", y = "maxgen")

# Box plot for Sex vs. maxgen
ggplot(refpop, aes(x = Sex, y = maxgen, fill = Sex)) +
  geom_boxplot() +
  labs(x = "Sex", y = "maxgen")

# Kernel Density Estimation (KDE) for maxgen
ggplot(refpop, aes(x = maxgen)) +
  geom_density() +
  labs(x = "maxgen", y = "Density")

# Additional visualizations for other variables (country, damline, sireline)

# Feature Engineering (if needed)

# Domain Knowledge (consult experts if available)

# Statistical Tests (if needed)

# Interpretation of Results


refpop<-read_excel("refpop.xlsx")
# ANOVA or t-tests for categorical variables
anova_results_country <- aov(maxgen ~ country, data = refpop)
print(summary(anova_results_country))

anova_results_damline <- aov(maxgen ~ damline, data = refpop)
print(summary(anova_results_damline))

anova_results_sireline <- aov(maxgen ~ sireline, data = refpop)
print(summary(anova_results_sireline))

# Visualizations for categorical variables
# Box plots for country, damline, sireline
ggplot(refpop, aes(x = country, y = maxgen, fill = country)) +
  geom_boxplot() +
  labs(x = "Country", y = "maxgen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

str(refpop)

ggplot(refpop, aes(x = damline, y = maxgen, fill = damline)) +
  geom_boxplot() +
  labs(x = "Dam Line", y = "maxgen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(refpop, aes(x = sireline, y = maxgen, fill = sireline)) +
  geom_boxplot() +
  labs(x = "Sire Line", y = "maxgen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Multiple Regression Analysis with selected variables
reg_model_selected <- lm(maxgen ~ yob + Sex + country + as.factor(damline), data = refpop)
summary(reg_model_selected)

reg_model_selected

# Visualizations for multiple regression analysis
# Scatter plot with regression line for yob vs. maxgen (colored by Sex)
ggplot(refpop, aes(x = yob, y = maxgen, color = Sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Year of Birth", y = "maxgen")

# Box plot for country vs. maxgen (colored by Sex)
ggplot(refpop, aes(x = country, y = maxgen, fill = Sex)) +
  geom_boxplot() +
  labs(x = "Country", y = "maxgen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Box plot for damline vs. maxgen (colored by Sex)
ggplot(refpop, aes(x = damline, y = maxgen, fill = Sex)) +
  geom_boxplot() +
  labs(x = "Dam Line", y = "maxgen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(refpop, aes(x = maxgen, fill = Sex)) +
  geom_density(alpha = 0.5) +
  labs(x = "maxgen", y = "Density") +
  theme_minimal()
# Interpretation of Results
# Further analysis, visualization, and domain knowledge interpretation as needed

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("glmnet")
library(glmnet)
names(refpop)

refpop<-read_excel("refpop-new.xlsx")
# Divide refpop into two groups based on maxgen
refpop <- refpop %>%
  mutate(group = ifelse(maxgen <= 30, "Group1", "Group2"))

# Encode categorical variables as factors
refpop <- refpop %>%
  mutate(across(c(country, damline, sireline, dam, sire), factor))

# Prepare the data for logistic regression
model_data <- refpop %>%
  select(-maxgen)  # Remove the maxgen variable from predictors

# Split the data into training and test sets
set.seed(123)
train_index <- sample(nrow(model_data), 0.8 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Convert categorical variables to numeric using one-hot encoding
train_data <- train_data %>%
  mutate(
         country = as.numeric(country),
         damline = as.numeric(damline),
         sireline = as.numeric(sireline),
         dam = as.numeric(dam),
         sire = as.numeric(sire))

test_data <- test_data %>%
  mutate(
    country = as.numeric(country),
    damline = as.numeric(damline),
    sireline = as.numeric(sireline),
    dam = as.numeric(dam),
    sire = as.numeric(sire))
        

# Remove non-predictor variables
predictor_matrix <- as.matrix(train_data[, -which(names(train_data) %in% c("group"))])

# Remove rows with missing values
complete_rows <- complete.cases(predictor_matrix)
cleaned_data <- train_data[complete_rows, ]

# Fit a logistic regression model using LASSO regularization
model <- cv.glmnet(as.matrix(cleaned_data[, -which(names(cleaned_data) %in% c("group"))]), 
                   cleaned_data$group, 
                   family = "binomial")


# Plot the cross-validated mean deviance for different lambda values
plot(model)

# Select the best lambda value based on cross-validation
best_lambda <- model$lambda.min

# Fit the final logistic regression model with the selected lambda
final_model <- glmnet(as.matrix(train_data[, -ncol(train_data)]), 
                      train_data$group, 
                      alpha = 1,
                      lambda = best_lambda,
                      family = "binomial")

summary(final_model)

# Predict the probabilities on the test set
test_probs <- predict(final_model, 
                      newx = as.matrix(test_data[, -ncol(test_data)]), 
                      s = best_lambda, 
                      type = "response")

# Evaluate the model's performance
test_data$predicted_group <- ifelse(test_probs > 0.5, "Group2", "Group1")
confusion_matrix <- table(test_data$group, test_data$predicted_group)
print(confusion_matrix)

# Calculate accuracy, sensitivity, and specificity
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])

cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
