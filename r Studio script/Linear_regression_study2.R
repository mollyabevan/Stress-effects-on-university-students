#install packages 
install.packages("foreign")
install.packages("stringr")
install.packages("ggplot2")
install.packages("here")
install.packages("apaTables")
install.packages("ordinal")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("readxl")
install.packages("psych")
install.packages("janitor")
install.packages("readxl")
install.packages("likert")
install.packages("parameters")

# Load packages:
library(foreign)
library(psych)
library(car)
library(stringr)
library(ggplot2)
library(likert) 
library(here) # package to have relative paths, for ex.: file = here("data_analysis/plots/age.pdf")
library(apaTables)
library(ordinal)
library(dplyr)
library(ggpubr)
library(readxl)
library(psych)
library(janitor)
library(readxl)
library(likert)

# Load the data:
study_data2 <- read_excel("study_data2.xlsx")
View(study_data2)

#Check column names
colnames(study_data2)

# Step 1: Rename the columns
names(study_data2)[names(study_data2) == "...62"] <- "stress_increase"
names(study_data2)[names(study_data2) == "...3"] <- "age"
names(study_data2)[names(study_data2) == "...2"] <- "gender"
names(study_data2)[names(study_data2) == "...69"] <- "mental_health_decline"

#Check column names
colnames(study_data2)

#recode the variables 

study_data2$stress_increase <- factor(study_data2$stress_increase, levels = c(0, 1), labels = c("no", "yes"))

study_data2$gender <- factor(study_data2$gender, levels = c(0, 1), labels = c("female", "male"))

study_data2$mental_health_decline <- factor(
  study_data2$mental_health_decline,
  levels = c(1, 2, 3, 4),
  labels = c("not at all", "somehow", "plenty", "severely"),
  ordered = TRUE
)

# Convert variables to numeric
study_data2$mental_health_decline <- as.numeric(study_data2$mental_health_decline)
study_data2$stress_increase <- as.numeric(study_data2$stress_increase)

# For gender, convert to numeric (this could be done using factor encoding if it's categorical)
study_data2$gender <- as.numeric(factor(study_data2$gender))

# For age, convert to numeric if it's not already
study_data2$age <- as.numeric(study_data2$age)

# Load the MASS package
install.packages("MASS")
library(MASS)

# Fit the ordinal regression model
model <- lm(mental_health_decline ~ stress_increase + age + gender, data = study_data2)

# Summary of the model
summary(model)
print(model_summary)
model_summary <- model_parameters(model, standardize = "refit", ci_method = "wald"") 
  

# 1. Descriptive statistics
summary(study_data2)
describe(study_data2)

# 2. Confidence intervals
confint(model)

# 4. Standardized coefficients (Beta)
study_data2$age_standardized <- scale(study_data2$age)
study_data2$stress_increase_standardized <- scale(study_data2$stress_increase)
study_data2$gender_standardized <- scale(study_data2$gender)

# Fit the model using standardized predictors
model_standardized <- lm(mental_health_decline ~ stress_increase_standardized + age_standardized + gender_standardized, data = study_data2)
summary(model_standardized)

# Run your model
model_mh <- lm(mental_health_decline ~ stress_increase + age + gender, data = study_data2)

# Get model summary with standardised beta, SE, 95% CI, t-value, p-value
model_mh_summary <- model_parameters(model_mh, standardize = "refit", ci_method = "wald")
print(model_mh_summary)

# Descriptive statistics for variables used in the model
describe(study_data2[, c("mental_health_decline", "stress_increase", "age", "gender")])

model <- lm(mental_health_decline ~ stress_increase + age + gender, data = study_data2)
standardised_model <- lm.beta(model)

summary(standardised_model)






