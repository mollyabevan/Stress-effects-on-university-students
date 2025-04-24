#install packages 
install.packages("tidyverse")
install.packages("psyntur")
install.packages("readxl")
install.packages("psych")

#load packages
library(tidyverse)
library(psyntur)
library(readxl)
library(psych)

#load and look at the data
study_data1 <- read_excel("study.data1.xlsx")
View(study_data1)

#Check column names
colnames(study_data1)

# Linear regression model to predict CR_post (correct responses after stress) based on cortisol (sCort_pre), age, and sex
model_CR_post <- lm(CR_post ~ sCort_pre + age + BMI + sex, data = study_data1)

# Display the model summary
summary(model_CR_post)

# Calculate the change in correct responses (delta)
study_data1$CR_DIF <- study_data1$CR_post - study_data1$CR_pre

# Linear regression model to predict the change in correct responses (CR_DIF) based on cortisol and other variables
model_CR_DIF <- lm(CR_DIF ~ sCort_pre + age + BMI + sex, data = study_data1)

# Display the model summary
summary(model_CR_DIF)

#Linear regression model including other stress related variables 
model_CR_post_multiple <- lm(CR_DIF ~ sCort_pre + sCort_1 + sCort_10 + sCort_20 + age + BMI + sex, data = study_data1)

# Display the model summary
summary(model_CR_post_multiple)

summary(model_CR_post_multiple)


# Scatterplot with regression line for CR_post vs sCort_pre
ggplot(study_data1, aes(x = sCort_pre, y = CR_post)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "blue") +  # Add regression line
  labs(title = "Effect of Pre-Stress Cortisol on Post-Stress Correct Responses",
       x = "Pre-Stress Cortisol Level",
       y = "Post-Stress Correct Responses") +
  theme_minimal()


# Linear regression model to predict RT_post (reaction time after stress) based on cortisol (sCort_pre), age, and sex
model_RT_post <- lm(RT_post ~ sCort_pre + age + BMI + sex, data = study_data1)

# Display the model summary
summary(model_RT_post)

# Calculate the change in reaction time (delta)
study_data1$RT_DIF <- study_data1$RT_post - study_data1$RT_pre

# Linear regression model to predict the change in reaction time (RT_DIF) based on cortisol and other variables
model_RT_DIF <- lm(RT_DIF ~ sCort_pre + age + BMI + sex, data = study_data1)

# Display the model summary
summary(model_RT_DIF)

#Linear regression model including other stress related variables 
model_RT_DIF_multiple <- lm(RT_DIF ~ sCort_pre + sCort_1 + sCort_10 + sCort_20 + age + BMI + sex, data = study_data1)

# Display the model summary
summary(model_RT_DIF_multiple)

# Scatterplot with regression line for RT_post vs sCort_pre
ggplot(study_data1, aes(x = sCort_pre, y = RT_post)) +
  geom_point() +  # Scatterplot
  geom_smooth(method = "lm", color = "blue") +  # Add regression line
  labs(title = "Effect of Pre-Stress Cortisol on Post-Stress Reaction Time",
       x = "Pre-Stress Cortisol Level",
       y = "Post-Stress Reaction Time") +
  theme_minimal()


# Install and load patchwork if you haven't already
install.packages("patchwork")
library(patchwork)

# Scatterplot 1: Cortisol levels before stress vs correct responses
plot1 <- ggplot(study_data1, aes(x = sCort_pre, y = CR_post)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Cortisol Before Stress vs Correct Responses",
       x = "Pre-Stress Cortisol Level",
       y = "Post-Stress Correct Responses") +
  theme_minimal()

# Scatterplot 2: Cortisol levels before stress vs reaction time
plot2 <- ggplot(study_data1, aes(x = sCort_pre, y = RT_post)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red") + 
  labs(title = "Cortisol Before Stress vs Reaction Time",
       x = "Pre-Stress Cortisol Level",
       y = "Post-Stress Reaction Time") +
  theme_minimal()

# Combine the two plots side by side
plot1 + plot2


#scatterplot of CR_DIF and key variables 
install.packages('ggplot2')
library(ggplot2)

ggplot(study_data1, aes(x = sCort_pre, y = CR_DIF)) +
  geom_point(alpha = 0.6, color = "#2C3E50") +  # scatterplot points
  geom_smooth(method = "lm", se = TRUE, color = "#E74C3C") +  # regression line with CI
  labs(
    title = "Scatterplot of Cortisol (Pre-Stress) vs. Change in Correct Responses",
    x = "sCort_pre (Pre-Stress Cortisol)",
    y = "CR_DIF (Change in Correct Responses)"
  ) +
  theme_minimal()


install.packages("sjPlot")
library(sjPlot)

# Create the table
tab_model(model_CR_post_multiple,
          show.ci = TRUE,
          show.std = TRUE,
          show.se = TRUE,
          p.style = "numeric",
          title = "Linear Regression Results for CR_DIF")


#get linear regression results for table
model <- lm(RT_DIF ~ sCort_pre + sCort_1 + sCort_10 + sCort_20 + age + BMI + sex, data = study_data1)

# Install packages if needed
install.packages(c("parameters", "sjPlot", "effectsize", "broom"))

# Load packages
library(parameters)
library(sjPlot)
library(effectsize)

# Get standardised coefficients with confidence intervals
model_summary <- model_parameters(model, standardize = "refit", ci_method = "wald")
print(model_summary)

#confidence intervals
confint(model)


# Install and load if needed
install.packages("lm.beta")
library(lm.beta)

# Run your model and get standardised coefficients
model <- lm(RT_DIF ~ sCort_pre + sCort_1 + sCort_10 + sCort_20 + age + BMI + sex, data = study_data1)
standardised_model <- lm.beta(model)

# View standardised betas and their standard errors
summary(standardised_model)

mean(study_data1$sCort_DIF)  # Mean of pre-stress cortisol
sd(study_data1$sCort_DIF)    # Standard deviation of pre-stress cortisol
range(study_data1$sCort_DIF) # Range (min, max) of pre-stress cortisol

mean(study_data1$RT_post)    # Mean of post-stress reaction time
sd(study_data1$RT_post)      # Standard deviation of post-stress reaction time
range(study_data1$RT_post)   # Range (min, max) of post-stress reaction time

# Get detailed descriptive statistics for all numeric variables in the dataset
describe(study_data1)