
# Install packages
install.packages(c("ROSE", "DMwR", "caret", "dplyr"))

# Load packages
library(ROSE)
library(dplyr)
library(caret)
library(MASS)
library(ggplot2)
library(pROC)

# Set as Working Directory
data <- read_excel("2023FishingStatistics.xlsx")

# Define bass_data as subset of data
bass_data <- data[data$Species == "Large Mouth Bass",] #example, replace condition with your own
# Remove rows with missing values
bass_data <- bass_data[complete.cases(bass_data),]

# Define remove_outliers function
remove_outliers <- function(df, column_name) {
  df[[column_name]] <- as.numeric(df[[column_name]])
  df <- na.omit(df) 
  quartiles <- quantile(df[[column_name]], probs = c(.25, .75), na.rm = TRUE)
  IQR <- IQR(df[[column_name]], na.rm = TRUE)
  lower_bound <- quartiles[1] - 1.5 * IQR
  upper_bound <- quartiles[2] + 1.5 * IQR
  df <- df[df[[column_name]] > lower_bound & df[[column_name]] < upper_bound,]
  return(df)
}

# Remove outliers
bass_data <- remove_outliers(bass_data, 'WaterTemperature')
bass_data <- remove_outliers(bass_data, 'OutsideTemperature')
bass_data <- remove_outliers(bass_data, 'hPa')

# Split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(bass_data$Weightlbs, p = .8, list = FALSE)
training <- bass_data[trainIndex,]
testing <- bass_data[-trainIndex,]

# Fit the initial linear model
lm_model <- lm(Weightlbs ~ ., data = training)

# Perform stepwise model selection using AIC
step_model_lm_AIC <- stepAIC(lm_model, direction = "both")

# Fit the final linear model
step_model_lm_AIC <- lm(formula = step_model_lm_AIC$call$formula, data = training)

# Combine the training and testing data
full_data <- rbind(training, testing)

# Convert the 'BaitType' column to factor
full_data$BaitType <- factor(full_data$BaitType)

# Convert 'OutsideTemperature' to numeric
full_data$OutsideTemperature <- as.numeric(full_data$OutsideTemperature)

# Recreate the training index after merging data
set.seed(123)
trainIndex <- createDataPartition(full_data$Weightlbs, p = .8, list = FALSE)

# Split back into training and testing
training <- full_data[trainIndex,]
testing <- full_data[-trainIndex,]

# Include all levels in the factor when creating it for 'BaitType'
all_levels_BaitType <- unique(full_data$BaitType)
training$BaitType <- factor(training$BaitType, levels = all_levels_BaitType)
testing$BaitType <- factor(testing$BaitType, levels = all_levels_BaitType)

# Evaluate the models on the testing set
predictions_AIC <- predict(step_model_lm_AIC, newdata = testing)

# Accuracy calculation would be different for a regression model, one could use mean squared error (MSE)
mse_AIC <- mean((predictions_AIC - testing$Weightlbs)^2)

# Print the selected variables and MSE for AIC model
cat("Selected variables for AIC model:\n")
print(summary(step_model_lm_AIC)$coefficients)
cat("Mean Squared Error for AIC model:", mse_AIC, "\n\n")




