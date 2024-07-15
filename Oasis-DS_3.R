# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(caTools)
library(ggcorrplot)

# Load the data
car_data <- read.csv(file.choose())

# Inspect the data
head(car_data)
summary(car_data)
str(car_data)

# Check for missing values
colSums(is.na(car_data))

# EDA: Visualize the distribution of the target variable (Selling_Price)
ggplot(car_data, aes(x = Selling_Price)) + 
  geom_histogram(binwidth = 0.5, fill = 'pink', color = 'black') +
  theme_minimal() + 
  ggtitle('Distribution of Car Selling Prices')

# EDA: Correlation matrix
cor_matrix <- cor(car_data %>% select_if(is.numeric))
ggcorrplot(cor_matrix, method = "circle")

# Convert categorical variables to factors
car_data$Fuel_Type <- as.factor(car_data$Fuel_Type)
car_data$Selling_type <- as.factor(car_data$Selling_type)
car_data$Transmission <- as.factor(car_data$Transmission)

# Check for factor levels in Car_Name (if it exists) and match levels
if("Car_Name" %in% colnames(car_data)) {car_data$Car_Name <- as.factor(car_data$Car_Name)}

# Split the data into training and testing sets
set.seed(123)
split <- sample.split(car_data$Selling_Price, SplitRatio = 0.7)
training_set <- subset(car_data, split == TRUE)
testing_set <- subset(car_data, split == FALSE)

# Ensure the factor levels match between training and testing sets for Car_Name
if("Car_Name" %in% colnames(car_data)) {levels(testing_set$Car_Name) <- levels(training_set$Car_Name)}

# Train a linear regression model
lm_model <- lm(Selling_Price ~ ., data =car_data)
summary(lm_model)

# Train a random forest model
rf_model <- randomForest(Selling_Price ~ ., data = training_set, ntree = 500, mtry = 3, importance = TRUE)
print(rf_model)
importance(rf_model)
varImpPlot(rf_model)

# Assuming train_set is your training dataset
testing_set$Car_Name <- factor(testing_set$Car_Name, levels = levels(car_data$Car_Name))

testing_set$Car_Name <- as.character(testing_set$Car_Name)

combined_set <- rbind(car_data, testing_set)
combined_set$Car_Name <- as.factor(combined_set$Car_Name)
lm_model <- lm(Selling_Price ~ Car_Name+Year+Present_Price+Driven_kms+Fuel_Type+Selling_type+Transmission+Owner, data =car_data )

# Adjust factor levels in the testing set
car_data$Car_Name = factor(car_data$Car_Name, levels = levels(car_data$Car_Name))

# Now, make predictions
lm_predictions <- predict(lm_model, newdata = testing_set)

# Evaluate the linear regression model
lm_predictions = predict(lm_model, newdata = testing_set)
lm_rmse <- sqrt(mean((lm_predictions - testing_set$Selling_Price)^2))
print(paste("Linear Regression RMSE:", lm_rmse))

# Evaluate the random forest model
rf_predictions <- predict(rf_model, newdata = testing_set)
rf_rmse <- sqrt(mean((rf_predictions - testing_set$Selling_Price)^2))
print(paste("Random Forest RMSE:", rf_rmse))

# Compare RMSE of both models
rmse_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  RMSE = c(lm_rmse, rf_rmse))
print(rmse_comparison)

