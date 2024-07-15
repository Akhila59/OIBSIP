install.packages(caret)
install.packages(ggplot2)

# Load necessary libraries
library(caret)
library(ggplot2)

# Load the dataset
iris <- read.csv(file.choose())

# Display few rows of the dataset
head(iris)

# Convert Species column to a factor
iris$Species <- as.factor(iris$Species)

# Pair plot to visualize the relationships
pairs(iris[, 2:5], col = as.numeric(iris$Species))

# ggplot2 visualization
ggplot(iris, aes(x = SepalLengthCm, y = SepalWidthCm, color = Species)) +
  geom_point() +
  theme_minimal()

# Set seed for reproducibility
set.seed(123)

# Split the data into training and testing sets
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Train a k-NN model
model <- train(Species ~ ., data = trainData[, -1], method = 'knn', tuneLength = 10)

# Print the model
print(model)

# Make predictions on the test data
predictions <- predict(model, testData[, -1])

# Confusion matrix to evaluate the performance
confusionMatrix(predictions, testData$Species)

# Save the model
saveRDS(model, "iris_knn_model.rds")

# Load the model
loaded_model <- readRDS("iris_knn_model.rds")




