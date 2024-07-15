# Load necessary libraries
install.packages(c("tm", "caret", "e1071", "dplyr", "ggplot2"))
library(tm)
library(caret)
library(e1071)
library(dplyr)
library(ggplot2)

# Load dataset
data <- read.csv(file.choose())

# Inspect the data
head(data) 

# Preprocessing
data$v2 <- iconv(data$v2, "UTF-8", "ASCII//TRANSLIT", sub = "")
data$v2 <- tolower(data$v2) 

# Remove punctuation, numbers, and stopwords
corpus <- VCorpus(VectorSource(data$v2))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)

# Convert to a matrix
dtm_matrix <- as.matrix(dtm)

# Combine with labels
spam_labels <- as.factor(data$v1)  # Use 'v1' for spam/ham labels
dataset <- as.data.frame(dtm_matrix)
dataset$label <- spam_labels

# Check if the dataset is not empty
if (nrow(dataset) == 0) {
  stop("Dataset is empty. Check previous steps.")
}

# Split the Data
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(dataset), 0.7 * nrow(dataset))
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

# Train the Model using Naive Bayes
spam_model <- naiveBayes(label ~ ., data = train_data)

# Make predictions
predictions <- predict(spam_model, newdata = test_data)

# Evaluate the Model
confusion_matrix <- table(test_data$label, predictions)
print(confusion_matrix)

# Calculate Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", accuracy, "\n")

# Visualize the Results (Confusion Matrix)
cm_df <- as.data.frame(confusion_matrix)
colnames(cm_df) <- c("Actual", "Predicted", "Freq")

ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +geom_tile() +geom_text(aes(label = Freq), color = "red") +theme_minimal() +labs(title = "Confusion Matrix", x = "Actual", y = "Predicted")
