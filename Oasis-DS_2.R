# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the dataset
unemployment=read.csv(file.choose())

# Display the first few rows of the dataset
head(unemployment)

# Display the structure of the dataset
str(unemployment)

# Clean the data
# Convert the 'Date' column to Date type using dmy()
unemployment <- unemployment %>%
  mutate(Date = dmy(Date))

# Remove rows with missing values
unemployment <- unemployment %>% drop_na()

# Alternatively, fill missing values (e.g., forward fill)
# unemployment <- unemployment %>% fill(Estimated.Unemployment.Rate...., .direction = "downup")

# Check for missing values again
sum(is.na(unemployment))

# Aggregate data by month
unemployment_monthly <- unemployment %>%
  mutate(Month = floor_date(Date, "month")) %>%
  group_by(Month) %>%
  summarize(Average_Unemployment_Rate = mean(Estimated.Unemployment.Rate...., na.rm = TRUE))

# Visualize the average unemployment rate per month using a bar chart
ggplot(unemployment_monthly, aes(x = Month, y = Average_Unemployment_Rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Average Unemployment Rate Per Month",
       x = "Month",
       y = "Average Unemployment Rate (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the bar chart
ggsave("average_unemployment_rate_per_month.png")

# Basic summary statistics
summary(unemployment$Estimated.Unemployment.Rate....)

# Additional analysis
# Calculate the average unemployment rate before and after Covid-19
pre_covid_data <- unemployment %>%
  filter(Date < ymd("2020-03-01"))

post_covid_data <- unemployment %>%
  filter(Date >= ymd("2020-03-01"))

pre_covid_avg <- mean(pre_covid_data$Estimated.Unemployment.Rate...., na.rm = TRUE)
post_covid_avg <- mean(post_covid_data$Estimated.Unemployment.Rate...., na.rm = TRUE)

cat("Average Unemployment Rate before Covid-19:", pre_covid_avg, "\n")
cat("Average Unemployment Rate after Covid-19:", post_covid_avg, "\n")







