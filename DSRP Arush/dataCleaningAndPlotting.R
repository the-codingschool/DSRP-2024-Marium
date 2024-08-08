#Loading Libraries
install.packages("reshape2")
library(reshape2)
library(tidyverse) 
library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(caTools)
library(readr)
library(downloader)
library(caTools)
library(FNN)
library(readxl)


#Loading Data
fileName <- list.files("DSRP Arush/data/")
data <- read.csv(paste0("DSRP Arush/data/",fileName))


#Data Cleaning
data[complete.cases(data), ]

data$start_coordinates = paste(data$start_lat, data$start_lng, sep=",")
data$end_coordinates = paste(data$end_lat, data$end_lng, sep=",")

cols.dont.want <- c("start_lat", "start_lng", "end_lat", "end_lng")
data <- data[, ! names(data) %in% cols.dont.want, drop = F]

unique_stations <- unique(data$start_station_name)
station_mapping <- setNames(seq_along(unique_stations), unique_stations)

print(station_mapping)
data$station_names_numeric <- station_mapping[data$start_station_name]

head(data)

data$rideable_type_numeric <- as.numeric(data$rideable_type)


#Visualizations
ggplot(data, aes(x = rideable_type)) +
  geom_bar(fill = "green", color = "black") +
  labs(title = "Amount of Each Bike Type",
       x = "Bike Type",
       y = "Amount of Bikes")


pie(table(data$rideable_type),
    main = "Distribution of Electric and Classic Bikes",
    labels = paste(names(table(data$rideable_type)), "\n", table(data$rideable_type), sep = ""))


aggregated_data <- data %>%
  group_by(station_names_numeric) %>%
  summarise(bike_count = n(), .groups = 'drop')
top10_stations <- aggregated_data %>%
  arrange(desc(bike_count)) %>%
  head(10)
ggplot(top10_stations, aes(x = reorder(factor(station_names_numeric), bike_count), y = bike_count)) +
  geom_bar(stat = "identity", fill = "green", color = "black") +
  labs(title = "Top 10 Most Common Start Stations",
       x = "Start Station Numeric",
       y = "Amount of Bikes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data, aes(x = rideable_type, y = station_names_numeric)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Stations Where Bikes Started",
       x = "Type of Bike",
       y = "Station Number")


contingency_table <- table(data$station_names_numeric, data$rideable_type)
melted_table <- melt(contingency_table)
ggplot(melted_table, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Station Number", y = "Rideable Type", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Testing and Finding Correlation
t.test(station_names_numeric ~ rideable_type, data = data)

correlation <- cor(data$station_names_numeric, data$rideable_type_numeric, use = "complete.obs")
print(correlation)

aov(station_names_numeric ~ rideable_type, data = data)

chisq.test(table(data$station_names_numeric, data$rideable_type))





#k-NN Model


set.seed(42)
missing_values_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_values_summary)

data_model <- data_clean[, c('station_names_numeric', 'rideable_type_numeric')]

missing_values_model <- sapply(data_model, function(x) sum(is.na(x)))
print(missing_values_model)

data_model <- na.omit(data_model)
split <- sample.split(data_model$rideable_type_numeric, SplitRatio = 0.8)
train_data <- subset(data_model, split == TRUE)
test_data <- subset(data_model, split == FALSE)

cat("Dimensions of Training Data:\n")
print(dim(train_data))
cat("Dimensions of Test Data:\n")
print(dim(test_data))

cat("Column Names of Training Data:\n")
print(names(train_data))
cat("Column Names of Test Data:\n")
print(names(test_data))

train_features <- train_data[, 'station_names_numeric', drop = FALSE]
test_features <- test_data[, 'station_names_numeric', drop = FALSE]

if (!identical(names(train_features), names(test_features))) {
  stop("The columns of train and test datasets do not match.")
}

k <- 5
knn_model <- knn(train = train_features,
                 test = test_features,
                 cl = train_data$rideable_type_numeric, k = k)

confusion_matrix <- table(test_data$rideable_type_numeric, knn_model)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

cat('Confusion Matrix (k-NN):\n')
print(confusion_matrix)
cat('\nAccuracy (k-NN):', accuracy, '\n')

k_values <- c(75, 80, 85, 90)
results <- sapply(k_values, function(k) {
  knn_model <- knn(train = train_features, test = test_features, cl = train_data$rideable_type_numeric, k = k)
  confusion_matrix <- table(test_data$rideable_type_numeric, knn_model)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  return(accuracy)
})
print(results)

confusion_matrix <- table(test_data$rideable_type_numeric, knn_model)
confusion_df <- as.data.frame(as.table(confusion_matrix))
colnames(confusion_df) <- c("Actual", "Predicted", "Frequency")
ggplot(confusion_df, aes(x = Actual, y = Frequency, fill = Predicted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Actual Values", y = "Frequency", fill = "Predicted Values", title = "Confusion Matrix Bar Plot") +
  theme_minimal()
