cars = read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/final-project/cars.csv',
                sep = ',',
                na.strings = c('NA', '?'))
attach(cars)



# Load necessary library
library(dplyr)

# Drop rows with '?' in any column except 'normalized-losses'
cars <- cars %>%
  filter(if_all(-normalized.losses, ~ .x != 'NA'))

# Convert 'normalized-losses' to numeric
cars$normalized.losses <- as.numeric(cars$normalized.losses)

# Calculate the average normalized loss for each 'make', ignoring NAs
average_normalized_losses <- cars %>%
  group_by(make) %>%
  summarize(avg_normalized_loss = mean(normalized.losses, na.rm = TRUE), .groups = 'drop')

# Join the average values back to the main data frame
cars <- left_join(cars, average_normalized_losses, by = "make")

# Replace NA in 'normalized-losses' with the average for each 'make'
cars$normalized.losses <- ifelse(is.na(cars$normalized.losses), cars$avg_normalized_loss, cars$normalized.losses)

# delete the  column avg_normalized_loss
cars$avg_normalized_loss <- NULL

# View the updated data
head(cars)

library(randomForest)

# Assuming all columns except 'symboling' are predictors
# Convert factor variables to factors if not already
cars[, sapply(cars, is.character)] <- lapply(cars[, sapply(cars, is.character)], as.factor)

rf_model <- randomForest(symboling ~ ., data = cars, importance = TRUE)
importance(rf_model)

# sort the feature importance
varImpPlot(rf_model, sort = TRUE, n.var = 10, main = "Top 10 important variables")

# only keep the top 10 important variables
imp_vars = importance(rf_model, type = 1)
imp_vars = imp_vars[order(imp_vars[, 1], decreasing = TRUE), ]
imp_vars = imp_vars[1:10]

# perform a linear descriminant analysis using imp_vars
library(MASS)

# Assuming all columns except 'symboling' are predictors
# Convert factor variables to factors if not already
cars[, sapply(cars, is.character)] <- lapply(cars[, sapply(cars, is.character)], as.factor)

features = c('make', 'num.of.doors', 'normalized.losses', 'wheel.base', 'body.style', 'curb.weight', 'width', 'engine.size', 'height', 'price')


lda_model <- lda(symboling~make+num.of.doors+normalized.losses+wheel.base+body.style+curb.weight+width+engine.size+height+price, data = cars)

# print the lda model
lda_model

library(klaR)

# Summary statistics
summary(cars)

# Visual exploration, example with ggplot2
ggplot(cars, aes(x = engine.size, y = price)) + geom_point() + theme_minimal()

# Visualizing data distributions
ggplot(cars, aes(x = price)) + geom_histogram(bins = 30) + theme_minimal() + ggtitle("Price Distribution")
ggplot(cars, aes(x = horsepower)) + geom_histogram(bins = 30) + theme_minimal() + ggtitle("Horsepower Distribution")

# Analyzing relationships
ggplot(cars, aes(x = engine.size, y = price)) + geom_point() + theme_minimal() + ggtitle("Engine Size vs Price")
corr_matrix <- cor(select_if(cars, is.numeric))
print(corr_matrix)

# Analyzing relationships
ggplot(cars, aes(x = engine.size, y = price)) + geom_point() + theme_minimal() + ggtitle("Engine Size vs Price")
corr_matrix <- cor(select_if(cars, is.numeric))
print(corr_matrix)

# Identifying outliers
boxplot(cars$price, main="Boxplot for Prices")
boxplot(cars$normalized.losses, main="Boxplot for Normalized Losses")

boxplot(cars$horsepower, main="Boxplot for Horsepower")
boxplot(cars$peak.rpm, main="Boxplot for Peak RPM")

# only print outlier rows for prices (i.e. > 30000)
cars[cars$price > 30000, ]

# visualize entries for price vs normalized.losses, color by make, add a line for price > 30000
ggplot(cars, aes(x = normalized.losses, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)

# visualize the plots for price vs every numeric variable, add a line for price > 30000
ggplot(cars, aes(x = wheel.base, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = curb.weight, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = width, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = engine.size, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = height, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = peak.rpm, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x= normalized.losses, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = horsepower, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = city.mpg, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)
ggplot(cars, aes(x = highway.mpg, y = price, color = make)) + geom_point() + theme_minimal() + geom_hline(yintercept = 30000)

# Ensure the variable is numeric
cars$price <- as.numeric(as.character(cars$price))
cars <- na.omit(cars) # Removing rows with NA values

# K-means clustering
set.seed(123) # For reproducibility
k <- 3 # Number of clusters
clusters <- kmeans(cars[, 'price'], centers = k)

# Adding cluster results to the data
cars$cluster <- as.factor(clusters$cluster)

# Visualizing the clusters
ggplot(cars, aes(x = price, fill = cluster)) + geom_histogram(bins = 30) + theme_minimal() + ggtitle("Price-based Clusters")

# Load necessary library
library(tidyverse)

# Normalizing function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalizing the data
cars$price <- as.numeric(as.character(cars$price))
cars$horsepower <- as.numeric(as.character(cars$horsepower))
cars$city.mpg <- as.numeric(as.character(cars$city.mpg))
cars$highway.mpg <- as.numeric(as.character(cars$highway.mpg))
cars$engine.size <- as.numeric(as.character(cars$engine.size))

normalized_cars <- na.omit(cars[, c('price', 'horsepower', 'city.mpg', 'highway.mpg', 'engine.size')])
normalized_cars <- as.data.frame(lapply(normalized_cars, normalize))

# Elbow method
set.seed(123)
wss <- sapply(1:10, function(k){kmeans(normalized_cars, k, nstart = 10 )$tot.withinss})
plot(1:10, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K", ylab="Total within-clusters sum of squares")

# K-means clustering with chosen k
chosen_k <- 3 # replace this with the chosen number of clusters based on the elbow plot
clusters <- kmeans(normalized_cars, centers = chosen_k)

# Adding cluster results to the original data
cars$cluster <- as.factor(clusters$cluster)

# Visualizing the clusters (example with price and horsepower)
ggplot(cars, aes(x = price, y = engine.size, color = cluster)) + geom_point() + theme_minimal() + ggtitle("Normalized Combination Clusters: Price, Horsepower, MPG")

# Convert categorical variables to factors
categorical_vars <- c('make', 'fuel.type', 'aspiration', 'num.of.doors', 'body.style',
                      'drive.wheels', 'engine.location', 'engine.type', 'num.of.cylinders', 'fuel.system')
cars[categorical_vars] <- lapply(cars[categorical_vars], factor)

# Convert numeric variables, handle missing values
numeric_vars <- c('wheel.base', 'length', 'width', 'height', 'curb.weight', 'engine.size',
                  'bore', 'stroke', 'compression.ratio', 'horsepower', 'peak.rpm', 'city.mpg', 'highway.mpg', 'price')
cars[numeric_vars] <- lapply(cars[numeric_vars], function(x) as.numeric(as.character(x)))
cars <- na.omit(cars)  # Removing rows with NA values

# Split data into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(cars$price, p = 0.80, list = FALSE)
train <- cars[splitIndex,]
test <- cars[-splitIndex,]

# Train Random Forest model
rf_model <- randomForest(price ~ ., data = train, ntree = 500)

# Summary of the model
print(rf_model)

# Predicting on test data
predictions <- predict(rf_model, test)

# Compute model performance metrics
test$predicted_price <- predictions
performance <- postResample(pred = test$predicted_price, obs = test$price)
print(performance)

# Feature Importance, sort by IncNodePurity
varImpPlot(rf_model, sort = TRUE, n.var = 10, main = "Top 10 important variables")
imp_vars = c('engine.size', 'horsepower', 'curb.weight')

# Load necessary libraries
library(tidyverse)
library(cluster)

# Selecting and normalizing important features
selected_features <- cars %>% select(make, engine.size, curb.weight, horsepower, highway.mpg)
normalized_features <- as.data.frame(scale(selected_features))

# K-means clustering
set.seed(123)  # for reproducibility
k <- 4  # You might need to experiment with this number
clusters <- kmeans(normalized_features, centers = k)

# Adding cluster results to the cars data
cars$cluster <- clusters$cluster

# Analyzing the clusters
aggregate(cars[, c('price', 'make', 'engine.size', 'curb.weight', 'horsepower', 'highway.mpg')], by = list(cars$cluster), mean)

# Filter out the specific cluster
collector_cars <- cars[cars$cluster == 3, ]

# Split this filtered data into training and testing sets
set.seed(123)
splitIndex <- createDataPartition(collector_cars$price, p = 0.80, list = FALSE)
train_collector <- collector_cars[splitIndex,]
test_collector <- collector_cars[-splitIndex,]

# Train a Random Forest model on this cluster
library(randomForest)
rf_model_collector <- randomForest(price ~ . - cluster, data = train_collector, ntree = 500)

# Predicting on test data
predictions_collector <- predict(rf_model_collector, test_collector)

# Compute model performance metrics
test_collector$predicted_price <- predictions_collector
performance_collector <- postResample(pred = test_collector$predicted_price, obs = test_collector$price)
print(performance_collector)

# Feature importance can also be checked to see what drives the price within this cluster
importance(rf_model_collector)

# predict collector car price for every car in cluster 1
collector_cars$predicted_price <- predict(rf_model_collector, collector_cars)

# print current price and collector price for every car in cluster 1
collector_cars[, c('price', 'predicted_price')]


lda1 = lda(cluster~engine.size+curb.weight, data = cars)
# Load necessary library
library(cluster)

# Assuming you've already clustered your data and have a 'cluster' column
# For demonstration, let's use k-means clustering
set.seed(123)  # Ensuring reproducibility
k <- 3  # Number of clusters, you can adjust this as needed
clusters <- kmeans(cars[, c('engine.size', 'curb.weight')], centers = k)
cars$cluster <- clusters$cluster

# Partitioning map between engine.size and curb.weight
clusplot(cars, cars$cluster, color=TRUE, shade=TRUE, labels=2, lines=0,
         xlab="Engine Size", ylab="Curb Weight", main="Partitioning Map")

# Install and load the klaR package
install.packages("klaR")
library(klaR)

# Assuming you have a dataset 'cars' with features and a categorical variable 'cluster'
# For demonstration, let's say you've performed clustering and have cluster labels

# Using partimat to visualize partitioning
partimat(cluster ~ engine.size + curb.weight, data = cars, method = "lda", image.colors =c("red", "green"))


