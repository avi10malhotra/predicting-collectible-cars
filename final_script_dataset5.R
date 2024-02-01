cars = read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/final-project/cars.csv',
                sep = ',',
                header = TRUE,
                na.strings = c('NA', '?'))
attach(cars)

### Data Cleaning, Transformation and Exploration

# print null values in every column
sapply(cars, function(x) sum(is.na(x)))

# null values in num-of-doors
cars[is.na(cars$num.of.doors),]

# get all data points having sedan body style and dodge and mazda make.
cars[cars$body.style == 'sedan' & (cars$make == 'dodge' | cars$make == 'mazda'),]

#replace the missing 'num-of-doors' values by 'four'
cars[is.na(cars$num.of.doors),]$num.of.doors = 'four'

#

# get all rows with null values in 'horsepower' column
cars[is.na(cars$horsepower),]

# check correlation with other numerical attributes
cor(cars[,c('horsepower', 'engine.size', 'curb.weight', 'city.mpg', 'highway.mpg')], use = 'complete.obs')

# plot scatter plot to check correlation between engine size and horsepower
plot(cars$engine.size, cars$horsepower)
cars[cars$engine.size > 127 & cars$engine.size < 137,]

# check other attributes affecting horsepower
attributes = c('aspiration', 'fuel.type')
par(mfrow = c(1,2))
for (i in attributes) {
  boxplot(horsepower ~ get(i), data = cars, main = paste('Horsepower vs', i))
}


# replacing horsepower with 113 for all rows with null values
cars[is.na(cars$horsepower),]$horsepower = 113

#

# get all rows with null values in 'peak-rpm' column
cars[is.na(cars$peak.rpm),]

# check correlation with other numerical attributes
cor(cars[,c('peak.rpm', 'engine.size', 'curb.weight', 'city.mpg', 'highway.mpg')], use = 'complete.obs')

# check correlation with categorical attributes
attributes = c("fuel.type","aspiration","drive.wheels","engine.location","engine.type","num.of.cylinders","fuel.system")
par(mfrow = c(3,3))
for (i in attributes) {
  boxplot(peak.rpm ~ get(i), data = cars, main = paste('Peak RPM vs', i), xlab = '', ylab = '')

}

# create dataframe by using above attributes and range of 4800-5500
nan_peak_rpm = cars[(cars$peak.rpm >= 4800) & (cars$peak.rpm <= 5500) & (cars$fuel.system == 'mpfi') & (cars$engine.location == 'front') & (cars$drive.wheels == 'rwd') & (cars$aspiration == 'std') & (cars$fuel.type == 'gas') & (cars$engine.type == 'ohc') & (cars$num.of.cylinders == 'four'),]
mean(nan_peak_rpm$peak.rpm)

# replacing peak-rpm with 5200 for all rows with null values
cars[is.na(cars$peak.rpm),]$peak.rpm = 5200

#

# get all rows with null values in 'bore' column
cars[is.na(cars$bore),]

# droping duplicates due to same categorical attributes
cars = cars[-c(55,56),]

# check correlation with other numerical attributes
cor(cars[,c('bore', 'engine.size', 'curb.weight', 'city.mpg', 'highway.mpg')], use = 'complete.obs')

# check correlation with categorical attributes
par(mfrow = c(3,3))
for (i in attributes) {
  boxplot(bore ~ get(i), data = cars, main = paste('Bore vs', i), xlab = '', ylab = '')
}

library(ggplot2)
ggplot(cars, aes(x = bore, y = make)) + geom_boxplot()

# create dataframe by using above attributes and range of 3.2-3.6
# nan_bore = data[(data['bore'] >= 3.3) & (data['bore'] <= 3.65)& (data['make']=='mazda')]
nan_bore = cars[(cars$bore >= 3.2) & (cars$bore <= 3.6) & (cars$make == 'mazda'),]

# replace with mode of 3.39
cars[is.na(cars$bore),]$bore = 3.39

#

# get all rows with null values in 'stroke' column
cars[is.na(cars$stroke),]

# check correlation with other numerical attributes
cor(cars[,c('stroke', 'engine.size', 'curb.weight', 'city.mpg', 'highway.mpg')], use = 'complete.obs')

# check correlation with categorical attributes
par(mfrow = c(3,3))
for (i in attributes) {
  boxplot(stroke ~ get(i), data = cars, main = paste('Stroke vs', i), xlab = '', ylab = '')
}

ggplot(cars, aes(x = stroke, y = make)) + geom_boxplot()

# create dataframe by using above attributes and range of 3.1-3.4
nan_stroke = cars[(cars$stroke >= 3.1) & (cars$stroke <= 3.4) & (cars$make == 'mazda'),]

# replace with mode of 3.39
cars[is.na(cars$stroke),]$stroke = 3.39

#

# get all rows with null values in 'normalized.losses' column
cars[is.na(cars$normalized.losses),]

# ploting to check distribution
par(mfrow = c(1,1))
hist(cars$normalized.losses, breaks = 10)
ggplot(cars, aes(x = normalized.losses)) + geom_density()

# replacing missing values with median
cars[is.na(cars$normalized.losses),]$normalized.losses = median(cars$normalized.losses, na.rm = TRUE)

#

# get all rows with null values in 'price' column
cars[is.na(cars$price),]

# replace these with the average price of the cars with the same make
cars[is.na(cars$price),]$price = mean(cars$price, na.rm = TRUE)

# check if any null values are left
sapply(cars, function(x) sum(is.na(x)))

#

# checking for price outliers for luxury cars
summary(cars$price)
boxplot(price, data = cars, main = 'Price Boxplot')
ggplot(cars, aes(x = make, y = price)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# creating a column called car_category for price outliers
cars$category = ifelse(cars$price > 30000, 'luxury', 'affordable')

### Extracting relevant features

#

# checking which features are important for predicting price, assumption: higher prices cars are more likely to be collectible

# creating a random forest model
library(randomForest)
set.seed(7)
rf_model = randomForest(price ~ ., data = cars, ntree = 500, importance = TRUE)

# checking the accuracy of the model
rf_model

# extracting important features
varImpPlot(rf_model)
imp_features = c('category', 'engine.size', 'curb.weight', 'highway.mpg', 'horsepower')

#


# perform PCA

# dummify categorical variables
library(caret)
categorical = c('make', 'fuel.type', 'aspiration', 'num.of.doors', 'body.style', 'drive.wheels', 'engine.location', 'engine.type', 'num.of.cylinders', 'fuel.system', 'category', 'symboling')
numerical = c('normalized.losses', 'wheel.base', 'length', 'width', 'height', 'curb.weight', 'engine.size', 'bore', 'stroke', 'compression.ratio', 'horsepower', 'peak.rpm', 'city.mpg', 'highway.mpg', 'price')

# scale numerical variables
cars_scale <- scale(cars[,numerical])

# specify the index of the categorical variables while creating dummy variables
cars_dummies <- dummyVars(~ ., data = cars[,categorical], fullRank = TRUE)
cars_transformed <- predict(cars_dummies, newdata = cars)
cars_transformed_df <- as.data.frame(cars_transformed)

cars_combined_df = cbind(cars_transformed_df, cars_scale)

pca <- prcomp(cars_combined_df, scale = TRUE)
pca


library(factoextra)
fviz_pca_ind(pca, label = "none", habillage = cars$category, addEllipses = TRUE, ellipse.level = 0.95, ggtheme = theme_minimal(),
                         title = "Principal Component Analysis")
# luxury cars have Dim1 < -5, exploring features in more detail to understand why

library(ggfortify)

# Calculate the significant loadings
loadings <- as.data.frame(pca$rotation)
threshold <- 0.27
loadings$Magnitude <- sqrt(loadings$PC1^2 + loadings$PC2^2)
significant_loadings <- subset(loadings, Magnitude > threshold)

# Extract the names of the significant loadings
significant_names <- row.names(significant_loadings)

make_df <- cbind(cars_transformed_df, cars$make)
colnames(make_df)[colnames(make_df) == 'cars$make'] <- 'make'

# Create a new factor variable for the makes of interest
make_df$make_colored <- ifelse(make_df$make %in% c("porsche", "mercedes-benz", "audi", 'jaguar', "bmw"),
                               as.character(make_df$make), "others")

# Convert the new variable to a factor
make_df$make_colored <- as.factor(make_df$make_colored)

# Plot the PCA, coloring points by the new make_colored variable
p <- autoplot(pca, data = make_df, colour = 'make_colored', loadings = FALSE, loadings.label = FALSE) +
  geom_segment(data = significant_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               color = 'grey', arrow = arrow(length = unit(0.3, 'cm'))) +
  geom_text(aes(label = row.names(significant_loadings)),
            data = significant_loadings, colour = 'black', vjust = 1.5) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    text = element_text(size = 10),
    axis.text = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("PCA of Car Manufacturers") +
  scale_colour_manual(values = c("porsche" = "red", "mercedes-benz" = "purple", "audi" = "blue", "jaguar" = "green", "others" = "light grey", "bmw" = "orange"))+
  lims(x = c(-0.35, 0.25))
# Print the plot
print(p)
# porsche has high horsepower,
# mercedes-benz has high wheel base, compression ratio, but also high curb weight and horsepower (most features)
# jaguar has high curb weight and horsepower
# bmw has high curb weight and horsepower
# audi is low on all features, thus not a collectible
# all others are collectible
# all these brands have very low city.mpg


### K-Means Clustering

# useful_colums = imp_features + significant_names
predictors_rf = c("engine.size", "curb.weight", "highway.mpg", "horsepower", "categoryluxury")
predictors_pca = c("fuel.typegas", "fuel.systemidi", "wheel.base", "length",
                  "height", "curb.weight", "horsepower", "city.mpg")
useful_colums = c(predictors_rf, predictors_pca)

setdiff(useful_colums, colnames(cars_combined_df))


set.seed(7)
wss <- sapply(2:10, function(k) {
  kmeans(cars_combined_df[,useful_colums], k, nstart = 10 )$tot.withinss
})
# write elbow plot as title
plot(2:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = "Elbow Plot")
opt_k = 6
opt_model = kmeans(cars_combined_df, opt_k, nstart = 10)

# display the price of the cars in each cluster, and the number of luxury cars in each cluster
cluster_df = cbind(cars, cluster_groups = opt_model$cluster)

info = cluster_df %>% group_by(cluster_groups) %>% summarise(avg_price = mean(price), num_luxury = sum(category == 'luxury'))
# cluster group 2 has most expensive cars on average
# 10/14 luxury cars are in cluster group 2


# visualize the clusters, price vs car make, color by cluster group
ggplot(cluster_df, aes(x = price, y = make, color = as.factor(cluster_groups))) +
  geom_point() +
  theme_minimal() +
  labs(color = "Cluster Group", x = "Price", y = "Car Make") +
  ggtitle("Price vs Car Make by Cluster Group") +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = 30000, linetype = "dashed", color = "dark grey", size = 1)

# not as cut and dry as before, there is only 1 collectible car from porche in cluster 2, and highly priced cars from mercedez and bmw, but still all cars from jaguar

ggplot(cluster_df, aes(x = horsepower, y = engine.size, color = as.factor(cluster_groups))) +
    geom_point() +
    theme_minimal() +
    labs(color = "Cluster Group", x = "Horsepower", y = "Engine Size") +
    ggtitle("Price vs Horsepower by Cluster Group") +
    scale_color_brewer(palette = "Set1")

# cluster 2 has high horsepower and engine size

# city mpg vs wheel base
ggplot(cluster_df, aes(x = city.mpg, y = wheel.base, color = as.factor(cluster_groups))) +
  geom_point() +
  theme_minimal() +
  labs(color = "Cluster Group", x = "City MPG", y = "Wheel Base") +
  ggtitle("City MPG vs Wheel Base by Cluster Group") +
  scale_color_brewer(palette = "Set1")

# cluster 2 has low city mpg and high wheel base

# fuel type vs length
ggplot(cluster_df, aes(x = fuel.system, y = length, color = as.factor(cluster_groups))) +
  geom_point() +
  theme_minimal() +
  labs(color = "Cluster Group", x = "Fuel Type", y = "Length") +
  ggtitle("Fuel System vs Car Length for Cluster 2 (blue)") +
  scale_color_manual(values = c("grey", "blue", "grey", "grey", "grey", "grey")) +
  guides(color = FALSE)

# cluster 2 has high length and fuel system mpfi