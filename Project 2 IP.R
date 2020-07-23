# Importing and Installing Libraries

install.packages("tidyverse")

install.packages("ggplot2")

install.packages("corrplot")

library(tidyverse)

library(ggplot2)

library(readr)

# Loading our dataset

ecommerce_data <- read_csv("C:/Users/color/Downloads/online_shoppers_intention.csv")

# Preview our data

head(ecommerce_data,5)

tail(ecommerce_data,5)

# Data Understanding

# Shape

dim(ecommerce_data)

# Column names

names(ecommerce_data)

# Data types on our dataframe

glimpse(ecommerce_data)

sapply(ecommerce_data, class)

# Checking for null values

colSums(is.na(ecommerce_data))

# Dealing with nulls

ecommerce = na.omit(ecommerce_data)

colSums(is.na(ecommerce)) # Confirming if nulls have been dropped

# Data Summary

summary(ecommerce)

# Checking for duplicates

is.double(ecommerce)

colSums(is.double(ecommerce))

# Checking for outliers

summary(ecommerce)


# Exploratory Data Analysis

## Univariate Analysis

summary(ecommerce)

## Bivariate Analysis

numeric_df = ecommerce[, sapply(ecommerce, is.numeric)]

head(numeric_df)

# Computing correlation matrix

corr_matrix <- cor(numeric_df)

head(round(corr_matrix,2))

# Visualizing the correlation matrix

library(corrplot)

corrplot(corr_matrix, method="circle")

names(ecommerce)

# Modeling

# K means 

# Feature Selection

feat = ecommerce[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

y = ecommerce[, 18]

feat

y

# NOrmalization

normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}
columns = c('Administrative','Administrative_Duration','Informational','Informational_Duration',
'ProductRelated','ProductRelated_Duration', 'PageValues', 'Browser', 'Weekend')
for (i in columns){
feat[, i] = normalize(feat[, i])
}


# Encoding our categorical data

cols.num <- c("Revenue", "Weekend" )
ecommerce[cols.num] <- sapply(ecommerce[cols.num],as.numeric)
sapply(ecommerce, class)

num_data = c('Month','VisitorType', 'Weekend')
for (i in num_data){
  feat[,i] = as.numeric(feat[,i])
}

#Fitting the data

# Feature Selection

feat = ecommerce[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]

y = ecommerce[, 18]

k_model <- kmeans(ecommerce,3)

#Checking cluster number

k_model$cluster_obs = as.numeric(y)
k_model$size

table(k_model$cluster,k_model$cluster_obs )

# Accuracy
mean(k_model$cluster_obs == k_model$cluster)

# Hierachical Clustering
#obtaining the euclidean distance
distance <- dist(feat, method = "euclidean")

#Fitting
# fitting the model
model <- hclust(distance, method = "ward.D2" )

#Visualiztion
# plotting
plot(model, cex = 0.6, hang = -1)

