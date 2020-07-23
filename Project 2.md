---
title: "Unsupervised Learning"
author : "Collins Kemboi"
---
# WEEK 13 IP

# DEFINING THE QUESTION


### a). Specifying the question

Kira Plastinina  is a Russian brand that is sold through a defunct chain of retail stores in Russia, Ukraine, Kazakhstan, Belarus, China, Philippines, and Armenia. The brand’s Sales and Marketing team would like to understand their customer’s behavior from data that they have collected over the past year. More specifically, they would like to learn the characteristics of customer groups


### b). Defining the metrics of success

Our metric of success will be defined by sucessful coming up with models with high accuracies.


### c) Understanding the Context

Kira Plastinina is a Russian brand that is sold through a defunct chain of retail stores in Russia, Ukraine, Kazakhstan, Belarus, China, Philippines, and Armenia. The brand’s Sales and Marketing team would like to understand their customer’s behavior from data that they have collected over the past year.

We are therefore tasked to develop two models K Means Clustering and Hierarical Clustering and comparing the two models. The insights unearthed will therfore be used by the sales and marketing team to make data driven decisions.



### d). Recording the Experimental Design


1.   Data Cleaning and Preparation
2.   Exploratory Data Analysis
3.   Feature Selection
4.  Modeling

*   K-Mean Clustering
*   Hierarchical Clustering

5.  Make predictions using the different Models
6.  Access accuracy of different models
7.  Make conclusions & Challenge the solution


### e) Data relevance/ Appropriateness of Data

Link to the dataset:http://bit.ly/EcommerceCustomersDataset

The dataset consists of 10 numerical and 8 categorical attributes. The 'Revenue' attribute can be used as the class label. Below are the column details;

*   "Administrative", "Administrative Duration", "Informational", "Informational Duration", "Product Related" and "Product Related Duration" represents the number of different types of pages visited by the visitor in that session and total time spent in each of these page categories. The values of these features are derived from the URL information of the pages visited by the user and updated in real-time when a user takes an action, e.g. moving from one page to another. 

*   The "Bounce Rate", "Exit Rate" and "Page Value" features represent the metrics measured by "Google Analytics" for each page in the e-commerce site. 

*    value of the "Bounce Rate" feature for a web page refers to the percentage of visitors who enter the site from that page and then leave ("bounce") without triggering any other requests to the analytics server during that session.

*   The value of the "Exit Rate" feature for a specific web page is calculated as for all pageviews to the page, the percentage that was the last in the session.

*   The "Page Value" feature represents the average value for a web page that a user visited before completing an e-commerce transaction. 

*   The "Special Day" feature indicates the closeness of the site visiting time to a specific special day (e.g. Mother’s Day, Valentine's Day) in which the sessions are more likely to be finalized with the transaction. The value of this attribute is determined by considering the dynamics of e-commerce such as the duration between the order date and delivery date. For example, for Valentina’s day, this value takes a nonzero value between February 2 and February 12, zero before and after this date unless it is close to another special day, and its maximum value of 1 on February 8. 

*   The dataset also includes the operating system, browser, region, traffic type, visitor type as returning or new visitor, a Boolean value indicating whether the date of the visit is weekend, and month of the year.

# Importing and Installing Libraries
```{r}
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("GGally")
install.packages("cluster")
install.packages("pander")
install.packages("Hmisc")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(pander)
library(Hmisc)
library(readr)
```
# Loading our dataset
```{r}
ecommerce_data <- read_csv("C:/Users/color/Downloads/online_shoppers_intention.csv")
```
# Preview our data
```{r}
head(ecommerce_data,5)

tail(ecommerce_data,5)
```
# Data Understanding
```{r}
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

```
# Exploratory Data Analysis

## Univariate Analysis
```{r}
summary(ecommerce)
```
## Bivariate Analysis
```{r}
numeric_df = ecommerce[, sapply(ecommerce, is.numeric)]

head(numeric_df)

# Computing correlation matrix

corr_matrix <- cor(numeric_df)

head(round(corr_matrix,2))

# Visualizing the correlation matrix

library(corrplot)

corrplot(corr_matrix, method="circle")

names(ecommerce)
```
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

# Follow up questions


a). Did we have the right data?

Yes
 
b). Do we need other data to answer our question?

no.

c). Did we have the right question?

yes
