
# Consumer Segmentation Using R

# Background Information

A Kenyan entrepreneur has created an online cryptography course and would want
to advertise it on her blog. She currently targets audiences originating from 
various countries. In the past, she ran ads to advertise a related course on the
same blog and collected data in the process. She would now like to employ your 
services as a Data Science Consultant to help her identify which individuals are
most likely to click on her ads. 

## Business Underdstanding
### Business Objective

The main goal is to gather insifghts from the data on who is more likely to click on the new online course advertisment.

### Business Success Criteria

Our data mining success will be measured on narrowing to the characteristics of individuals who are more likely to click on the course advertisment.

## Data Understanding

The data used for this data mining project was sourced from;[http://bit.ly/IPAdvertisingData]

The dataset contains 10 rows with 1000 records.

Our assumption is the data collected is accurate and is a true reflection of the population.

## Data Preparation

We will load data to R enviroment ready for analysis. The below steps would bve followed;
1. Identification of outliers
2. Identification of duplicates
3. Identification of Null values

If found all will be dealt with accordingly.


## 1. Defining the Question

### a. Specifying the Question

A Kenyan entrepreneur has created an online cryptography course and would want
to advertise it on her blog. She currently targets audiences originating from 
various countries. In the past, she ran ads to advertise a related course on the
same blog and collected data in the process. She would now like to employ your 
services as a Data Science Consultant to help her identify which individuals are
most likely to click on her ads. 

### b. Defining the metric of success

Successfully perform consumer segmentation and developing a decsion tree and random forest model with an accuracy of over 80%.

### c. Understanding the Context

As a data Scientist we have been tasked to understand the type of individuals
who are more likely to subscribe to our clients services based on data shared.

### d. Recording the Experimental Design

1. Univariate Analysis
2. Bivariate analysis
3. Decision Tree
4. Random Forest

### e. Data Relevance

The data provided is relevant to answering the research question.

## 2. Reading the Data
```{r}
library(readr)

advertising <- read_csv("C:/Users/color/Downloads/advertising.csv")
```
# 3. Checking the Data
```{r}
head(advertising)

tail(advertising)
```
## 4. Tidying the Data set
```{r}
summary(advertising)

names(advertising)

colSums(is.na(advertising))

is.double(advertising)

colSums(is.double(advertising))

### Checking for Outliers on our numerical data

boxplot_Income = boxplot(advertising$'Area Income',
                            main = "Boxplot for Area Income variable",
                            xlab = "Area Income",
                            col = "blue",
                            border = "grey",
                            horizontal = TRUE,
                            notch = TRUE)

boxplot_Age = boxplot(advertising$Age,
                            main = "Boxplot for Age",
                            xlab = "Area Income",
                            col = "blue",
                            border = "grey",
                            horizontal = TRUE,
                            notch = TRUE)

boxplot_TimeSpent_on_Site = boxplot(advertising$'Daily Time Spent on Site',
                            main = "Boxplot for Time spent on the site",
                            xlab = "Area Income",
                            col = "blue",
                            border = "grey",
                            horizontal = TRUE,
                            notch = TRUE)

boxplot_internet_usage = boxplot(advertising$'Daily Internet Usage',
                            main = "Boxplot for Daily time on internet",
                            xlab = "Area Income",
                            col = "blue",
                            border = "grey",
                            horizontal = TRUE,
                            notch = TRUE)
6
3
```
## 5. Exploratory Data Analysis

### a. Univariate Analysis
```{r}
names(advertising)



histogram.Age = hist(x=advertising$Age,
                     main="Histogram on Age",
                     col = "green",
                     xlab = "overall Age")

histogram.Income = hist(x=advertising$`Area Income`,
                        main="Histogram on Area Income",
                        col = "green",
                        xlab = "overall Area Income")

histogram.Time = hist(x=advertising$`Daily Time Spent on Site`,
                      main="Histogram on Time Spent",
                      col = "green",
                      xlab = "overall Time Spent")

histogram.Internet_Usage  = hist(x=advertising$`Daily Internet Usage`,
                                 main="Histogram on Daily Internet Usage",
                                 col = "green",
                                 xlab = "Daily Internet Usage")

```
### b. Bivariate Analysis

#Checking Correlation Between our Variables

#Import COrrelation Library
```{r}
install.packages("corrplot")

numeric_df = advertising[, sapply(advertising, is.numeric)]

head(numeric_df)
```
# Computing correlation matrix
```{r}
corr_matrix <- cor(numeric_df)

head(round(corr_matrix,2))

# Visualizing the correlation matrix
```
```{r}
library(corrplot)
corrplot(corr_matrix, method="circle")

names(advertising)

# Relationship between Age Vs Clicked on Ad

counts = table(advertising$Age, advertising$`Clicked on Ad`)
barplot(counts, main = "Clicks with respect to Age", xlab = "Clicks on Ads", col = c("black", "red"), legend = rownames(counts), beside = TRUE)

# Relationship between Sex Vs Clicked on Ad

counts = table(advertising$Male, advertising$`Clicked on Ad`)
barplot(counts, main = "Clicks with respect to Sex", xlab = "Clicks on Ads", col = c("blue", "green"), legend = rownames(counts), beside = TRUE)
```
## Conclusion

From our analysis we were able to not that our data was clean and had no duplicates. It is therefore safe to conclude that indviduals aged between 30-40 years of age are more likely to click on ads majority being females.


## 6. Modeling

### Decision Trees & Random Forest

```{r}
#Selecting our numerical columns

advert_new = advertising[, c(1,2,3,4,7,10)]
head(advert_new)
names(advert_new)
names(advertising)

# get column names
colnames(advert_new)

# Rename column where names is "Sepal.Length"
names(advert_new)[names(advert_new) == "Clicked on Ad"] <- "Clicked.on.Ad"
names(advert_new)[names(advert_new) == "Daily Time Spent on Site"] <- "Daily.Time.Spent.on.Site"
names(advert_new)[names(advert_new) == "Area Income"] <- "Area.Income"
names(advert_new)[names(advert_new) == "Daily Internet Usage"] <- "Daily.Internet.Usage"
advert_new

# We are going to normalize the data first

data_norm <- function(x){((x - min(x))/(max(x) - min(x)))}
advert_norm <- as.data.frame(lapply(advert_new[, -6], data_norm))

# Shuffling rows

rows <- sample(nrow(advert_new))

# Shuffle

advert_new <- advert_new[rows, ]
head(advert_new,10)

#Splitting the data

install.packages("caret")

install.packages("tidyverse")

library(caret)

install.packages("skimr")

library(skimr)

options(warn = -1)

set.seed(100)

#For every 10 observations 8 will be used for training and 2 testing

train_set = createDataPartition(advert_new$Clicked.on.Ad, p=0.80, list=FALSE)
```
#train dataset
```{r}
train = advert_new[train_set,]
```
```{r}
#Test dataset
test = advert_new[-train_set,]

# Preview of 10 records

head(train,10)

head(test,10)

# Setting the variables
x = train
y = train$Clicked.on.Ad

head(x,5)
head(y,5)
```
### Training the model using Random Forest
```{r}
install.packages("randomForest")

library(randomForest)


# Fitting the model
model = randomForest(Clicked.on.Ad ~ ., data = train, importance = TRUE)
model

#Checking prediction
pred = predict(model, train, type = "class")

#Checking the accuracy of classification
mean(pred == train$Clicked.on.Ad)
```
```{r}
# Accuracy
accuracy = table(pred, train$Clicked.on.Ad)
accuracy

#Prediction on the test set
pred_test <- predict(model, test, type = "class")

#Checking the classification accuracy
mean(pred_test == test$Clicked.on.Ad)

# Training: Decision Trees

install.packages('rpart.plot')

library(rpart)

library(rpart.plot)

# Fitting the model
# Training the model
#
model<- rpart(Clicked.on.Ad ~ ., data = train ,
              method = "class")


# Plotting the tree

rpart.plot(model)

# Prediction on test dataset
pred <- predict(model, test, type = "class")
table(pred, test$Clicked.on.Ad)
```
## Conclusion

From our accuracy score it is observed that both our models performed well with an accuracy sco

## 7. Follow up Questions

a). Did we have the right data?

We had reelevant data to meet our research question

b). Do we need other data to answer our question?

More data would be useful to have a broad understanding of the individuals/

c). Did we have the right question?

Yes.