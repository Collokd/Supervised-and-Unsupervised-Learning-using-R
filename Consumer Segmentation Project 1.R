# Consumer Segmentation Using R

# 1. Reading the Data

library(readr)

advertising <- read_csv("C:/Users/color/Downloads/advertising.csv")

# 2. Checking the Data

head(advertising)

tail(advertising)

# 3. Tidying the Data set

summary(advertising)

names(advertising)

colSums(is.na(advertising))

is.double(advertising)

colSums(is.double(advertising))

# Checking for Outliers on our numerical data

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

# 4. Exploratory Data Analysis

# a. Univariate Analysis

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


# b. Bivariate Analysis

#Checking Correlation Between our Variables

#Import COrrelation Library

install.packages("corrplot")

numeric_df = advertising[, sapply(advertising, is.numeric)]

head(numeric_df)

# Computing correlation matrix
corr_matrix <- cor(numeric_df)

head(round(corr_matrix,2))

# Visualizing the correlation matrix

library(corrplot)
corrplot(corr_matrix, method="circle")

names(advertising)

# Relationship between Age Vs Clicked on Ad

counts = table(advertising$Age, advertising$`Clicked on Ad`)
barplot(counts, main = "Clicks with respect to Age", xlab = "Clicks on Ads", col = c("black", "red"), legend = rownames(counts), beside = TRUE)

# Relationship between Sex Vs Clicked on Ad

counts = table(advertising$Male, advertising$`Clicked on Ad`)
barplot(counts, main = "Clicks with respect to Sex", xlab = "Clicks on Ads", col = c("blue", "green"), legend = rownames(counts), beside = TRUE)

# Modeling

# Decision Trees & Random Forest


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

#train dataset
train = advert_new[train_set,]

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

# Training the model using Random Forest

install.packages("randomForest")

library(randomForest)


# Fitting the model
model = randomForest(Clicked.on.Ad ~ ., data = train, importance = TRUE)
model

#Checking prediction
pred = predict(model, train, type = "class")

#Checking the accuracy of classification
mean(pred == train$Clicked.on.Ad)

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