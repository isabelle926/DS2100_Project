---
  title: "project"
output: html_document
---
  
# Loading in the data
library(tidyverse)
df_raw <- read.csv("Steam Trends 2023 by @evlko and @Sadari - Games Data.csv")

# Peeping the data
head(df_raw)
nrow(df_raw)


# Cleaning the data
# 1. selecting relevant columns
# 2. renaming columns for readability and easier analysis
# 3. converting rating and price columns to double 
# remove commas, then remove %, convert to int, divide by 100

df_games <- df_raw |>
  rename(title = Title, reviews = Reviews.Total, rating = Reviews.Score.Fancy, price = Launch.Price, tags = Tags) |>
  mutate(rating = rating |> str_remove("%") |> str_replace_all(",", ".") |> as.numeric(),
         price = price |> str_remove("\\$") |> str_replace_all(",", ".") |> as.numeric(), 
         indie = if_else(str_detect(tags, "Indie"), 1, 0)) |>
  select(title, reviews, rating, price, indie) |>
  filter(reviews > 1000, price < 100) 

nrow(df_games)
head(df_games, 40)

# Descriptive statistics
# number of reviews
median(df_games$reviews)
mean(df_games$reviews)
range(df_games$reviews)

# rating score
median(df_games$rating)
mean(df_games$rating)
range(df_games$rating)

# launch price
median(df_games$price)
mean(df_games$price)
range(df_games$price)

# indie tag
indie_games <- nrow(df_games |> filter(indie == "1"))
perc_indie <- indie_games / nrow(df_games) * 100

indie_games
perc_indie


# Test 1: correlation coefficient, linear regression
cor(df_games$rating, df_games$price)
model_linear <- lm(df_games$rating ~ df_games$price)

plot(df_games$rating ~ df_games$price,
     xlab = "Price", 
     ylab = "Review Score",
     main = "Price vs. Review Score")
abline(model_linear, col = "purple")

summary(model_linear)

# 2. Splitting data based on price groups
hist(df_games$price,
     main = "Histogram of prices",
     xlab = "Price",
     ylab = "Number of games",
     col = "lightblue")

groups <- df_games |>
  mutate(group = case_when(
    price <= 10 ~ "Broke",
    (price > 10 & price <= 30) ~ "Cheapskate",
    (price > 30 & price <= 50) ~ "Spendthrift",
    price > 50 ~ "Filthy Rich"
  ))

anova <- aov(groups$rating ~ groups$group)
summary(anova)

# fisher LSD test
pairwise.t.test(groups$rating, groups$group, p.adjust.method="none")

boxplot(groups$rating ~ groups$group,
        main = "Review Score by Price Range",
        xlab = "Price Group", ylab = "Review Score")

# 3. Indie vs AAA for kicks
df_indie <- df_games |>
  filter(indie == 1)

df_AAA <- df_games |>
  filter(indie == 0)

cor(df_indie$rating, df_indie$price)
model_indie <- lm(df_indie$rating ~ df_indie$price)
summary(model_indie)

cor(df_AAA$rating, df_AAA$price)
model_AAA <- lm(df_AAA$rating ~ df_AAA$price)
summary(model_AAA)


# I love machine learning!!!! (real)
## Organizing the data
```{r}
library(caret)


# cool random seed for reproducibility
set.seed(2100)

# split data
training_index <- createDataPartition(df_games$price, p=0.8, list = FALSE)
training_set <- df_games[training_index, ]
testing_set <- df_games[-training_index, ]

## Build the training model
model <- train(rating ~ price, data = training_set,
               method = "lm",
               trControl = trainControl(method="none"))

## Apply model for prediction
model.training <- predict(model, training_set)
model.testing <- predict(model, testing_set)

## plot our models!!
plot(model.training, training_set$rating, 
     col = "blue",
     xlab = "Prediction for rating based on training set",
     ylab = "Actual rating")
model_train <- lm(training_set$rating ~ model.training)
abline(model_train, col = "black")

plot(model.testing, testing_set$rating, 
     col = "red",
     xlab = "Prediction for rating based on testing set",
     ylab = "Actual rating")
model_test <- lm(testing_set$rating ~ model.testing)
abline(model_test, col = "black")

summary(model)


