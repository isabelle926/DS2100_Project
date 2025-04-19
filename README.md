# DS2100_Project
R code for the DS 2100 Final project

Data: https://www.reddit.com/r/gamedev/comments/165cii0/this_year_we_gathered_data_about_65000_games_in/

.
.
.
.
## Loading in the data
```{r}
library(tidyverse)
df_raw <- read.csv("Steam Trends 2023 by @evlko and @Sadari - Games Data.csv")
```

## Peeping the data
```{r}
head(df_raw)
nrow(df_raw)
```
## Check for outliers
```{r}
boxplot(df_raw$price, main="Price Distribution")
boxplot(df_raw$rating, main="Rating Distribution")
```

## Cleaning the data
```{r}
# 1. selecting relevant columns
# 2. renaming columns for readability and easier analysis
# 3. converting rating and price columns to double 
# remove commas, then remove %, convert to int, divide by 100

df_games <- df_raw |>
  select(Title, Reviews.Total, Reviews.Score.Fancy, Launch.Price, Tags) |>
  rename(title = Title, reviews = Reviews.Total, rating = Reviews.Score.Fancy, price = Launch.Price, tags = Tags) |>
  mutate(rating = rating |> str_remove("%") |> str_replace_all(",", ".") |> as.numeric(),
         price = price |> str_remove("\\$") |> str_replace_all(",", ".") |> as.numeric(), 
         indie = if_else(str_detect(tags, "Indie"), 1, 0)) |>
  filter(reviews > 1000, price < 100) 

```
## Descriptive statistics
```{r}
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
```

## Test 1: correlation coefficient, linear regression
```{r}
cor(df_games$rating, df_games$price)
model_linear <- lm(df_games$rating ~ df_games$price)

plot(df_games$rating ~ df_games$price,
     xlab = "Price", 
     ylab = "Review Score",
     main = "Price vs. Review Score")
abline(model_linear, col = "purple")

summary(model_linear)
```

## Test 2. Splitting data based on price groups
```{r}
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

order <- c("Broke", "Cheapskate", "Spendthrift", "Filthy Rich")
groups$group <- factor(groups$group, levels = order)

boxplot(groups$rating ~ groups$group,
        main = "Review Score by Price Range",
        xlab = "Price Group", ylab = "Review Score")
```
## Test 3. Indie vs AAA for kicks
```{r}
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
```


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
```

## Build the training model
```{r}
model <- train(rating ~ price, data = training_set,
               method = "lm",
               trControl = trainControl(method="none"))
```

## Apply model for prediction
```{r}
model.training <- predict(model, training_set)
model.testing <- predict(model, testing_set)
```

## plot our models!!
```{r}
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
```
