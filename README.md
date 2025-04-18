# DS2100_Project
R code for the DS 2100 Final project

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

## Test 1: correlation testing
```{r}
# apparently we can run a "correlation test," which gives both correlation coefficient and a t-test on the correlation coefficient (with the null being: r-sqaured is equal to 0)
cor.test(df_games$price, df_games$rating, use = "complete.obs")
```

## Test 2. linear regression
```{r}
model <- lm(df_games$price ~ df_games$rating)
plot(df_games$price, df_games$rating, main = "Price vs. Review Score", xlab = "Price", ylab = "Review Score")
abline(model, col = "purple")
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
plot(training_set$rating, model.training, col = "blue")
plot(testing_set$rating, model.testing, col = "red")
```
