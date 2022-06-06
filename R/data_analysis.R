library(nycflights13)
library(dplyr)
library("tidyverse")
library("tidyr")
library("ggplot2")
library(datasets)
library(readr)



train_df <- read_csv("C:/Users/khlzz/Desktop/train_df.csv")
x_values <- train_df[,c(200,208,209,212,222,223)]
x_values
y_train <- read_csv("C:/Users/khlzz/Desktop/y_train.csv")
y_train
y_values <- y_train[,2]
y_values


library(randomForest)

df_1 <-cbind(x_values,y_values)
df_1
head(df_1)

linear_fit <- lm(SalePrice~.,df_1)
summary(linear_fit)
plot(linear_fit)


ranfo_fit <- randomForest(SalePrice~OverallQual + TotalBsmtSF + GrLivArea + GarageCars + GarageArea, data = df_1)
summary(ranfo_fit)

head(test)
test <- read_csv("sample_submission.csv")
head(test)
pred_linear <- predict(linear_fit,newdata = test)
pred_linear
pred_randfo <- predict(ranfo_fit, newdata = test)
pred_randfo

rmse_linear <- (sum((test$SalePrice-pred_linear)^2))^0.5
rmse_randfo <- (sum((test$SalePrice-pred_randfo)^2))^0.5

write.csv(pred_linear,file="linear.csv")
write.csv(pred_randfo,file="randfo.csv")
