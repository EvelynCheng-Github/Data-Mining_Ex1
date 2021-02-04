library(tidyverse)
library(ggplot2)
library(mosaic)
library(FNN)
library(foreach)
library(rsample)
library(caret)
library(modelr)
library(parallel)

sclass = read.csv('../data/sclass.csv')

sclass350 = subset(sclass, trim == '350')
sclass65AMG = subset(sclass, trim == '65 AMG')

sclass350_split = initial_split(sclass350, prop=0.9)
sclass350_train = training(sclass350_split)
sclass350_test  = testing(sclass350_split)

sclass65AMG_split = initial_split(sclass65AMG, prop=0.9)
sclass65AMG_train = training(sclass65AMG_split)
sclass65AMG_test  = testing(sclass65AMG_split)

k_grid = c(2, 3, 4, 6, 8, 10, 14, 15, 16, 20, 25, 30, 35, 40,
           50, 60, 70, 80, 100, 125, 150, 175, 200)

rmse_out = foreach(i=1:20, .combine='rbind') %dopar% {
  this_rmse = foreach(k = k_grid, .combine='c') %do% {
    knn_model = knnreg(price ~ mileage, data=sclass350_train, k = k, use.all=TRUE)
    modelr::rmse(knn_model, sclass350_test)
  }
  data.frame(k=k_grid, rmse=this_rmse)
}
rmse_out = arrange(rmse_out, k)
ggplot(rmse_out) + 
  geom_boxplot(aes(x=factor(k), y=rmse)) + 
  theme_bw(base_size=10)


rmse_out = foreach(i=1:20, .combine='rbind') %dopar% {
  this_rmse = foreach(k = k_grid, .combine='c') %do% {
    knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = k, use.all=TRUE)
    modelr::rmse(knn_model, sclass65AMG_test)
  }
  data.frame(k=k_grid, rmse=this_rmse)
}
rmse_out = arrange(rmse_out, k)
ggplot(rmse_out) + 
  geom_boxplot(aes(x=factor(k), y=rmse)) + 
  theme_bw(base_size=10)

knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = 4, use.all=TRUE)
p_train = ggplot(data = sclass65AMG_train) + 
  geom_point(mapping = aes(x = price, y = mileage), color='darkgrey')
p_train + geom_path(mapping = aes(x = price, y = mileage), color='red', size=0.5)
