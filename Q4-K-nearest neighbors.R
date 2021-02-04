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

k_grid = unique(round(exp(seq(log(80), log(2), length=5))))
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass350_train, k = k, use.all=TRUE)
  modelr::rmse(knn_model, sclass350_test)
}
rmse_grid_out = data.frame(K = k_grid, RMSE = rmse_grid_out)
p_out = ggplot(data=rmse_grid_out) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5)

ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]

rmse_grid_in = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass350_train, k = k, use.all=TRUE)
  modelr::rmse(knn_model, sclass350_train)
}
rmse_grid_in = data.frame(K = k_grid, RMSE = rmse_grid_in)
p_out + geom_path(data=rmse_grid_in, aes(x=K, y=RMSE, color='trainset'),size=0.5) +
  ylim(8000, 12000) +
  xlim(10,15) + 
  scale_colour_manual(name="RMSE",
                      values=c(testset="black", trainset="grey")) + 
  geom_vline(xintercept=k_best, color='darkgreen', size=1) +
  labs (titles = "K-nearest neighbors: test - 350")



sclass65AMG_split = initial_split(sclass65AMG, prop=0.9)
sclass65AMG_train = training(sclass65AMG_split)
sclass65AMG_test  = testing(sclass65AMG_split)

rmse_grid_out2 = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = k, use.all=TRUE)
  modelr::rmse(knn_model, sclass65AMG_test)
}
rmse_grid_out2 = data.frame(K = k_grid, RMSE = rmse_grid_out2)
p_out = ggplot(data=rmse_grid_out2) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5)

ind_best = which.min(rmse_grid_out2$RMSE)
k_best = k_grid[ind_best]

rmse_grid_in2 = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = k, use.all=TRUE)
  modelr::rmse(knn_model, sclass65AMG_train)
}
rmse_grid_in2 = data.frame(K = k_grid, RMSE = rmse_grid_in2)
p_out + geom_path(data=rmse_grid_in2, aes(x=K, y=RMSE, color='trainset'),size=0.5) +
  scale_colour_manual(name="RMSE",
                      values=c(testset="black", trainset="grey")) + 
  geom_vline(xintercept=k_best, color='darkgreen', size=1)+
  labs (titles = "K-nearest neighbors: test - 65 AMG")

g1 = ggplot(data = sclass65AMG_train) + 
  geom_point(mapping = aes(x = price, y = mileage), color='darkgrey')
knn5 = knnreg(price ~ mileage, data=sclass65AMG_train, k = 5, use.all=TRUE)
knn5_pred = function(x) {
  predict(knn5, newdata=data.frame(price=x))
}
g1 + stat_function(fun=knn5_pred, color='red', size=1, n=1001)

