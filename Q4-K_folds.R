library(tidyverse)
library(ggplot2)
library(mosaic)
library(FNN)
library(foreach)
library(rsample)
library(caret)
library(modelr)
library(parallel)

sclass <- read.csv("D:/UT-Austin/ECO395M_Data Mining/Excercise1/sclass.csv")

sclass350 = subset(sclass, trim == '350')
sclass65AMG = subset(sclass, trim == '65 AMG')

sclass350_split = initial_split(sclass350, prop=0.9)
sclass350_train = training(sclass350_split)
sclass350_test  = testing(sclass350_split)

sclass65AMG_split = initial_split(sclass65AMG, prop=0.9)
sclass65AMG_train = training(sclass65AMG_split)
sclass65AMG_test  = testing(sclass65AMG_split)

K_folds = 5
sclass350 = sclass350 %>%
  mutate(fold_id = rep(1:K_folds, length=nrow(sclass350)) %>% sample)
head(sclass350)

rmse_cv = foreach(fold = 1:K_folds, .combine='c') %do% {
  knn100 = knnreg(price ~ mileage,
                  data=filter(sclass350, fold_id != fold), k=100)
  modelr::rmse(knn100, data=filter(sclass350, fold_id == fold))
}

mean(rmse_cv)
sd(rmse_cv)/sqrt(K_folds)  

sclass350_folds = crossv_kfold(sclass350, k=K_folds)
models = map(sclass350_folds$train, ~ knnreg(price ~ mileage, k=100, data = ., use.all=FALSE))
errs = map2_dbl(models, sclass350_folds$test, modelr::rmse)

mean(errs)
sd(errs)/sqrt(K_folds)
k_grid = c(2, 4, 6, 10, 14, 15, 16, 20, 25, 30, 35, 40,
           50, 60, 70, 80, 100, 125, 150, 175, 200, 250, 300)

cv_grid = foreach(k = k_grid, .combine='rbind') %dopar% {
  models = map(sclass350_folds$train, ~ knnreg(price ~ mileage, k=k, data = ., use.all=FALSE))
  errs = map2_dbl(models, sclass350_folds$test, modelr::rmse)
  c(k=k, err = mean(errs), std_err = sd(errs)/sqrt(K_folds))
} %>% as.data.frame

ggplot(cv_grid) + 
  geom_point(aes(x=k, y=err)) + 
  geom_errorbar(aes(x=k, ymin = err-std_err, ymax = err+std_err)) + 
  scale_x_log10()