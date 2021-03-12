---
title: "Excercise 2"
author: "Xiaohan Sun / Liyuan Zhang / Evelyn Cheng"
date: "2021/3/12"
output: word_document
---

## Problem 1: visualization

### first figure + caption

```{r echo=FALSE, results='hide',message=FALSE}
library(tidyverse)
library(ggplot2)
capmetro_UT = read.csv('../data/capmetro_UT.csv')
capmetro_UT = mutate(capmetro_UT,
                     day_of_week = factor(day_of_week,
                                          levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                     month = factor(month,
                                    levels=c("Sep", "Oct","Nov")))
head(capmetro_UT)
capmetro_UT1 = capmetro_UT %>%
  group_by(hour_of_day, day_of_week, month) %>%
  summarise(boarding=mean(boarding))

ggplot(data=capmetro_UT1) + 
  geom_line(aes(x=hour_of_day, y=boarding, color=month)) + 
  facet_wrap(~day_of_week) + 
  labs(title="AVG boardings grouped by hour, week & month", caption="Capital Metro" )
```

The hour of peak boardings change from day to day. During Monday to Friday which is the working day, the peak is 16-17 o'clock. During weekends, there is a smooth peak on 17-18 o'clock.

I reckon that the first Monday in September is Labor Day and there is one day off. So, average boardings on Mondays in September look lower.

Because there are two holidays in November, the 11th is a day off for Veterans Day. The second is Thanksgiving, the fourth Thursday in November, and there are two days off. Therefore, average boardings on Weds/Thurs/Fri in November look lower.

\newpage

### Second figure + caption

```{r echo=FALSE, results='hide',message=FALSE}
capmetro_UT2 = capmetro_UT %>%
  group_by(hour_of_day, temperature, weekend) %>%
  summarise(boarding=mean(boarding))

ggplot(data=capmetro_UT2) + 
  geom_point(aes(x=temperature, y=boarding, color=weekend), alpha=0.8, size=1) + 
  facet_wrap(~hour_of_day) + 
  labs(title="AVG boardings grouped by temperature & weekend",
       caption="Capital Metro" )
```

Temperature doesn't seem to have a noticeable effect on the number of UT students riding the bus. Since the points of the same color look like a straight line, there is no obvious slope. 

## problem 2: Saratoga house prices

### Linear model

```{r,echo=FALSE, message=FALSE, warning=FALSE,include=FALSE}
library(tidyverse)
library(mosaic)
library(foreach)
library(modelr)
library(caret)
library(parallel)
library(rsample)
data(SaratogaHouses)

saratoga_split = initial_split(SaratogaHouses, prop = 0.8)
saratoga_train = training(saratoga_split)
saratoga_test = testing(saratoga_split)

lm_medium = lm(price ~ lotSize + age + livingArea + bedrooms + 
                 fireplaces + bathrooms + rooms + heating + fuel + centralAir, 
               data=saratoga_train)

lm0= lm(price ~ 1, data=saratoga_train)
lm_forward = step(lm0, direction='forward',
                  scope=~(lotSize + age + livingArea + bedrooms + 
                            fireplaces + bathrooms + rooms + heating + fuel + 
                            centralAir)^2)

lm_big = lm(price ~ (lotSize + age + livingArea + pctCollege + bedrooms + 
                       fireplaces + bathrooms + rooms + heating + fuel + centralAir +
                       landValue + sewer + newConstruction + waterfront)^2, 
            data= saratoga_train)

lm_step = step(lm_medium, 
               scope=~(.)^2)
```

```{r,echo=FALSE, message=FALSE, warning=FALSE}
rmse(lm_medium, saratoga_test)
rmse(lm_big, saratoga_test)
rmse(lm_forward, saratoga_test)
rmse(lm_step, saratoga_test)
getCall(lm_forward)
```

The best linear model I found is  price = livingArea + centralAir + bathrooms + bedrooms +heating + lotSize + rooms + livingArea*centralAir + bathrooms*heating +bedrooms*heating + livingArea*rooms + bedrooms*rooms + livingArea*lotSize +lotSize*rooms + centralAir*bedrooms. The RMSE is 65430.21, which is lower than the RMSE in professor's medium regression

### KNN model

```{r, echo=FALSE, message=FALSE, include=FALSE}
lm0= lm(price ~ 1, data=saratoga_train)
lm_forward_k = step(lm0, direction='forward',
                  scope=~lotSize + age + livingArea + bedrooms + 
                            fireplaces + bathrooms + rooms + heating + fuel + 
                            centralAir)
```
```{r, echo=FALSE, message=FALSE}
getCall(lm_forward_k)
```

I will use the regression price=livingArea + centralAir + bathrooms + bedrooms +lotSize + rooms + heating to find the best RMSE in KNN model

```{r, echo=FALSE, message=FALSE, include=FALSE}
k_grid = c(2, 4, 6, 10, 14, 15, 16, 20, 25, 30, 35, 40, 
           50, 60, 70, 80, 100, 125, 150, 175, 200, 250, 300) 
rmse_grid_out = foreach(k = k_grid, .combine='rbind') %do% {
  knn_model = knnreg(price ~ lotSize + age + livingArea + bedrooms + 
                       bathrooms + rooms + heating + centralAir + landValue + 
                       waterfront + newConstruction, data=saratoga_train, k = k, use.all=TRUE)
  RMSE_= modelr::rmse(knn_model, saratoga_test)
  c(K=k, RMSE_=RMSE_)} %>% as.data.frame
rmse_grid_out = foreach(k = k_grid, .combine='c') %do% {
  knn_model = knnreg(price ~ lotSize + age + livingArea + bedrooms + 
                       bathrooms + rooms + heating + centralAir + landValue + waterfront + 
                       newConstruction, data=saratoga_train,k = k, use.all=TRUE)
  modelr::rmse(knn_model, saratoga_test)
}
rmse_grid_out = data.frame(K = k_grid, RMSE = rmse_grid_out)
ind_best = which.min(rmse_grid_out$RMSE)
k_best = k_grid[ind_best]
```
```{r, echo=FALSE, message=FALSE}
k_best
```

I will use this best K to find the RMSE

```{r, echo=FALSE, message=FALSE,include=FALSE}
knn = knnreg(price ~ lotSize + age + livingArea + bedrooms + 
               bathrooms + rooms + heating + centralAir + landValue + waterfront + 
               newConstruction, data=saratoga_train, k=k_best)
```
```{r, echo=FALSE, message=FALSE}
rmse(knn, saratoga_test)
```

I think KNN model is much better than the linear model, since the RMSE in KNN model is greater than the linear model.

## Problem 3: Classification and retrospective sampling

```{r echo=FALSE, results='hide',message=FALSE}
library(ggplot2)
library(tidyverse)
german_credit = read.csv('../data/german_credit.csv')

german_credit_mean=german_credit %>%
  group_by(history) %>%
  summarize(Mean_Default=mean(Default))
```

### Make a bar plot of default probability by credit history

```{r echo=FALSE, results='hide',message=FALSE}
ggplot(german_credit_mean,aes(x=Mean_Default,y=history)) + 
  geom_bar(stat='identity', alpha=0.7) +
  labs(title="Bar plot of default probability by credit history", x="default probability", y="credit history") 
```

### Build a logistic regression model for predicting default probability

```{r echo=FALSE}
default_probability_glm=glm(Default ~ duration + amount + installment + age + history + purpose + foreign, family=binomial, data=german_credit)
summary(default_probability_glm)
```

**What do you notice about the history variable vis-a-vis predicting defaults? What do you think is going on here?**

Banks provide high amount loans to people with good credit history , but without collateral. This is why people with terrible credit history default on their loans when they do this. Since defaults are rare, the bank conducted a sample survey of a group of defaulted loans. Banks try to match each default behavior with similar loan groups that have not defaulted, resulting in a large number of default over-sampling. In the graph we've made, the lower the historical credit of the borrower, the lower the probability of default.

**In light of what you see here, do you think this data set is appropriate for building a predictive model of defaults, if the purpose of the model is to screen prospective borrowers to classify them into "high" versus "low" probability of default? Why or why not---and if not, would you recommend any changes to the bank's sampling scheme?**

According to what I've done in this question, I reckon that this data set isn't appropriate for building a predictive model of defaults. Because there is a substantial oversampling of defaults. In my opinion, I recommend bank should reduce the sample of defaults. Using proportional sampling instead, it maybe more appropriate for predictive model of defaults.

## Problem 4: Children and hotel reservations

```{r echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
library(tidyverse)
library(mosaic)
library(foreach)
library(modelr)
library(rsample)
library(foreach)
```

### Model building

```{r echo=FALSE, results='hide',message=FALSE, warning=FALSE}
hotels_dev = read.csv('../data/hotels_dev.csv')
hotels_dev_split = initial_split(hotels_dev, prop = 0.8)
hotels_dev_train = training(hotels_dev_split)
hotels_dev_test = testing(hotels_dev_split)
```

##### Baseline 1

For baseline 1, the model is just contain 4 features with no interaction, and we use MSE to evaluate the out-of-sample performance. 

```{r echo=FALSE, results='hide',message=FALSE, warning=FALSE}
lm_small = lm(children ~ market_segment + adults + customer_type + is_repeated_guest, data=hotels_dev_train)

rmse(lm_small, hotels_dev_test)
```

MSE for baseline 1: 0.2678641.

##### Baseline 2

For baseline 2, this is a big model with all features and interactions except `arrival_date`. 

```{r echo=FALSE, results='hide',message=FALSE,warning=FALSE}
lm_big = lm(children ~ (.-arrival_date)^2, data=hotels_dev_train)

rmse(lm_big, hotels_dev_test)
```

MSE for baseline 2: 0.2244956.

##### Baseline 3

For baseline 3, there are two models that use different selection methods. One uses stepwise selection, the other uses forward selection. 

For forward selection, we choose the features that we think is essential to the outcome such as `stays_in_weekend_nights`,  `adults`,  `meal`,  `reserved_room_type`, etc.

```{r echo=FALSE, message=FALSE, warning=FALSE, results='hide'}
lm0 = lm(children ~ 1, data=hotels_dev_train)

lm_forward = step(lm0, direction='forward',
                  scope=~(hotel+lead_time+stays_in_weekend_nights+stays_in_week_nights+adults+meal+previous_cancellations+reserved_room_type++average_daily_rate+required_car_parking_spaces+total_of_special_requests)^2)

lm_step = step(lm_small,
                  scope=~(. -arrival_date)^2)

rmse(lm_step, hotels_dev_test)
rmse(lm_forward, hotels_dev_test)
```

MSE for model with stepwise selection: 0.2628751.

MSE for model with forward selection: 0.2173147.

As a result, the model with forward selection is better.

After comparing the MSE values of these models, we find that the model in baseline 3 with forward selection is the best.

### Model validation: step 1

This is the ROC curve we produced for our best model. The data we use is `hotels_val`. And we just zoomed in for FPR between 0 and 0.2.

```{r echo=FALSE, message=FALSE, warning=FALSE}
hotels_val = read.csv('../data/hotels_val.csv')
phat_val_best = predict(lm_forward, hotels_val, type='response')

thresh_grid = seq(0.95, 0.05, by=-0.005)
roc_curve = foreach(thresh = thresh_grid, .combine='rbind') %do% {
  yhat_test_linear = ifelse(phat_val_best >= thresh, 1, 0)
  
# FPR, TPR for linear model
confusion_out_linear = table(y = hotels_val$children, yhat = yhat_test_linear)
out_lin = data.frame(model = "best fit model",
                     TPR = confusion_out_linear[2,2]/sum(hotels_val$children==1),
                     FPR = confusion_out_linear[1,2]/sum(hotels_val$children==0))
  
  rbind(out_lin)
} %>% as.data.frame()

ggplot(roc_curve) + 
  geom_line(aes(x=FPR, y=TPR, color=model)) + 
  labs(title="ROC curves: The best fit model") +
  theme_bw(base_size = 10)+
  xlim(0, 0.20)
```

### Model validation: step 2

The following table shows the actual and predict number of children, also the difference between these two. And we find that most of predict numbers is close to the real number. If we set that the difference that beyond 5 is false value, then the precision of this model is 80%.

```{r echo=FALSE, results='hide',message=FALSE, warning=FALSE}
# Step2
K_folds = 20

# create specific fold IDs for each row
hotels_val = hotels_val %>%
  mutate(fold_id = rep(1:K_folds, length=nrow(hotels_val)) %>% sample)

phat_val = predict(lm_forward,hotels_val,type='response')

summary(phat_val)

hotels_val = hotels_val %>%
  mutate(phat_val = predict(lm_forward,hotels_val,type='response'))

summary(hotels_val$phat_val)

hotels_val = hotels_val %>%
  mutate(yhat_val = ifelse(phat_val > 0.223, 1, 0))

table = hotels_val %>%
  group_by(fold_id) %>%
  summarise(Actual = sum(children == 1), Predict = sum(yhat_val == 1), diff = Actual - Predict)
```

```{r echo=FALSE,message=FALSE, warning=FALSE}
table
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

