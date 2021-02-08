---
title: "Excercise1"
author: "Xiaohan Sun / Liyuan Zhang / Evelyn Cheng"
date: "2021/2/8"
output: word_document
---
# ECO 395M Homework 1: Xiaohan Sun / Liyuan Zhang / Evelyn Cheng 

## 1) Data visualization: gas prices

(A)

```{r}
library(tidyverse)
library(ggplot2)
GasPrices = read.csv('../data/GasPrices.csv')
head(GasPrices)
ggplot(data=GasPrices) + 
  geom_boxplot(aes(x=Price, y=Competitors))
```

**Claim:** Gas stations charge more if they lack direct competition in sight.

**Conclusion:** As the graph shows, the median price for gas stations that lacking competitors is higher than the one having competitors. Also, upper edge and upper quartile price are higher than the one having competitors. So, this claim is correct.

(B)

```{r}
summary(GasPrices$Income)
ggplot(data=GasPrices) + 
  geom_point(aes(x=Price, y=Income))+
  ylim(12780,128560)
```
  
**Claim:** The richer the area, the higher the gas price.

**Conclusion:** As shown in the figure, when the income is higher, the dots will fall more on the right. The trend for this scatter plot is increasing. So, this claim is correct.

(C)

```{r}
GasPricesC = GasPrices %>%
  group_by(Brand) %>%
  summarise(meanprice=mean(Price))
ggplot(data=GasPricesC) + 
  geom_col(aes(x=meanprice, y=Brand), alpha=0.6) + 
  scale_x_continuous(breaks=seq(0,2,0.2)) +
  geom_vline(xintercept=1.84,col="red")
```
  
**Claim:** Shell charges more than other brands.

**Conclusion:** In the bar chart, the bar of shell has higher price compared with Other brands. So, this claim is correct.

(D)

```{r}
ggplot(data=GasPrices,aes(x=Price,fill=Stoplight))+geom_histogram(bins = 30, alpha=0.4, position = "identity")
```

**Claim:** Gas stations at stoplights charge more.

**Conclusion:** The histogram present that the pick price for gas stations at stoplights (which is the blue one) is higher than the one doesn't (pink bars). So, this claim is correct.

(E)

```{r}
ggplot(data=GasPrices) + 
  geom_boxplot(aes(x=Price, y=Highway))
```


**Claim:** Gas stations with direct highway access charge more.

**Conclusion:** As the graph shows, the median price for gas stations  with direct highway access is higher than the one without. Also, upper quartile price are higher than the one without direct highway access. So, this claim is correct.

## 2) Data visualization: a bike share network

```{r}
library(tidyverse)
library(ggplot2)
bikeshare=read.csv('../Data/bikeshare.csv')

#plot A
d1=bikeshare %>%
  group_by(hr) %>%
  summarize(bikeshare_mean=mean(total))

ggplot(data=d1)+
  geom_line(aes(x=hr,y=bikeshare_mean))+
              labs(title="average bike rentals versus hour of the day",
                   caption = "capitalbikeshare.com",
                   x="hour of the day",
                   y="average bike rentals")+
  scale_x_continuous(breaks = 0:23)
```

* According to the graph, we can see that between 5:00pm-6:00pm there is the most popular time for renting bikes, with 8:00am being the second most popular.At 4 a.m., there is the least number of people rent bikes.

```{r}
#plot B
d2=bikeshare %>%
  group_by(workingday,hr) %>%
  summarize(bikeshare_mean=mean(total))

ggplot(data=d2)+
  geom_line(aes(x=hr,y=bikeshare_mean))+
  facet_wrap(~workingday,nrow = 2)+
  labs(title="average bike rentals per hour whether it is a working day",
       caption = "capitalbikeshare.com",
       x="hour of the day",
       y="average bike rentals")+
  scale_x_continuous(breaks = 0:23)
```

* For non-working day, people will rent bike during 11:00am-4:00pm. However, there are two busy time plots for renting bike on the working day, they are 8:00am and 5:00pm.

```{r}
#plot C
d3=bikeshare %>%
  filter(hr==8) %>%
  group_by(workingday,weathersit) %>%
  summarize(bikeshare_mean=mean(total))

ggplot(d3)+
  geom_col(mapping=aes(x=weathersit,y=bikeshare_mean),
           position = "dodge")+
  facet_wrap(~workingday)+
  labs(title="average ridership by weather situaion during 8AM",
       caption = "capitalbikeshare.com",
       x="weather situation",
       y="average bike rentals")+
  scale_x_continuous(breaks = 0:23)
```

*on the non-working day, most people rent the bike when the weather is clear,few clouds or partly cloudy during the 8:00am, and there are the least bike rent when the weather is light snow or light rain. However, on the working day, when the weather is clear, there still are the most rental bikes. comparing working day and non-working day, we recognize that the number of people renting bikes on working days is 5 times the number of people renting bikes on non-working days in the all weather situations.

## 3) Data visualization: flights at ABIA

```{r}
library(tidyverse)
library(ggplot2)
ABIA = read.csv('../data/ABIA.csv')
```

**Overall:**

In 2008, a total of 49,623 flights are scheduled to depart from Austin, for which American Airlines and Southwest Airlines have the largest number of flights.

```{r}
library(tidyverse)
library(ggplot2)
Total_origin = ABIA %>%
  filter(Origin == 'AUS') %>%
  group_by(UniqueCarrier)%>%
  summarize(total_count = n())

summary(Total_origin)

ggplot(data = Total_origin) + 
  geom_col(mapping = aes(x=UniqueCarrier, y=total_count),fill = "blue", alpha=0.6)+
  labs(title="The number of flights that departure from AUS", 
       y="The number of flights",
       x = "Airlines")

```

By month, the number of flights departing from Austin in 2008 peaked in June, and then continued to decline until September. Even though the number of flights increased in October, the number of flights in November reached the lowest point of the year.

```{r}
origin_month = ABIA %>%
  filter(Origin == 'AUS') %>%
  group_by(Month)%>%
  summarize(flights = n())

origin_month

ggplot(data = origin_month) + 
  geom_line(mapping = aes(x=Month, y=flights),color = "blue")+
  scale_x_continuous(breaks = 1:12)+
  labs(title="Changes in the number of flights that departure from AUS (months)", 
       y="The number of flights",
       x = "Month")
```

To specific, the main factor that influenced the number of flights by month is that some airlines have drastically reduced the number of flights after June, and even no longer provide services, like EV, NW, OH, etc. 

```{r}
origin_month_carrier = ABIA %>%
  filter(Origin == 'AUS') %>%
  group_by(Month,UniqueCarrier)%>%
  summarize(flights = n())

origin_month_carrier


ggplot(data = origin_month_carrier) + 
  geom_line(mapping = aes(x=Month, y=flights,color=(UniqueCarrier)))+
  scale_x_continuous(breaks = 1:12)+
  scale_y_log10(breaks = c(2,4,20,40,100,200,900))+
  labs(title="Changes in the number of flights that departure from AUS (airlines)", 
       y="The number of flights",
       x = "Month")
```

**Flight delay without cancellation**

From the bar chart, we know that the airlines of EV and WN have the highest departure delay rates, and the departure delay rate of WN is even close to 50%.

```{r}
ABIA = ABIA %>%
  mutate(if_delay = ifelse(DepDelay > 0, 1, 0))
head(ABIA)

P1= ABIA %>%
  filter(Origin == 'AUS', Cancelled == 0) %>%
  group_by(UniqueCarrier) %>%
  summarize(total_count = n(),delay_num =sum(if_delay),delay_rate = delay_num/total_count)
P1

ggplot(data = P1) + 
  geom_col(mapping = aes(x=UniqueCarrier, y=delay_rate),fill = "blue", alpha=0.6)+
  labs(title="Delay Rate of Airlines in 2008", 
       y="Delay Rate",
       x = "Airlines")
```

From the line chart, we find that the departure delay rates on Tuesday and Saturday are the lowest, and the departure delay rates on Thursday and Friday are relatively higher than others.

```{r}
P2= ABIA %>%
  filter(Origin == 'AUS', Cancelled == 0) %>%
  group_by(DayOfWeek)%>%
  summarise(total_count = n(),delay_num =sum(if_delay),delay_rate = delay_num/total_count)
P2

ggplot(data = P2) + 
  geom_line(mapping = aes(x=DayOfWeek, y=delay_rate),color = "blue")+
  scale_x_continuous(breaks = 1:7)+
  labs(title="Delay Rate in 2008 (Day of Week) ", 
       y="Delay Rate",
       x = "Day of Week")
```

**The reason of cancellation** 

In 2008, the airline of EQ has the highest rate of cancellation, the airline of NW has the lowest rate of cancellation. 
```{r}
Total_origin_cancel = ABIA %>%
  filter(Origin == 'AUS') %>%
  group_by(UniqueCarrier)%>%
  summarize(total_count = n(),cancellation_num = sum(Cancelled), cancellation_rate = cancellation_num/total_count)

Total_origin_cancel

ggplot(data = Total_origin_cancel) + 
  geom_col(mapping = aes(x=UniqueCarrier, y=cancellation_rate),fill = "blue", alpha=0.6)+
  labs(title="Cancellation Rate of Airlines in 2008 ", 
       y="Cancellation Rate",
       x = "Airlines")
```


In the bar chart, there are three cancellation reasons(A = carrier, B = weather, C = NAS). Obviously, most cancellation is due to carrier.

```{r}
p4 = ABIA %>%
  filter(Origin == 'AUS',Cancelled==1) %>%
  group_by(CancellationCode)%>%
  summarize(total_count = n())
p4

ggplot(data = p4) + 
  geom_col(mapping = aes(x=CancellationCode,y=total_count),fill = "blue", alpha=0.6)+
  labs(title="Reasons for Cancellation in 2008 ", 
       y="The number of Cancellation",
       x = "Reasons")
```

Furthermore, among those carriers, we find that AA carrier has the largest number of cancellations. For carrier of AA, most of the reason for cancellation is because of the carrier itself; for carrier of WN, the number of cancellations due to reason B accounts for the majority.

```{r}
p5 = ABIA %>%
  filter(Origin == 'AUS',Cancelled==1) %>%
  group_by(CancellationCode,UniqueCarrier)%>%
  summarize(total_count = n())
p5

ggplot(data = p5) + 
  geom_col(mapping = aes(x=CancellationCode,y=total_count))+
  facet_wrap(~UniqueCarrier)+
  labs(title="The Reason of Cancellation", 
       y="The number of cancellation",
       x = "Cancellation Code",
       fill="CancellationCode")
```

**In conclusion:** if you want to depart from Austin by plane, you'd better avoid Tuesday and Saturday, and buy other airlines besides EV, WN and MQ.

## 4) K-nearest neighbors

i. 350

```{r}
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

# Split the data into a training and a testing set
sclass350_split = initial_split(sclass350, prop=0.9)
sclass350_train = training(sclass350_split)
sclass350_test  = testing(sclass350_split)

# RMSE for each value of K
N = nrow(sclass350)
N_train = floor(0.8*N)
k_grid = unique(round(exp(seq(log(N_train), log(2), length=100))))

rmse_out = foreach(k = k_grid, .combine='rbind') %dopar% {
  this_rmse = foreach(k = k_grid, .combine='c') %do% {
    knn_model = knnreg(price ~ mileage, data=sclass350_train, k = k, use.all=TRUE)
    modelr::rmse(knn_model, sclass350_test)
  }
  data.frame(k=k_grid, rmse=this_rmse)
}
rmse_out = arrange(rmse_out, k)
ggplot(rmse_out) + 
  geom_boxplot(aes(x=factor(k), y=rmse)) + 
  theme_bw(base_size=10) +
  scale_x_discrete(breaks=c(5,10,15,20,25,30,40,50,80,100)) +
  labs (titles = "RMSE for each value of K - 350")

#  K-nearest-neighbors
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
  scale_colour_manual(name="RMSE",
                      values=c(testset="black", trainset="grey")) + 
  geom_vline(xintercept=k_best, color='darkgreen', size=1) +
  labs (titles = "K-nearest neighbors: test - 350")

# fitted model
knn = knnreg(price ~ mileage, data=sclass350_train, k=k_best)
sclass350 = sclass350 %>%
  mutate(price_pre = predict(knn, sclass350))
g350 = ggplot(data = sclass350) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey')
g350 + geom_line(aes(x = mileage, y = price_pre), color='red', size=1.5) +
  labs (titles = "fitted model - 350")

```

ii. 65 AMG

```{r}
# Split the data into a training and a testing set
sclass65AMG = subset(sclass, trim == '65 AMG')

sclass65AMG_split = initial_split(sclass65AMG, prop=0.9)
sclass65AMG_train = training(sclass65AMG_split)
sclass65AMG_test  = testing(sclass65AMG_split)

# RMSE for each value of K
N65AMG = nrow(sclass65AMG)
N_train65AMG = floor(0.8*N65AMG)
k_grid65AMG = unique(round(exp(seq(log(N_train65AMG), log(2), length=100))))
rmse_out65AMG = foreach(k = k_grid65AMG, .combine='rbind') %dopar% {
  this_rmse = foreach(k = k_grid65AMG, .combine='c') %do% {
    knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = k, use.all=TRUE)
    modelr::rmse(knn_model, sclass65AMG_test)
  }
  data.frame(k=k_grid65AMG, rmse=this_rmse)
}
rmse_out65AMG = arrange(rmse_out65AMG, k)
ggplot(rmse_out65AMG) + 
  geom_boxplot(aes(x=factor(k), y=rmse)) + 
  theme_bw(base_size=8) +
  scale_x_discrete(breaks=c(5,10,15,20,25,30,40,50,80,100)) +
  labs (titles = "RMSE for each value of K - 65AMG")

#  K-nearest-neighbors
rmse_grid_out65AMG = foreach(k = k_grid65AMG, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = k, use.all=TRUE)
  modelr::rmse(knn_model, sclass65AMG_test)
}
rmse_grid_out65AMG = data.frame(K = k_grid65AMG, RMSE = rmse_grid_out65AMG)
p_out = ggplot(data=rmse_grid_out65AMG) + 
  theme_bw(base_size = 10) + 
  geom_path(aes(x=K, y=RMSE, color='testset'), size=0.5)

ind_best65AMG = which.min(rmse_grid_out65AMG$RMSE)
k_best65AMG = k_grid65AMG[ind_best65AMG]

rmse_grid_in2 = foreach(k = k_grid65AMG, .combine='c') %do% {
  knn_model = knnreg(price ~ mileage, data=sclass65AMG_train, k = k, use.all=TRUE)
  modelr::rmse(knn_model, sclass65AMG_train)
}
rmse_grid_in2 = data.frame(K = k_grid65AMG, RMSE = rmse_grid_in2)
p_out + geom_path(data=rmse_grid_in2, aes(x=K, y=RMSE, color='trainset'),size=0.5) +
  scale_colour_manual(name="RMSE",
                      values=c(testset="black", trainset="grey")) + 
  geom_vline(xintercept=k_best65AMG, color='darkgreen', size=1)+
  labs (titles = "K-nearest neighbors: test - 65 AMG")

# fitted model
knn65AMG = knnreg(price ~ mileage, data=sclass65AMG_train, k=k_best65AMG)
sclass65AMG = sclass65AMG %>%
  mutate(price_pre = predict(knn65AMG, sclass65AMG))
g65AMG = ggplot(data = sclass65AMG) + 
  geom_point(mapping = aes(x = mileage, y = price), color='darkgrey')
g65AMG + geom_line(aes(x = mileage, y = price_pre), color='red', size=1.5) +
  labs (titles = "fitted model - 65 AMG")

```

```{r}
k_best
k_best65AMG
dim(sclass350)
dim(sclass65AMG)
```

Trim 350 yields a larger optimal value of K. In the plot of RMSE versus K, trim 350 has the higher K. I reckon that it's due to trim 350 has more number of data than trim 65AMG. If the value of K is small, once there are noise components, they will have a greater impact on the prediction. When the value of K is large, it is equivalent to predicting with data in a larger neighborhood, and the approximate error of learning will increase. Because of dataset "sclass350" has more points , the optimal value of K can be larger in order to reduce the bias.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

