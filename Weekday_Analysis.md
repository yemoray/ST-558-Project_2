Monday Analysis
================
7/4/2021

-   [Packages required to load and analyse the
    data](#packages-required-to-load-and-analyse-the-data)
-   [MON Report](#mon-report)
    -   [Introduction](#introduction)
    -   [Data preparation](#data-preparation)
    -   [Train/Test split](#traintest-split)
    -   [Data Summarizations and
        discussions](#data-summarizations-and-discussions)
        -   [Overall Count](#overall-count)
        -   [Weather Condition](#weather-condition)
        -   [Seasons](#seasons)
        -   [Temperature and Feeling
            Temperature](#temperature-and-feeling-temperature)
        -   [Humidity](#humidity)
        -   [Wind Speed](#wind-speed)
    -   [Modeling](#modeling)
        -   [Linear Regression Model](#linear-regression-model)
        -   [Ensemble Tree Model](#ensemble-tree-model)
        -   [Model Comparison](#model-comparison)

``` r
knitr::opts_chunk$set(fig.path='Figs/')
```

# Packages required to load and analyse the data

The following packages are needed for reading and analyzing the data:

# MON Report

## Introduction

In this project we decided to present an analysis report about bike
sharing data. The data comes from the UCI Machine Learning Repository,
contains the hourly and daily count of rental bikes between years 2011
and 2012 in Capital bikeshare system with the corresponding weather and
seasonal information. 731 days of data were collected.

We would like to explore if the weather and season factors may affect
the number of users. We aim to develop accurate models by both linear
regression and ensemble tree.

The following 13 characteristics were collected for each days along with
the count of casual/registered users.

`Date`

`Season`: Winter, spring, summer and fall.

`Year`: 2011 and 2012.

`Month`

`Hour`

`Holiday`: Weather day is holiday or not.

`Weekday`

`workingday`: if day is neither weekend nor holiday is 1, otherwise is
0.

`Weathersit`: 4 levels from smooth to terrible.

`Temperature`: Normalized.

`Feeling temperature`: Normalized.

`Humidity`: Normalized.

`Windspeed`: Normalized.

## Data preparation

Reading in the data using a relative path

``` r
day_data  <-  read_csv("day.csv")
```

Converting the weekday variable to a factor containing the 7 days of the
week:

``` r
day_as_fac_data <- day_data %>%
    mutate(week_day = wday(weekday +1,label=TRUE,locale="English_United States")) 
day_as_char_data <- day_as_fac_data %>% mutate_if(is.factor, as.character)
```

Converting the year, season, and weather situation variables:

``` r
#categorizing the season variable as factor
day_as_char_data <- mutate(day_as_char_data, Season = 
                  ifelse(season == 1, "Winter",
                  ifelse(season == 2, "Spring",
                  ifelse(season == 3, "Summer",
                  ifelse(season == 4, "Fall", "")))))
day_as_char_data$Season <- factor(day_as_char_data$Season, levels=c("Winter","Spring", "Summer","Fall"))

#categorizing the year variable as factor
day_as_char_data <- mutate(day_as_char_data, Year = 
                  ifelse(yr == 0, "2011",
                  ifelse(yr == 1, "2012", "")))
day_as_char_data$Year <- factor(day_as_char_data$Year, levels=c("2011","2012"))

#categorizing the weathersit variable as factor
day_as_char_data <- mutate(day_as_char_data, weather_situation = 
                  ifelse(weathersit == 1, "Clear Weather",
                  ifelse(weathersit == 2, "Misty/Cloudy Weather",
                  ifelse(weathersit == 3, "Light Snow/Rain/Thunderstorm Weather",
                  ifelse(weathersit == 4, "Heavy Rain/Snow/Fog/Icy Weather", "")))))
day_as_char_data$weather_situation <- factor(day_as_char_data$weather_situation, levels=c("Clear Weather","Misty/Cloudy Weather", "Light Snow/Rain/Thunderstorm Weather","Heavy Rain/Snow/Fog/Icy Weather"))


#reordering the day_data and renaming mth, cnt and hum
day_as_char_data <- day_as_char_data %>% dplyr::select(-c("instant")) %>% rename(month = mnth,count = cnt,humidity = hum)
```

``` r
day_as_char_data$count_cat  <-  ifelse(day_as_char_data$count >= 5000, "High", "Low")
```

Setting up the dataset for later automation. The code will be automated
to analysis data for the day from the day: “weekday” seen in the YAML
header:

``` r
data_weekday <- day_as_char_data %>% 
  filter(day_as_char_data$week_day == params$day)
 data_weekday$count_cat <- as.factor(data_weekday$count_cat)
```

## Train/Test split

Doing a 70:30 split for training/testing:

``` r
set.seed(365)
train  <-  sample(1:nrow(data_weekday), size = 0.7*nrow(data_weekday))
test  <- setdiff(1:nrow(data_weekday), train)
day_data_Train <- data_weekday[train, ]
day_data_Test <- data_weekday[test, ]
```

## Data Summarizations and discussions

### Overall Count

The averages for selected variables for each year is shown below

``` r
grouped_mean  <-  day_data_Train %>%
  group_by(Year) %>%
  summarise(
    n=n(),
    avg_registered_user = round(mean(registered), 0),
    avg_casual_user = round(mean(casual), 0),
    avg_windspeed = round(mean(windspeed*67), 2),
    avg_humidity = round(mean(humidity*100), 2),
  )

kable(grouped_mean, caption="Averages of selected variables for each year")
```

| Year |   n | avg\_registered\_user | avg\_casual\_user | avg\_windspeed | avg\_humidity |
|:-----|----:|----------------------:|------------------:|---------------:|--------------:|
| 2011 |  35 |                  2703 |               519 |          12.52 |         63.69 |
| 2012 |  38 |                  4187 |               702 |          13.52 |         63.73 |

Averages of selected variables for each year

The standard deviations by year for each of the variables above are
summarized below.

``` r
grouped_sd  <-  day_data_Train %>%
  group_by(Year) %>%
  summarise(
    n=n(),
    avg_registered_user = round(sd(registered), 0),
    avg_casual_user = round(sd(casual), 0),
    avg_windspeed = round(sd(windspeed*67), 3),
    avg_humidity = round(sd(humidity*100), 3),
  )
  
kable(grouped_sd, caption="Standard deviation of selected variables for each year")
```

| Year |   n | avg\_registered\_user | avg\_casual\_user | avg\_windspeed | avg\_humidity |
|:-----|----:|----------------------:|------------------:|---------------:|--------------:|
| 2011 |  35 |                  1005 |               352 |          5.912 |        13.925 |
| 2012 |  38 |                  1586 |               531 |          5.620 |        14.234 |

Standard deviation of selected variables for each year

### Weather Condition

The table for weather condition vs. year for this particular day is
shown below:

``` r
year_season_table <- table(day_data_Train$Year, day_data_Train$weather_situation )
kable(year_season_table, caption="Table of Year vs. Weather situation")
```

|      | Clear Weather | Misty/Cloudy Weather | Light Snow/Rain/Thunderstorm Weather | Heavy Rain/Snow/Fog/Icy Weather |
|:-----|--------------:|---------------------:|-------------------------------------:|--------------------------------:|
| 2011 |            24 |                   10 |                                    1 |                               0 |
| 2012 |            21 |                   16 |                                    1 |                               0 |

Table of Year vs. Weather situation

This is visualized below:

``` r
ggplot(data=day_data_Train, aes(x=day_data_Train$Year))+
  geom_bar(aes(fill=as.factor(day_data_Train$weather_situation)))+ 
  labs(x="Year", title="Weather situation by year for this week day")+ 
  scale_fill_discrete(name="Weather situation", labels=c("Clear Weather","Misty/Cloudy Weather", "Light Snow/Rain/Thunderstorm Weather","Heavy Rain/Snow/Fog/Icy Weather")) 
```

![](Figs/unnamed-chunk-10-1.png)<!-- -->

This bar plot shows the weather pattern for this particular day in 2011
and 2012.

We can also see if for a given year, weather condition has an effect on
the number of rentals for this week day

``` r
ggplot(data=day_data_Train, aes(x=weather_situation, y= count))+
  geom_boxplot(aes(fill=weather_situation))+
  labs(x="Weather Situation", y="Total number of rentals", fill="Weather Situation", title = "Boxplot of Total number of rentals by Weather condition for this week day")+  theme(axis.text.x = element_text(size  = 10,angle = 45,hjust = 1,vjust = 1)) 
```

![](Figs/unnamed-chunk-11-1.png)<!-- -->

### Seasons

The number of casual and registered users on this particular day from
2011-2012 are categorized below:

``` r
#categorizing the number casual users as factor
day_data_Train <- mutate(day_data_Train, casual_format = 
                  ifelse(casual %in% 0:1000, "Low number of casual users",
                  ifelse(casual %in% 1000:2000, "Medium number of casual users",
                  ifelse(casual > 2000, "High number of casual users", ""))))
day_data_Train$casual_format <- factor(day_data_Train$casual_format, levels=c("Low number of casual users","Medium number of casual users", "High number of casual users"))

#categorizing the number registered users as factor
day_data_Train <- mutate(day_data_Train, registered_format = 
                  ifelse(registered %in% 0:2000, "Low number of registered users",
                  ifelse(registered %in% 2000:4000, "Medium number of registered users",
                  ifelse(registered > 4000, "High number of registered users", ""))))
day_data_Train$registered_format <- factor(day_data_Train$registered_format, levels=c("Low number of registered users","Medium number of registered users", "High number of registered users"))
```

The number of casual users vs season for 2011-2012 are shown in the
table and bar chart below:

``` r
casual_users_table <- table(day_data_Train$casual_format, day_data_Train$Season )
kable(casual_users_table, caption="Casual users vs Season for 2011-2012")
```

|                               | Winter | Spring | Summer | Fall |
|:------------------------------|-------:|-------:|-------:|-----:|
| Low number of casual users    |     19 |     15 |      8 |   18 |
| Medium number of casual users |      0 |      5 |      6 |    1 |
| High number of casual users   |      0 |      1 |      0 |    0 |

Casual users vs Season for 2011-2012

``` r
ggplot(data=day_data_Train, aes(x=day_data_Train$Season))+
  geom_bar(aes(fill=as.factor(day_data_Train$casual_format)))+ 
  labs(x="Season", title="Casual users vs Season for this week day from 2011-2012")+ 
  scale_fill_discrete(name="Casual Users", labels=c("Low number of casual users","Medium number of casual users", "High number of casual users")) + 
  coord_flip()
```

![](Figs/unnamed-chunk-13-1.png)<!-- -->

The same information for number of registered users is shown in the
table and bar chart below:

``` r
registered_users_table <- table(day_data_Train$registered_format, day_data_Train$Season )
kable(registered_users_table, caption="Registered users vs Season for 2011-2012")
```

|                                   | Winter | Spring | Summer | Fall |
|:----------------------------------|-------:|-------:|-------:|-----:|
| Low number of registered users    |     11 |      1 |      0 |    1 |
| Medium number of registered users |      7 |     13 |      5 |    9 |
| High number of registered users   |      1 |      7 |      9 |    9 |

Registered users vs Season for 2011-2012

``` r
ggplot(data=day_data_Train, aes(x=day_data_Train$Season))+
  geom_bar(aes(fill=as.factor(day_data_Train$registered_format)))+ 
  labs(x="Season", title="Registered users vs Season for this week day from 2011-2012")+ 
  scale_fill_discrete(name="Registered Users", labels=c("Low number of registered users","Medium number of registered users", "High number of registered users")) + 
  coord_flip()
```

![](Figs/unnamed-chunk-14-1.png)<!-- -->

We can inspect the trend of casual and registered users across seasons
to see if there is a seasonal effect present.

Next, it would be nice to see how total bike rentals vary by season and
weather situation for this particular day.

``` r
ggplot(data=day_data_Train, aes(x=Season, y= count))+
  geom_boxplot(aes(fill=Season))+
  labs(x="Season", y="Total number of rentals", fill="Season", title = "Boxplot of Total number of rentals by Season for this week day")
```

![](Figs/unnamed-chunk-15-1.png)<!-- -->

This plot will let us know if there is a seasonal effect on number of
rentals for this particular week day.

### Temperature and Feeling Temperature

We are curious about how difference is between temperature and feeling
temperature, so we decided to make a comparison line plot.

``` r
tem_plot <- day_data_Train %>%
  dplyr::select(dteday, temp, atemp, Year, count) %>%
  gather('temp','value',2:3)

ggplot(tem_plot) + geom_line(aes(x=dteday,y=value,color=temp)) + facet_wrap(~Year, scales = 'free') + theme_bw() + scale_colour_discrete(name = '', labels=c('temp','feeling temp')) + labs(x='Date', y='Temp', title = 'Temperature vs. feeling temperature')
```

![](Figs/unnamed-chunk-16-1.png)<!-- -->

There’s no significant difference between these 2 variables from the
first look, so we decided to explore them deeply. First we want to
output the numeric summary of `temperature` and `feeling temperature`.

``` r
temp_out <- day_data_Train %>% 
  dplyr::select(temp,atemp)
apply(temp_out,2, summary) %>%
  kable(caption = 'Temperature vs Feeling Temperature', digits = 2, col.names = c('Temperature','Feeling Temperature'))
```

|         | Temperature | Feeling Temperature |
|:--------|------------:|--------------------:|
| Min.    |        0.10 |                0.12 |
| 1st Qu. |        0.30 |                0.28 |
| Median  |        0.45 |                0.46 |
| Mean    |        0.46 |                0.45 |
| 3rd Qu. |        0.64 |                0.60 |
| Max.    |        0.78 |                0.72 |

Temperature vs Feeling Temperature

From the numeric summary, we noticed that the variance of `temperature`
is a little bit wider than `feeling temperature`. It looks like these 2
variables do distributed pretty close, but in order to get statistical
support, we still need a 2 sample t-test.

``` r
T_test <- t.test(day_data_Train$temp, day_data_Train$atemp, alternative = 'two.sided')
T_test
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  day_data_Train$temp and day_data_Train$atemp
    ## t = 0.57364, df = 142.1, p-value = 0.5671
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.04149768  0.07542758
    ## sample estimates:
    ## mean of x mean of y 
    ##  0.464701  0.447736

The p-value is about 0.5671206, which is pretty large, so we don’t have
enough evidence to reject the null hypothesis.

We decided to drop one of them due to the consideration of collinearity.
Since the variance of `temperature` is a little bit larger than
`feeling temperature`, we may expect `temperature` to be more sensitive
when fitting the model, so we decided to drop `feeling temperature` in
some of our models.

### Humidity

We would like to explore the general relationship between humidity and
users count.

``` r
hum_plot <- day_data_Train %>%
  dplyr::select(humidity, casual, registered, Year) %>%
  gather('regist', 'count', 2:3)

ggplot(data = hum_plot, aes(x=humidity, y=count, group=regist)) + geom_point(aes(color=regist)) + geom_smooth(aes(group=regist), color='black') + facet_wrap(~Year, scales = 'free') + theme_bw() + scale_colour_discrete(name = '') + labs(title = 'Relationship between Humidity and User Count')
```

![](Figs/unnamed-chunk-19-1.png)<!-- -->

We can inspect the relationship of users with humidity using this plot
roughly. The points are nearly evenly distributed, which means humidity
can seldom affect the users if we control all other variables.

We added a categorical variable for original data set with 2 levels, one
level is for those whose daily total count over than 5000; and another
one is for smaller than 5000. We wish to see the numeric summary of
humidity among these 2 groups.

``` r
temp_out <- day_data_Train %>% 
  dplyr::select(humidity, count_cat)

index <- temp_out$count_cat=='Low'

output <- data.frame(cbind(summary(temp_out$humidity[index]),summary(temp_out$humidity[-index])))

output %>%
  kable(caption = 'Numeric Summary for Humidity vs Count', digits = 2, col.names = c('Less than 5000','Greater than 5000'))
```

|         | Less than 5000 | Greater than 5000 |
|:--------|---------------:|------------------:|
| Min.    |           0.30 |              0.30 |
| 1st Qu. |           0.52 |              0.53 |
| Median  |           0.65 |              0.66 |
| Mean    |           0.64 |              0.64 |
| 3rd Qu. |           0.76 |              0.75 |
| Max.    |           0.91 |              0.91 |

Numeric Summary for Humidity vs Count

### Wind Speed

``` r
ggplot(data = day_data_Train, aes(x=windspeed, y=..density..)) + geom_histogram(bins = 20) + geom_density(size=2.5,color='red',adjust=1/3) + facet_wrap(~Year, scales = 'free') + theme_bw()
```

![](Figs/unnamed-chunk-21-1.png)<!-- -->

This plot shows the distribution of windspeed. Although the number of
samples is small, it still shows a certain degree of normality.

Same as humidity, we are curious about the relationship between
windspeed and user count after controlled all other variables.

``` r
wind_plot <- day_data_Train %>%
  dplyr::select(windspeed, casual, registered, Year) %>%
  gather('regist', 'count', 2:3)

ggplot(data = wind_plot, aes(x=windspeed, y=count, group=regist)) + geom_point(aes(color=regist)) + geom_smooth(aes(group=regist), color='black') + facet_wrap(~Year, scales = 'free') + theme_bw() + scale_colour_discrete(name = '') + labs(title = 'Relationship between Windspeed and User Count')
```

![](Figs/unnamed-chunk-22-1.png)<!-- -->

We can inspect the relationship of users with windspeed using this plot
roughly.

## Modeling

### Linear Regression Model

Linear regression modeling is one of the supervised learning methods
where the output is known, and the goal is to establish a function that
best approximates the relationship between desired outputs and the
provided sample data. Specifically, linear regression accomplishes this
by learning a model that best fits the linear relationship between the
predictor and response variables.The model is fit by minimizing the sum
of squared residuals (difference between the observed and predicted
responses). Linear regression models fall into two categories: Simple
Linear Regression and Multiple Linear Regression.

Simple Linear Regression Model is one where there is only one
independent variable (or predictor) and the goal is to learn the linear
relationship between it and the response variable. Multiple Linear
Regression (MLR) has more than one predictor, and the although
relationship between predictors and response remains linear in terms of
the model parameters, the MLR model could contain interaction, quadratic
and polynomial terms.

Underlying assumptions for the linear regression model are  
\* Linearity: The model is linear in model parameters (betas) (Can be
checked using histogram or Q-Q plots)  
\* Normality: The predictor and response variables are multivariate
normal  
\* Multicollinearity: There is little to no multicollinearity among the
predictor variables. (can be checked using Variance Inflation Factor)  
\* Homoscedasticity: Residuals are randomly distributed across the
regression line (Can be checked using the Residual vs. Fitted value
scatter plot. The plot must have to discernable pattern)  
\* Autocorrelation: Residuals must be independent of each other (Can be
checked using Durbin-Watson’s test).

#### First linear regression Model

##### Subsetting the predictors that should best predict the total count of users

To select the candidate models, I first subset the dataset to include
only variables of interest for MLR.

``` r
day_data_Train_sub <- day_data_Train %>% dplyr::select(-c(dteday,weekday,registered,casual,week_day,Season,Year,weather_situation,casual_format,registered_format,count_cat,atemp,workingday))

day_data_Test_sub <- day_data_Test %>% dplyr::select(-c(dteday,weekday,registered,casual,week_day,Season,Year,weather_situation,count_cat,atemp,workingday))   #The test data is subsetted as well
```

The first linear regression model contributed is:

``` r
Lin_reg_model <- lm(count ~ ., data = day_data_Train_sub) 
```

This code checks if the linear regression model satisfies the
requirements:

``` r
par(mfrow = c(2, 2))
plot(Lin_reg_model)
```

![](Figs/unnamed-chunk-25-1.png)<!-- -->

Training the linear regression model using the training data:

``` r
trctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)
Lin_reg_1_train <- train(count ~ ., data = day_data_Train_sub, 
                 method='lm',
                 trControl=trctrl,
                 preProcess=c("center", "scale"))
```

``` r
#Predict the response variable using the test data to evaluate model performance
Lin_reg_1_predict <- predict(Lin_reg_1_train, newdata = day_data_Test_sub)

#The rmse will be shown in the model comparison table below
```

#### Second linear regression Model (Jiashu, please add your linear model here)

Then we decided to use the best subset method to select the variables.

``` r
log_full <- glm(count ~ ., data = day_data_Train_sub)
log_step <- log_full %>% stepAIC(trace = F)

coef(log_step)
```

    ## (Intercept)      season          yr  weathersit        temp    humidity 
    ##   2313.7220    490.7763   1713.5816   -418.0680   5586.7481  -2528.2935 
    ##   windspeed 
    ##  -3670.2917

Above shows the coefficients of the logistic regression selected by best
subset method with 6 variables.

``` r
#Predict the response variable using the test data to evaluate model performance
Lin_reg_2_predict <- predict(log_step, newdata = day_data_Test_sub)

#Get the missclassification rate 
Lin_reg_2_predict_tbl <- table(Lin_reg_2_predict, day_data_Test_sub$count)
Lin_reg_2_misclass <- 1-(sum(diag(Lin_reg_2_predict_tbl))/sum(Lin_reg_2_predict_tbl))
```

### Ensemble Tree Model

#### First Ensemble Tree Model: Random Forest Model

I’ll be using a Random Forest as my ensemble method. It works by
creating a number of decision trees from bootstrap samples using the
training data set, with no interaction between the trees, and aggregates
the result from these trees before outputting the most optimal result.
*mtry* is used as the tuning parameter, this is the number of variables
randomly sampled at each split. The number of variables to use in the
model is selected by 10-fold repeated cross validation from the `caret`
package using accuracy as the metric.

``` r
trctrl <- trainControl(method = "repeatedcv", number=10, repeats=3)
rf_grid <- expand.grid(mtry = 1:11)
rf_train <- train(count ~., 
                 data= day_data_Train_sub, 
                 method='rf', 
                 trControl=trctrl,
                 tuneGrid = rf_grid,
                 preProcess=c("center", "scale"))


#Predict the response variable using the test data to evaluate model performance
rf_predict <- predict(rf_train, newdata = day_data_Test_sub)

#The rmse will be shown in the model comparison table below
```

#### Second Ensemble Tree Model: Boosted Tree Model (Jiashu, please put your boosted tree model here)

### Model Comparison

The four models (2 linear regression models, one random forest model and
one boosted tree model) are compared below.The RMSE is used as the basis
for comparison

``` r
library(ModelMetrics)

print("lm rmse:")
```

    ## [1] "lm rmse:"

``` r
print(rmse(predict(Lin_reg_1_train, day_data_Test_sub), day_data_Test_sub$count))
```

    ## [1] 680.4003

``` r
print("rf rmse:")
```

    ## [1] "rf rmse:"

``` r
print(rmse(predict(rf_train$finalModel, day_data_Test_sub), day_data_Test_sub$count))
```

    ## [1] 1766.071

The model with the lowest `rmse` value is the winner.
