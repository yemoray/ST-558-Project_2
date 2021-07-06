Monday Analysis
================
7/4/2021

  - [Packages required to load and analyse the
    data](#packages-required-to-load-and-analyse-the-data)
  - [MON Report](#mon-report)
      - [Introduction](#introduction)
      - [Data preparation](#data-preparation)
      - [Train/Test split](#traintest-split)
      - [Data Summarizations and
        discussions](#data-summarizations-and-discussions)
          - [Overall Count](#overall-count)
          - [Weather Condition](#weather-condition)
          - [Seasons](#seasons)
          - [Temperature and Feeling
            Temperature](#temperature-and-feeling-temperature)
          - [Humidity](#humidity)
          - [Wind Speed](#wind-speed)

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

Converting the year, season, and weather situation variables:

``` r
#categorizing the season variable as factor
day_data <- mutate(day_data, Season = 
                  ifelse(season == 1, "Winter",
                  ifelse(season == 2, "Spring",
                  ifelse(season == 3, "Summer",
                  ifelse(season == 4, "Fall", "")))))
day_data$Season <- factor(day_data$Season, levels=c("Winter","Spring", "Summer","Fall"))

#categorizing the year variable as factor
day_data <- mutate(day_data, Year = 
                  ifelse(yr == 0, "2011",
                  ifelse(yr == 1, "2012", "")))
day_data$Year <- factor(day_data$Year, levels=c("2011","2012"))

#categorizing the weathersit variable as factor
day_data <- mutate(day_data, weather_situation = 
                  ifelse(weathersit == 1, "Clear Weather",
                  ifelse(weathersit == 2, "Misty/Cloudy Weather",
                  ifelse(weathersit == 3, "Light Snow/Rain/Thunderstorm Weather",
                  ifelse(weathersit == 4, "Heavy Rain/Snow/Fog/Icy Weather", "")))))
day_data$weather_situation <- factor(day_data$weather_situation, levels=c("Clear Weather","Misty/Cloudy Weather", "Light Snow/Rain/Thunderstorm Weather","Heavy Rain/Snow/Fog/Icy Weather"))

#reordering the day_data and renaming mth, cnt and hum
day_data <- day_data %>% select(-c("instant","season","yr", "weathersit")) %>% rename(month = mnth,count = cnt,humidity = hum)
```

Converting the weekday variable to a factor containing the 7 days of the
week:

``` r
day_as_fac_data <- day_data %>%
    mutate(week_day = wday(weekday +1,label=TRUE,locale="English_United States")) 
day_as_char_data <- day_as_fac_data %>% mutate_if(is.factor, as.character)
```

Setting up the dataset for later automation. The code will be automated
to analysis data for the day from the day: “weekday” seen in the YAML
header:

``` r
data_weekday <- day_as_char_data %>% 
  filter(day_as_char_data$week_day == params$day)
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

| Year |  n | avg\_registered\_user | avg\_casual\_user | avg\_windspeed | avg\_humidity |
| :--- | -: | --------------------: | ----------------: | -------------: | ------------: |
| 2011 | 35 |                  2703 |               519 |          12.52 |         63.69 |
| 2012 | 38 |                  4187 |               702 |          13.52 |         63.73 |

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

| Year |  n | avg\_registered\_user | avg\_casual\_user | avg\_windspeed | avg\_humidity |
| :--- | -: | --------------------: | ----------------: | -------------: | ------------: |
| 2011 | 35 |                  1005 |               352 |          5.912 |        13.925 |
| 2012 | 38 |                  1586 |               531 |          5.620 |        14.234 |

Standard deviation of selected variables for each year

### Weather Condition

The table for weather condition vs. year for this particular day is
shown
below:

``` r
year_season_table <- table(day_data_Train$Year, day_data_Train$weather_situation )
kable(year_season_table, caption="Table of Year vs. Weather situation")
```

|      | Clear Weather | Light Snow/Rain/Thunderstorm Weather | Misty/Cloudy Weather |
| ---- | ------------: | -----------------------------------: | -------------------: |
| 2011 |            24 |                                    1 |                   10 |
| 2012 |            21 |                                    1 |                   16 |

Table of Year vs. Weather situation

This is visualized below:

``` r
ggplot(data=day_data_Train, aes(x=day_data_Train$Year))+
  geom_bar(aes(fill=as.factor(day_data_Train$weather_situation)))+ 
  labs(x="Year", title="Weather situation by year for this week day")+ 
  scale_fill_discrete(name="Weather situation", labels=c("Clear Weather","Misty/Cloudy Weather", "Light Snow/Rain/Thunderstorm Weather","Heavy Rain/Snow/Fog/Icy Weather")) 
```

![](Figs/unnamed-chunk-9-1.png)<!-- -->

This bar plot shows the weather pattern for this particular day in 2011
and 2012.

We can also see if for a given year, weather condition has an effect on
the number of rentals for this week day

``` r
ggplot(data=day_data_Train, aes(x=weather_situation, y= count))+
  geom_boxplot(aes(fill=weather_situation))+
  labs(x="Weather Situation", y="Total number of rentals", fill="Weather Situation", title = "Boxplot of Total number of rentals by Weather condition for this week day")+  theme(axis.text.x = element_text(size  = 10,angle = 45,hjust = 1,vjust = 1)) 
```

![](Figs/unnamed-chunk-10-1.png)<!-- -->

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
table and bar chart
below:

``` r
casual_users_table <- table(day_data_Train$casual_format, day_data_Train$Season )
kable(casual_users_table, caption="Casual users vs Season for 2011-2012")
```

|                               | Fall | Spring | Summer | Winter |
| ----------------------------- | ---: | -----: | -----: | -----: |
| Low number of casual users    |   18 |     15 |      8 |     19 |
| Medium number of casual users |    1 |      5 |      6 |      0 |
| High number of casual users   |    0 |      1 |      0 |      0 |

Casual users vs Season for 2011-2012

``` r
ggplot(data=day_data_Train, aes(x=day_data_Train$Season))+
  geom_bar(aes(fill=as.factor(day_data_Train$casual_format)))+ 
  labs(x="Season", title="Casual users vs Season for this week day from 2011-2012")+ 
  scale_fill_discrete(name="Casual Users", labels=c("Low number of casual users","Medium number of casual users", "High number of casual users")) + 
  coord_flip()
```

![](Figs/unnamed-chunk-12-1.png)<!-- -->

The same information for number of registered users is shown in the
table and bar chart
below:

``` r
registered_users_table <- table(day_data_Train$registered_format, day_data_Train$Season )
kable(registered_users_table, caption="Registered users vs Season for 2011-2012")
```

|                                   | Fall | Spring | Summer | Winter |
| --------------------------------- | ---: | -----: | -----: | -----: |
| Low number of registered users    |    1 |      1 |      0 |     11 |
| Medium number of registered users |    9 |     13 |      5 |      7 |
| High number of registered users   |    9 |      7 |      9 |      1 |

Registered users vs Season for 2011-2012

``` r
ggplot(data=day_data_Train, aes(x=day_data_Train$Season))+
  geom_bar(aes(fill=as.factor(day_data_Train$registered_format)))+ 
  labs(x="Season", title="Registered users vs Season for this week day from 2011-2012")+ 
  scale_fill_discrete(name="Registered Users", labels=c("Low number of registered users","Medium number of registered users", "High number of registered users")) + 
  coord_flip()
```

![](Figs/unnamed-chunk-13-1.png)<!-- -->

We can inspect the trend of casual and registered users across seasons
to see if there is a seasonal effect present.

Next, it would be nice to see how total bike rentals vary by season and
weather situation for this particular day.

``` r
ggplot(data=day_data_Train, aes(x=Season, y= count))+
  geom_boxplot(aes(fill=Season))+
  labs(x="Season", y="Total number of rentals", fill="Season", title = "Boxplot of Total number of rentals by Season for this week day")
```

![](Figs/unnamed-chunk-14-1.png)<!-- --> This plot will let us know if
there is a seasonal effect on number of rentals for this particular week
day.

### Temperature and Feeling Temperature

We are curious about how difference is between temperature and feeling
temperature, so we decided to make a comparison line plot.

``` r
tem_plot <- day_data_Train %>%
  select(dteday, temp, atemp, Year, count) %>%
  gather('temp','value',2:3)

ggplot(tem_plot) + geom_line(aes(x=dteday,y=value,color=temp)) + facet_wrap(~Year, scales = 'free') + theme_bw() + scale_colour_discrete(name = '', labels=c('temp','feeling temp')) + labs(x='Date', y='Temp', title = 'Temperature vs. feeling temperature')
```

![](Figs/unnamed-chunk-15-1.png)<!-- -->

Basically, When the ture temperature is high, the body temperature will
be lower; when the actual temperature is very low, the opposite is true.

There’s no difference between these 2 variables, so we decided to drop
`feeling temperature` due to the consideration of collinearity. Besides,
the variance of ture `temperature` is a little bit larger than `feeling
temperature`, so we expect `temperature` to be more sensitive when
fitting the model.

### Humidity

We would like to explore the general relationship between humidity and
users count.

``` r
hum_plot <- day_data_Train %>%
  select(humidity, casual, registered, Year) %>%
  gather('regist', 'count', 2:3)

ggplot(data = hum_plot, aes(x=humidity, y=count, group=regist)) + geom_point(aes(color=regist)) + geom_smooth(aes(group=regist), color='black') + facet_wrap(~Year, scales = 'free') + theme_bw() + scale_colour_discrete(name = '') + labs(title = 'Relationship between Humidity and User Count')
```

![](Figs/unnamed-chunk-16-1.png)<!-- -->

We can inspect the relationship of users with humidity using this plot
roughly. The points are nearly evenly distributed, which means humidity
can seldom affect the users if we control all other
variables.

### Wind Speed

``` r
ggplot(data = day_data_Train, aes(x=windspeed, y=..density..)) + geom_histogram(bins = 20) + geom_density(size=2.5,color='red',adjust=1/3) + facet_wrap(~Year, scales = 'free') + theme_bw()
```

![](Figs/unnamed-chunk-17-1.png)<!-- -->

This plot shows the distribution of windspeed. Although the number of
samples is small, it still shows a certain degree of normality.

Same as humidity, we are curious about the relationship between
windspeed and user count after controlled all other variables.

``` r
wind_plot <- day_data_Train %>%
  select(windspeed, casual, registered, Year) %>%
  gather('regist', 'count', 2:3)

ggplot(data = wind_plot, aes(x=windspeed, y=count, group=regist)) + geom_point(aes(color=regist)) + geom_smooth(aes(group=regist), color='black') + facet_wrap(~Year, scales = 'free') + theme_bw() + scale_colour_discrete(name = '') + labs(title = 'Relationship between Windspeed and User Count')
```

![](Figs/unnamed-chunk-18-1.png)<!-- -->

We can inspect the relationship of users with windspeed using this plot
roughly.