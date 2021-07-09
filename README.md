ST 558 Project 2
================
Adeyemi Adepetun/Jiashu Zhao  

7/4/2021

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(dplyr)
library(knitr)
```

## Purpose of this repository
The goal here is to read in the [bike sharing data set](https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset) , perform some elementary data analysis, create and compare predictive models and automate Markdown reports for different days using the day.csv data.

The packages required to run the analysis are `readr`,`ggplot2`, `dplyr`, `knitr`,`caret`,`tidyr`,`lubridate`,`leaps`,`regclass`, and `MASS`.


## Code needed to automate the Markdown reports

```{r}
week_days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
out_file <- paste0(week_days, ".md")
params <- lapply(week_days, FUN=function(x){
  return(list(day=x))
})
reports <- tibble(week_days, out_file, params)
reports
```


## Render function needed to generate the reports
```{r, eval=F}
library(rmarkdown)
apply(reports, MARGIN = 1, FUN=function(x){
  render(input = "Weekday_Analysis.Rmd", output_file = x[[2]], params=x[[3]])
})
```


Reports that are generated for each day:   

* The analysis for [Monday is available here](Weekday_Analysis.md).    
* The analysis for [Tuesday is available here](tuesday.md).    
* The analysis for [Wednesday is available here](wednesday.md).    
* The analysis for [Thursday is available here](thursday.md).    
* The analysis for [Friday is available here](friday.md).    
* The analysis for [Saturday is available here](saturday.md).    
* The analysis for [Sunday is available here](sunday.md).    
