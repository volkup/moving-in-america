---
title: "Number of Movers in the United States"
output: html_document
date: "2023-02-13"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Estimating the number of people that move in the United States

Using data from Annual Social and Economic Supplement to the Current Population Survey, as provided by IPUMS, let's estimate the number of people in the United States who move residence for each of the last twenty-odd years.

```{r cars}
#load packages
library(tidyverse)
library(ipumsr) #this package is specifically for working with IPUMS / CPS data

#read in the extract of the Current Population Survey, downloaded from IPUMS
moving_data <- read_ipums_micro(data_file = 'cps_00035.csv.gz',
                           ddi = 'cps_00035.xml')

#calculate how many people move (= change residence) 
#in the United States in the last 20 years

#get move counts and percentages of moves by year  
moves_over_time <- moving_data %>% 
  group_by(YEAR) %>% #group by year
  summarise(movers_count = sum(ASECWT[MIGRATE1 > 1 & MIGRATE1 < 6]), #calculate number movers using the individual weight variable 
            movers_pct = sum(ASECWT[MIGRATE1 > 1 & MIGRATE1 < 6]) / sum(ASECWT)) %>% #calculate % of movers using the individual weight variable 
  view() #quick preview to check the final table
```

## Let's plot the percentage of movers

Here's the code for an area graph plot:

```{r pressure, echo=FALSE}
# plot the data
ggplot(moves_over_time, aes(x=YEAR, y=movers_pct)) +
geom_area(fill = "#83C865") +
  labs(y = "% of population who moved in a given year",  x = "(Source: U.S. Census Bureau)",
       title = "Two Decades of Moving in America",
       subtitle = "For the first time in a decade, the share of Americans who moved grew year-over-year in 2022",
       colour = "#2A3B52") +
  scale_y_continuous(expand = expansion(mult =0),breaks = c(0,0.05,0.1,0.15),limits = c(0, 0.15),labels = scales::percent) +
  scale_x_continuous(expand = expansion(mult =0),breaks = c(2005,2010,2015,2020),limits = c(2001, 2022)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(face="bold", color="#2A3B52"),
        axis.text.y = element_text(size=8, color="#2A3B52"),
        axis.title.x = element_text(size=11, color="#2A3B52", margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.line.x.bottom = element_line(color = "#2A3B52", size = 0.6),
        axis.line.y.left = element_line(color = "#2A3B52", size = 0.6),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(size = 15, hjust = 0, margin = margin(t=10,r=0,b=10,l=0), face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t=1,r=0,b=10,l=0)),
        text = element_text(family = "Roboto Condensed", color = "#2A3B52"),
        plot.margin=unit(c(0.2,0.5,0.5,0.3),"cm"))
```
