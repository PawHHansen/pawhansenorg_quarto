---
title: "Dot-dot-dot: Three dotplots to stake your life on"
author: ''
format: hugo-md
date: '2023-06-09'
slug: []
categories:
  - statistical analysis
tags: []
subtitle: 'Celebrating the beauty of dots and points in plotting data.'
excerpt: 'Celebrating the beauty of dots and points in plotting data.'
draft: no
series: null
layout: single
editor_options: 
  chunk_output_type: console
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />


```r
library(tidyverse)
library(broom)
library(modelsummary)
library(ggsci)
library(gapminder)

theme_set(theme_minimal())
data("gapminder")
```


Here is a quick post on a few basic plots you can make with `geom_point`. The goal is simply to celebrate _the dot_ as a way of graphing data. Who knows? Maybe someday there will be a post on the line too! For all plots, we will be working with the `gapminder` data. 

### Plot 1: The Cleveland dotplot 

To get started, let's make a classic Cleveland dot plot. These are good for providing at quick glance of summary statistics, here the average life expectancy for each continent:  



```r
gapminder %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp, na.rm = T)) %>%
  mutate(continent = fct_reorder(continent, mean)) %>%
  ggplot(aes(mean, continent)) + 
  geom_point(size = 4) +
  labs(x = "Years",
       y = NULL,
       caption = "Source: gapminder")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/fig-life-exp-1.png" alt="Mean life expectancy across continents" width="672" />
<p class="caption"><span id="fig:fig-life-exp"></span>Figure 1: Mean life expectancy across continents</p>
</div>


A small tweak could be to highlight one of the continents. Say your analysis was about explaining what's going on with one particular observation. For example, when looking at the above plot, Africa, seems to be falling behind the other continents. An easy way to highlight that would be using color: 



```r
gapminder %>%
  group_by(continent) %>%
  summarise(mean = mean(lifeExp, na.rm = T)) %>%
  mutate(continent = fct_reorder(continent, mean)) %>%
  ggplot(aes(mean, continent)) + 
  geom_point(aes(color = I(ifelse(continent == "Africa", 
                                  "firebrick", 
                                  "black"))), 
             size = 4) +
  labs(x = "Years",
       y = NULL, 
       caption = "Source: gapminder")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/fig-africa-1.png" alt="What's the matter with Africa? Mean life expectancy across continents" width="672" />
<p class="caption"><span id="fig:fig-africa"></span>Figure 2: What's the matter with Africa? Mean life expectancy across continents</p>
</div>


This kind of graph is en easy but powerfull way to spark initial curiosity with readers, which is a key part of [storytelling with data](https://clauswilke.com/dataviz/telling-a-story.html)


### Plot 2: The dot-and-whisker
Next up, we have the dot-and-whisker plot. Often, researchers and data analyst will report model estimates using tables. Some scholars, teasingly, call these 'BUTONs' Big Ugly Table of Numbers. They're probably right using that language. 

A simple way to improve presentation is a dot-and-whisker plot of the model estimates and corresponding uncertainties. Let's fit a model and make a plot:



```r
# Rescale population and GDP per capita to more meaningfull scale. 
gapminder <- 
  gapminder %>% 
  mutate(population_scaled = pop/100000000,
         gdpPercap_scaled = gdpPercap/10000)

# The fit model
gap_model <- 
  lm(lifeExp ~ continent + year +  population_scaled + gdpPercap_scaled, data = gapminder)
```


```r
gap_model %>%
  tidy(conf.int = T) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_replace_all(term, 
                                "continent", 
                                "Continent: "),
         term = str_to_title(term),
         term = fct_reorder(term, 
                            estimate)) %>%
  ggplot(aes(estimate, 
             term, 
             xmin = conf.low, 
             xmax = conf.high)) + 
  geom_vline(xintercept = 0, 
             linetype = "dashed",
             color = "grey") + 
  geom_pointrange() + 
  labs(y = NULL, 
       x = "Difference in years")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/fig-life-exp-world-1.png" alt="Explaining life expectancy across the world" width="672" />
<p class="caption"><span id="fig:fig-life-exp-world"></span>Figure 3: Explaining life expectancy across the world</p>
</div>


For reference, this is what that would have looked like had we used a standard regression table:



```r
modelsummary(models = list("Life expectancy" = gap_model), 
             fmt = fmt_significant(2),
             stars = T,
             title = "Explaining life expectancy",
             gof_map = c("nobs", "rmse","r.squared"),
             output = "kableExtra")
```

<table style="NAborder-bottom: 0; width: auto !important; margin-left: auto; margin-right: auto;" class="table">
<caption><span id="tab:tbl-modelsummarylifeexp"></span>Table 1: Explaining life expectancy</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:center;"> Life expectancy </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:center;"> −518*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (20) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> continentAmericas </td>
   <td style="text-align:center;"> 14.29*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.49) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> continentAsia </td>
   <td style="text-align:center;"> 9.38*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.47) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> continentEurope </td>
   <td style="text-align:center;"> 19.36*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.52) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> continentOceania </td>
   <td style="text-align:center;"> 20.6*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (1.5) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> year </td>
   <td style="text-align:center;"> 0.29*** </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.01) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> population_scaled </td>
   <td style="text-align:center;"> 0.18 </td>
  </tr>
  <tr>
   <td style="text-align:left;">  </td>
   <td style="text-align:center;"> (0.16) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> gdpPercap_scaled </td>
   <td style="text-align:center;"> 3.0*** </td>
  </tr>
  <tr>
   <td style="text-align:left;box-shadow: 0px 1.5px">  </td>
   <td style="text-align:center;box-shadow: 0px 1.5px"> (0.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Num.Obs. </td>
   <td style="text-align:center;"> 1704 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RMSE </td>
   <td style="text-align:center;"> 6.87 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> R2 </td>
   <td style="text-align:center;"> 0.717 </td>
  </tr>
</tbody>
<tfoot><tr><td style="padding: 0; " colspan="100%">
<sup></sup> + p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</td></tr></tfoot>
</table>


The dot-and-whisker plot helps readers quickly compare the model estimates and their corresponding uncertainty. 

### Plot 3: The dumbell chart

Finally, the _dumbell chart_ can be useful when the goal is to highlight differences, especially across groups. Let's make one to compare the change in mean life expectancy from the 19050s to the 2000s across all five continents:   



```r
gapminder |> 
  mutate(decade = factor(year %/% 10 * 10)) |> 
  filter(decade %in% c(1950, 2000)) |> 
  group_by(continent, decade) |> 
  summarize(lifeexp = mean(lifeExp, na.rm = T)) |> 
  mutate(continent = fct_reorder(continent, lifeexp)) |> 
  ggplot(aes(lifeexp, continent, 
             group = decade)) + 
  geom_line(aes(group = continent), 
            color = "grey50", 
            linewidth = 1) + 
  geom_point(aes(color = continent), 
             size = 5) + 
  scale_color_aaas() + 
  labs(x = "Life expectancy (years)",
       y = NULL,
       caption ="Source: gapminder") + 
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/fig-dumbell-1.png" alt="Mean life expectancy in 1950s and 2000s" width="672" />
<p class="caption"><span id="fig:fig-dumbell"></span>Figure 4: Mean life expectancy in 1950s and 2000s</p>
</div>


Thus, Oceania has the highest average but Asia sees the highest growth. 

### Final thoughts: Three dotplots to stake your life on

Points or dots can be a useful way of plotting data. In this post, I have shown you three different and quite versatile plots you can use in your next analysis. 

### Cool! Where can I learn more?

* Healy, Kieran. _Data visualization: a practical introduction_. Princeton University Press, 2018.




