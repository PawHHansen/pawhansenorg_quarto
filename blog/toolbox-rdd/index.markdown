---
title: "The policy evaluator's toolbox: Plotting your next regression discontinuity
  analysis"
author: ''
date: '2023-06-01'
slug: []
categories:
  - statistical analysis
tags: []
subtitle: 'When evaluating public policies, regression discontinuity designs are impossible to live without. Here is how to do one.'
excerpt: 'When evaluating public policies, regression discontinuity designs are impossible to live without. Here is how to do one.'
draft: no
series: ~
layout: single
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



In the previous post of my “policy evaluator’s toolbox” series, I showed you two basic ways of plotting results from randomized experiments. A randomized experiment should be the design you are going for whenever possible since it allows for easy estimation and clean causal inference.

In practice, though, randomized experiments may not be feasible. Often policymakers are unwilling to assign policies at random, so the question becomes what we can do in those cases if we still want to evaluate if a policy had an effect.

Today, we’ll have a look at one widely-used approach: the **regression discontinuity design**. Here is the definition I got when asking ChatGPT:

> “A regression discontinuity design (RDD) is a quasi-experimental research design used to estimate causal effects by taking advantage of a discontinuity in the treatment assignment process. The basic logic behind RDD is that when a certain threshold or cutoff point is used to determine whether an individual receives a treatment or not, individuals on either side of the cutoff are expected to be similar in all other relevant aspects, except for the treatment itself. By comparing the outcomes of individuals just above and just below the cutoff, researchers can attribute any differences to the treatment rather than other confounding factors.”

This is not a bad definition but I do want to emphasize one core assumptions: the assignment variable should be _continuous_ or "smooth" around the cutoff. 

In my field of public welfare, many such cutoff variables exist:

* *Age*. For example, everybody younger than 21 years old might receive free dental care
* *Income*. For example, everybody earning above some level might lose eligibility for housing benefits
* *Time*. For example, a new tutoring program may be implemented on January 1st making it possible to estimate its effects by comparing units just before and just after the cutoff date 

Finally, a popular threshold is *test scores*, which we will work with today. Specifically, we’ll use the `Chile Schools` data taken from (the highly recommendable) _Regression and Other Stories_. Let’s get our packages and load up the data:


```r
library(tidyverse)
library(broom)
library(ggsci)
library(readr)
library(kableExtra)

theme_set(theme_minimal(base_size = 12))
```



The `Chile Schools` data is a good example of something you will often encounter when doing policy evaluations. In 1992, the Chilean government wanted to improve poor-performing schools. To do so, the government assigned extra resources (teacher training and after-school tutoring) to all schools scoring lower than a predefined cutoff on the average fourth-grade test score. This assignment is clearly not random. In fact, we can be quite certain that the schools that received the extra resources are different from those that did not (that is why those schools received extra help!). 

However, as we move closer to the cutoff, it seems more and more likely that the schools falling on either side are not _that_ different. For example, say the cutoff was 60 points on a 100-point scale. Two schools with an average score of 30 and 70 are clearly different, and it makes no sense to compare those. But what about two schools scoring 58 and 62? Do we really believe that those schools differ fundamentally from each other? The key assumption we make in a regression discontinuity design is that the units we look at on either side of the cutoff are as-if random.

### Plot 1: Showing the raw data

A good place to start is to show the raw data on either side of the cutoff. In the plot below, all units above the threshold (here scaled to zero) received extra help, while those below did not.


```r
# Make a variable saying if schools received the treatment or not
chile <-
  chile %>% 
  mutate(condition = ifelse(rule2 < 0, "Treatment", "Control")) 


chile %>% 
  ggplot(aes(rule2, read92)) + 
  geom_vline(xintercept = 0, size = 1, color = "grey50", linetype = "dashed") + 
  geom_point(aes(color = condition), alpha = .6) + 
  scale_color_jama() + 
  labs(x = "Assignment variable",
       y = "Reading test score") + 
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" alt="Reading test scores in 1992 by assignment variable. The assignment variable is centered reading test scores in 1988. Having a score less than 0 meant that schools would be included in the program." width="672" />
<p class="caption"><span id="fig:unnamed-chunk-3"></span>Figure 1: Reading test scores in 1992 by assignment variable. The assignment variable is centered reading test scores in 1988. Having a score less than 0 meant that schools would be included in the program.</p>
</div>

However, since we wanted schools to be "alike" (in terms of potential outcomes), we should probably focus the graph on those units that are close to the threshold. Let's define a cutoff range from -5 through 5 and make a new plot:


```r
chile_sub <- 
  chile %>% 
  filter(rule2 < 5, rule2 > -5) 

chile_sub %>% 
  ggplot(aes(rule2, read92)) + 
  geom_vline(xintercept = 0, 
             size = 1, 
             color = "grey50", 
             linetype = "dashed") + 
  geom_point(aes(color = condition)) + 
 scale_color_jama() + 
  labs(x = "Assignment variable",
       y = "Reading test score") + 
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" alt="Reading test scores in 1992 by assignment variable. In this version of the graph, only schools close to the cutoff (-5 through 5) are plotted." width="672" />
<p class="caption"><span id="fig:unnamed-chunk-4"></span>Figure 2: Reading test scores in 1992 by assignment variable. In this version of the graph, only schools close to the cutoff (-5 through 5) are plotted.</p>
</div>

And we can even make this plot a bit clearer if we use binned averages instead of showing all data points:


```r
chile_sub %>% 
  ggplot(aes(rule2, read92)) + 
  geom_vline(xintercept = 0, 
             size = 1, 
             color = "grey50", 
             linetype = "dashed") + 
  stat_summary_bin(aes(color = condition), 
                   fun = "mean", 
                   size = 2, 
                   geom = "point") + 
  scale_color_jama() + 
  labs(x = "Assignment variable",
       y = "Reading test score") + 
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-5-1.png" alt="Reading test scores in 1992 by assignment variable. In this version of the graph, schools are plotted using binned averages on the assignment variable." width="672" />
<p class="caption"><span id="fig:unnamed-chunk-5"></span>Figure 3: Reading test scores in 1992 by assignment variable. In this version of the graph, schools are plotted using binned averages on the assignment variable.</p>
</div>

### Plot 2: Adding the model 

Showing the data (to others but also yourself) is always a good place to start and a key part of of [exploratory data analysis](https://r4ds.had.co.nz/exploratory-data-analysis.html). However, to answer the question of interest (did assigning extra help to schools improve reading?), we need a model. 

Luckily, the modeling part of a regression discontinuity analysis is straightforward: We simply include the condition and the assignment variable as predictors in our regression model: 


```r
chile_rs <- 
  lm(read92 ~ condition + rule2, data = chile_sub) 

chile_rs %>% 
  tidy(conf.int=T,) %>% 
  select(-c("statistic", "std.error", "p.value")) %>% 
  mutate(term = c("Intercept", "In program", "Assignment variable")) %>% 
  kbl(col.names = c("Term", "Estimate", "Lower bound", "Upper bound"),
      caption = "Effect of being in policy program",
      booktabs = T,
      digits = 2) %>% 
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-6"></span>Table 1: Effect of being in policy program</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Lower bound </th>
   <th style="text-align:right;"> Upper bound </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Intercept </td>
   <td style="text-align:right;"> 59.83 </td>
   <td style="text-align:right;"> 58.80 </td>
   <td style="text-align:right;"> 60.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> In program </td>
   <td style="text-align:right;"> 2.15 </td>
   <td style="text-align:right;"> 0.27 </td>
   <td style="text-align:right;"> 4.04 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Assignment variable </td>
   <td style="text-align:right;"> 0.95 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 1.28 </td>
  </tr>
</tbody>
</table>


Thus, the estimated effect of being in the program is about 2.15 points. 

To understand this result better, and to present it in an intuitive way to stakeholders, we can plot the model on either side of the cutoff (i.e. the regression line):


```r
chile_sub %>% 
  ggplot(aes(rule2, read92)) + 
  geom_vline(xintercept = 0, size = 1, color = "grey50", linetype = "dashed") + 
  geom_point(aes(color = condition), alpha = .6) + 
  geom_smooth(
    data = chile_sub %>%  filter(rule2 < 0),
    method = "lm",
    color = "black",
    se = F) +
  geom_smooth(
    data = chile_sub %>%  filter(rule2 > 0),
    method = "lm",
    color = "black",
    se = F) +
  annotate(geom="text", 
           x  = -2.5, 
           y = 33, 
           color = "#DF8F44FF", 
           label = "Included in program") + 
    annotate(geom="text", 
           x  = 2.5, 
           y = 33, 
           color = "#374E55FF", 
           label = "Not included in program") + 
  scale_color_jama() + 
  labs(x = "Assignment variable",
       y = "Reading test score") + 
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" alt="Reading test scores in 1992 by assignment variable. In this version of the graph, the fitted model on either side of the threshold is overlaid on the raw data." width="672" />
<p class="caption"><span id="fig:unnamed-chunk-7"></span>Figure 4: Reading test scores in 1992 by assignment variable. In this version of the graph, the fitted model on either side of the threshold is overlaid on the raw data.</p>
</div>


The estimated effect of 2.15 points corresponds to the difference (the "kink") between the two lines.

### Building the final model

Finally, in most real-world applications, it makes sense to include pre-treatment variables that strongly predict the outcome since this can make our model estimates more precise. In our example, we have schoolchildren’s math and reading scores in 1988, which is probably useful when trying to predict reading scores in 1992. Let’s include those:


```r
chile_rs_final <- 
  lm(read92 ~ condition + rule2 + read88 + math88, data = chile_sub) 

chile_rs_final %>% 
  tidy(conf.int=T,) %>% 
  select(-c("statistic", "std.error", "p.value")) %>% 
  mutate(term = c("Intercept", 
                  "In program", 
                  "Assignment variable",
                  "Reading score in 1988",
                  "Math score in 1988")) %>% 
  kbl(col.names = c("Term", "Estimate", "Lower bound", "Upper bound"),
      caption = "Effect of being in policy program, covariate adjusted",
      booktabs = T,
      digits = 2) %>% 
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption><span id="tab:unnamed-chunk-8"></span>Table 2: Effect of being in policy program, covariate adjusted</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Term </th>
   <th style="text-align:right;"> Estimate </th>
   <th style="text-align:right;"> Lower bound </th>
   <th style="text-align:right;"> Upper bound </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Intercept </td>
   <td style="text-align:right;"> 23.35 </td>
   <td style="text-align:right;"> 14.84 </td>
   <td style="text-align:right;"> 31.85 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> In program </td>
   <td style="text-align:right;"> 2.08 </td>
   <td style="text-align:right;"> 0.28 </td>
   <td style="text-align:right;"> 3.88 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Assignment variable </td>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> -0.22 </td>
   <td style="text-align:right;"> 0.51 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Reading score in 1988 </td>
   <td style="text-align:right;"> 0.61 </td>
   <td style="text-align:right;"> 0.46 </td>
   <td style="text-align:right;"> 0.75 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Math score in 1988 </td>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;"> -0.01 </td>
   <td style="text-align:right;"> 0.32 </td>
  </tr>
</tbody>
</table>

As can be seen, our confidence intervals are now slightly narrower, reflecting the extra information we have included in our analysis. 

### Final thoughts: plotting your next regression discontinuity analysis
Regression discontinuity designs (RDD) are a popular way of evaluating policies when randomization b y the researcher is not an option. When doing RDD, we look for naturally occurring cutoffs that split units into a treatment and a control group. 

Cutoff variables should be continuous. Finally, be aware that RDD is _quasi_-experimental. It is _not_ a randomized experiment, and it is on _you_ to prove that the assumptions hold.    

### Cool! Where can I learn more?

* Gelman, A., Hill, J., & Vehtari, A. (2020). _Regression and other stories_. Cambridge University Press.
* Cunningham, S. (2021). _Causal inference: The mixtape_. Yale university press. 
 
