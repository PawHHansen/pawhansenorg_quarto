---
title: The Human Scale Principle for presenting statistical results
author: Paw Hansen
date: '2023-05-17'
slug: []
categories:
  - statistical analysis
tags: []
subtitle: Don’t rely on the default computer output when presenting statistical results. Do this instead.
excerpt: Don’t rely on the default computer output when presenting statistical results. Do this instead.
draft: no
series: null
layout: single
editor_options: 
  chunk_output_type: console
---



Computer software, such as R and Python, makes it easy to build complex statistical models. And with a few lines of code, you can get all sorts of outputs summarizing the model parameters.  

However, the default computer output for most statistical models is rarely the most intuitive or compelling way to present your results to stakeholders. 

In this post, I introduce a few basic guidelines for how to present statistical results in a way that is easy to interpret, compelling to readers, and requires no special knowledge to understand. Applying these guidelines will ensure that your next data analysis actually  makes the impact on stakeholders you intend it to make. 

### The problem with accepting the default computer output
Consider the following output from a basic logistic regression analysis. The data comes from a survey (ref.), the outcome is respondents’ support of marihuana legalization (yes/no), and the model contains two predictors: age (in years) and whether respondents voted for Obama in the 2016 election (yes/no). 


```r
# Load packages and data 
library(tidyverse)
library(socviz)
library(broom)
library(scales)

theme_set(theme_minimal(base_size = 12))
```


```r
data("gss_sm")

# Recode
gss_sm <- 
  gss_sm %>%
  drop_na(grass, obama) %>%
  mutate(grass = ifelse(grass == "Legal", 1, 0),
         obama = ifelse(obama == 1, "Obama", "Not Obama"))

# Fit model
log_mod <- 
  glm(grass ~ age + as.factor(obama), data = gss_sm, family = "binomial")
```

Let's check out our results:


```r
summary(log_mod)
```

```
## 
## Call:
## glm(formula = grass ~ age + as.factor(obama), family = "binomial", 
##     data = gss_sm)
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)            1.167099   0.248084   4.704 2.55e-06 ***
## age                   -0.025051   0.004091  -6.123 9.16e-10 ***
## as.factor(obama)Obama  1.103243   0.132795   8.308  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1481.2  on 1114  degrees of freedom
## Residual deviance: 1358.0  on 1112  degrees of freedom
##   (2 observations deleted due to missingness)
## AIC: 1364
## 
## Number of Fisher Scoring iterations: 4
```

The table contains several numbers, some of which have asterisks next to them. If this makes you happy, beware. 

Before going further, quiz yourself and see if you can answer the following question without using any statistical jargon: what can we say about the relationship between age and support for legalization? 

If your answer begins “There is a statistically significant relationship between…”, then you are going off track. If your answer begins “The coefficient reveals that the log(odds) of age…”, all the worse. 

Being a reader of this blog post, you likely have some training in statistical modeling. But your stakeholders might not. If you have a hard time drawing intuitive conclusions from the output, so will your stakeholders. 
What to do instead: predict on a human scale 

The general rule of thumb I propose here is that you always try to present your results ‘on a human scale’. Thinking of someone with no formal training in statistics, what would make sense to that person? What kind of questions would someone caring about the substance (but not the methods) be interested in? 

For example, log(odds) do not buy you anything down at the supermarket. They have no value or interpretation outside the realms of statistics. Neither do standard deviations, z-scores, or p-values. Without statistical training, these numbers convey no information (and even people with statistical training often get them wrong).

### What to do instead: Make predictions on a 'human scale'
Instead, use your model to make predictions on a human scale. When deciding on what predictions to make, list all the different questions that someone with no statistical training would want to know the answer to. In the case of our logistic regression, questions might be:  

* How likely is it that an average Republican will support legalization?  
* How much more/less is that than the expected support from an average Democrat?
* Does the support stay the same over the course of a lifetime, or does it drop? By how much does it change? 
* How certain are we of these conclusions?

Using R, the workhorse function will be good old predict(). 

We first set up a dataframe with the values we want to see predictions for. Here I use a range of age going from 18 through 80 years and people who voted for/did not vote for Barack Obama.


```r
new_dat <- 
  crossing(
    age = 18:80, 
    obama = c("Obama", "Not Obama")
  )
```

With that in place, we can calculate our predicted probabilities:


```r
preds <-
  augment(log_mod, newdata = new_dat, type.predict = "response", se_fit = T) %>%
  mutate(lwr = .fitted - 1.96 * .se.fit,
         upr = .fitted + 1.96 * .se.fit) 
```

If you can make a graph addressing these questions, all the better. Here is my suggestion:


```r
preds %>%
  ggplot(aes(age, .fitted, color = obama)) + 
  geom_line(linewidth = .6) +
  geom_line(aes(x = age, y = lwr, group = obama), alpha = .7, linetype = "dashed") + 
  geom_line(aes(x = age, y = upr, group = obama),  alpha = .7, linetype = "dashed") + 
  scale_y_continuous(label = percent_format()) + 
  scale_color_brewer(type = "qual", palette = "Set1") + 
  labs(x = "Age (years)",
       y = "Probability of support",
       color = "Vote in 2016 election") + 
  theme(legend.position = "top")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-6-1.png" alt="Probability of supporting marihuana legalization, by age" width="672" />
<p class="caption"><span id="fig:unnamed-chunk-6"></span>Figure 1: Probability of supporting marihuana legalization, by age</p>
</div>

When writing up the analysis, you just carefully go over the graph. 

Furthermore, we can use the predictions we used to make the graph to provide stakeholders with illustrative quantities. For example:

 * Compared to an 18-year old who does _not_ support Obama, an 18-year old Obama supporter is 30 percent more likely to support marijuana legalization 
 * An 18-year old Republican is as supportive as a 60-year old Obama supporter.
 * Over a lifespan, the probability of supporting marihuana legalization drops remarkably. For example, a 74-year old Republican is only about half as likely to support legalization as is an 18-year old.

> “Age is a strong predictor of legalization support. All other things being equal, the probability of supporting marihuana legalization drops by about xx percentage points per year, plus or minus about xx”. 

As a final advice, make sure that you round your estimates instead of just throwing useless decimals at your reader. And always provide readers with a sense of the uncertainty around your estimates. 

### Final thoughts: Working on a human scale

As a data analyst, you should not just “get the numbers right”, but also “get the right numbers”. To do so, you must work on a human scale; thinking about the questions that would be compelling to someone who cares about the substance but not the methods. 
Use your model to make predictions that address those questions. Then use those predictions to make a compelling graph and provide a few illustrative examples. Done.  

### Cool! Where can I learn more?

 * King, G., Tomz, M., and Wittenberg, J. (2000). Making the most of statistical analyses: Improving interpretation and presentation. _American journal of political science_, 347-361.
