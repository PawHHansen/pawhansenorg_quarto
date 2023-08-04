---
title: 'The policy evaluator’s toolbox: Plotting results from randomized experiments'
author: ''
date: '2023-05-24'
slug: []
categories:
  - statistical analysis
tags: []
subtitle: 'Sensible out-of-the-box plots for your next completely randomized experiment.'
excerpt: 'Sensible out-of-the-box plots for your next completely randomized experiment.'
draft: no
series: null
layout: single
editor_options: 
  chunk_output_type: console
---



In this series of blog posts, I go over some of the most common experimental designs, including natural experiments such as difference-in-difference, regression discontinuity designs, and so on. The goal is to provide you with some sensible default plots you can use when writing up your next experimental analysis. For more detail on the technicalities of the individual designs, check out the sources I link to at the bottom of this post. 


```r
# Packages we'll be using
library(tidyverse)
library(broom)
library(kableExtra)
library(ggsci)

theme_set(theme_minimal(base_size = 12))
```


What better way to kick off a series on experimental designs than the one-and-only  randomized experiment. To get started, I'll simulate some fake data we can work with. 


```r
set.seed(1234)
n <- 1000

dat <-
  tibble(
  treat = sample(c(0, 1),n, replace = T),
  covariate = runif(n, 0, 5),
  outcome = 25 + 2.5 * treat + 1.5* covariate + 0.5 * (treat * covariate) + rnorm(n, 0, 4)
) %>%
  mutate(treat = ifelse(treat == 0, "Control", "Treatment"))
```

### Plot 1: Difference-in-means
To get started, we'll make a simple graph of the two treatment groups and use OLS to calculate the unadjusted difference-in-means: 


```r
means <-
  dat %>%
  group_by(treat) %>%
  summarise(avg = mean(outcome),
            sd = sd(outcome)) 

avg_control <-
  means %>%
  filter(treat == "Control") %>%
  select(avg)

dat %>%
  ggplot(aes(treat, outcome)) +
  geom_jitter(aes(color = treat), alpha = .4) +
  geom_hline(yintercept = avg_control$avg, linetype = "dashed", color = "grey50") +
  geom_pointrange(data = means, aes(x = treat, 
                                    y = avg, 
                                    ymin = avg + sd, 
                                    ymax = avg - sd)) +
  labs(color = "Condition", 
       x = NULL, 
       y = "Outcome") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" alt="Outcome across experimental conditions" width="672" />
<p class="caption"><span id="fig:unnamed-chunk-3"></span>Figure 1: Outcome across experimental conditions</p>
</div>

This is good place to start for most basic experimental designs. The graph reveals all of the data as well as the key model parameters, which in this case is simply the two group means. Assuming all of the usual assumptions hold, the difference between these two means represent the average treatment effect. 

### Plot 2: Interactions with covariates 

Consider a slightly more complicated example where the treatment effect is moderated by some covariate. For example, if the outcome were students’ test scores and the treatment meant being assigned to a tailored learning programme, then parents’ socio-economic resources might interact with how effective the treatment was. 

To plot this, we first complicate the model slightly to take interactions into account:  


```r
lm_mod <-
  lm(outcome ~ treat + covariate + treat*covariate, data = dat) 
```

To make our plot, we want the predictions for each condition (treatment/control) depending on the level of the covariate. 

We first set up a data frame using `crossing()`: 



```r
pred_dat <-
  crossing(treat = c("Control", "Treatment"), 
           covariate = seq(min(dat$covariate), max(dat$covariate), by = .25)
           )
```


Then we can use `augment()` to calculate the predictions: 


```r
preds <-
  augment(lm_mod, 
        newdata = pred_dat,
        se_fit = T,
        interval = "confidence")
```

And with those predictions, we can make the plot: 


```r
pred_labels <-
  tibble(treat = c("Control", "Treatment"),
         covariate = c(4, 3),
         .fitted = c(28.5, 36))

preds %>%
  ggplot(aes(covariate, .fitted, color = treat)) +
  geom_line() +
  geom_line(aes(y = .lower), linetype = "dashed", alpha = .4) +
  geom_line(aes(y = .upper), linetype = "dashed", alpha = .4) +
  geom_text(data = pred_labels, aes(label = treat)) +
  labs(color = "Condition", 
       x = "Covariate", 
       y = "Outcome") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "none")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-7-1.png" alt="Outcome across experimental conditions, by covariate" width="672" />
<p class="caption"><span id="fig:unnamed-chunk-7"></span>Figure 2: Outcome across experimental conditions, by covariate</p>
</div>

In theory, you could also add the raw data to this plot:


```r
preds %>%
  ggplot(aes(covariate, .fitted, color = treat)) +
  geom_point(data = dat, aes(x = covariate, y = outcome), alpha = .2) +
  geom_line() +
  geom_line(aes(y = .lower), linetype = "dashed", alpha = .6) +
  geom_line(aes(y = .upper), linetype = "dashed",  alpha = .6) +
  labs(color = "Condition", 
       x = "Covariate", 
       y = "Outcome") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-8-1.png" alt="Outcome across experimental conditions, by covariate with data overlaid" width="672" />
<p class="caption"><span id="fig:unnamed-chunk-8"></span>Figure 3: Outcome across experimental conditions, by covariate with data overlaid</p>
</div>

Be careful, though, as the graph quickly gets crammed. Adding emphasis on part of a piece of advice from Kieran Healy: show the data when you can. Use common sense and professional judgment when making up your mind  

### Final thoughts: plotting results from completely randomized experiments

Plotting the results from a completely randomized experiment is usually straightforward. In this post, I have shown you two basic plots that will get you a long way in most analyses. First, whenever possible make sure to plot the data and the fitted model together (Plot 1). Second, when you need to present interactions, do not rely on tables or interaction terms as these are notoriously easy to get wrong. Instead, use your model to generate predictions on the original outcome scale of the data and then plot those predictions (Plot 2). 

### Cool! Where can I learn more?

* Coppock, A. (2021). Visualize as you randomize. _Advances in experimental political science_. Cambridge University Press.  
* Healy, K. (2018). _Data visualization: a practical introduction_. Princeton University Press.
