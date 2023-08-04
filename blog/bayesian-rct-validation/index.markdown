---
title: 'Bayesian analysis of a randomized controlled trial II: Defining and validating
  the model'
author: ''
date: '2023-07-02'
slug: []
categories:
  - statistical analysis
tags:
  - Bayesian analysis
subtitle: Part two of my three-part series on Bayesian analysis of randomized controlled trials. You've built a model but is it any good?
excerpt: Part two of my three-part series on Bayesian analysis of randomized controlled trials. You've built a model but is it any good?
draft: no
series: null
layout: single
editor_options: 
  chunk_output_type: console
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



The aim of this post is to _validate_ our models. Basically, we'll be asking two questions:

1. How well does the model fit the data?
2. How well does the model fit _new_ data?

For the first question, we will do posterior predictive simulation to see if data simulated from the model has features similar to the original data (which was used to fit the model). 

For the second question, we will 

The packages we'll be using: 


```r
library(tidyverse)
library(rstanarm)
library(bayesrules)
library(tidybayes)
library(bayesplot)
library(kableExtra)
library(patchwork)

theme_set(theme_minimal(base_size = 12))
```



Recall our two models from my previous post:

* In one model, I used a _weakly informative prior_ 
* In another, model I used an _evidence-based prior_


Before moving on, we should check the stability of our simulations. 

For the model using a weakly informative prior...


```r
# MCMC trace, density, & autocorrelation plots - weakly informative prior 
p1 <- mcmc_trace(fail_model_weakinf) + scale_x_continuous(breaks = c(0, 5000))

p2 <- mcmc_dens_overlay(fail_model_weakinf)
p3 <- mcmc_acf(fail_model_weakinf)

p1 / p2 / p3 + plot_annotation(tag_levels = 'A')
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/plot-diag-weak-1.png" alt="Diagnostic plots for the stability of our simulation results concerning the model using weakly informative priors. All looks good." width="672" />
<p class="caption"><span id="fig:plot-diag-weak"></span>Figure 1: Diagnostic plots for the stability of our simulation results concerning the model using weakly informative priors. All looks good.</p>
</div>

All looks good. For the evidence based model...


```r
# MCMC trace, density, & autocorrelation plots - evidence based model
p4 <- mcmc_trace(fail_model_evidence) + scale_x_continuous(breaks = c(0, 5000))
p5 <- mcmc_dens_overlay(fail_model_evidence)
p6 <- mcmc_acf(fail_model_evidence)

p1 / p2 / p3 + plot_annotation(tag_levels = 'A')
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/plot-diag-evidence-1.png" alt="Diagnostic plots for the stability of our simulation results concerning the evidence-based model. All looks good." width="672" />
<p class="caption"><span id="fig:plot-diag-evidence"></span>Figure 2: Diagnostic plots for the stability of our simulation results concerning the evidence-based model. All looks good.</p>
</div>

same. 

All set, let's get on with model validation!

### Check #1: How well does the model fit the data?

To see this, we simulate 100 data sets from our posterior distribution. For each data set, we then calculate the number of failed tests to see if this matches up with that of the original data.  


```r
calc_prop_fail <- function(x) {mean(x == 1)
}
```


```r
pp_check_model_weakinf <- 
  pp_check(fail_model_weakinf, 
           plotfun = "stat", 
           stat = "calc_prop_fail",
           seed = 2307) + 
  xlab("Share of failed reading tests") +
  xlim(0,1) + 
  theme(legend.position = "none")

pp_check_model_evidence <-
  pp_check(fail_model_evidence, 
           plotfun = "stat", 
           stat = "calc_prop_fail",
           seed = 2307) + 
  xlab("Share of failed reading tests") +
  xlim(0,1) + 
  theme(legend.position = "none")

pp_check_model_weakinf / pp_check_model_evidence + plot_annotation(tag_levels = 'A')
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/pp-checks-1.png" alt="Posterior predictve checks of the two models. Histograms show the proportion of failed students in a number of simulated datasets based on each model, whereas the dark blue lines mark the real proportion of failed students in the data. (a) With a weakly informative prior, the simulated data sets are unwilling to say much about the share of failed reading tests. (b) In contrast, using an evidence-based prior results in posterior simulations that resemble the original data well." width="672" />
<p class="caption"><span id="fig:pp-checks"></span>Figure 3: Posterior predictve checks of the two models. Histograms show the proportion of failed students in a number of simulated datasets based on each model, whereas the dark blue lines mark the real proportion of failed students in the data. (a) With a weakly informative prior, the simulated data sets are unwilling to say much about the share of failed reading tests. (b) In contrast, using an evidence-based prior results in posterior simulations that resemble the original data well.</p>
</div>

### Check #2: How well does the model fit _new_ data?

Because we are working with a categorical outcome, we can be _either_ right or wrong. The question is how often are we right? 


```r
set.seed(0407)

class_sum_weakinf <- 
  classification_summary(model = fail_model_weakinf, data = fake, cutoff = 0.5) 

class_sum_evidence <- 
  classification_summary(model = fail_model_evidence, data = fake, cutoff = 0.5) 
```

* **overall accuracy** captures the proportion of all Y observations that are accurately classified
* **sensitivity (true positive rate)** captures the proportion of Y = 1 observations that are accurately classified 
* **specificity (true negative rate)** the proportion of Y = 0 observations that are accurately classified:

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption><span id="tab:tab-perf-measures"></span>Table 1: How well do the two models fit the data?</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Measure </th>
   <th style="text-align:right;"> Weakly informative priors </th>
   <th style="text-align:right;"> Evidence-based priors </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Sensitivity (true positive rate) </td>
   <td style="text-align:right;"> 0.57 </td>
   <td style="text-align:right;"> 0.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Specificity (true negative rate) </td>
   <td style="text-align:right;"> 0.43 </td>
   <td style="text-align:right;"> 0.54 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Overall accuracy </td>
   <td style="text-align:right;"> 0.50 </td>
   <td style="text-align:right;"> 0.54 </td>
  </tr>
</tbody>
</table>

