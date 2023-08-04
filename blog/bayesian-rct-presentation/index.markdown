---
title: 'Bayesian analysis of a randomized controlled trial III: Interpretation and
  presentation'
author: 'Paw Hansen'
date: '2023-07-02'
slug: []
categories:
  - statistical analysis
tags:
  - Bayesian analysis
subtitle: Final part of my three-part series on Bayesian analysis of randomized controlled trials. Get ready to interpret and present your results!
excerpt: Final part of my three-part series on Bayesian analysis of randomized controlled trials. Get ready to interpret and present your results!
draft: no
series: null
layout: single
editor_options: 
  chunk_output_type: console
---
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />



The packages we'll be using:


```r
library(tidyverse)
library(rstanarm)
library(tidybayes)
library(kableExtra)
library(patchwork)

theme_set(theme_minimal(base_size = 12))
```

Welcome to the last part of my three-part series on Bayesian analysis of randomized controlled trial. So far, we have specified priors to take previous knowledge into account and we have fit and validated two different models.

In this final post, we'll look at how we could go about presenting and interpreting our results. In a report or scientific article, this is the part that should go into the Results section.

In my view, presenting results is where Bayesian analysis really gets to shine. As I hope you will see, taking a Bayesian approach makes it easy to present meaningful and intuitive quantities of interest and gives us incredible flexibility when it comes to answering question of real-world importance.



Recall that in our hypothetical experiment, students were randomly assigned to either a treatment or a control group. The outcome is dichotomous: did the students fail the reading test (indicating dyslexia) or not.

Before we go into any calculations, let's look at four core quantities of interest commonly used for controlled trial settings:

-   absolute risk
-   absolute risk reduction
-   relative risk
-   numbers needed to treat

What do these numbers mean? The **absolute risk** is the risk of an event happening given the exposure. The **absolute risk reduction** is the difference between the observed risks the treatment and the control group. The **relative risk** is the risk of an event occurring in the exposed group divided by the risk of the event occurring in the non-exposed group. Finally, the **number needed to treat** is the number of individuals you need to treat to prevent one additional bad outcome.

### Analysis "by hand"


```r
abs_risk <- 
  fake |> 
  summarize(abs_risk = mean(fail_test, na.rm = T), 
            .by = condition)

abs_risk |> 
  kable(col.names = c("Condition", 
                      "Absoulte risk of failing reading test"), 
        digits = 2, 
        caption = "Absolute risk of failing the reading test, by experimental condition") |> 
  kable_styling(full_width = F)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption><span id="tab:calc-abs-risk"></span>Table 1: Absolute risk of failing the reading test, by experimental condition</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Condition </th>
   <th style="text-align:right;"> Absoulte risk of failing reading test </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Control </td>
   <td style="text-align:right;"> 0.52 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Treatment </td>
   <td style="text-align:right;"> 0.41 </td>
  </tr>
</tbody>
</table>

To begin, table 1 shows the **absolute risk** of failing the reading test. At first sight, it seems the treatment worked. The absolute risk of failing the reading test was 0.52 in the control group but only 0.41 in the treatment group.

Second, the **absolute risk reduction** between the treatment and the control group is 0.11. For an individual, it describes the estimated difference in the probability of experiencing the event, in this case 11 percent.

Third, the **relative risk** of failing the reading test for students in the treatment group was only 0.78 as compared to students in the control group.



Finally, for calculating the **number needed to treat**, we simply take 1 and divide it by the absolute risk reduction. Our results suggests that for every 9 students placed in the program, we could prevent 1 student from failing the reading test.

Calculating each of the four causal estimands "by hand" has given us a solid foundation into what these numbers are. In practice, though, we will never want to do it this way ever again. Instead, we will use our posterior simulations.

### Analysis using `rstanarm`



The core principle for presenting results is that we don't want to just show a list of model parameters. These have little interest in and off themselves. Instead, we'll use the model to make predictions, i.e. show the [*implications* of the model.](https://www.pawhansen.org/blog/2023-05-17-the-human-scale-principle-for-presenting-statistical-results/).

To this end, we set up a data frame containing the values of the predictors we intend to predict from (aka a "predictor matrix"). In our context of a controlled trial, these are simply the treatment and the control group:


```r
pred_dat <- 
  crossing(
    condition = c("Control", "Treatment")
    )
```

We can then use this data frame to estimate the absolute risk for each group:


```r
# Make predictions: risk of failing reading test in treatment and control group
model_preds <- 
  add_epred_draws(newdata = pred_dat, mod_rstan)
```

With our predictions in place, we can plot distributions of each of the four quantities of interest, for example for including in a presentation. The following code chunk shows how to plot each of the four quantities from before:


```r
# abs risk of passing reading test
p_abs_risk <- 
  model_preds |> 
  ggplot(aes(.epred, fill = condition)) + 
  geom_density(alpha = .4) + 
  annotate(geom = "text", x = .41, y = 2, label ="Treatment") + 
  annotate(geom = "text", x = .52, y = 2, label ="Control") + 
  scale_x_continuous(labels = scales::label_percent()) + 
  scale_y_continuous(labels = NULL) +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Absolute risk",
       y = NULL, 
       fill = "Condition") + 
  theme(legend.position = "none")

# rel risk of failing
rel_risk <- 
  as_tibble(posterior_epred(mod_rstan, newdata = pred_dat)) |> 
  mutate(control = `1`, treatment = `2`) |> 
  mutate(relrisk = treatment / control) 

p_rel_risk <- 
  rel_risk |> 
  ggplot(aes(relrisk)) + 
  geom_histogram(fill = "lightblue", color = "white") + 
  geom_vline(xintercept = median(rel_risk$relrisk), 
             lty = 2,
             color = "firebrick") + 
  scale_y_continuous(labels = NULL) +
  labs(x = "Relative risk", 
       y = NULL)

# abs risk reduction 
abs_rr <- 
  as_tibble(posterior_epred(mod_rstan, newdata = pred_dat)) |> 
  mutate(control = `1`, treatment = `2`) |> 
  mutate(abs_rr = control - treatment) 

p_risk_reduc <- 
  abs_rr |> 
  ggplot(aes(abs_rr)) + 
  geom_histogram(fill = "lightblue", color = "white") +
  geom_vline(xintercept = median(abs_rr$abs_rr), 
             lty = 2,
             color = "firebrick") + 
  scale_y_continuous(labels = NULL) +
  labs(x = "Absoulte risk reduction",
       y = NULL)

# Numbers needed to treat 
nntt <- 
  abs_rr |> 
  mutate(nn_treat = 1/abs_rr) 

p_nntt <- 
  nntt |> 
  ggplot(aes(nn_treat)) +
  geom_histogram(fill = "lightblue", color = "white") +
  geom_vline(xintercept = median(nntt$nn_treat), 
             lty = 2,
             color = "firebrick") + 
  scale_y_continuous(labels = NULL) +
  xlim(0, 25) + 
  labs(x = "Numbers needed to treat", 
       y = NULL)

full_plot <- 
  (p_abs_risk + p_rel_risk) / (p_risk_reduc + p_nntt)

full_plot + plot_annotation(tag_levels = 'a') 
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/fig-stan-qois-1.png" alt="Quantities of interest with uncertainties, calculated using rstanarm. (a) Absolute risk of failing reading test. (b) Relative risk of failing reading test when given treatment as opposed to control. (c) Absolute risk reduction in failing reading test when given treatment (d) Numbers needed to treat: How many students should be placed in the program to prevent one student from failing the reading test?" width="672" />
<p class="caption"><span id="fig:fig-stan-qois"></span>Figure 1: Quantities of interest with uncertainties, calculated using rstanarm. (a) Absolute risk of failing reading test. (b) Relative risk of failing reading test when given treatment as opposed to control. (c) Absolute risk reduction in failing reading test when given treatment (d) Numbers needed to treat: How many students should be placed in the program to prevent one student from failing the reading test?</p>
</div>

When writing up our analysis, we could include one or more of these plots and then use the predictions to make meaningful statements about the model implications and uncertainties. Taking the number needed to treat as an example, we might want to calculate the 80 percent prediction interval:


```r
round(quantile(nntt$nn_treat, c(.1, .5, .9)))
```

```
## 10% 50% 90% 
##   6   9  14
```

From that, we could write something like: "Using our Bayesian model, we estimated the number needed to treat. We found that the intervention should be applied to 9 students to prevent one case of dyslexia, plus or minus about 4 students."

### Taking the posterior out for a spin: probability of a practically significant effect

So far we have covered most the standard quantities of interest for analyzing randomized controlled trials. But Bayesian analysis has more to offer.

When working within a Bayesian framework, a lot of the hard work lies in specifying a sensible prior and then fitting a proper model. But once we have our posterior distribution(s) at hand, we can calculate *any* quantity that might be of interest.

To give a brief example, suppose a policy maker is only interested in scaling up the intervention if the absolute risk reduction is at least 15 percent (perhaps out of political or cost-effectiveness considerations).

Looking at the histogram in Figure <a href="#fig:fig-stan-qois">1</a>c, we see that *some* of the distribution is above .15. It is not impossible that the treatment effect *could* be .15 or more. But how *likely* is that? Put another way, given what we know (model, data), what is the probability that the absolute risk reduction is above 15 percent?



We can approach this graphically by drawing the **empirical cumulative distribution**, which shows the share of observations being lower than the specified value on the x-axis.



Figure <a href="#fig:fig-ecdf">2</a>a shows a distribution of plausible risk reductions given the data and model. The shaded region starting at 15 percent is what we care about. This region is an incredibly powerful concept because it allows us to think about effect sizes in terms of what _matters rather than what is "statistically significant".  

What is the probability that our risk reduction lies within the ROPE? Figure <a href="#fig:fig-ecdf">2</a>b, shows a so-called quantile function, plotting the potential risk reductions on the x-axis and the inverse cumulative probability on the y-axis. Substantially, the inverse cumulative probability describes the probability of the risk reduction being greater than the number on the x-axis. 

Looking at the figure, we see that the probability is about 13. Is this a lot or a little? Well, that depends... 

Of course 13 percent, or about 1-in-8, does not sound like a lot. But suppose that running the intervention is really cheap. Then, policy makers may want to "roll the dice" and see what happens. After all, the probability that the effect size is greater than 15 percent is small, but the probability that the effect size is at least 5 percent is 98 percent!

The point is that when doing Bayesian analysis, we do not bake decision-making into the analysis, such as saying that the treatment "worked" because _p_<0.05). Instead, we live with the uncertainty; estimating the effect size and uncertainties the best we can, and then leave the rest up to stakeholders.  


```r
# Density curve 
p_abs_rr <- 
  abs_rr |> 
  ggplot(aes(abs_rr)) + 
  geom_density() +
  geom_vline(xintercept = .15, 
             linetype = "dashed", 
             color = "steelblue") + 
  geom_area(data = abs_rr_density_tibble, 
            aes(x = abs_rr, y = density), 
            fill = "lightblue",
            alpha = .4) + 
  scale_x_continuous(labels = scales::label_percent()) + 
  scale_y_continuous(labels = NULL) + 
  labs(x = "Absolute risk reduction",
       y = NULL)

# ECDF
p_ecdf <- 
  abs_rr |> 
  ggplot(aes(abs_rr)) +
  geom_vline(xintercept = .15, color = "steelblue", linetype = "dashed") + 
  stat_ecdf(aes(y = 1 - ..y..)) + 
  geom_point(data = secret_dot, aes(abs_rr, ecdf), color = "steelblue") + 
  annotate(geom = "text", x = .21, y = .14, label = str_c("Pr(ARR > 15 pct.): \n", secret_prob, " pct."), color = "steelblue", size = 3) + 
  scale_x_continuous(labels = scales::label_percent(),  limits = c(0, .25)) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  labs(x = "Absolute risk reduction", 
       y = "Inverse cumulative probability")

full_plot_prob <- 
  (p_abs_rr + p_ecdf) 

full_plot_prob + plot_annotation(tag_levels = 'a') 
```

<div class="figure">
<img src="{{< blogdown/postref >}}index_files/figure-html/fig-ecdf-1.png" alt="What is the probability that the risk reduction is above 15 percent? (a) Distribution of the plausable risk reductions given the data and model. Policy maker only cares about the blue area. (b) Quantile fundction showing the distribution of plausable risk reductions. The graph shows the probability of the absoulute risk reduction being lower than the specified value on the x-axis." width="672" />
<p class="caption"><span id="fig:fig-ecdf"></span>Figure 2: What is the probability that the risk reduction is above 15 percent? (a) Distribution of the plausable risk reductions given the data and model. Policy maker only cares about the blue area. (b) Quantile fundction showing the distribution of plausable risk reductions. The graph shows the probability of the absoulute risk reduction being lower than the specified value on the x-axis.</p>
</div>

Of course, in addition to eyeballing the graph, we can ask R to extract the exact probability for us:


```r
mean(abs_rr$abs_rr > .15)
```

```
## [1] 0.12675
```

As a final tip, interpreting a quantile function may be tricky to non-technical audiences. If I were advising, say, a politician, I might prefer to present some meaningful risk reductions and then simply present these in a small table:

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption><span id="tab:tbl-arr-prob"></span>Table 2: Probability of different risk reductions</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Minimum risk reduction </th>
   <th style="text-align:right;"> Probability </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.98 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.68 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 0.13 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.20 </td>
   <td style="text-align:right;"> 0.00 </td>
  </tr>
</tbody>
</table>

### Final thoughts: Bayesian analysis of a randomized controlled trial - interpretation and presentation

Bayesian analysis has a lot to offer when conducting and interpreting statistical analyses of randomized controlled trials. It allows analysts to interpret results in a way that is usually far more intuitive and useful to policy makers than standard reports relying on statistical significance tests. Rather than discretizing results (the treatment either "worked" or "failed"), Bayesian analysis makes it possible to make more nuanced statements ("the probability that the treatment effect is above the desired 15 percentage points is 97 percent"). Taken together, I hope this post and the two previous ones in the three-part series has encouraged you to go and do your first Bayesian analysis!

### Cool! Where can I learn more?

-   Johnson, A. A., Ott, M. Q., & Dogucu, M. (2022). *Bayes rules!: An introduction to applied Bayesian modeling*. CRC Press.

-   Gelman, A., Hill, J., & Vehtari, A. (2020). *Regression and other stories*. Cambridge University Press.

-   Ryan, E. G., Harrison, E. M., Pearse, R. M., & Gates, S. (2019). Perioperative haemodynamic therapy for major gastrointestinal surgery: the effect of a Bayesian approach to interpreting the findings of a randomised controlled trial. *BMJ open, 9(3)*.

-   Goodrich, B., Gabry, J., Ali, I., & Brilleman, S. (2020). *rstanarm: Bayesian applied regression modeling via Stan*. Find many of their useful vignettes [here](https://mc-stan.org/rstanarm/).
