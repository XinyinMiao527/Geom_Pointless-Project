---
title: "Preliminary results"
author: "Yiming Cao, Zichen Fan, Xinyin Miao, Teresa Sha"
output:
  pdf_document:
    latex_engine: xelatex
---





# Nonlinear Exposure--Response Analysis (Xinyin Miao)

## Load packages


``` r
library(dplyr)
library(ggplot2)
library(splines)
library(lubridate)
library(purrr)
library(tidyr)
```

## Read and prepare data


``` r
df <- read.csv("data/analysis_2024.csv") |>
  mutate(
    date = as.Date(date),
    time = seq_len(n()),
    month_name = factor(month(date, label = TRUE, abbr = TRUE),
                        levels = month.abb),
    season = case_when(
      month(date) %in% c(12, 1, 2) ~ "Winter",
      month(date) %in% c(3, 4, 5)  ~ "Spring",
      month(date) %in% c(6, 7, 8)  ~ "Summer",
      TRUE                         ~ "Fall"
    ) |> factor(levels = c("Winter", "Spring", "Summer", "Fall")),
    weekday_name = factor(
      weekdays(date),
      levels = c("Monday", "Tuesday", "Wednesday",
                 "Thursday", "Friday", "Saturday", "Sunday")
    ),
    is_weekend = as.numeric(is_weekend)
  )
```

## Model specification

Primary nonlinear model:

$$
\log(\mu_t) = \beta_0 + \operatorname{ns}(\mathrm{PM}_{2.5,t}, 3)
+ \operatorname{ns}(\mathrm{time}_t, 4)
+ \operatorname{ns}(\mathrm{tmean}_t, 3)
+ \beta_1 \cdot \mathrm{is\_weekend}_t
+ \beta_2 \cdot \mathrm{precip}_t
$$

where

$$
Y_t \sim \text{Quasi-Poisson}, \qquad \mu_t = E(Y_t).
$$

For comparison, the linear specification is:

$$
\log(\mu_t) = \beta_0 + \beta_1 \cdot \mathrm{PM}_{2.5,t}
+ \operatorname{ns}(\mathrm{time}_t, 4)
+ \operatorname{ns}(\mathrm{tmean}_t, 3)
+ \beta_2 \cdot \mathrm{is\_weekend}_t
+ \beta_3 \cdot \mathrm{precip}_t
$$

## Fit linear and nonlinear models

### Mental health outcome


``` r
fit_mh_linear <- glm(
  mental_health ~ pm25 + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df
)

fit_mh_nonlinear <- glm(
  mental_health ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df
)

summary(fit_mh_linear)
```

```
## 
## Call:
## glm(formula = mental_health ~ pm25 + ns(time, 4) + ns(tmean, 
##     3) + is_weekend + precip, family = quasipoisson(link = "log"), 
##     data = df)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    6.1079674  0.0481610 126.824  < 2e-16 ***
## pm25          -0.0027390  0.0022091  -1.240 0.217593    
## ns(time, 4)1  -0.1885992  0.0767155  -2.458 0.015472 *  
## ns(time, 4)2  -0.1272162  0.0481845  -2.640 0.009457 ** 
## ns(time, 4)3  -0.0699661  0.0716241  -0.977 0.330730    
## ns(time, 4)4  -0.0344335  0.0307842  -1.119 0.265705    
## ns(tmean, 3)1  0.2279141  0.0480185   4.746 6.11e-06 ***
## ns(tmean, 3)2  0.4927748  0.1061062   4.644 9.28e-06 ***
## ns(tmean, 3)3  0.2499008  0.0727788   3.434 0.000833 ***
## is_weekend    -0.1858636  0.0145385 -12.784  < 2e-16 ***
## precip        -0.0020177  0.0008388  -2.405 0.017775 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 2.230983)
## 
##     Null deviance: 727.73  on 123  degrees of freedom
## Residual deviance: 253.91  on 113  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 3
```

``` r
summary(fit_mh_nonlinear)
```

```
## 
## Call:
## glm(formula = mental_health ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 
##     3) + is_weekend + precip, family = quasipoisson(link = "log"), 
##     data = df)
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    6.1255768  0.0588560 104.077  < 2e-16 ***
## ns(pm25, 3)1   0.0291699  0.0356936   0.817  0.41555    
## ns(pm25, 3)2  -0.1361293  0.0711396  -1.914  0.05825 .  
## ns(pm25, 3)3  -0.1455901  0.0633298  -2.299  0.02338 *  
## ns(time, 4)1  -0.1538828  0.0781149  -1.970  0.05133 .  
## ns(time, 4)2  -0.1062392  0.0495617  -2.144  0.03425 *  
## ns(time, 4)3  -0.0551672  0.0711053  -0.776  0.43949    
## ns(time, 4)4  -0.0304625  0.0309187  -0.985  0.32665    
## ns(tmean, 3)1  0.2113338  0.0488964   4.322 3.38e-05 ***
## ns(tmean, 3)2  0.4633315  0.1069611   4.332 3.26e-05 ***
## ns(tmean, 3)3  0.2190613  0.0739955   2.960  0.00376 ** 
## is_weekend    -0.1895067  0.0148372 -12.772  < 2e-16 ***
## precip        -0.0020941  0.0008364  -2.504  0.01375 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 2.179859)
## 
##     Null deviance: 727.73  on 123  degrees of freedom
## Residual deviance: 244.17  on 111  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 3
```

### Suicide outcome


``` r
fit_sui_linear <- glm(
  suicide ~ pm25 + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df
)

fit_sui_nonlinear <- glm(
  suicide ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df
)

summary(fit_sui_linear)
```

```
## 
## Call:
## glm(formula = suicide ~ pm25 + ns(time, 4) + ns(tmean, 3) + is_weekend + 
##     precip, family = quasipoisson(link = "log"), data = df)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    0.736720   0.350293   2.103  0.03767 * 
## pm25           0.008381   0.013412   0.625  0.53331   
## ns(time, 4)1  -0.308464   0.473974  -0.651  0.51649   
## ns(time, 4)2  -0.256250   0.300188  -0.854  0.39511   
## ns(time, 4)3  -0.424342   0.463240  -0.916  0.36160   
## ns(time, 4)4  -0.161496   0.204770  -0.789  0.43196   
## ns(tmean, 3)1  0.974544   0.314486   3.099  0.00245 **
## ns(tmean, 3)2  2.228649   0.779844   2.858  0.00508 **
## ns(tmean, 3)3  0.519953   0.453429   1.147  0.25392   
## is_weekend    -0.031001   0.088285  -0.351  0.72613   
## precip        -0.004389   0.005359  -0.819  0.41450   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 0.9540781)
## 
##     Null deviance: 129.88  on 123  degrees of freedom
## Residual deviance: 108.10  on 113  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 4
```

``` r
summary(fit_sui_nonlinear)
```

```
## 
## Call:
## glm(formula = suicide ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 
##     3) + is_weekend + precip, family = quasipoisson(link = "log"), 
##     data = df)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)   
## (Intercept)    1.145531   0.415638   2.756  0.00684 **
## ns(pm25, 3)1  -0.012199   0.218571  -0.056  0.95559   
## ns(pm25, 3)2  -0.369700   0.427636  -0.865  0.38917   
## ns(pm25, 3)3   0.158129   0.354445   0.446  0.65637   
## ns(time, 4)1  -0.350083   0.487845  -0.718  0.47451   
## ns(time, 4)2  -0.327219   0.310337  -1.054  0.29399   
## ns(time, 4)3  -0.393869   0.463369  -0.850  0.39715   
## ns(time, 4)4  -0.095809   0.209856  -0.457  0.64889   
## ns(tmean, 3)1  1.033787   0.321506   3.215  0.00171 **
## ns(tmean, 3)2  1.970187   0.789624   2.495  0.01406 * 
## ns(tmean, 3)3  0.572451   0.467593   1.224  0.22345   
## is_weekend    -0.066440   0.091470  -0.726  0.46915   
## precip        -0.005562   0.005445  -1.022  0.30921   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 0.9532159)
## 
##     Null deviance: 129.88  on 123  degrees of freedom
## Residual deviance: 105.58  on 111  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 4
```

## Model comparison


``` r
model_compare <- data.frame(
  Model = c("MH linear", "MH nonlinear", "Suicide linear", "Suicide nonlinear"),
  Residual_Deviance = c(
    fit_mh_linear$deviance,
    fit_mh_nonlinear$deviance,
    fit_sui_linear$deviance,
    fit_sui_nonlinear$deviance
  ),
  DF_Residual = c(
    fit_mh_linear$df.residual,
    fit_mh_nonlinear$df.residual,
    fit_sui_linear$df.residual,
    fit_sui_nonlinear$df.residual
  ),
  Dispersion = c(
    summary(fit_mh_linear)$dispersion,
    summary(fit_mh_nonlinear)$dispersion,
    summary(fit_sui_linear)$dispersion,
    summary(fit_sui_nonlinear)$dispersion
  )
)

model_compare
```

```
##               Model Residual_Deviance DF_Residual Dispersion
## 1         MH linear          253.9137         113  2.2309826
## 2      MH nonlinear          244.1738         111  2.1798593
## 3    Suicide linear          108.1019         113  0.9540781
## 4 Suicide nonlinear          105.5798         111  0.9532159
```


``` r
drop1(fit_mh_nonlinear, test = "F")
```

```
## Single term deletions
## 
## Model:
## mental_health ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + 
##     precip
##              Df Deviance  F value    Pr(>F)    
## <none>            244.17                       
## ns(pm25, 3)   3   257.36   1.9974   0.11852    
## ns(time, 4)   4   262.11   2.0386   0.09384 .  
## ns(tmean, 3)  3   297.89   8.1394 6.018e-05 ***
## is_weekend    1   608.67 165.6982 < 2.2e-16 ***
## precip        1   258.01   6.2891   0.01359 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

``` r
drop1(fit_sui_nonlinear, test = "F")
```

```
## Single term deletions
## 
## Model:
## suicide ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + 
##     precip
##              Df Deviance F value   Pr(>F)   
## <none>            105.58                    
## ns(pm25, 3)   3   108.47  1.0130 0.389823   
## ns(time, 4)   4   107.45  0.4928 0.741031   
## ns(tmean, 3)  3   118.37  4.4823 0.005212 **
## is_weekend    1   106.09  0.5328 0.466964   
## precip        1   106.61  1.0816 0.300604   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Create exposure--response curve function


``` r
make_er_curve <- function(model, data, outcome_name,
                          pm_var = "pm25",
                          ref_value = median(data[[pm_var]], na.rm = TRUE),
                          n_points = 200) {

  pm_seq <- seq(min(data[[pm_var]], na.rm = TRUE),
                max(data[[pm_var]], na.rm = TRUE),
                length.out = n_points)

  newdata <- data.frame(
    pm25 = pm_seq,
    time = median(data$time, na.rm = TRUE),
    tmean = median(data$tmean, na.rm = TRUE),
    is_weekend = 0,
    precip = median(data$precip, na.rm = TRUE)
  )

  refdata <- newdata[1, ]
  refdata$pm25 <- ref_value

  X_new <- model.matrix(delete.response(terms(model)), newdata)
  X_ref <- model.matrix(delete.response(terms(model)), refdata)

  beta <- coef(model)
  V    <- vcov(model)

  diff_mat <- X_new - matrix(X_ref, nrow = nrow(X_new), ncol = ncol(X_new), byrow = TRUE)

  eta_diff <- as.vector(diff_mat %*% beta)
  se_diff  <- sqrt(diag(diff_mat %*% V %*% t(diff_mat)))

  data.frame(
    pm25 = pm_seq,
    RR = exp(eta_diff),
    lower = exp(eta_diff - 1.96 * se_diff),
    upper = exp(eta_diff + 1.96 * se_diff),
    outcome = outcome_name,
    ref_value = ref_value
  )
}

make_linear_curve <- function(model, data, outcome_name,
                              pm_var = "pm25",
                              ref_value = median(data[[pm_var]], na.rm = TRUE),
                              n_points = 200) {

  pm_seq <- seq(min(data[[pm_var]], na.rm = TRUE),
                max(data[[pm_var]], na.rm = TRUE),
                length.out = n_points)

  beta <- coef(model)["pm25"]
  se   <- sqrt(vcov(model)["pm25", "pm25"])

  eta_diff <- beta * (pm_seq - ref_value)
  se_diff  <- abs(pm_seq - ref_value) * se

  data.frame(
    pm25 = pm_seq,
    RR = exp(eta_diff),
    lower = exp(eta_diff - 1.96 * se_diff),
    upper = exp(eta_diff + 1.96 * se_diff),
    outcome = outcome_name,
    model_type = "Linear"
  )
}
```

## Generate exposure--response curves


``` r
curve_mh  <- make_er_curve(fit_mh_nonlinear, df, "Mental health")
curve_sui <- make_er_curve(fit_sui_nonlinear, df, "Suicide")
```

## Plot nonlinear exposure--response curves


``` r
ggplot(curve_mh, aes(x = pm25, y = RR)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  labs(
    title = "PM2.5 exposure-response curve: Mental health EMS calls",
    subtitle = paste0("Reference PM2.5 = ", round(unique(curve_mh$ref_value), 2), " \u03bcg/m^3"),
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-9-1.pdf)<!-- --> 


``` r
ggplot(curve_sui, aes(x = pm25, y = RR)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  labs(
    title = "PM2.5 exposure-response curve: Suicide-related EMS calls",
    subtitle = paste0("Reference PM2.5 = ", round(unique(curve_sui$ref_value), 2), " \u03bcg/m^3"),
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-10-1.pdf)<!-- --> 

## Compare linear vs nonlinear PM2.5 specification


``` r
curve_mh_linear  <- make_linear_curve(fit_mh_linear, df, "Mental health")
curve_sui_linear <- make_linear_curve(fit_sui_linear, df, "Suicide")

curve_mh_nl_plot  <- curve_mh  |> mutate(model_type = "Nonlinear")
curve_sui_nl_plot <- curve_sui |> mutate(model_type = "Nonlinear")

curve_compare_mh  <- bind_rows(curve_mh_linear, curve_mh_nl_plot)
curve_compare_sui <- bind_rows(curve_sui_linear, curve_sui_nl_plot)
```


``` r
ggplot(curve_compare_mh, aes(x = pm25, y = RR, color = model_type)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  labs(
    title = "Linear vs nonlinear PM2.5 relationship: Mental health calls",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio",
    color = "Model"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 


``` r
ggplot(curve_compare_sui, aes(x = pm25, y = RR, color = model_type)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2, color = "gray40") +
  labs(
    title = "Linear vs nonlinear PM2.5 relationship: Suicide-related calls",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio",
    color = "Model"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-13-1.pdf)<!-- --> 


## Sensitivity analysis 1: PM2.5 spline df = 4


``` r
fit_mh_nonlinear_df4 <- glm(
  mental_health ~ ns(pm25, 4) + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df
)

fit_sui_nonlinear_df4 <- glm(
  suicide ~ ns(pm25, 4) + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df
)

curve_mh_df4  <- make_er_curve(fit_mh_nonlinear_df4, df, "Mental health") |> mutate(df_pm = "df = 4")
curve_sui_df4 <- make_er_curve(fit_sui_nonlinear_df4, df, "Suicide") |> mutate(df_pm = "df = 4")

curve_mh_df3  <- curve_mh  |> mutate(df_pm = "df = 3")
curve_sui_df3 <- curve_sui |> mutate(df_pm = "df = 3")

curve_mh_sens  <- bind_rows(curve_mh_df3, curve_mh_df4)
curve_sui_sens <- bind_rows(curve_sui_df3, curve_sui_df4)
```


``` r
ggplot(curve_mh_sens, aes(pm25, RR, color = df_pm)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to PM2.5 spline df: Mental health",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio",
    color = "PM2.5 spline"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-15-1.pdf)<!-- --> 


``` r
ggplot(curve_sui_sens, aes(pm25, RR, color = df_pm)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to PM2.5 spline df: Suicide",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio",
    color = "PM2.5 spline"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-16-1.pdf)<!-- --> 

## Sensitivity analysis 2: exclude high PM2.5 days


``` r
pm95 <- quantile(df$pm25, 0.95, na.rm = TRUE)

df_no_high <- df |> filter(pm25 <= pm95)

fit_mh_no_high <- glm(
  mental_health ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df_no_high
)

fit_sui_no_high <- glm(
  suicide ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + precip,
  family = quasipoisson(link = "log"),
  data = df_no_high
)

curve_mh_no_high <- make_er_curve(fit_mh_no_high, df_no_high, "Mental health") |>
  mutate(sample = "Exclude >95th percentile")

curve_sui_no_high <- make_er_curve(fit_sui_no_high, df_no_high, "Suicide") |>
  mutate(sample = "Exclude >95th percentile")

curve_mh_main <- curve_mh |> mutate(sample = "Main sample")
curve_sui_main <- curve_sui |> mutate(sample = "Main sample")
```


``` r
ggplot(bind_rows(curve_mh_main, curve_mh_no_high), aes(pm25, RR, color = sample)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to extreme PM2.5 days: Mental health",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-18-1.pdf)<!-- --> 


``` r
ggplot(bind_rows(curve_sui_main, curve_sui_no_high), aes(pm25, RR, color = sample)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to extreme PM2.5 days: Suicide",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-19-1.pdf)<!-- --> 

## Sensitivity analysis 3: alternative time-trend df (3--6)


``` r
fit_time_sensitivity <- function(outcome, time_df, data) {
  form <- as.formula(
    paste0(outcome,
           " ~ ns(pm25, 3) + ns(time, ", time_df, ") + ns(tmean, 3) + is_weekend + precip")
  )

  glm(form, family = quasipoisson(link = "log"), data = data)
}

time_df_results_mh <- map_df(3:6, function(k) {
  mod <- fit_time_sensitivity("mental_health", k, df)
  curve <- make_er_curve(mod, df, "Mental health")
  curve$time_df <- paste0("time df = ", k)
  curve
})

time_df_results_sui <- map_df(3:6, function(k) {
  mod <- fit_time_sensitivity("suicide", k, df)
  curve <- make_er_curve(mod, df, "Suicide")
  curve$time_df <- paste0("time df = ", k)
  curve
})
```


``` r
ggplot(time_df_results_mh, aes(pm25, RR, color = time_df)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to time-trend df: Mental health",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio",
    color = "Time spline"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-21-1.pdf)<!-- --> 


``` r
ggplot(time_df_results_sui, aes(pm25, RR, color = time_df)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to time-trend df: Suicide",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio",
    color = "Time spline"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-22-1.pdf)<!-- --> 

## Sensitivity analysis 4: nonlinear precipitation


``` r
fit_mh_nl_precip <- glm(
  mental_health ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + ns(precip, 3),
  family = quasipoisson(link = "log"),
  data = df
)

fit_sui_nl_precip <- glm(
  suicide ~ ns(pm25, 3) + ns(time, 4) + ns(tmean, 3) + is_weekend + ns(precip, 3),
  family = quasipoisson(link = "log"),
  data = df
)

curve_mh_nl_precip <- make_er_curve(fit_mh_nl_precip, df, "Mental health") |>
  mutate(model = "Nonlinear precip")

curve_sui_nl_precip <- make_er_curve(fit_sui_nl_precip, df, "Suicide") |>
  mutate(model = "Nonlinear precip")

curve_mh_main2 <- curve_mh |> mutate(model = "Linear precip")
curve_sui_main2 <- curve_sui |> mutate(model = "Linear precip")
```


``` r
ggplot(bind_rows(curve_mh_main2, curve_mh_nl_precip), aes(pm25, RR, color = model)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to precipitation functional form: Mental health",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-24-1.pdf)<!-- --> 


``` r
ggplot(bind_rows(curve_sui_main2, curve_sui_nl_precip), aes(pm25, RR, color = model)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Sensitivity to precipitation functional form: Suicide",
    x = "PM2.5 (\u03bcg/m^3)",
    y = "Rate ratio"
  ) +
  theme_bw()
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-25-1.pdf)<!-- --> 


## Results section outline

### Nonlinear exposure–response analysis

I fit quasi-Poisson time-series models with a natural spline for same-day PM2.5, adjusting for seasonal trend, temperature, weekend, and precipitation, and compared the nonlinear PM2.5 specification with the corresponding linear model for both outcomes.

### Mental health EMS calls

For mental health EMS calls, the adjusted nonlinear exposure–response curve was close to null over most of the observed PM2.5 range. The curve showed only a modest mid-range rise followed by a gradual decline at higher concentrations, with no clear threshold or strong positive nonlinear pattern. Confidence intervals widened at the tails, especially at higher PM2.5 levels.

### Suicide-related EMS calls

For suicide-related EMS calls, the nonlinear curve decreased toward the reference value at lower PM2.5 concentrations and then gradually increased at higher concentrations. However, confidence intervals were wide throughout, particularly at the upper end, indicating substantial uncertainty.

### Comparison of linear and nonlinear PM2.5 specifications

For mental health EMS calls, the linear and nonlinear models gave similar overall conclusions, with no strong positive association. For suicide-related EMS calls, both models suggested higher risk at higher PM2.5 levels, but the nonlinear model showed more curvature.

### Sensitivity analyses

For mental health EMS calls, the curve was highly stable across sensitivity analyses, including alternative PM2.5 spline degrees of freedom, time-trend adjustment, and precipitation specification. Excluding PM2.5 values above the 95th percentile reduced the downward trend at the highest concentrations, suggesting some influence of extreme exposure days.

For suicide-related EMS calls, the overall pattern was also similar across sensitivity analyses, with a dip near the reference value followed by an upward trend at higher PM2.5 levels, although the exact shape was somewhat less stable than for mental health EMS calls.


# Quasi-Poisson Time Series GLM (Yiming Cao)

## Model fitting

Using the same data loaded above. The main model keeps PM2.5 as a linear exposure term — the goal here is to estimate the rate ratio per 10 μg/m³ increase, adjusting for seasonal trend, temperature, weekend effect, and precipitation.


``` r
library(splines)

# time variable as days since Jan 1
df$time_days <- as.numeric(df$date - min(df$date))

# mental health model
fit_qp_mh <- glm(mental_health ~ pm25 + ns(time_days, 4) + ns(tmean, 3) +
                    is_weekend + precip,
                  family = quasipoisson(link = "log"),
                  data = df)

# suicide model
fit_qp_sui <- glm(suicide ~ pm25 + ns(time_days, 4) + ns(tmean, 3) +
                     is_weekend + precip,
                   family = quasipoisson(link = "log"),
                   data = df)
```


``` r
summary(fit_qp_mh)
```

```
## 
## Call:
## glm(formula = mental_health ~ pm25 + ns(time_days, 4) + ns(tmean, 
##     3) + is_weekend + precip, family = quasipoisson(link = "log"), 
##     data = df)
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        6.1066967  0.0481988 126.698  < 2e-16 ***
## pm25              -0.0026775  0.0022112  -1.211  0.22845    
## ns(time_days, 4)1 -0.1815532  0.0759208  -2.391  0.01844 *  
## ns(time_days, 4)2 -0.1248743  0.0477825  -2.613  0.01019 *  
## ns(time_days, 4)3 -0.0700981  0.0718951  -0.975  0.33164    
## ns(time_days, 4)4 -0.0340114  0.0309432  -1.099  0.27404    
## ns(tmean, 3)1      0.2261008  0.0482952   4.682 7.97e-06 ***
## ns(tmean, 3)2      0.4913475  0.1065191   4.613 1.05e-05 ***
## ns(tmean, 3)3      0.2444941  0.0726428   3.366  0.00104 ** 
## is_weekend        -0.1859232  0.0145624 -12.767  < 2e-16 ***
## precip            -0.0020202  0.0008409  -2.402  0.01792 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 2.238154)
## 
##     Null deviance: 727.73  on 123  degrees of freedom
## Residual deviance: 254.69  on 113  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 3
```


``` r
summary(fit_qp_sui)
```

```
## 
## Call:
## glm(formula = suicide ~ pm25 + ns(time_days, 4) + ns(tmean, 3) + 
##     is_weekend + precip, family = quasipoisson(link = "log"), 
##     data = df)
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)   
## (Intercept)        0.734732   0.350062   2.099  0.03806 * 
## pm25               0.008530   0.013405   0.636  0.52584   
## ns(time_days, 4)1 -0.296227   0.468428  -0.632  0.52841   
## ns(time_days, 4)2 -0.247127   0.297292  -0.831  0.40758   
## ns(time_days, 4)3 -0.424140   0.464401  -0.913  0.36303   
## ns(time_days, 4)4 -0.164994   0.205850  -0.802  0.42451   
## ns(tmean, 3)1      0.967794   0.315754   3.065  0.00272 **
## ns(tmean, 3)2      2.222648   0.781310   2.845  0.00528 **
## ns(tmean, 3)3      0.508253   0.452109   1.124  0.26332   
## is_weekend        -0.031116   0.088301  -0.352  0.72521   
## precip            -0.004363   0.005364  -0.813  0.41775   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 0.9544157)
## 
##     Null deviance: 129.88  on 123  degrees of freedom
## Residual deviance: 108.12  on 113  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 4
```

## Dispersion check


``` r
cat("Mental health dispersion:", round(summary(fit_qp_mh)$dispersion, 2), "\n")
```

```
## Mental health dispersion: 2.24
```

``` r
cat("Suicide dispersion:", round(summary(fit_qp_sui)$dispersion, 2), "\n")
```

```
## Suicide dispersion: 0.95
```

## Rate ratios per 10 μg/m³ PM2.5


``` r
extract_rr <- function(model, label) {
  b <- coef(model)["pm25"]
  se <- sqrt(vcov(model)["pm25", "pm25"])
  data.frame(
    outcome = label,
    RR = exp(b * 10),
    lower = exp((b - 1.96 * se) * 10),
    upper = exp((b + 1.96 * se) * 10),
    p = summary(model)$coefficients["pm25", 4]
  )
}

rr_table <- rbind(
  extract_rr(fit_qp_mh, "Mental health"),
  extract_rr(fit_qp_sui, "Suicide")
)
rr_table$RR <- round(rr_table$RR, 3)
rr_table$lower <- round(rr_table$lower, 3)
rr_table$upper <- round(rr_table$upper, 3)
rr_table$p <- round(rr_table$p, 3)
rr_table
```

```
##             outcome    RR lower upper     p
## pm25  Mental health 0.974 0.932 1.017 0.228
## pm251       Suicide 1.089 0.837 1.416 0.526
```

## Rate ratio forest plot


``` r
ggplot(rr_table, aes(x = RR, y = outcome)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.15) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  labs(x = "Rate ratio per 10 μg/m³ PM2.5", y = NULL,
       title = "PM2.5 association with EMS outcomes (Quasi-Poisson GLM)") +
  theme_bw(base_size = 11)
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-31-1.pdf)<!-- --> 

## Sensitivity analysis: time trend df (3–6)


``` r
sens_df <- data.frame()
for (d in 3:6) {
  fit_tmp <- glm(mental_health ~ pm25 + ns(time_days, d) + ns(tmean, 3) +
                   is_weekend + precip,
                 family = quasipoisson, data = df)
  b <- coef(fit_tmp)["pm25"]
  se <- sqrt(vcov(fit_tmp)["pm25", "pm25"])

  sens_df <- rbind(sens_df, data.frame(
    df_time = d,
    outcome = "Mental health",
    RR = exp(b * 10),
    lower = exp((b - 1.96 * se) * 10),
    upper = exp((b + 1.96 * se) * 10)
  ))

  fit_tmp2 <- glm(suicide ~ pm25 + ns(time_days, d) + ns(tmean, 3) +
                     is_weekend + precip,
                   family = quasipoisson, data = df)
  b2 <- coef(fit_tmp2)["pm25"]
  se2 <- sqrt(vcov(fit_tmp2)["pm25", "pm25"])

  sens_df <- rbind(sens_df, data.frame(
    df_time = d,
    outcome = "Suicide",
    RR = exp(b2 * 10),
    lower = exp((b2 - 1.96 * se2) * 10),
    upper = exp((b2 + 1.96 * se2) * 10)
  ))
}
```


``` r
ggplot(sens_df, aes(x = factor(df_time), y = RR)) +
  geom_point(size = 2.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  facet_wrap(~outcome, scales = "free_y") +
  labs(x = "Time trend spline df", y = "Rate ratio per 10 μg/m³ PM2.5",
       title = "Sensitivity of PM2.5 effect to time trend flexibility") +
  theme_bw(base_size = 11)
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-33-1.pdf)<!-- --> 

## Sensitivity analysis: exclude high PM2.5 days


``` r
pm95_cutoff <- quantile(df$pm25, 0.95)
df_trimmed <- df[df$pm25 <= pm95_cutoff, ]
cat("Excluded", nrow(df) - nrow(df_trimmed), "days with PM2.5 >",
    round(pm95_cutoff, 1), "μg/m³\n")
```

```
## Excluded 7 days with PM2.5 > 12.9 μg/m³
```

``` r
fit_trim_mh <- glm(mental_health ~ pm25 + ns(time_days, 4) + ns(tmean, 3) +
                      is_weekend + precip,
                    family = quasipoisson, data = df_trimmed)
fit_trim_sui <- glm(suicide ~ pm25 + ns(time_days, 4) + ns(tmean, 3) +
                       is_weekend + precip,
                     family = quasipoisson, data = df_trimmed)

rr_trim <- rbind(
  extract_rr(fit_trim_mh, "Mental health (trimmed)"),
  extract_rr(fit_trim_sui, "Suicide (trimmed)")
)
rr_trim$RR <- round(rr_trim$RR, 3)
rr_trim$lower <- round(rr_trim$lower, 3)
rr_trim$upper <- round(rr_trim$upper, 3)
rr_trim$p <- round(rr_trim$p, 3)

rbind(rr_table, rr_trim)
```

```
##                        outcome    RR lower upper     p
## pm25             Mental health 0.974 0.932 1.017 0.228
## pm251                  Suicide 1.089 0.837 1.416 0.526
## pm252  Mental health (trimmed) 1.015 0.956 1.077 0.629
## pm2511       Suicide (trimmed) 1.023 0.699 1.498 0.908
```

## Residual diagnostics


``` r
par(mfrow = c(1, 2))
plot(fitted(fit_qp_mh), residuals(fit_qp_mh, type = "deviance"),
     xlab = "Fitted", ylab = "Deviance residuals",
     main = "Mental health: residuals vs fitted", pch = 16, cex = 0.7)
abline(h = 0, lty = 2)

plot(df$date, residuals(fit_qp_mh, type = "deviance"),
     xlab = "Date", ylab = "Deviance residuals",
     main = "Mental health: residuals over time", pch = 16, cex = 0.7)
abline(h = 0, lty = 2)
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-35-1.pdf)<!-- --> 


``` r
pacf(residuals(fit_qp_mh, type = "deviance"),
     main = "PACF of deviance residuals (mental health)")
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-36-1.pdf)<!-- --> 


``` r
par(mfrow = c(1, 2))
plot(fitted(fit_qp_sui), residuals(fit_qp_sui, type = "deviance"),
     xlab = "Fitted", ylab = "Deviance residuals",
     main = "Suicide: residuals vs fitted", pch = 16, cex = 0.7)
abline(h = 0, lty = 2)

plot(df$date, residuals(fit_qp_sui, type = "deviance"),
     xlab = "Date", ylab = "Deviance residuals",
     main = "Suicide: residuals over time", pch = 16, cex = 0.7)
abline(h = 0, lty = 2)
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-37-1.pdf)<!-- --> 

## Predicted vs observed


``` r
df$pred_mh <- fitted(fit_qp_mh)

ggplot(df, aes(x = date)) +
  geom_point(aes(y = mental_health), alpha = 0.4, size = 1.2, color = "gray40") +
  geom_line(aes(y = pred_mh), color = "#E15759", linewidth = 0.8) +
  labs(x = "Date", y = "Daily count",
       title = "Mental health calls: observed vs quasi-Poisson fitted values") +
  theme_bw(base_size = 11)
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-38-1.pdf)<!-- --> 

## Results section outline (Quasi-Poisson GLM)

The quasi-Poisson GLM estimated a rate ratio of 0.974 (95% CI: 0.932–1.017) per 10 μg/m³ increase in same-day PM2.5 for mental health EMS calls, after adjusting for seasonal trend, temperature, weekend, and precipitation (p = 0.228). For suicide-related calls, the estimated rate ratio was 1.089 (95% CI: 0.837–1.416, p = 0.526). Neither association was statistically significant.

Key points for the results write-up:

- The dispersion parameter for mental health was 2.24, confirming overdispersion relative to standard Poisson. For suicide calls the dispersion was 0.95, close to 1.
- The PM2.5 coefficient was not statistically significant for either outcome.
- Weekend indicator was a strong predictor for mental health calls (lower on weekends, p < 0.001), consistent with the EDA.
- Sensitivity analysis: the PM2.5 rate ratio for mental health was stable across time trend df = 3–6 (ranging from 0.972 to 0.976). However, excluding high-PM2.5 days (>95th percentile) shifted the estimate from 0.974 to 1.015, suggesting the slight negative association in the main model may be driven by a few high-exposure days.
- Residual diagnostics: check PACF plot for any remaining temporal autocorrelation after the time spline adjustment.





Case-crossover--Teresa Sha


``` r
library(ggplot2)
library(dplyr)
library(lubridate)
library(broom)
```



``` r
############################################################
## case_crossover_sensitivity.R
## Sensitivity analysis:
## time-stratified case-crossover using conditional Poisson
############################################################

## ---------------------------
## 0. packages
## ---------------------------
packages <- c("dplyr", "lubridate", "splines", "gnm", "broom")

to_install <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(lubridate)
library(splines)
library(gnm)
library(broom)
```


``` r
## ---------------------------
## 1. read data
## ---------------------------
## Choose ONE path that matches your own folder structure.

## Option A: if your script is inside the project folder
## and analysis_2024.csv is in data/
file_path <- "data/analysis_2024.csv"

## Option B: if analysis_2024.csv is in the same folder as this script
## file_path <- "analysis_2024.csv"

## Option C: if you want to use full local path on your computer
## file_path <- "/Users/yourname/Desktop/aa_project/data/analysis_2024.csv"

df <- read.csv(file_path)
```


``` r
## ---------------------------
## 2. basic cleaning
## ---------------------------
df <- df %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month_num = month(date),
    dow_num = wday(date, week_start = 1),  # Monday = 1
    dow_name = factor(
      dow_num,
      levels = 1:7,
      labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    )
  ) %>%
  arrange(date)

## Quick check
cat("Rows:", nrow(df), "\n")
```

```
## Rows: 124
```

``` r
cat("Date range:", as.character(min(df$date)), "to", as.character(max(df$date)), "\n")
```

```
## Date range: 2024-01-01 to 2024-12-29
```

``` r
print(names(df))
```

```
##  [1] "date"          "pm25"          "mental_health" "suicide"      
##  [5] "tmax"          "tmin"          "tmean"         "precip"       
##  [9] "windspeed"     "rh_max"        "month"         "weekday"      
## [13] "is_weekend"    "heat_day"      "year"          "month_num"    
## [17] "dow_num"       "dow_name"
```


``` r
## ---------------------------
## 3. create case-crossover strata
## ---------------------------
## Time-stratified control selection:
## same year + same month + same day of week
## This matches the usual case-crossover setup.

df <- df %>%
  mutate(
    stratum = interaction(year, month_num, dow_name, drop = TRUE)
  )

## Keep only strata with at least 2 observed days
## because strata with only 1 row do not contribute within-stratum comparison
df_cc <- df %>%
  group_by(stratum) %>%
  mutate(n_in_stratum = n()) %>%
  ungroup() %>%
  filter(n_in_stratum >= 2)

cat("Rows after dropping singleton strata:", nrow(df_cc), "\n")
```

```
## Rows after dropping singleton strata: 79
```

``` r
cat("Number of strata kept:", n_distinct(df_cc$stratum), "\n")
```

```
## Number of strata kept: 39
```


``` r
## ---------------------------
## 4. create lag variables
## ---------------------------
## Since PM2.5 is measured only on selected days,
## these lags are based on previous observed row in the merged dataset.
## For this sensitivity analysis, lag 0 and lag 1 are enough.

df_cc <- df_cc %>%
  arrange(date) %>%
  mutate(
    pm25_lag1 = lag(pm25, 1),
    pm25_lag2 = lag(pm25, 2),
    tmean_lag1 = lag(tmean, 1),
    rh_max_lag1 = lag(rh_max, 1)
  )

## Datasets for lag models
df_cc_lag1 <- df_cc %>%
  filter(!is.na(pm25_lag1), !is.na(tmean_lag1), !is.na(rh_max_lag1))

df_cc_lag2 <- df_cc %>%
  filter(!is.na(pm25_lag1), !is.na(pm25_lag2),
         !is.na(tmean_lag1), !is.na(rh_max_lag1))
```


``` r
## ---------------------------
## 5. helper function for IRR output
## ---------------------------
get_irr <- function(model, exposure_name = "pm25", increment = 1) {
  beta <- coef(model)[exposure_name]
  se   <- sqrt(vcov(model)[exposure_name, exposure_name])

  est  <- exp(beta * increment)
  low  <- exp((beta - 1.96 * se) * increment)
  high <- exp((beta + 1.96 * se) * increment)

  out <- data.frame(
    term = exposure_name,
    increment = increment,
    IRR = est,
    CI_low = low,
    CI_high = high
  )
  return(out)
}
```


``` r
## ---------------------------
## 6. main conditional Poisson models
## ---------------------------
## Outcome 1: mental_health
## Model 1: lag 0 only
m_mh_lag0 <- gnm(
  mental_health ~ pm25 + ns(tmean, df = 3) + ns(rh_max, df = 3),
  eliminate = stratum,
  family = quasipoisson(link = "log"),
  data = df_cc
)
```


``` r
## Model 2: lag 0 + lag 1
m_mh_lag01 <- gnm(
  mental_health ~ pm25 + pm25_lag1 + ns(tmean, df = 3) + ns(rh_max, df = 3),
  eliminate = stratum,
  family = quasipoisson(link = "log"),
  data = df_cc_lag1
)
```


``` r
## Model 3: interaction with heat_day
m_mh_interaction <- gnm(
  mental_health ~ pm25 * heat_day + ns(tmean, df = 3) + ns(rh_max, df = 3),
  eliminate = stratum,
  family = quasipoisson(link = "log"),
  data = df_cc
)
```


``` r
## Outcome 2: suicide
## Model 4: lag 0 only
m_su_lag0 <- gnm(
  suicide ~ pm25 + ns(tmean, df = 3) + ns(rh_max, df = 3),
  eliminate = stratum,
  family = quasipoisson(link = "log"),
  data = df_cc
)
```


``` r
## Model 5: lag 0 + lag 1
m_su_lag01 <- gnm(
  suicide ~ pm25 + pm25_lag1 + ns(tmean, df = 3) + ns(rh_max, df = 3),
  eliminate = stratum,
  family = quasipoisson(link = "log"),
  data = df_cc_lag1
)
```


``` r
## ---------------------------
## 7. optional: two-pollutant-style lag sensitivity
## ---------------------------
## This one is optional. Use only if you want one extra robustness check.
m_mh_lag012 <- gnm(
  mental_health ~ pm25 + pm25_lag1 + pm25_lag2 +
    ns(tmean, df = 3) + ns(rh_max, df = 3),
  eliminate = stratum,
  family = quasipoisson(link = "log"),
  data = df_cc_lag2
)
```


``` r
## ---------------------------
## 8. model summaries
## ---------------------------
cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("Mental health model: lag 0\n")
```

```
## Mental health model: lag 0
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(summary(m_mh_lag0))
```

```
## 
## Call:
## gnm(formula = mental_health ~ pm25 + ns(tmean, df = 3) + ns(rh_max, 
##     df = 3), eliminate = stratum, family = quasipoisson(link = "log"), 
##     data = df_cc)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4181  -0.7097  -0.0283   0.6894   2.3079  
## 
## Coefficients of interest:
##                      Estimate Std. Error t value Pr(>|t|)  
## pm25                -0.001336   0.004401  -0.304   0.7634  
## ns(tmean, df = 3)1   0.228609   0.113062   2.022   0.0513 .
## ns(tmean, df = 3)2   0.607092   0.236943   2.562   0.0152 *
## ns(tmean, df = 3)3   0.235283   0.125015   1.882   0.0687 .
## ns(rh_max, df = 3)1 -0.052486   0.049808  -1.054   0.2996  
## ns(rh_max, df = 3)2 -0.037057   0.164707  -0.225   0.8234  
## ns(rh_max, df = 3)3 -0.059026   0.043303  -1.363   0.1821  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 3.018922)
## 
## Residual deviance: 99.724 on 33 degrees of freedom
## AIC: NA
## 
## Number of iterations: 2
```

``` r
cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("Mental health model: lag 0 + lag 1\n")
```

```
## Mental health model: lag 0 + lag 1
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(summary(m_mh_lag01))
```

```
## 
## Call:
## gnm(formula = mental_health ~ pm25 + pm25_lag1 + ns(tmean, df = 3) + 
##     ns(rh_max, df = 3), eliminate = stratum, family = quasipoisson(link = "log"), 
##     data = df_cc_lag1)
## 
## Deviance Residuals: 
##       Min         1Q     Median         3Q        Max  
## -2.356804  -0.621264  -0.001059   0.593190   2.245231  
## 
## Coefficients of interest:
##                       Estimate Std. Error t value Pr(>|t|)   
## pm25                -0.0009464  0.0044853  -0.211  0.83426   
## pm25_lag1            0.0011654  0.0037695   0.309  0.75927   
## ns(tmean, df = 3)1   0.2596876  0.1142668   2.273  0.03013 * 
## ns(tmean, df = 3)2   0.7190841  0.2441077   2.946  0.00607 **
## ns(tmean, df = 3)3   0.2343291  0.1281730   1.828  0.07715 . 
## ns(rh_max, df = 3)1 -0.0363880  0.0496487  -0.733  0.46912   
## ns(rh_max, df = 3)2 -0.0782739  0.1638991  -0.478  0.63630   
## ns(rh_max, df = 3)3 -0.0566455  0.0428757  -1.321  0.19612   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 2.896589)
## 
## Residual deviance: 89.881 on 31 degrees of freedom
## AIC: NA
## 
## Number of iterations: 2
```

``` r
cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("Mental health model: PM2.5 * heat_day\n")
```

```
## Mental health model: PM2.5 * heat_day
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(summary(m_mh_interaction))
```

```
## 
## Call:
## gnm(formula = mental_health ~ pm25 * heat_day + ns(tmean, df = 3) + 
##     ns(rh_max, df = 3), eliminate = stratum, family = quasipoisson(link = "log"), 
##     data = df_cc)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.40868  -0.71331   0.03417   0.68187   2.29825  
## 
## Coefficients of interest:
##                      Estimate Std. Error t value Pr(>|t|)  
## pm25                -0.001568   0.006838  -0.229   0.8202  
## heat_day            -0.016494   0.122033  -0.135   0.8934  
## ns(tmean, df = 3)1   0.228188   0.124671   1.830   0.0768 .
## ns(tmean, df = 3)2   0.617295   0.259168   2.382   0.0235 *
## ns(tmean, df = 3)3   0.249308   0.170699   1.461   0.1542  
## ns(rh_max, df = 3)1 -0.050795   0.052916  -0.960   0.3445  
## ns(rh_max, df = 3)2 -0.032444   0.173362  -0.187   0.8528  
## ns(rh_max, df = 3)3 -0.059834   0.046012  -1.300   0.2031  
## pm25:heat_day        0.000676   0.009116   0.074   0.9414  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 3.211766)
## 
## Residual deviance: 99.665 on 31 degrees of freedom
## AIC: NA
## 
## Number of iterations: 2
```

``` r
cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("Suicide model: lag 0\n")
```

```
## Suicide model: lag 0
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(summary(m_su_lag0))
```

```
## 
## Call:
## gnm(formula = suicide ~ pm25 + ns(tmean, df = 3) + ns(rh_max, 
##     df = 3), eliminate = stratum, family = quasipoisson(link = "log"), 
##     data = df_cc)
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -2.003e+00  -4.045e-01   6.951e-05   3.900e-01   1.492e+00  
## 
## Coefficients of interest:
##                      Estimate Std. Error t value Pr(>|t|)
## pm25                 0.027971   0.021892   1.278    0.210
## ns(tmean, df = 3)1   0.375105   0.584034   0.642    0.525
## ns(tmean, df = 3)2   1.989290   1.371568   1.450    0.156
## ns(tmean, df = 3)3  -0.258564   0.624833  -0.414    0.682
## ns(rh_max, df = 3)1 -0.008876   0.263953  -0.034    0.973
## ns(rh_max, df = 3)2  0.187422   0.884855   0.212    0.834
## ns(rh_max, df = 3)3 -0.277068   0.227031  -1.220    0.231
## 
## (Dispersion parameter for quasipoisson family taken to be 0.9043248)
## 
## Residual deviance: 31.777 on 33 degrees of freedom
## AIC: NA
## 
## Number of iterations: 3
```

``` r
cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("Suicide model: lag 0 + lag 1\n")
```

```
## Suicide model: lag 0 + lag 1
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(summary(m_su_lag01))
```

```
## 
## Call:
## gnm(formula = suicide ~ pm25 + pm25_lag1 + ns(tmean, df = 3) + 
##     ns(rh_max, df = 3), eliminate = stratum, family = quasipoisson(link = "log"), 
##     data = df_cc_lag1)
## 
## Deviance Residuals: 
##       Min         1Q     Median         3Q        Max  
## -2.081054  -0.433767   0.006725   0.386493   1.600515  
## 
## Coefficients of interest:
##                     Estimate Std. Error t value Pr(>|t|)  
## pm25                 0.03884    0.02287   1.699   0.0994 .
## pm25_lag1            0.03036    0.02006   1.514   0.1402  
## ns(tmean, df = 3)1   0.16424    0.60389   0.272   0.7875  
## ns(tmean, df = 3)2   1.47155    1.42981   1.029   0.3114  
## ns(tmean, df = 3)3  -0.51265    0.64134  -0.799   0.4302  
## ns(rh_max, df = 3)1  0.01841    0.26880   0.068   0.9458  
## ns(rh_max, df = 3)2  0.04446    0.89961   0.049   0.9609  
## ns(rh_max, df = 3)3 -0.31777    0.22803  -1.394   0.1734  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 0.8970259)
## 
## Residual deviance: 29.479 on 31 degrees of freedom
## AIC: NA
## 
## Number of iterations: 2
```

``` r
cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("Mental health model: lag 0 + lag 1 + lag 2\n")
```

```
## Mental health model: lag 0 + lag 1 + lag 2
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(summary(m_mh_lag012))
```

```
## 
## Call:
## gnm(formula = mental_health ~ pm25 + pm25_lag1 + pm25_lag2 + 
##     ns(tmean, df = 3) + ns(rh_max, df = 3), eliminate = stratum, 
##     family = quasipoisson(link = "log"), data = df_cc_lag2)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3538  -0.6301   0.0000   0.6232   2.2422  
## 
## Coefficients of interest:
##                      Estimate Std. Error t value Pr(>|t|)   
## pm25                 0.000929   0.005214   0.178  0.85982   
## pm25_lag1            0.001119   0.003862   0.290  0.77396   
## pm25_lag2            0.003531   0.004430   0.797  0.43191   
## ns(tmean, df = 3)1   0.251940   0.119974   2.100  0.04455 * 
## ns(tmean, df = 3)2   0.756069   0.263116   2.874  0.00752 **
## ns(tmean, df = 3)3   0.214398   0.132799   1.614  0.11726   
## ns(rh_max, df = 3)1 -0.062144   0.061997  -1.002  0.32445   
## ns(rh_max, df = 3)2 -0.142861   0.190343  -0.751  0.45897   
## ns(rh_max, df = 3)3 -0.070973   0.048557  -1.462  0.15459   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasipoisson family taken to be 3.030036)
## 
## Residual deviance: 87.964 on 29 degrees of freedom
## AIC: NA
## 
## Number of iterations: 2
```


``` r
## ---------------------------
## 9. IRR tables
## ---------------------------
irr_results <- bind_rows(
  cbind(model = "mental_health_lag0_1ug", get_irr(m_mh_lag0, "pm25", 1)),
  cbind(model = "mental_health_lag0_5ug", get_irr(m_mh_lag0, "pm25", 5)),
  cbind(model = "mental_health_lag01_pm25_1ug", get_irr(m_mh_lag01, "pm25", 1)),
  cbind(model = "mental_health_lag01_pm25lag1_1ug", get_irr(m_mh_lag01, "pm25_lag1", 1)),
  cbind(model = "suicide_lag0_1ug", get_irr(m_su_lag0, "pm25", 1)),
  cbind(model = "suicide_lag0_5ug", get_irr(m_su_lag0, "pm25", 5)),
  cbind(model = "suicide_lag01_pm25_1ug", get_irr(m_su_lag01, "pm25", 1)),
  cbind(model = "suicide_lag01_pm25lag1_1ug", get_irr(m_su_lag01, "pm25_lag1", 1))
)

cat("\n==============================\n")
```

```
## 
## ==============================
```

``` r
cat("IRR results\n")
```

```
## IRR results
```

``` r
cat("==============================\n")
```

```
## ==============================
```

``` r
print(irr_results)
```

```
##                                          model      term increment       IRR
## pm25...1                mental_health_lag0_1ug      pm25         1 0.9986652
## pm25...2                mental_health_lag0_5ug      pm25         5 0.9933437
## pm25...3          mental_health_lag01_pm25_1ug      pm25         1 0.9990540
## pm25_lag1...4 mental_health_lag01_pm25lag1_1ug pm25_lag1         1 1.0011661
## pm25...5                      suicide_lag0_1ug      pm25         1 1.0283657
## pm25...6                      suicide_lag0_5ug      pm25         5 1.1501062
## pm25...7                suicide_lag01_pm25_1ug      pm25         1 1.0396042
## pm25_lag1...8       suicide_lag01_pm25lag1_1ug pm25_lag1         1 1.0308260
##                  CI_low  CI_high
## pm25...1      0.9900881 1.007317
## pm25...2      0.9514132 1.037122
## pm25...3      0.9903097 1.007876
## pm25_lag1...4 0.9937964 1.008590
## pm25...5      0.9851735 1.073452
## pm25...6      0.9280332 1.425320
## pm25...7      0.9940402 1.087257
## pm25_lag1...8 0.9910897 1.072156
```


``` r
## ---------------------------
## 10. save outputs
## ---------------------------
if (!dir.exists("results")) dir.create("results")

write.csv(
  irr_results,
  "results/case_crossover_irr_results.csv",
  row.names = FALSE
)

saveRDS(m_mh_lag0, "results/m_mh_lag0.rds")
saveRDS(m_mh_lag01, "results/m_mh_lag01.rds")
saveRDS(m_mh_interaction, "results/m_mh_interaction.rds")
saveRDS(m_su_lag0, "results/m_su_lag0.rds")
saveRDS(m_su_lag01, "results/m_su_lag01.rds")
saveRDS(m_mh_lag012, "results/m_mh_lag012.rds")

cat("\nSaved model outputs to /results folder.\n")
```

```
## 
## Saved model outputs to /results folder.
```


``` r
## ---------------------------
## 11. optional compact coefficient table
## ---------------------------
coef_table <- bind_rows(
  tidy(m_mh_lag0) %>% mutate(model = "m_mh_lag0"),
  tidy(m_mh_lag01) %>% mutate(model = "m_mh_lag01"),
  tidy(m_mh_interaction) %>% mutate(model = "m_mh_interaction"),
  tidy(m_su_lag0) %>% mutate(model = "m_su_lag0"),
  tidy(m_su_lag01) %>% mutate(model = "m_su_lag01"),
  tidy(m_mh_lag012) %>% mutate(model = "m_mh_lag012")
)

write.csv(
  coef_table,
  "results/case_crossover_coef_table.csv",
  row.names = FALSE
)

cat("Saved coefficient table.\n")
```

```
## Saved coefficient table.
```



``` r
#PM2.5 Time series
ggplot(df_cc, aes(x = date, y = pm25)) +
  geom_line(color = "#2C7FB8", linewidth = 0.7) +
  labs(
    title = "Daily PM2.5 Levels in Case-Crossover Dataset",
    x = "Date",
    y = expression("PM"[2.5] * " (µg/m³)")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-56-1.pdf)<!-- --> 


``` r
#Mental health outcome time series
ggplot(df_cc, aes(x = date, y = mental_health)) +
  geom_line(color = "#D95F02", linewidth = 0.7) +
  labs(
    title = "Daily Mental Health EMS Calls",
    x = "Date",
    y = "Number of Calls"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-57-1.pdf)<!-- --> 


``` r
#Suicide outcome time series
ggplot(df_cc, aes(x = date, y = suicide)) +
  geom_line(color = "#7570B3", linewidth = 0.7) +
  labs(
    title = "Daily Suicide-Related EMS Calls",
    x = "Date",
    y = "Number of Calls"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-58-1.pdf)<!-- --> 


``` r
#Monthly average PM2.5
df_pm25_month <- df_cc %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(mean_pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

ggplot(df_pm25_month, aes(x = month, y = mean_pm25)) +
  geom_line(color = "#1B9E77", linewidth = 0.8) +
  geom_point(color = "#1B9E77", size = 2) +
  labs(
    title = "Monthly Average PM2.5",
    x = "Month",
    y = expression("Mean PM"[2.5] * " (" * mu * "g/" * m^3 * ")")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-59-1.pdf)<!-- --> 


``` r
#PM2.5 by heat day
ggplot(df_cc, aes(x = factor(heat_day), y = pm25)) +
  geom_boxplot(fill = "#A6CEE3") +
  labs(
    title = "Distribution of PM2.5 by Heat Day",
    x = "Heat Day",
    y = expression("PM"[2.5] * " (" * mu * "g/" * m^3 * ")")
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-60-1.pdf)<!-- --> 


``` r
#Mental health calls by heat day
ggplot(df_cc, aes(x = factor(heat_day), y = mental_health)) +
  geom_boxplot(fill = "#FDBF6F") +
  labs(
    title = "Mental Health EMS Calls by Heat Day",
    x = "Heat Day",
    y = "Number of Calls"
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-61-1.pdf)<!-- --> 


``` r
#Suicide calls by heat day
ggplot(df_cc, aes(x = factor(heat_day), y = suicide)) +
  geom_boxplot(fill = "#CAB2D6") +
  labs(
    title = "Suicide-Related EMS Calls by Heat Day",
    x = "Heat Day",
    y = "Number of Calls"
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-62-1.pdf)<!-- --> 


``` r
irr_plot_data <- irr_results %>%
  filter(increment == 1) %>%
  mutate(
    model_label = c(
      "Mental health: lag 0",
      "Mental health: lag 0 + lag 1 (pm25)",
      "Mental health: lag 0 + lag 1 (pm25 lag1)",
      "Suicide: lag 0",
      "Suicide: lag 0 + lag 1 (pm25)",
      "Suicide: lag 0 + lag 1 (pm25 lag1)"
    )
  )
```


``` r
#IRR forest plot
ggplot(irr_plot_data, aes(x = IRR, y = reorder(model_label, IRR))) +
  geom_point(size = 3, color = "#1F78B4") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "#1F78B4") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "IRRs for PM2.5 from Case-Crossover Models",
    x = "Incidence Rate Ratio (95% CI)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-64-1.pdf)<!-- --> 


``` r
#Lag comparison plot for mental health
mh_lag_compare <- bind_rows(
  get_irr(m_mh_lag0, "pm25", 1) %>% mutate(model = "Lag 0"),
  get_irr(m_mh_lag01, "pm25", 1) %>% mutate(model = "Lag 0 + Lag 1"),
  get_irr(m_mh_lag012, "pm25", 1) %>% mutate(model = "Lag 0 + Lag 1 + Lag 2")
)

ggplot(mh_lag_compare, aes(x = IRR, y = reorder(model, IRR))) +
  geom_point(size = 3, color = "#33A02C") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "#33A02C") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Lag Structure Comparison for Mental Health Outcome",
    x = "IRR for a 1-unit Increase in PM2.5",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-65-1.pdf)<!-- --> 


``` r
#Heat day interaction plot
df_interaction_plot <- df_cc %>%
  group_by(heat_day) %>%
  summarise(
    mean_pm25 = mean(pm25, na.rm = TRUE),
    mean_mh = mean(mental_health, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(df_interaction_plot, aes(x = factor(heat_day), y = mean_mh)) +
  geom_col(fill = "#FB9A99") +
  labs(
    title = "Average Mental Health EMS Calls by Heat Day",
    x = "Heat Day",
    y = "Average Number of Calls"
  ) +
  scale_x_discrete(labels = c("0" = "No", "1" = "Yes")) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
```

![](Preliminary-results_files/figure-latex/unnamed-chunk-66-1.pdf)<!-- --> 


``` r
final_table <- irr_results %>%
  filter(increment == 1) %>%
  mutate(
    Outcome = case_when(
      grepl("mental", model) ~ "Mental health",
      grepl("suicide", model) ~ "Suicide"
    ),
    Model = case_when(
      grepl("lag012", model) ~ "Lag 0–2",
      grepl("lag01", model) & grepl("lag1", model) ~ "Lag 0–1 (lag 1 term)",
      grepl("lag01", model) ~ "Lag 0–1 (lag 0 term)",
      grepl("lag0", model) ~ "Lag 0"
    ),
    IRR = round(IRR, 3),
    `95% CI` = paste0("(", round(CI_low, 3), ", ", round(CI_high, 3), ")")
  ) %>%
  select(Outcome, Model, IRR, `95% CI`) %>%
  as.data.frame()

rownames(final_table) <- NULL

final_table
```

```
##         Outcome                Model   IRR         95% CI
## 1 Mental health                Lag 0 0.999  (0.99, 1.007)
## 2 Mental health Lag 0–1 (lag 0 term) 0.999  (0.99, 1.008)
## 3 Mental health Lag 0–1 (lag 1 term) 1.001 (0.994, 1.009)
## 4       Suicide                Lag 0 1.028 (0.985, 1.073)
## 5       Suicide Lag 0–1 (lag 0 term) 1.040 (0.994, 1.087)
## 6       Suicide Lag 0–1 (lag 1 term) 1.031 (0.991, 1.072)
```


``` r
knitr::kable(
  final_table,
  caption = "Table 1. Case-crossover model results for PM2.5 and EMS outcomes"
)
```



Table: Table 1. Case-crossover model results for PM2.5 and EMS outcomes

|Outcome       |Model                |   IRR|95% CI         |
|:-------------|:--------------------|-----:|:--------------|
|Mental health |Lag 0                | 0.999|(0.99, 1.007)  |
|Mental health |Lag 0–1 (lag 0 term) | 0.999|(0.99, 1.008)  |
|Mental health |Lag 0–1 (lag 1 term) | 1.001|(0.994, 1.009) |
|Suicide       |Lag 0                | 1.028|(0.985, 1.073) |
|Suicide       |Lag 0–1 (lag 0 term) | 1.040|(0.994, 1.087) |
|Suicide       |Lag 0–1 (lag 1 term) | 1.031|(0.991, 1.072) |

Case-crossover preliminary results：

Using a time-stratified case-crossover design with conditional Poisson regression, we examined the short-term association between PM2.5 and EMS outcomes. For mental health-related EMS calls, the estimated IRRs were close to 1 across lag 0 and lag 0–1 specifications, suggesting little evidence of a strong short-term association in the preliminary analysis. For suicide-related EMS calls, the point estimates were slightly above 1, but the confidence intervals still included 1, indicating that these results were imprecise. Overall, the preliminary case-crossover results did not show a clear association, but additional refinement of lag structure and model specification will be explored in the final analysis.

