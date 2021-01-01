---
title: "Data Exploration using the lung disease dataset"
author: "kd91"
date: "12/30/2020"
output: 
  html_document:
    keep_md: true
#rmarkdown::github_document
---
## Required libraries

```r
library(ggplot2)
```
## Importing Data into R

```r
setwd(getwd())

lung_data <- read.csv("lungDisease.csv", header=T)
summary(lung_data)
```

```
##      rvtlc       yrs_tot_smoke     yrs_smoke          age       
##  Min.   :16.00   Min.   : 0.00   Min.   : 0.00   Min.   :22.00  
##  1st Qu.:30.67   1st Qu.:13.50   1st Qu.: 0.00   1st Qu.:49.00  
##  Median :34.48   Median :23.00   Median :14.00   Median :58.00  
##  Mean   :34.36   Mean   :22.64   Mean   :13.44   Mean   :56.19  
##  3rd Qu.:37.97   3rd Qu.:34.00   3rd Qu.:23.20   3rd Qu.:63.00  
##  Max.   :50.35   Max.   :46.00   Max.   :41.00   Max.   :85.00  
##  NA's   :6
```
## data exploration

```r
g <- ggplot(data = lung_data)
g + geom_histogram(aes(x=rvtlc), bins = 30, color = "black") + ggtitle("Distribution of the rvtlc") + xlab("rvtlc") + ylab("Count")
```

```
## Warning: Removed 6 rows containing non-finite values (stat_bin).
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
g + geom_histogram(aes(x=yrs_tot_smoke), bins = 30, color = "blue") + ggtitle("Distribution of the total smoked years") + xlab("total smoked years") + ylab("Count")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
g + geom_density(aes(x=yrs_smoke),fill = "cyan") + ggtitle("Distribution of the no. of years smoked") + xlab("no. of years smoked") + ylab("Density")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
g + geom_histogram(aes(x=age), bins = 30, color = "red") + ggtitle("Distribution of patient's age (years)") + xlab("age (years)") + ylab("Count")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

```r
g + geom_point (aes(x=rvtlc, y=age)) + ggtitle("Distribution of rvtlc vs patient's age") +
  xlab("rvtlc") + ylab("patient's age")
```

```
## Warning: Removed 6 rows containing missing values (geom_point).
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-3-5.png)<!-- -->

```r
g + geom_point (aes(x=rvtlc, y=yrs_tot_smoke)) + ggtitle("Distribution of rvtlc vs total number of yrs smoked") +
  xlab("rvtlc") + ylab("total number of yrs smoked")
```

```
## Warning: Removed 6 rows containing missing values (geom_point).
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-3-6.png)<!-- -->

## Linear regression

```r
# linear main effects model
fit_main <- lm(rvtlc ~ yrs_tot_smoke + yrs_smoke + age, data=lung_data)
summary(fit_main)
```

```
## 
## Call:
## lm(formula = rvtlc ~ yrs_tot_smoke + yrs_smoke + age, data = lung_data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -11.336  -2.398  -0.118   2.334  12.729 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   10.43366    2.29048   4.555 1.01e-05 ***
## yrs_tot_smoke  0.04616    0.04510   1.024    0.308    
## yrs_smoke     -0.04498    0.06133  -0.733    0.464    
## age            0.41804    0.04360   9.588  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.021 on 165 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.4991,	Adjusted R-squared:   0.49 
## F-statistic: 54.81 on 3 and 165 DF,  p-value: < 2.2e-16
```

```r
# the model is not a good fit. the adjusted R squared value is very high (0.49).
# thus, the model explains only 49% variance of rvtlc as a fraction of total explanable variation.
# only one predictor variable : age, is statistically significant to the model. Its p-value is < 0.05.
# the model's overall p-value is less than 0.05, which could indicate model statistical significance.
# But the model is not statistically significant since all its response variables are not significant 
# and the adjusted r-squared values are very high.
# fitted coefficients interpretation:
# rvtlc = 0.04616 * yrs_tot_smoke - 0.04498 * yrs_smoke + 0.41804 * age + 10.43366
# For each year increase in yrs_tot_smoke, rvtlc (chances of COPD) increases on average by 0.04616 units, assuming all other variables are held constant
# For each year increase in yrs_smoke, rvtlc (chances of COPD) decreases on average by 0.04498 units, assuming all other variables are held constant
# For each year increase in age , rvtlc (chances of COPD) increases on average by 0.41804 units, assuming all other variables are held constant
```


```r
# linear model of named fit_smoke of variable rvtlc 
fit_smoke <- lm(rvtlc ~ yrs_smoke, data=lung_data)
summary(fit_smoke)
```

```
## 
## Call:
## lm(formula = rvtlc ~ yrs_smoke, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -15.2384  -3.2484  -0.2386   2.8548  16.3115 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 31.23842    0.59738   52.29  < 2e-16 ***
## yrs_smoke    0.23334    0.03416    6.83 1.51e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.993 on 167 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.2184,	Adjusted R-squared:  0.2137 
## F-statistic: 46.65 on 1 and 167 DF,  p-value: 1.506e-10
```

```r
# As compared to fit_main, in fit_smoke the effect of the years smoked reduces on rvtlc.
# the more number of years the patient has smoked, has less effect it has as compared to the total number of years smoked by the patient.
# for each year increase in yrs_smoke, chances of getting COPD (higher rvtlc) increases by  0.23334 units.
```


```r
# main effects model with 2nd order interactions with variable as rvtlc
fit_full <- lm(rvtlc ~ (yrs_tot_smoke + yrs_smoke + age)^2, data=lung_data)
summary(fit_full)
```

```
## 
## Call:
## lm(formula = rvtlc ~ (yrs_tot_smoke + yrs_smoke + age)^2, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4970  -2.4485  -0.3102   2.2507  11.7835 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             12.532467   3.193681   3.924 0.000128 ***
## yrs_tot_smoke           -0.013215   0.274950  -0.048 0.961725    
## yrs_smoke                0.025905   0.318295   0.081 0.935235    
## age                      0.359291   0.062032   5.792 3.52e-08 ***
## yrs_tot_smoke:yrs_smoke -0.007933   0.003430  -2.313 0.021976 *  
## yrs_tot_smoke:age        0.002206   0.005070   0.435 0.664052    
## yrs_smoke:age            0.002499   0.005385   0.464 0.643221    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.991 on 162 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.5157,	Adjusted R-squared:  0.4978 
## F-statistic: 28.75 on 6 and 162 DF,  p-value: < 2.2e-16
```

```r
# in fit main only age was  asignificant variable.
# this model indicates that besides from age, the interaction between yrs_tot_smoke & yrs_smoke is also statistically significant.
# the adjusted R sqaured value & p-value of fit_main & fit_full are very close.
# this model is not a good fit, and not very statistically significant.
```


```r
# removing insignificant variables one-by-one
fit_full <- lm(rvtlc ~ yrs_tot_smoke + yrs_smoke + age + yrs_tot_smoke:yrs_smoke + 
              yrs_tot_smoke:age + yrs_smoke:age, data=lung_data)
summary(fit_full)
```

```
## 
## Call:
## lm(formula = rvtlc ~ yrs_tot_smoke + yrs_smoke + age + yrs_tot_smoke:yrs_smoke + 
##     yrs_tot_smoke:age + yrs_smoke:age, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4970  -2.4485  -0.3102   2.2507  11.7835 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             12.532467   3.193681   3.924 0.000128 ***
## yrs_tot_smoke           -0.013215   0.274950  -0.048 0.961725    
## yrs_smoke                0.025905   0.318295   0.081 0.935235    
## age                      0.359291   0.062032   5.792 3.52e-08 ***
## yrs_tot_smoke:yrs_smoke -0.007933   0.003430  -2.313 0.021976 *  
## yrs_tot_smoke:age        0.002206   0.005070   0.435 0.664052    
## yrs_smoke:age            0.002499   0.005385   0.464 0.643221    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.991 on 162 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.5157,	Adjusted R-squared:  0.4978 
## F-statistic: 28.75 on 6 and 162 DF,  p-value: < 2.2e-16
```

```r
# removing yrs_tot_smoke from #4
fit1 <- lm(rvtlc ~ yrs_smoke + age + yrs_tot_smoke:yrs_smoke + 
             yrs_tot_smoke:age + yrs_smoke:age, data=lung_data)
summary(fit1)
```

```
## 
## Call:
## lm(formula = rvtlc ~ yrs_smoke + age + yrs_tot_smoke:yrs_smoke + 
##     yrs_tot_smoke:age + yrs_smoke:age, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4849  -2.4555  -0.2706   2.2730  11.7816 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             12.4405872  2.5505323   4.878 2.53e-06 ***
## yrs_smoke                0.0136731  0.1905767   0.072   0.9429    
## age                      0.3608863  0.0522505   6.907 1.05e-10 ***
## yrs_smoke:yrs_tot_smoke -0.0078898  0.0033005  -2.390   0.0180 *  
## age:yrs_tot_smoke        0.0019667  0.0009463   2.078   0.0393 *  
## yrs_smoke:age            0.0027028  0.0033156   0.815   0.4162    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.978 on 163 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.5157,	Adjusted R-squared:  0.5008 
## F-statistic: 34.71 on 5 and 163 DF,  p-value: < 2.2e-16
```

```r
# removing yrs_smoke
fit2 <- lm(rvtlc ~ age + yrs_tot_smoke:yrs_smoke + yrs_tot_smoke:age + yrs_smoke:age, data=lung_data)
summary(fit2)
```

```
## 
## Call:
## lm(formula = rvtlc ~ age + yrs_tot_smoke:yrs_smoke + yrs_tot_smoke:age + 
##     yrs_smoke:age, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4933  -2.4514  -0.2944   2.2737  11.7904 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             12.5009248  2.4005778   5.207 5.67e-07 ***
## age                      0.3598472  0.0500507   7.190 2.18e-11 ***
## yrs_tot_smoke:yrs_smoke -0.0078658  0.0032736  -2.403   0.0174 *  
## age:yrs_tot_smoke        0.0019729  0.0009395   2.100   0.0373 *  
## age:yrs_smoke            0.0029005  0.0018382   1.578   0.1165    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.966 on 164 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.5157,	Adjusted R-squared:  0.5039 
## F-statistic: 43.65 on 4 and 164 DF,  p-value: < 2.2e-16
```

```r
# removing yrs_smoke:age
fit_final <- lm(rvtlc ~ age + yrs_tot_smoke:yrs_smoke + yrs_tot_smoke:age, data=lung_data)
summary(fit_final)
```

```
## 
## Call:
## lm(formula = rvtlc ~ age + yrs_tot_smoke:yrs_smoke + yrs_tot_smoke:age, 
##     data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.8692  -2.5293  -0.2437   2.4085  12.4191 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             10.5355444  2.0613993   5.111 8.79e-07 ***
## age                      0.4066733  0.0404853  10.045  < 2e-16 ***
## yrs_tot_smoke:yrs_smoke -0.0036194  0.0018722  -1.933   0.0549 .  
## age:yrs_tot_smoke        0.0019010  0.0009426   2.017   0.0454 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.984 on 165 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.5083,	Adjusted R-squared:  0.4994 
## F-statistic: 56.86 on 3 and 165 DF,  p-value: < 2.2e-16
```

```r
# fitted model equation:
# rvtlc = 0.406 * age - 0.0036 * yrs_tot_smoke:yrs_smoke + 0.0019 * age:yrs_tot_smoke + 10.535
# For each year increase in age & yrs_tot_smoke, rvtlc increases on average by 0.0019 units, assuming all other variables are held constant
# For each year increase in yrs_tot_smoke & yrs_smoke, rvtlc decreases on average by 0.0036 units, assuming all other variables are held constant
# For each year increase in age , rvtlc increases on average by 0.406 units, assuming all other variables are held constant
```
#### adding a new variable for age-group to the df

```r
lung_data$has_disease <- ifelse(lung_data$rvtlc >= 35, 1, 0)

# visualising data with the new variable
g <- ggplot(data=lung_data)

g + geom_bar(aes(x=factor(has_disease)), color="brown") + ggtitle("Distribution of patients with and without disease") +  
  xlab("patient with(1) or without(0) disease") + ylab("Count")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
g + geom_histogram(aes(x=rvtlc, fill=factor(has_disease)), bins = 30, color = "black") + ggtitle("Distribution of the rvtlc") +  
  xlab("rvtlc") + ylab("Count")
```

```
## Warning: Removed 6 rows containing non-finite values (stat_bin).
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
# since rvtlc greater than 35 has disease (COPD), the histogram clearly indicates lower rvtlc without disease
# and rvtlc value higher than 35, having disease (has_disease= 1)

g + geom_histogram(aes(x=yrs_tot_smoke, fill=factor(has_disease)), bins = 20, color = "white") + 
  ggtitle("Distribution of total number of years smoked") + xlab("total no. of years smoked") + ylab("Count")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
# for patients with total number of years smoked more,have higher chances of having COPD(has_disease=1)
# than patients with less no. of years smoked

g + geom_density(aes(x=yrs_tot_smoke, fill=factor(has_disease)), alpha=0.5) + 
  ggtitle("Distrubution of years smoked in total for patients with & without disease (0=no COPD, 1=COPD)") +
  xlab("total years smoked") + ylab("density")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-4.png)<!-- -->

```r
g + geom_histogram(aes(x=yrs_smoke, fill=factor(has_disease)), bins = 15, color = "white") + 
  ggtitle("Distribution of number of years smoked") + xlab("no. of years smoked") + ylab("Count")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-5.png)<!-- -->

```r
# for patients with less no. of years smoked, there are less patients with COPD (has_disease=1),
# for more no. of years smoked, there are more patients with chances of having COPD

g + geom_boxplot(aes(x=factor(has_disease), y=yrs_smoke), color="blue") +
 ggtitle (" Distribution of patient having disease or not vs their no. of yrs smoked") +
  xlab("patient has disease(=1) or not (=0)") + ylab("no. of yrs smoked")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-6.png)<!-- -->

```r
g + geom_histogram(aes(x=age, fill=factor(has_disease)), bins = 20, color = "black") + 
  ggtitle("Distribution of age") + xlab("age (in years)") + ylab("Count")
```

![](DEA_lung_disease_files/figure-html/unnamed-chunk-8-7.png)<!-- -->

```r
# compared to younger patients, older patients have higher chances of having COPD (has_disease=1)
```
## Logistic Regression

```r
# glm model usign all predictor variables
fit_main_glm <- glm(has_disease ~ yrs_tot_smoke + yrs_smoke + age, family=binomial(), data = lung_data)
summary(fit_main_glm)
```

```
## 
## Call:
## glm(formula = has_disease ~ yrs_tot_smoke + yrs_smoke + age, 
##     family = binomial(), data = lung_data)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0201  -0.7053  -0.2906   0.8158   2.4162  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -10.62919    1.99111  -5.338 9.38e-08 ***
## yrs_tot_smoke   0.03427    0.02874   1.193    0.233    
## yrs_smoke      -0.01710    0.03857  -0.443    0.657    
## age             0.17342    0.03526   4.918 8.75e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 233.28  on 168  degrees of freedom
## Residual deviance: 166.09  on 165  degrees of freedom
##   (6 observations deleted due to missingness)
## AIC: 174.09
## 
## Number of Fisher Scoring iterations: 5
```

```r
# in both models, age is statistically significant to rvtlc (in model #2) and to has_disease in this model
# the AIC is low (174), which indicates good fit
# but all variables (except age) are not statistically significant ( p > 0.05)
# the null & residual deviance values have very less difference, which doesn't indicate a good overall model
```


```r
# using the step function to improve our logistic regression model

# upper model
fit_full <- lm(rvtlc ~ (yrs_tot_smoke + yrs_smoke + age)^2 , data=lung_data)
summary(fit_full)
```

```
## 
## Call:
## lm(formula = rvtlc ~ (yrs_tot_smoke + yrs_smoke + age)^2, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4970  -2.4485  -0.3102   2.2507  11.7835 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             12.532467   3.193681   3.924 0.000128 ***
## yrs_tot_smoke           -0.013215   0.274950  -0.048 0.961725    
## yrs_smoke                0.025905   0.318295   0.081 0.935235    
## age                      0.359291   0.062032   5.792 3.52e-08 ***
## yrs_tot_smoke:yrs_smoke -0.007933   0.003430  -2.313 0.021976 *  
## yrs_tot_smoke:age        0.002206   0.005070   0.435 0.664052    
## yrs_smoke:age            0.002499   0.005385   0.464 0.643221    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.991 on 162 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.5157,	Adjusted R-squared:  0.4978 
## F-statistic: 28.75 on 6 and 162 DF,  p-value: < 2.2e-16
```

```r
# Fit a regression model for the null model: rvtlc as a function of the intercept only.
fit_null <- lm(rvtlc ~ 1, data = lung_data)
summary(fit_null)
```

```
## 
## Call:
## lm(formula = rvtlc ~ 1, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -18.3635  -3.6935   0.1165   3.6065  15.9865 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  34.3635     0.4331   79.33   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 5.631 on 168 degrees of freedom
##   (6 observations deleted due to missingness)
```

```r
fit_step1 = step(fit_null, scope=list(lower=fit_null, upper=fit_full),direction="both")
```

```
## Start:  AIC=585.15
## rvtlc ~ 1
## 
##                 Df Sum of Sq    RSS    AIC
## + age            1   2641.36 2685.3 471.40
## + yrs_smoke      1   1163.14 4163.6 545.51
## + yrs_tot_smoke  1    747.74 4579.0 561.59
## <none>                       5326.7 585.15
## 
## Step:  AIC=471.4
## rvtlc ~ age
## 
##                 Df Sum of Sq    RSS    AIC
## <none>                       2685.3 471.40
## + yrs_tot_smoke  1      8.62 2676.7 472.85
## + yrs_smoke      1      0.38 2685.0 473.37
## - age            1   2641.36 5326.7 585.15
```

```r
summary(fit_step1)
```

```
## 
## Call:
## lm(formula = rvtlc ~ age, data = lung_data)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.7980  -2.3021  -0.2655   2.0609  12.3403 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 11.09240    1.84172   6.023 1.06e-08 ***
## age          0.41411    0.03231  12.817  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.01 on 167 degrees of freedom
##   (6 observations deleted due to missingness)
## Multiple R-squared:  0.4959,	Adjusted R-squared:  0.4929 
## F-statistic: 164.3 on 1 and 167 DF,  p-value: < 2.2e-16
```

```r
# AIC = 471.4 & p-value<0.05 indicate a good model than the results above 
# In our model, age seems to be the most significant variable with reponse to rvtlc
```


```r
# side_note: glm model with has_disease as a function of rvtlc only : Explains how correlated variables appear in the glm model
fit_glm <- glm(has_disease ~rvtlc, family=binomial(), data = lung_data)
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
summary(fit_glm)
```

```
## 
## Call:
## glm(formula = has_disease ~ rvtlc, family = binomial(), data = lung_data)
## 
## Deviance Residuals: 
##        Min          1Q      Median          3Q         Max  
## -5.929e-04  -2.000e-08  -2.000e-08   2.000e-08   3.065e-04  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -7549.5   504796.1  -0.015    0.988
## rvtlc          216.2    14457.1   0.015    0.988
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2.3328e+02  on 168  degrees of freedom
## Residual deviance: 6.3345e-07  on 167  degrees of freedom
##   (6 observations deleted due to missingness)
## AIC: 4
## 
## Number of Fisher Scoring iterations: 25
```

```r
# there is a warning message, due to perfect linear separation
# the variables : has_disease & rvtlc have high correlation 
# since the variable has_disease is a derived variable from values of rvtlc
# the variable rvtlc has a very high p-value (0.988), and no statistical significance
# the AIC is very less , but the null & residual deviances have very high difference
# all indicating that the model has perfectly linear relationship and that another model should be considered
```
