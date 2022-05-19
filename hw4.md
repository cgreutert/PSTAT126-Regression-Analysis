Homework Assignment 4
================
Carly Greutert (8408916)
11 March 2022

1.  

``` r
library(ISLR)
head(Auto)
```

    ##   mpg cylinders displacement horsepower weight acceleration year origin
    ## 1  18         8          307        130   3504         12.0   70      1
    ## 2  15         8          350        165   3693         11.5   70      1
    ## 3  18         8          318        150   3436         11.0   70      1
    ## 4  16         8          304        150   3433         12.0   70      1
    ## 5  17         8          302        140   3449         10.5   70      1
    ## 6  15         8          429        198   4341         10.0   70      1
    ##                        name
    ## 1 chevrolet chevelle malibu
    ## 2         buick skylark 320
    ## 3        plymouth satellite
    ## 4             amc rebel sst
    ## 5               ford torino
    ## 6          ford galaxie 500

1a)

``` r
str(Auto)
```

    ## 'data.frame':    392 obs. of  9 variables:
    ##  $ mpg         : num  18 15 18 16 17 15 14 14 14 15 ...
    ##  $ cylinders   : num  8 8 8 8 8 8 8 8 8 8 ...
    ##  $ displacement: num  307 350 318 304 302 429 454 440 455 390 ...
    ##  $ horsepower  : num  130 165 150 150 140 198 220 215 225 190 ...
    ##  $ weight      : num  3504 3693 3436 3433 3449 ...
    ##  $ acceleration: num  12 11.5 11 12 10.5 10 9 8.5 10 8.5 ...
    ##  $ year        : num  70 70 70 70 70 70 70 70 70 70 ...
    ##  $ origin      : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ name        : Factor w/ 304 levels "amc ambassador brougham",..: 49 36 231 14 161 141 54 223 241 2 ...

The qualitative predictor are origin and name. The quantitative
predictors are mpg, cylinders, displacement, horsepower, weight,
acceleration, and year.  
1b)

``` r
automod <- lm(mpg ~ cylinders+displacement+horsepower+weight+acceleration+year+factor(origin)-name, data=Auto)
summary(automod)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ cylinders + displacement + horsepower + weight + 
    ##     acceleration + year + factor(origin) - name, data = Auto)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -9.009 -2.078 -0.098  1.986 13.361 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     -1.80e+01   4.68e+00   -3.84  0.00014 ***
    ## cylinders       -4.90e-01   3.21e-01   -1.52  0.12821    
    ## displacement     2.40e-02   7.65e-03    3.13  0.00186 ** 
    ## horsepower      -1.82e-02   1.37e-02   -1.33  0.18549    
    ## weight          -6.71e-03   6.55e-04  -10.24  < 2e-16 ***
    ## acceleration     7.91e-02   9.82e-02    0.81  0.42110    
    ## year             7.77e-01   5.18e-02   15.01  < 2e-16 ***
    ## factor(origin)2  2.63e+00   5.66e-01    4.64  4.7e-06 ***
    ## factor(origin)3  2.85e+00   5.53e-01    5.16  3.9e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 3.31 on 383 degrees of freedom
    ## Multiple R-squared:  0.824,  Adjusted R-squared:  0.821 
    ## F-statistic:  224 on 8 and 383 DF,  p-value: <2e-16

We can reject the null hypothesis for displacement, weight, year and
origin. Note that, holding other variables fixed, cars from outside the
U.S. affect the mpg more. We cannot reject the null for cylinders,
horsepower, and acceleration (i.e. they have no linear relation to
mpg).  
1c) The coefficient estimate for origin 2 should be interpreted as the
average difference in mpg between European and American cars. The
coefficient estimate for origin 3 should be interpreted as the average
difference in mpg between Japanese and American cars.  
1d)

``` r
summary(automod)$coef[1,1]+(3*summary(automod)$coef[2,1])+(100*summary(automod)$coef[3,1])+(85*summary(automod)$coef[4,1])+(3000*summary(automod)$coef[5,1])+(20*summary(automod)$coef[6,1])+(80*summary(automod)$coef[7,1])+(1*summary(automod)$coef[9,1])
```

    ## [1] 27.89

The predicted mpg of a Japanese car with three cylinders, displacement
100, horsepower of 85, weight of 3000, acceleration of 20, built in the
year 1980 is 27.89 mpg.  
1e) On average, the difference between a Japanese and European car’s mpg
is 0.2232.

``` r
summary(automod)$coef[9,1]-summary(automod)$coef[8,1]
```

    ## [1] 0.2232

1f)

``` r
automod1 <- lm(mpg~factor(origin)*horsepower, data=Auto)
summary(automod1)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ factor(origin) * horsepower, data = Auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -10.741  -2.955  -0.639   2.398  14.249 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                 34.4765     0.8907   38.71  < 2e-16 ***
    ## factor(origin)2             10.9972     2.3962    4.59  6.0e-06 ***
    ## factor(origin)3             14.3397     2.4643    5.82  1.2e-08 ***
    ## horsepower                  -0.1213     0.0071  -17.10  < 2e-16 ***
    ## factor(origin)2:horsepower  -0.1005     0.0277   -3.63  0.00033 ***
    ## factor(origin)3:horsepower  -0.1087     0.0290   -3.75  0.00020 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.42 on 386 degrees of freedom
    ## Multiple R-squared:  0.683,  Adjusted R-squared:  0.679 
    ## F-statistic:  166 on 5 and 386 DF,  p-value: <2e-16

Our fitted model is now
$\\hat{mpg}=34.4765+10.9972\*I(origin=2)+14.3397\*I(origin=3)-0.1213\*horsepower-0.1005\*I(origin=2)\*horsepower-0.1087\*I(origin=3)\*horsepower$.

1g) Given the model above, on average, with a one-unit increase in
horsepower, the mpg of a Japanese car changes by -0.1213-0.1087=-0.23.

1h)

``` r
autodeg1 <- lm(mpg~weight, data=Auto)
summary(autodeg1)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ weight, data = Auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -11.974  -2.756  -0.336   2.138  16.519 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 46.216525   0.798672    57.9   <2e-16 ***
    ## weight      -0.007647   0.000258   -29.6   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.33 on 390 degrees of freedom
    ## Multiple R-squared:  0.693,  Adjusted R-squared:  0.692 
    ## F-statistic:  879 on 1 and 390 DF,  p-value: <2e-16

``` r
autodeg2 <- lm(mpg~weight+I(weight^2), data=Auto)
summary(autodeg2)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ weight + I(weight^2), data = Auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.625  -2.713  -0.348   1.827  16.087 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.23e+01   2.99e+00   20.80  < 2e-16 ***
    ## weight      -1.85e-02   1.97e-03   -9.38  < 2e-16 ***
    ## I(weight^2)  1.70e-06   3.06e-07    5.55  5.4e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.18 on 389 degrees of freedom
    ## Multiple R-squared:  0.715,  Adjusted R-squared:  0.714 
    ## F-statistic:  488 on 2 and 389 DF,  p-value: <2e-16

``` r
autodeg3 <- lm(mpg~weight+I(weight^2)+I(weight^3), data=Auto)
summary(autodeg3)
```

    ## 
    ## Call:
    ## lm(formula = mpg ~ weight + I(weight^2) + I(weight^3), data = Auto)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -12.626  -2.708  -0.355   1.838  16.082 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.17e+01   1.10e+01    5.59  4.4e-08 ***
    ## weight      -1.79e-02   1.09e-02   -1.64     0.10    
    ## I(weight^2)  1.52e-06   3.45e-06    0.44     0.66    
    ## I(weight^3)  1.85e-11   3.50e-10    0.05     0.96    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.18 on 388 degrees of freedom
    ## Multiple R-squared:  0.715,  Adjusted R-squared:  0.713 
    ## F-statistic:  325 on 3 and 388 DF,  p-value: <2e-16

Using polynomial regression, we see that the proper degree should be 2
since weight is no longer significant when we are degree 3.

1i)

``` r
library(leaps)
autoback <- regsubsets(mpg~.-name, data = Auto, method = "backward")
resultback <- summary(autoback)
resultback$adjr2
```

    ## [1] 0.6918 0.8072 0.8160 0.8162 0.8177 0.8184 0.8182

``` r
plot(resultback$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
```

![](hw4_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
summary(autoback)
```

    ## Subset selection object
    ## Call: regsubsets.formula(mpg ~ . - name, data = Auto, method = "backward")
    ## 7 Variables  (and intercept)
    ##              Forced in Forced out
    ## cylinders        FALSE      FALSE
    ## displacement     FALSE      FALSE
    ## horsepower       FALSE      FALSE
    ## weight           FALSE      FALSE
    ## acceleration     FALSE      FALSE
    ## year             FALSE      FALSE
    ## origin           FALSE      FALSE
    ## 1 subsets of each size up to 7
    ## Selection Algorithm: backward
    ##          cylinders displacement horsepower weight acceleration year origin
    ## 1  ( 1 ) " "       " "          " "        "*"    " "          " "  " "   
    ## 2  ( 1 ) " "       " "          " "        "*"    " "          "*"  " "   
    ## 3  ( 1 ) " "       " "          " "        "*"    " "          "*"  "*"   
    ## 4  ( 1 ) " "       "*"          " "        "*"    " "          "*"  "*"   
    ## 5  ( 1 ) " "       "*"          "*"        "*"    " "          "*"  "*"   
    ## 6  ( 1 ) "*"       "*"          "*"        "*"    " "          "*"  "*"   
    ## 7  ( 1 ) "*"       "*"          "*"        "*"    "*"          "*"  "*"

The best model based on the adjusted *R*<sup>2</sup> criterion is when
*R*<sup>2</sup> = 0.8184 and has 6 predictor variable. These predictors
are cylinders, displacement, horsepower, weight, year, and origin.

2a)  
Let $p=\\frac{e^z}{1+e^z}$ be the logistic function. Solving for z, we
observe:  
$p=1-\\frac{1}{1+e^z}$  
$\\frac{1}{1+e^z}=1-p$  
$1+e^z=\\frac{1}{1-p}$  
$1+e^z=\\frac{1-p}{1-p}+\\frac{p}{1-p}$  
$e^z=\\frac{p}{1-p}$  
$z=ln(\\frac{p}{1-p})$  
Thus, the logit function is the inverse of the logistic function.  
2b) Assume *z* = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>1</sub> and
$p=logistic(z)=\\frac{e^z}{1+e^z}$. Then,  
$p=\\frac{e^{\\beta_0+\\beta_1x_1}}{1+e^{\\beta_0+\\beta_1x_1}}$  
So if we increase *x*<sub>1</sub> by 2, the log odds ratio would
increase, so the odds of the outcome also increase.  
Now, assume *β*<sub>1</sub> is negative, then:  
$p=\\lim\_{x_1 \\longrightarrow \\infty} \\frac{e^{\\beta_0-\\beta_1x_1}}{1+e^{\\beta_0-\\beta_1x_1}}=\\frac{0}{1+0}=0$  
and  
$p=\\lim\_{x_1 \\longrightarrow -\\infty}\\frac{e^{\\beta_0-\\beta_1x_1}}{1+e^{\\beta_0-\\beta_1x_1}}=\\lim\_{x_1 \\longrightarrow -\\infty}\\frac{1}{\\frac{1}{e^{\\beta_0-\\beta_1x_1}}+1}=\\frac{1}{1}=1$  
Therefore when *x*<sub>1</sub> approaches infinity and negative
infinity, p equals 0 and 1 respectively.
