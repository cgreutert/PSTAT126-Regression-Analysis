Homework Assignment 1
================
Carly Greutert (8408916)
May 18, 2022

1.  

``` r
cov.data <- data.frame(row.names = c("San Bernardino County",
                                     "Riverside County",
                                     "Orange County",
                                     "San Diego County",
                                     "Santa Clara County",
                                     "Kern County",
                                     "Sacramento County",
                                     "Fresno County",
                                     "Alameda County"))
cov.data$population <- c(2149000, 2411000, 3168000, 3316000, 1927000, 887000, 1525000, 985000, 1657000)
cov.data$deaths <- c(6056, 5589, 5897, 4476, 1964, 1918, 2552, 2379, 1546)
```

1a) In this problem, a California county’s population is the predictor
variable and the deaths is the response variable.  
1b)

``` r
xavg <- mean(cov.data$population)
yavg <- mean(cov.data$deaths)
num <- (2149000-xavg)*(6056-yavg)+(2411000-xavg)*(5589-yavg)+(3168000-xavg)*(5897-yavg)+(3316000-xavg)*(4476-yavg)+(1927000-xavg)*(1964-yavg)+(887000-xavg)*(1918-yavg)+(1525000-xavg)*(2552-yavg)+(985000-xavg)*(2379-yavg)+(1657000-xavg)*(1546-yavg)
denom <-(2149000-xavg)^2+(2411000-xavg)^2+(3168000-xavg)^2+(3316000-xavg)^2+(1927000-xavg)^2+(887000-xavg)^2+(1525000-xavg)^2+(985000-xavg)^2+(1657000-xavg)^2
beta1hat <- num/denom
beta0hat <- yavg-(beta1hat*xavg)
```

$$\\hat{\\beta_1} = \\frac{\\sum\_{i=1}^{9}(x_i-\\frac{1}{9}\\sum\_{i=1}^{9}x_i)(y_i-\\frac{1}{9}\\sum\_{i=1}^{9}y_i)}{\\sum\_{i=1}^{9}(x_i-\\frac{1}{9}\\sum\_{i=1}^{9}x_i)^2}=0.0016$$
$$\\hat{\\beta_0} = \\frac{1}{9}\\sum\_{i=1}^{9}y_i-\\frac{1}{9}\\hat{\\beta_1}\\sum\_{i=1}^{9}x_i=391.3846$$

1c) $\\hat{\\beta_0}$ tells us the intercept, and in this problem
represents a baseline number of deaths for each county (\~391).
$\\hat{\\beta_1}$ tells us the proportionality constant for the increase
of the number of the deaths as the population increases (\~0.0016).  
1d) $\\hat{y}=\\hat{\\beta_0}+\\hat{\\beta_1}x=391.3846+0.0016x$  
1e)
$$\\hat{y}=\\hat{\\beta_0}+\\hat{\\beta_1}x=391.3846+0.0016\*(1000000)=1991$$
The predicted number of deaths from COVID-19 of a county with 1,000,000
people is 1991.  
2.`require(datasets)                                                                                 trees`  
2a) There are 31 rows and 3 columns in the dataset. The variable names
are Girth, Height, and Volume.  
2b)

``` r
pairs(log(trees))
```

![](hw1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> 2c)

``` r
cor(log(trees))
```

    ##         Girth Height Volume
    ## Girth  1.0000 0.5302 0.9767
    ## Height 0.5302 1.0000 0.6486
    ## Volume 0.9767 0.6486 1.0000

2d)There are not any missing values. 2e)

``` r
treemod <- lm(formula=log(Volume)~log(Girth)+log(Height),data=trees)
summary(treemod)
```

    ## 
    ## Call:
    ## lm(formula = log(Volume) ~ log(Girth) + log(Height), data = trees)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.16856 -0.04849  0.00243  0.06364  0.12922 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -6.632      0.800   -8.29  5.1e-09 ***
    ## log(Girth)     1.983      0.075   26.43  < 2e-16 ***
    ## log(Height)    1.117      0.204    5.46  7.8e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.0814 on 28 degrees of freedom
    ## Multiple R-squared:  0.978,  Adjusted R-squared:  0.976 
    ## F-statistic:  613 on 2 and 28 DF,  p-value: <2e-16

2f)

``` r
model.matrix(treemod)
```

    ##    (Intercept) log(Girth) log(Height)
    ## 1            1      2.116       4.248
    ## 2            1      2.152       4.174
    ## 3            1      2.175       4.143
    ## 4            1      2.351       4.277
    ## 5            1      2.370       4.394
    ## 6            1      2.380       4.419
    ## 7            1      2.398       4.190
    ## 8            1      2.398       4.317
    ## 9            1      2.407       4.382
    ## 10           1      2.416       4.317
    ## 11           1      2.425       4.369
    ## 12           1      2.434       4.331
    ## 13           1      2.434       4.331
    ## 14           1      2.460       4.234
    ## 15           1      2.485       4.317
    ## 16           1      2.557       4.304
    ## 17           1      2.557       4.443
    ## 18           1      2.588       4.454
    ## 19           1      2.617       4.263
    ## 20           1      2.625       4.159
    ## 21           1      2.639       4.357
    ## 22           1      2.653       4.382
    ## 23           1      2.674       4.304
    ## 24           1      2.773       4.277
    ## 25           1      2.791       4.344
    ## 26           1      2.851       4.394
    ## 27           1      2.862       4.407
    ## 28           1      2.885       4.382
    ## 29           1      2.890       4.382
    ## 30           1      2.890       4.382
    ## 31           1      3.025       4.466
    ## attr(,"assign")
    ## [1] 0 1 2

``` r
xtreemod<-model.matrix(treemod)
vol <- log(trees$Volume)
ytreemod<-vol
betahat<-solve(t(xtreemod)%*%xtreemod)%*%t(xtreemod)%*%ytreemod
betahat
```

    ##               [,1]
    ## (Intercept) -6.632
    ## log(Girth)   1.983
    ## log(Height)  1.117

The least squares coefficients match what is listed in the summary
report above.  
2g) Predicted Response *ŷ* = *X**β̂*

``` r
yhat <- xtreemod%*%betahat
yhat
```

    ##     [,1]
    ## 1  2.310
    ## 2  2.298
    ## 3  2.309
    ## 4  2.808
    ## 5  2.977
    ## 6  3.023
    ## 7  2.803
    ## 8  2.946
    ## 9  3.036
    ## 10 2.981
    ## 11 3.057
    ## 12 3.031
    ## 13 3.031
    ## 14 2.975
    ## 15 3.118
    ## 16 3.247
    ## 17 3.401
    ## 18 3.475
    ## 19 3.320
    ## 20 3.218
    ## 21 3.468
    ## 22 3.524
    ## 23 3.478
    ## 24 3.643
    ## 25 3.755
    ## 26 3.929
    ## 27 3.966
    ## 28 3.983
    ## 29 3.994
    ## 30 3.994
    ## 31 4.355

Residuals *y* − *ŷ*

``` r
resid <- ytreemod-yhat
resid
```

    ##         [,1]
    ## 1   0.021874
    ## 2   0.034264
    ## 3   0.013841
    ## 4  -0.010619
    ## 5  -0.043031
    ## 6  -0.041961
    ## 7  -0.055660
    ## 8  -0.044315
    ## 9   0.082173
    ## 10  0.009259
    ## 11  0.129223
    ## 12  0.013173
    ## 13  0.032041
    ## 14  0.083801
    ## 15 -0.168561
    ## 16 -0.146549
    ## 17  0.119002
    ## 18 -0.164525
    ## 19 -0.073211
    ## 20 -0.003299
    ## 21  0.073269
    ## 22 -0.067780
    ## 23  0.113363
    ## 24  0.002431
    ## 25 -0.002998
    ## 26  0.085102
    ## 27  0.054006
    ## 28  0.082405
    ## 29 -0.052661
    ## 30 -0.062417
    ## 31 -0.011641

Estimate of $\\hat{\\sigma^2}=0.006624$

``` r
var <- (t(resid)%*%resid)/28
var
```

    ##          [,1]
    ## [1,] 0.006624

3.  We can start the process of minimization by taking the derivative of
    the sum of squared residuals with respect to *β*<sub>0</sub> and
    *β*<sub>1</sub>.
    $SSR=\\sum\_{i=1}^{n}(y_i-\\beta_0-\\beta_1 x_i)^2$  
    $\\frac{\\partial SSR}{\\partial \\beta_0}=-2\\sum\_{i=1}^{n}(y_i-\\beta_0-\\beta_1x_i)$  
    $\\frac{\\partial SSR}{\\partial \\beta_1}=-2\\sum\_{i=1}^{n}x_i(y_i-\\beta_0-\\beta_1x_i)$  
    Setting these equations and rearranging to get *y*<sub>*i*</sub> on
    one side and solving for *β*<sub>0</sub> we get the following:  
    $\\beta_0 n + \\beta_1 \\sum\_{i=1}^n x_i = \\sum\_{i=1}^n y_i$  
    $\\beta_0 n =\\sum\_{i=1}^n y_i -\\beta_1 \\sum\_{i=1}^n x_i$  
    $\\hat{\\beta_0} = \\frac{\\sum\_{i=1}^n y_i -\\beta_1 \\sum\_{i=1}^n x_i}{n}=\\overline{y}-\\beta_1 \\overline{x}$  
    Now, solving for *β*<sub>1</sub> and substituting in our
    *β*<sub>0</sub> equation:  
    $0=-2\\sum\_{i=1}^{n}x_i(y_i-\\overline{y}-\\beta_1 \\overline{x}-\\beta_1x_i)$  
    $0=\\sum\_{i=1}^{n}x_i(y_i-\\overline{y}-\\beta_1(x_i-\\overline{x}))$  
    $0=\\sum\_{i=1}^{n}x_i(y_i-\\overline{y})-\\beta_1\\sum\_{i=1}^{n}x_i(x_i-\\overline{x})$  
    $\\beta_1\\sum\_{i=1}^{n}x_i(x_i-\\overline{x})=\\sum\_{i=1}^{n}x_i(y_i-\\overline{y})$  
    $\\hat{\\beta_1}=\\frac{\\sum\_{i=1}^{n}x_i(y_i-\\overline{y})}{\\sum\_{i=1}^{n}x_i(x_i-\\overline{x})}$  
    $\\hat{\\beta_1}=\\frac{\\sum\_{i=1}^{n}(x_i-\\overline{x})(y_i-\\overline{y})}{\\sum\_{i=1}^{n}(x_i-\\overline{x})^2}$  
    Note these are the results we wanted and that the last equality was
    substituted by using formulas:
    $\\sum (x_i-\\overline{x})^2 = \\sum x_i^2-n\\overline{x}^2$ and
    $\\sum (x_i-\\overline{x})(y_i-\\overline{y})=\\sum x_iy_i-n\\overline{xy}$
    given in the textbook.
