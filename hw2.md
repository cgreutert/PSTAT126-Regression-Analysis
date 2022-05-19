Homework Assignment 2
================
Carly Greutert (8408916)
May 18, 2022

1a)

``` r
fat <- read.csv(file = "C:/Users/carly/Downloads/fat.csv")
brozekmod <- lm(formula= brozek ~ age+weight+height+neck+chest+abdom+hip, data=fat)
summary(brozekmod)
```

    ## 
    ## Call:
    ## lm(formula = brozek ~ age + weight + height + neck + chest + 
    ##     abdom + hip, data = fat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -10.765  -2.977  -0.068   2.665  10.600 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -10.74598   14.43248   -0.74    0.457    
    ## age          -0.00236    0.02568   -0.09    0.927    
    ## weight       -0.06014    0.04430   -1.36    0.176    
    ## height       -0.14704    0.08839   -1.66    0.097 .  
    ## neck         -0.44881    0.20273   -2.21    0.028 *  
    ## chest        -0.01786    0.09155   -0.20    0.845    
    ## abdom         0.90758    0.07868   11.54   <2e-16 ***
    ## hip          -0.14298    0.12968   -1.10    0.271    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.11 on 244 degrees of freedom
    ## Multiple R-squared:  0.727,  Adjusted R-squared:  0.719 
    ## F-statistic: 92.9 on 7 and 244 DF,  p-value: <2e-16

1b) The predictor variable that is significant under the threshold 0.01
is abdom (Abdomen circumference).  
1c)The proportion of variation that is explained by predictors is R^2
(0.7271) in this case.  
1d)The null hypothesis of the global F-test is
*H*<sub>0</sub> : *β*<sub>1</sub> = *β*<sub>2</sub> = .... = *β*<sub>7</sub> = 0.

``` r
brozekmod_m <- lm(formula=brozek~1, data=fat)
anova(brozekmod_m, brozekmod)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: brozek ~ 1
    ## Model 2: brozek ~ age + weight + height + neck + chest + abdom + hip
    ##   Res.Df   RSS Df Sum of Sq    F Pr(>F)    
    ## 1    251 15079                             
    ## 2    244  4114  7     10965 92.9 <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The p-value for the global F-test is 2.2e-16 (p\<0.001), so the 7
predictor variables covers a significant amount of variation.  
1e) Let *H*<sub>0</sub> : *β*<sub>*a**g**e*</sub> = 0.

``` r
tage <- summary(brozekmod)$coef[2,1]/summary(brozekmod)$coef[2,2]
pt(-tage, 244, lower.tail=FALSE)*2
```

    ## [1] 0.9269

This is identical to the p-value in the original model.  
1f) The corresponding null test for this statistical test is
*H*<sub>0</sub> : *β*<sub>*w**e**i**g**h**t*</sub> = *β*<sub>*h**e**i**g**h**t*</sub> = 0.

``` r
brozekmod_mwvh <- lm(formula=brozek~age+neck+chest+abdom+hip, data=fat)
anova(brozekmod_mwvh, brozekmod)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: brozek ~ age + neck + chest + abdom + hip
    ## Model 2: brozek ~ age + weight + height + neck + chest + abdom + hip
    ##   Res.Df  RSS Df Sum of Sq    F Pr(>F)   
    ## 1    246 4275                            
    ## 2    244 4114  2       161 4.77 0.0093 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

With this model, we see the p-value from the resulting F-test is 0.0093,
so we reject the null hypothesis at the 0.01 significance level. Thus,
weight or height do have an effect on body fat  
percentage.  
1g)

``` r
babdom <- summary(brozekmod)$coef[7,1]
seabdom <- summary(brozekmod)$coef[7,2]
abdomq <- qt(0.995,244)
babdom-(abdomq*seabdom)
```

    ## [1] 0.7033

``` r
babdom+(abdomq*seabdom)
```

    ## [1] 1.112

Thus, a 99% confidence interval for *β*<sub>*a**b**d**o**m*</sub> is
\[0.7033, 1.112\]. This indicates that we should reject the null
hypothesis because 0 ∉ \[0.7033,1.112\].  
1h)

``` r
summary(brozekmod)$coef[1,1]+(40*summary(brozekmod)$coef[2,1])+(170*summary(brozekmod)$coef[3,1])+(70*summary(brozekmod)$coef[4,1])+(38*summary(brozekmod)$coef[5,1])+(100*summary(brozekmod)$coef[6,1])+(91*summary(brozekmod)$coef[7,1])+(99*summary(brozekmod)$coef[8,1])
```

    ## [1] 18.24

The predicted brozek for a person with those predictor values is
18.24%.  
1i)

``` r
x_0 <- data.frame(age=40,weight=170,height=70,neck=38,chest=100,abdom=91,hip=99)
predict(brozekmod,newdata=x_0,interval="prediction", level=0.95)
```

    ##     fit   lwr   upr
    ## 1 18.24 10.11 26.36

Thus a 95% confidence interval for this person is \[10.11,26.36\].

2)Let *y* = *X**β* + *ϵ* where *ϵ* *N*(0,*σ*<sup>2</sup>*I*).First, note
that *β̂* is an  
unbiased estimator. Then,  
*C**o**v*(*β̂*)  
 = *C**o**v*\[(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>*Y*\|*X*\]  
 = (*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>\[*C**o**v*(*Y*\|*X*)\]*X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>  
 = (*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>\[*σ*<sup>2</sup>*I*\]*X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>  
 = *σ*<sup>2</sup>(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>*X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>  
 = *σ*<sup>2</sup>(*X*<sup>*T*</sup>*X*)<sup>−1</sup>
