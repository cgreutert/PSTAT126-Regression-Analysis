Homework Assignment 3
================
Carly Greutert (8408916)
28 February 2022

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

``` r
head(brozekmod$fitted.values)
```

    ##     1     2     3     4     5     6 
    ## 15.87 10.43 18.82 12.80 26.65 16.45

``` r
head(brozekmod$residuals)
```

    ##      1      2      3      4      5      6 
    ## -3.266 -3.525  5.779 -1.900  1.149  4.146

1b)

``` r
plot(fitted(brozekmod),residuals(brozekmod), xlab = "Fitted", ylab = "Residuals")
```

![](hw3_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

The diagnostic I used is a plot of residuals against fitted values. We
observe constant symmetrical variation, so we conclude the equal
variance assumption holds.

1c)

``` r
qqnorm(residuals(brozekmod), ylab="Residuals")
qqline(residuals(brozekmod))
```

![](hw3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

The diagnostic I used was a QQ-plot. The normal assumption is held in
this case since the residual points follows closely along the residual
line.  
1d)

``` r
shapiro.test(residuals(brozekmod))
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  residuals(brozekmod)
    ## W = 0.99, p-value = 0.5

The p-value (0.5) is not low enough to reject the null hypothesis that
the random errors follow a normal distribution. Thus, the assumption is
formally held (i.e we do not reject the null hypothesis) using the
Shapiro-Wilk normality test.  
1e)

``` r
n <- nrow(fat)
plot(tail(residuals(brozekmod), n - 1)~head(residuals(brozekmod), n-1), xlab="i-th residual", ylab="(i+1)-th residual")
abline(h=0,v=0)
```

![](hw3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The model appears to have serial correlation. This tells us that the
random errors  
independent.  
1f)

``` r
h <- hatvalues(brozekmod)
head(h)
```

    ##       1       2       3       4       5       6 
    ## 0.02861 0.02588 0.04732 0.02451 0.08749 0.02590

``` r
sum(h)
```

    ## [1] 8

``` r
p<-7
p+1
```

    ## [1] 8

Thus, the values match.  
1g)

``` r
dat <- data.frame(index=seq(length(h)), leverage=h)
plot(leverage ~ index, col='white', data=dat, pch = NULL)
text(leverage ~ index, labels = index, data=dat, cex=0.9, font=2)
abline(h=(p+1)/n, col='blue')
abline(h=3 *(p+1)/n, col = "red", lty =2)
```

![](hw3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
avglev <- (p+1)/n
```

There are three high leverage points. I used the criterion if their
leverage was 2\*3 times greater than the average
($\\frac{p+1}{n}=.03175$) then it should be considered a high leverage
point.  
1h)

``` r
r <- rstandard(brozekmod)
r
```

    ##         1         2         3         4         5         6         7         8 
    ## -0.806902 -0.869815  1.441813 -0.468412  0.292817  1.023026  0.293699 -0.699794 
    ##         9        10        11        12        13        14        15        16 
    ## -0.850602  0.169322 -0.564687 -1.018718  0.781838 -0.709994  0.209246 -0.273962 
    ##        17        18        19        20        21        22        23        24 
    ##  1.950935  0.956339  0.052002 -1.205830 -0.515342 -1.425274  1.463482  1.450999 
    ##        25        26        27        28        29        30        31        32 
    ##  1.386882 -1.331590 -0.129097  1.065889 -0.842491 -0.846901 -0.529040 -1.606785 
    ##        33        34        35        36        37        38        39        40 
    ##  1.303393 -0.134566  0.071177  0.836547 -0.076489  1.563717 -3.025477  0.214299 
    ##        41        42        43        44        45        46        47        48 
    ## -1.278144 -0.278659 -0.311235  1.461326 -0.977408  0.808014  0.594925 -1.256263 
    ##        49        50        51        52        53        54        55        56 
    ## -0.967091 -0.331021 -1.401149 -0.966495 -1.456610 -0.928888 -1.331397 -0.275938 
    ##        57        58        59        60        61        62        63        64 
    ## -1.155827  0.255857  0.607208 -0.122860  0.066051  1.671935  0.662519 -0.250111 
    ##        65        66        67        68        69        70        71        72 
    ##  1.021171  1.190458  1.562130 -0.615973 -0.371944 -0.319676  0.961560 -0.834152 
    ##        73        74        75        76        77        78        79        80 
    ## -0.733355  0.411140 -0.762594  1.268364  0.260255  0.097558 -0.411686 -1.616921 
    ##        81        82        83        84        85        86        87        88 
    ##  1.687724  1.923790 -0.937128  1.064125 -0.106614  1.767941 -0.533385  0.481660 
    ##        89        90        91        92        93        94        95        96 
    ## -1.009585  0.055376 -0.412552 -0.198080 -0.699094  0.499216 -1.250739 -0.271948 
    ##        97        98        99       100       101       102       103       104 
    ## -1.674885 -1.051029 -0.237685  1.076940  0.644656  0.227130  0.642625  0.836125 
    ##       105       106       107       108       109       110       111       112 
    ##  0.183518 -0.288831 -1.595760 -1.184890  1.049648 -0.032957  0.263016 -0.679123 
    ##       113       114       115       116       117       118       119       120 
    ##  0.288089  0.576417  1.657569 -0.026324  0.692040 -0.348148  2.040147  1.120098 
    ##       121       122       123       124       125       126       127       128 
    ##  1.542361  0.509549  0.508393 -0.124496  0.030465 -0.224384  1.897901  2.086410 
    ##       129       130       131       132       133       134       135       136 
    ##  0.652446 -0.007145 -0.170552  0.957114 -0.105727  1.536441  1.967883  0.065524 
    ##       137       138       139       140       141       142       143       144 
    ##  1.585869  1.237505  1.016417 -1.582004  1.171893 -0.604215  0.930911  0.483677 
    ##       145       146       147       148       149       150       151       152 
    ##  0.081844  0.481934 -0.431668  1.581422 -0.643753 -0.331764  0.122334 -0.233934 
    ##       153       154       155       156       157       158       159       160 
    ##  1.149494 -0.278713 -0.674907  1.305098  0.623818 -1.589170  0.726472  0.585673 
    ##       161       162       163       164       165       166       167       168 
    ## -0.686263 -1.092471 -0.852833 -0.079069  0.034224  0.607484  0.713526  0.188306 
    ##       169       170       171       172       173       174       175       176 
    ##  0.116851 -0.440253 -2.151757 -2.105655  0.810852  0.047999 -0.516235 -0.314102 
    ##       177       178       179       180       181       182       183       184 
    ## -0.261168  0.459698  0.881599 -1.303268  0.343331 -1.557534 -0.919265 -1.097515 
    ##       185       186       187       188       189       190       191       192 
    ## -0.071785 -0.505093 -0.984235 -0.367594 -0.063749 -0.589821  0.010267  1.713492 
    ##       193       194       195       196       197       198       199       200 
    ## -0.424622 -0.349921  1.786849  0.565722  1.292689  0.029085  0.203832  1.072091 
    ##       201       202       203       204       205       206       207       208 
    ## -0.875857  1.292347  0.382026 -1.917354  0.096403 -0.343490  2.620990  1.781331 
    ##       209       210       211       212       213       214       215       216 
    ## -1.022387 -0.948907 -1.111854  0.626206  0.622006 -0.695740  0.997392  1.133314 
    ##       217       218       219       220       221       222       223       224 
    ##  0.330597 -0.880765  0.280583 -0.735173 -1.830618 -0.816361 -1.212340 -2.239930 
    ##       225       226       227       228       229       230       231       232 
    ## -2.226569 -0.194061 -1.047664  1.182810 -0.374054 -0.991366 -1.837678 -1.309666 
    ##       233       234       235       236       237       238       239       240 
    ##  0.078800  0.507343  1.107327 -1.221900  0.600740 -1.601969  0.058813  1.025701 
    ##       241       242       243       244       245       246       247       248 
    ##  0.658143 -0.794942  0.567286  0.020229 -0.078558  0.188157  0.433472 -1.385198 
    ##       249       250       251       252 
    ##  1.357858 -1.764931 -0.126146  0.384023

``` r
mean(r)
```

    ## [1] -0.001951

``` r
sd(r)
```

    ## [1] 1.004

Without drawing a plot, I see that -3.025 is an outlier based on it
being further than three standard deviations away from the mean.  
1i)

``` r
d <- cooks.distance(brozekmod)
dat2 <- data.frame(index=seq(length(d)), cooks=d)
plot(cooks ~ index, col = 'white', data=dat2, pch=NULL)
text(cooks ~ index, labels = index, data = dat2, cex=0.9, font=2)
abline(h=.02, col='red', lty=2)
```

![](hw3_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> The observations
39,42,250,207 all have Cook’s distances greater than .02, making them
influential observations.  
1j)

``` r
fat_new <- fat[fat$brozek!=0,]
brozekmod1 <- lm(brozek ~ ., data=fat_new)
library(MASS)
boxcox(brozekmod1)
```

![](hw3_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Because 1 is in the 95% confidence interval, it is reasonable to assume
a Box-Cox transformation is not necessary. We needed to remove the
response variables equal to 0 because Box Cox transformations are
designed for strictly positive response because of lambda is equal to
zero log(y) is inconclusive.

2.  1.  *H* = *X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>
        so,
        *H**X* = *X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>*X*
         = *X*  
        b)*H**H* = *X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup>*X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup> = *X*(*X*<sup>*T*</sup>*X*)<sup>−1</sup>*X*<sup>*T*</sup> = *H*  
        *H*(*I*−*H*) = *H**I* − *H**H* = *H* − *H* = 0  
        (*I*−*H*)(*I*−*H*) = *I*<sup>2</sup> − 2*I**H* + *H*<sup>2</sup> = *I* − 2*H* + *H* = *I* − *H*  

3.  Because the covariance matrix of *ϵ* is a diagonal matrix of the
    variances of 1, .., *n*, we know that *ϵ*<sub>*i*</sub> is normally
    distributed and identically and independently distributed. Suppose
    we have the multiple linear regression mean function
    *E*(*Y*\|*X*) = *β*<sup>*T*</sup>*X*)and assume
    *V**a**r*(*Y*\|*X*) = *V**a**r*(*ϵ*) = *σ*<sup>2</sup>*W*<sup>−1</sup>
    where W is a diagonal matrix of  
    $w_i=\\frac{1}{\\sigma_i^2}$. Then, we want to minimize
    *S**S**R* = (*Y*−*X**β*)<sup>*T*</sup>*W*(*Y*−*X**β*)  
    *S**S**R* = (*Y*<sup>*T*</sup>*W*−*β*<sup>*T*</sup>*X*<sup>*T*</sup>*W*)(*Y*−*X**β*)
    *S**S**R* = *Y*<sup>*T*</sup>*W**Y* − *Y*<sup>*T*</sup>*W**X**β* − *β*<sup>*T*</sup>*X*<sup>*T*</sup>*W**Y* + *β*<sup>*T*</sup>*X*<sup>*T*</sup>*W**X**β*
    $\\frac{\\partial SSR}{\\partial \\beta}=2(-X^TWY+X^TWX\\beta)$
    Setting this equal to zero, we observe  
     − *X*<sup>*T*</sup>*W**Y* + *X*<sup>*T*</sup>*W**X**β* = 0  
    *X*<sup>*T*</sup>*W**X**β* = *X*<sup>*T*</sup>*W**Y* Thus,
    *β̂* = (*X*<sup>*T*</sup>*W**X*)<sup>−1</sup>*X*<sup>*T*</sup>*W**Y*
    is the weighted least estimator.
