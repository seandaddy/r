Poisson Regression
================

## Poisson Regression

When we perform a table analysis, logistic regression and log-linear
models are most likely. Logistic regression deal with the ratio of odds
but Poisson does the fixed amount as total number observation.

$ log = X $

## Example

This is data about melanoma cases, in other words skin cancers.

``` r
mela=read.csv("data/melanoma.csv")
mel=xtabs(cases~age+region, data=mela)
out=glm(cases~age+region, family=poisson,offset=log(total),data=mela)
summary(out)
```

    ## 
    ## Call:
    ## glm(formula = cases ~ age + region, family = poisson, data = mela, 
    ##     offset = log(total))
    ## 
    ## Deviance Residuals: 
    ##       1        2        3        4        5        6        7        8  
    ##  0.4780  -0.4273  -0.5302   0.7469  -1.3610   0.8686  -0.4581   0.3667  
    ##       9       10       11       12  
    ##  0.4279  -0.5932   0.8904  -0.8283  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -10.65831    0.09518 -111.97   <2e-16 ***
    ## age35-44      1.79737    0.12093   14.86   <2e-16 ***
    ## age45-54      1.91309    0.11844   16.15   <2e-16 ***
    ## age55-64      2.24180    0.11834   18.94   <2e-16 ***
    ## age65-74      2.36572    0.13152   17.99   <2e-16 ***
    ## age75+        2.94468    0.13205   22.30   <2e-16 ***
    ## regionsouth   0.81948    0.07103   11.54   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 895.8197  on 11  degrees of freedom
    ## Residual deviance:   6.2149  on  5  degrees of freedom
    ## AIC: 92.44
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
mu=fitted(out)
cbind(mela[,c(1,2)], cases=rpois(12,mu))
```

    ##      age region cases
    ## 1  35-44  south    83
    ## 2  45-54  south    74
    ## 3  55-64  south    65
    ## 4  65-74  south    35
    ## 5    75+  south    37
    ## 6    <35  south    47
    ## 7  35-44  north    77
    ## 8  45-54  north    87
    ## 9  55-64  north   104
    ## 10 65-74  north    59
    ## 11   75+  north    77
    ## 12   <35  north    60
