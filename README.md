
# myANOVA

<!-- badges: start -->
[![R-CMD-check](https://github.com/jdta95/myANOVA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jdta95/myANOVA/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/jdta95/myANOVA/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jdta95/myANOVA?branch=master)
<!-- badges: end -->

The myANOVA package is designed to perform ANOVA analyses on linear regression models. The primary function is the my_anova function. All other functions in this package are designed to be called in the my_anova function and are not intended for use on their own.

* The my_anova function is not intended for ANOVA analysis of a single cell-means coded linear model input. After searching online for answers, I could not determine how the anova function calculates sums of squares when given a single cell-means coded linear model.


## Installation

You can install the development version of myANOVA from [GitHub](https://github.com/jdta95/myANOVA) with:

``` r
# install.packages("devtools")
devtools::install_github("jdta95/myANOVA")
```

## Example

There are 2 ways to use my_anova. When given one linear model object, my_anova sequentially tests significance of each term in the model. When given multiple nested linear model objects, my_anova compares and tests for significant differences between models.

For a full breakdown of the output, please see the myANOVA vignette.

``` r
library(myANOVA)

# sample data
data(iris)

# linear regression models
mod1 = lm(Sepal.Length ~ Sepal.Width, data = iris)
mod2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)

# my_anova for a single linear regression model
my_anova(mod2)

# my_anova for more than 1 linear regression model
my_anova(mod1, mod2)
```
