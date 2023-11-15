
# myANOVA

<!-- badges: start -->
[![R-CMD-check](https://github.com/jdta95/myANOVA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jdta95/myANOVA/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/jdta95/myANOVA/branch/master/graph/badge.svg)](https://app.codecov.io/gh/jdta95/myANOVA?branch=master)
<!-- badges: end -->

The myANOVA package is designed to perform ANOVA analyses primarily on main effect linear regression models. The primary function for doing this is the my_anova function. All other functions in this package are designed to be called in the my_anova function and are not intended for use on their own.

Note my_anova makes valid comparisons for multiple nested linear interaction models. However, it does not correctly perform an ANOVA analysis on just one linear interaction model. Thus, my_anova will return an error if only one linear interaction model is provided.


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
mod3 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)

# my_anova for a single linear regression model
my_anova(mod3)

# my_anova for more than 1 linear regression model
my_anova(mod1, mod2, mod3)
```
