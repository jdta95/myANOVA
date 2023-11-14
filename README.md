
# myANOVA

<!-- badges: start -->
[![R-CMD-check](https://github.com/jdta95/myANOVA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jdta95/myANOVA/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The myANOVA package is designed to perform ANOVA analyses on linear regression models. The primary function is the my_anova function. All other functions in this package are designed to be called in the my_anova function and are not intended for use on their own.


## Installation

You can install the development version of myANOVA from [GitHub](https://github.com/jdta95/myANOVA) with:

``` r
# install.packages("devtools")
devtools::install_github("jdta95/myANOVA")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(myANOVA)
## example code

data(mtcars)

mod1 = lm(mpg ~ cyl, data = mtcars)
mod2 = lm(mpg ~ cyl + disp, data = mtcars)

my_anova(mod2)
my_anova(mod1, mod2)
```

