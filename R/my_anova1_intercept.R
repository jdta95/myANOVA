#' my_anova1_intercept
#'
#' Performs an ANOVA analysis on an intercept-only linear regression model.
#'
#' @param mod an "lm" object containing an intercept-only linear regression model
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' For a complete breakdown of the table output, please see the myANOVA vignette.
#'
#' @examples
#' data(iris)
#' mod = lm(Sepal.Length ~ 1, data = iris)
#' my_anova1_intercept(mod)
#'
#' @export

my_anova1_intercept = function(mod) {
  n = nrow(mod$model)

  # Calculate degrees of freedom (df)
  df = n - 1

  # Calculate sum of square residuals
  ## SS = sum((Y - Ybar) ^ 2)
  SS = sum((mod$model[, 1] - mod$coefficients) ^ 2)

  # Calculate mean sum of squares
  MSS = SS / df

  # Create output
  ret = list(
    Df = df,
    `Sum Sq` = SS,
    `Mean Sq` = MSS,
    `F value` = NA * 0,
    `Pr(>F)` = NA * 0
  )
  attributes(ret)$row.names = "Residuals"
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Response: ", names(mod$model), collapse = ""))
  return(ret)
}
