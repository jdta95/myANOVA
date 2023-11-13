#' my_anova_intercept
#'
#' Performs an ANOVA analysis on an intercept-only linear regression model.
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#' @export
#'
#' @examples
#' data(mtcars)
#' mod = lm(mpg ~ 1, data = mtcars)
#' my_anova_intercept(mod)

my_anova_intercept = function(mod) {
  n = nrow(mod$model)
  df = n - 1
  SS = sum((mod$model[, 1] - mod$coefficients) ^ 2)
  MSS = SS / df
  ret = list(Df = df,
             `Sum Sq` = SS,
             `Mean Sq` = MSS,
             `F value` = NA * 0,
             `Pr(>F)` = NA * 0)
  attributes(ret)$row.names = "Residuals"
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Response: ", names(mod$model), collapse = ""))
  return(ret)
}
