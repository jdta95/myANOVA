#' my_anova
#'
#' Perform ANOVA analyses on one or more linear regression models
#'
#' @param ... one or more objects of class "lm"
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' @examples
#' data(mtcars)
#' mod1 = lm(mpg ~ cyl, data = mtcars)
#' mod2 = lm(mpg ~ cyl + disp, data = mtcars)
#' my_anova(mod2)
#' my_anova(mod1, mod2)
#'
#' @export

my_anova = function(...) {
  mods = list(...)
  if (any(sapply(mods, function(mod) class(mod) != "lm"))) {
    stop("At least one of the objects supplied to my_anova does not have class \"lm\".")
  }
  if (length(mods) > 1) {
    return(my_anova2(mods))
  }
  else if (ncol(mods[[1]]$model) == 1) {
    return(my_anova_intercept(mods[[1]]))
  }
  else {
    return(my_anova1(mods[[1]]))
  }
}
