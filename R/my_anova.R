#' my_anova
#'
#' Perform ANOVA analyses on one or more linear regression models
#'
#' @param ... one or more objects of class "lm"
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' For a complete breakdown of the table output, please see the myANOVA vignette.
#'
#' @examples
#' data(iris)
#' mod1 = lm(Sepal.Length ~ Sepal.Width, data = iris)
#' mod2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' my_anova(mod2)
#' my_anova(mod1, mod2)
#'
#' @export

my_anova = function(...) {
  mods = list(...)
  # Error if any mods are not lm objects
  if (any(sapply(mods, function(mod)
    class(mod) != "lm"))) {
    stop("At least one of the objects supplied to my_anova does not have class \"lm\".")
  }

  # If mods contains multiple models, run my_anova2
  else if (length(mods) > 1) {
    return(my_anova2(mods))
  }

  # If mods contains just one no-coefficient model, run my_anova1_nocoeffs
  else if (ncol(mods[[1]]$model) == 1 &
           (any(grepl(" -\\s*1", mods[[1]]$call)) |
            any(grepl(" +\\s*0", mods[[1]]$call)))) {
    return(my_anova1_nocoeffs(mods[[1]]))
  }

  # If mods contains just one no-intercept model, run my_anova1_nointercept
  else if (length(mods) == 1 &
           (any(grepl(" -\\s*1", mods[[1]]$call)) |
            any(grepl(" +\\s*0", mods[[1]]$call)))) {
    return(my_anova1_nointercept(mods[[1]]))
  }

  # If mods contains just one intercept-only model, run my_anova_intercept
  else if (ncol(mods[[1]]$model) == 1) {
    return(my_anova_intercept(mods[[1]]))
  }

  # If mods contains just one model with at least one covariate, run my_anova1
  else {
    return(my_anova1(mods[[1]]))
  }
}
