#' my_anova1_nocoeffs
#'
#' Performs an ANOVA analysis on one no-coefficient linear regression model.
#'
#' @param mod an "lm" object with at least one covariate in the linear regression model
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' For a complete breakdown of the table output, please see the myANOVA vignette.
#'
#' @examples
#' data(iris)
#' mod = lm(Sepal.Length ~ -1, data = iris)
#' my_anova1_nocoeffs(mod)
#'
#' @export

my_anova1_nocoeffs = function(mod) {
  # degrees of freedom (df) = n
  dfs = nrow(mod$model)

  # sum of squares (SS) = sum(Y ^ 2)
  SSs = sum(mod$model[, 1] ^ 2)

  # mean sum of squares (MSS) = SS / df
  MSSs = SSs / dfs

  # No hypothesis testing
  Fs = NA * 0
  ps = NA * 0

  # Create output
  ret = list(
    Df = dfs,
    `Sum Sq` = SSs,
    `Mean Sq` = MSSs,
    `F value` = Fs,
    `Pr(>F)` = ps
  )
  attributes(ret)$row.names = "Residuals"
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Response: ", names(mod$model), collapse = ""))
  return(ret)
}
