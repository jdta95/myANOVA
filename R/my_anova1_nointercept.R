#' my_anova1_nointercept
#'
#' Performs a sequential ANOVA analysis on one no-intercept linear regression model.
#'
#' @param mod an "lm" object with at least one covariate in the linear regression model
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' For a complete breakdown of the table output, please see the myANOVA vignette.
#'
#' @examples
#' data(iris)
#' mod = lm(Sepal.Length ~ -1 + Sepal.Width + Species, data = iris)
#' my_anova1_nointercept(mod)
#'
#' @export

my_anova1_nointercept = function(mod) {
  par = ncol(mod$model) - 1
  Ysumsquare = sum(mod$model[, 1] ^ 2)
  dfs = unname(c(sapply(mod$model[,-1, drop = FALSE], get_dfs), mod$df.residual))
  if (par > 1 | dfs[1] > 1) {
    index = min(which(sapply(mod$model[,-1, drop = FALSE],
                             function(col) {
                               class(col) == "factor" | class(col) == "character"
                             })))
    dfs[index] = dfs[index] + 1
  }
  SSs = numeric(par + 1)
  for (i in 1:par) {
    formula_str = paste0("mod$model[, 1] ~ - 1 + ",
                         paste0("mod$model[, ", 2:(i + 1), collapse = "] + "),
                         "]")
    SSs[i] = Ysumsquare - sum((lm(as.formula(formula_str))$residuals) ^ 2) - sum(SSs)
  }
  SSs[i + 1] = sum(mod$residuals ^ 2)

  MSSs = SSs / dfs
  Fs = c(MSSs[-length(MSSs)] / MSSs[length(MSSs)], NA)
  ps = pf(Fs, dfs[-length(dfs)], dfs[length(dfs)], lower.tail = FALSE)
  ret = list(
    Df = dfs,
    `Sum Sq` = SSs,
    `Mean Sq` = MSSs,
    `F value` = Fs,
    `Pr(>F)` = ps
  )
  model_names = names(mod$model)
  attributes(ret)$row.names = c(model_names[2:(par + 1)],
                                "Residuals")
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Response: ", model_names[1], collapse = ""))
  return(ret)
}
