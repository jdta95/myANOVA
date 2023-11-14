#' my_anova1
#'
#' Performs a sequential ANOVA analysis on one linear regression model.
#'
#' @param mod an "lm" object with at least one covariate in the linear regression model
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' For a complete breakdown of the table output, please see the myANOVA vignette.
#'
#' @examples
#' data(iris)
#' mod = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
#' my_anova1(mod)
#'
#' @export

my_anova1 = function(mod) {
  n = nrow(mod$model)
  par = ncol(mod$model) - 1
  Ybar = mean(mod$model[, 1])

  # Use get_dfs on each covariate to get the degrees of freedom (df)
  dfs = unname(c(sapply(mod$model[,-1, drop = FALSE], get_dfs), mod$df.residual))

  # Calculate the sums of squares (SS) for each sequential model
  ## SS = sum((Yhat - Ybar) ^ 2) - (previous SS's)
  SSs = numeric(par + 1)
  SSs[1] = sum((lm(mod$model[, 1] ~ mod$model[, 2])$fitted.values - Ybar) ^ 2)
  ## if mod is a multivariate linear regression
  if (par > 1) {
    for (i in 2:par) {
      pars = 2:i
      formula_str = paste0("mod$model[, 1] ~ ",
                           paste0("mod$model[, ", 2:(i + 1), collapse = "] + "),
                           "]")
      SSs[i] = sum((lm(as.formula(formula_str))$fitted.values - Ybar) ^ 2) - sum(SSs)
    }
    SSs[i + 1] = sum((mod$model[, 1] - mod$fitted.values) ^ 2)
  }
  ## if mod is a simple linear regression
  else {
    SSs[2] = sum((mod$model[, 1] - mod$fitted.values) ^ 2)
  }

  # Calculate mean sums of squares
  ## MSS = SS / df
  MSSs = SSs / dfs

  # Calculate F-statistics
  ## model MSS / residuals MSS
  Fs = c(MSSs[-length(MSSs)] / MSSs[length(MSSs)], NA)

  # Calculate p-values
  ## p = pf(F, model df, residual df)
  ps = pf(Fs, dfs[-length(dfs)], dfs[length(dfs)], lower.tail = FALSE)

  # Create output
  ret = list(Df = dfs,
             `Sum Sq` = SSs,
             `Mean Sq` = MSSs,
             `F value` = Fs,
             `Pr(>F)` = ps)
  model_names = names(mod$model)
  attributes(ret)$row.names = c(model_names[2:(par + 1)],
                                "Residuals")
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Response: ", model_names[1], collapse = ""))
  return(ret)
}
