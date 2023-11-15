#' my_anova1_nointercept
#'
#' Performs a sequential ANOVA analysis on one no-intercept linear regression model.
#'
#' @param mod an "lm" object with no intercept in the linear model
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

  # sum(Y ^ 2)
  Ysumsquare = sum(mod$model[, 1] ^ 2)

  # Use get_dfs on each covariate to get the degrees of freedom (df)
  ## if numeric, df = 1
  ## if first categorical, df = number of groups
  ## if additional categorical, df = number of groups - 1
  dfs = unlist(unname(c(sapply(mod$model[,-1, drop = FALSE], get_dfs), mod$df.residual)))
  cat_var = sapply(mod$model[, -1, drop = FALSE], function(col) {
    is.factor(col) | is.character(col)
  })
  if (any(cat_var)) {
    index = min(which(cat_var))
    dfs[index] = dfs[index] + 1
  }

  # Calculate the sums of squares (SS) for each sequential model
  ## SS = sum(Y ^ 2) - sum((Y - Yhat) ^ 2) - (previous SS's)
  SSs = numeric(par + 1)
  if (par == 0) {
    SSs = sum(mod$model[, 1] ^ 2)
  }
  else {
    for (i in 1:par) {
      formula_str = paste0("mod$model[, 1] ~ - 1 + ",
                           paste0("mod$model[, ", 2:(i + 1), collapse = "] + "),
                           "]")
      SSs[i] = Ysumsquare - sum((lm(as.formula(formula_str))$residuals) ^ 2) - sum(SSs)
    }
    SSs[i + 1] = sum(mod$residuals ^ 2)
  }

  # Calculate mean sums of squares
  ## MSS = SS / df
  MSSs = SSs / dfs

  # Calculate F-statistics
  ## model MSS / residuals MSS
  Fs = c(MSSs[-length(MSSs)] / MSSs[length(MSSs)], NA * 0)

  # Calculate p-values
  ## p = pf(F, model df, residual df)
  ps = pf(Fs, dfs[-length(dfs)], dfs[length(dfs)], lower.tail = FALSE)
  if (length(ps) == 0) {
    ps = NA * 0
  }

  # Create output
  ret = list(
    Df = dfs,
    `Sum Sq` = SSs,
    `Mean Sq` = MSSs,
    `F value` = Fs,
    `Pr(>F)` = ps
  )
  model_names = names(mod$model)
  if (par == 0) {
    attributes(ret)$row.names = "Residuals"
  }
  else {
    attributes(ret)$row.names = c(model_names[2:(par + 1)],
                                "Residuals")
  }
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Response: ", model_names[1], collapse = ""))
  return(ret)
}
