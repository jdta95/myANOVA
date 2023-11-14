data(iris)
mod1 = lm(Sepal.Length ~ Species + Sepal.Width,
           data = iris)
blah = lm(Sepal.Length ~ -1 + Species,
          data = iris)
mod1b = lm(Sepal.Length ~ -1 + Species + Sepal.Width,
           data = iris)
out = anova(mod1b)

mod = mod1b

SST = sum((iris$Sepal.Length - mean(iris$Sepal.Length)) ^ 2)
SSM = sum((blah$coefficients - mean(iris$Sepal.Length)) ^ 2 * table(iris$Species))
SSE = SST - SSM

mean(iris$Sepal.Length[iris$Species == "virginica"])

sum(50 * (blah$coefficients - mean(iris$Sepal.Length)) ^ 2)

my_anova1_cellmeans = function(mod) {
  n = nrow(mod$model)
  par = ncol(mod$model) - 1
  Ybar = mean(mod$model[, 1])
  dfs = unname(c(sapply(mod$model[, -1, drop = FALSE], get_dfs), mod$df.residual))
  index = min(which(sapply(mod$model[, -1, drop = FALSE],
                           function(col) {
                             class(col) == "factor" | class(col) == "character"
                           })))
  dfs[index] = dfs[index] + 1



  SSs = numeric(par + 1)
  SSs[1] = sum((lm(mod$model[, 1] ~ -1 + mod$model[, 2])$fitted.values - Ybar) ^ 2) - sum(SSs)
  if (par > 1) {
    for (i in 2:par) {
      pars = 2:i
      formula_str = paste0("mod$model[, 1] ~ -1 + ",
                           paste0("mod$model[, ", 2:(i + 1), collapse = "] + "),
                           "]")
      SSs[i] = sum((lm(as.formula(formula_str))$fitted.values - Ybar) ^ 2) - sum(SSs)
    }
    SSs[i + 1] = sum((mod$model[, 1] - mod$fitted.values) ^ 2)
  }
  else {
    SSs[2] = sum((mod$model[, 1] - mod$fitted.values) ^ 2)
  }
  MSSs = SSs / dfs
  Fs = c(MSSs[-length(MSSs)] / MSSs[length(MSSs)], NA)
  ps = pf(Fs, dfs[-length(dfs)], dfs[length(dfs)], lower.tail = FALSE)
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
