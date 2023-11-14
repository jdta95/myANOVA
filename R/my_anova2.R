#' my_anova2
#'
#' Perform ANOVA analyses on multiple linear regression models
#'
#' @param mods list containing at least 2 objects of type "lm"
#'
#' @return This function returns an object of class anova. These objects represent analysis-of-variance tables.
#'
#' @examples
#' data(iris)
#' mod1 = lm(Sepal.Length ~ Sepal.Width, data = iris)
#' mod2 = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)
#' my_anova2(mod1, mod2)
#'
#' @export

my_anova2 = function(mods) {
  len = length(mods)
  res_dfs = sapply(mods, function(mod) mod$df.residual)
  RSSs = sapply(mods, function(mod) sum(mod$residuals ^ 2))
  dfs = integer(len)
  dfs[1] = NA
  dfs[2:len] = res_dfs[1:(len - 1)] - res_dfs[2:len]
  SSs = numeric(len)
  SSs[1] = NA
  SSs[2:len] = RSSs[1:(len - 1)] - RSSs[2:len]
  Fs = numeric(len)
  Fs[1] = NA
  Fs[2:len] = ((RSSs[1:(len - 1)] - RSSs[2:len]) / dfs[2:len]) /
    (RSSs[len] / res_dfs[len])
  ps = pf(Fs, dfs, res_dfs[len], lower.tail = FALSE)
  ret = list(Res.Df = res_dfs,
             RSS = RSSs,
             Df = dfs,
             `Sum of Sq` = SSs,
             `F` = Fs,
             `Pr(>F)` = ps)
  attributes(ret)$class = c("anova", "data.frame")
  attributes(ret)$row.names = as.character(1:len)
  formula_strs = sapply(mods, function(mod) as.character(mod$call)[2])
  attributes(ret)$heading = c("Analysis of Variance Table\n",
                              paste0("Model ",
                                     1:len,
                                     ": ",
                                     formula_strs,
                                     collapse = "\n"))
  return(ret)
}
