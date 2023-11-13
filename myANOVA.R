get_dfs = function(col) {
  if (class(col) == "factor" | class(col) == "character") {
    df = length(unique(col)) - 1
  }
  else {
    df = 1
  }
}

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

my_anova1 = function(mod) {
  n = nrow(mod$model)
  par = ncol(mod$model) - 1
  Ybar = mean(mod$model[, 1])
  dfs = unname(c(sapply(mod$model[, -1, drop = FALSE], get_dfs), mod$df.residual))
  SSs = numeric(par + 1)
  SSs[1] = sum((lm(mod$model[, 1] ~ mod$model[, 2])$fitted.values - Ybar) ^ 2) - sum(SSs)
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

# set.seed(12345)
#
# df = data.frame(A = rbinom(200, 1, 0.5),
#                 B = as.factor(floor(runif(200, 0, 5))),
#                 C = runif(200),
#                 D = rnorm(200))
#
# mod0 = lm(A ~ 1, data = df)
# mod1 = lm(A ~ C, data = df)
# mod2 = lm(A ~ B + C, data = df)
# mod3 = lm(A ~ C + B + D, data = df)
#
# out = anova(mod1)
# my_out = my_anova(mod1)
# all.equal(out, my_out)
#
# out = anova(mod1)
# my_out = my_anova(mod1)
# all.equal(out, my_out)
#
# out = anova(mod2)
# my_out = my_anova(mod2)
# all.equal(out, my_out)
#
# out = anova(mod3)
# my_out = my_anova(mod3)
# all.equal(out, my_out)
#
# out = anova(mod0, mod1)
# my_out = my_anova(mod0, mod1)
# all.equal(out, my_out)
#
# out = anova(mod1, mod2)
# my_out = my_anova(mod1, mod2)
# all.equal(out, my_out)
#
# out = anova(mod1, mod2, mod3)
# my_out = my_anova(mod1, mod2, mod3)
# all.equal(out, my_out)
#
# out = anova(mod0, mod1, mod2, mod3)
# my_out = my_anova(mod0, mod1, mod2, mod3)
# all.equal(out, my_out)
#
# out
# out$Res.Df
# out$RSS
# out$Df
# out$`Sum of Sq`
# out$F
# out$`Pr(>F)`
#
# out = anova(mod0)
# my_out = my_anova1(mod0)
# all.equal(out, my_out)
