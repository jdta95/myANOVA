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
