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
