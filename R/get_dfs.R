#' get_dfs
#'
#' Calculates the degrees of freedom for a covariate in a linear regression model
#'
#' @param col column of the regression design matrix
#'
#' @return This function returns a numeric value indicating the degrees of freedom of a linear regression covariate. Note this function assumes the linear model contains an intercept.
#'
#' @examples
#' data(iris)
#' mod = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
#' get_dfs(mod$model[["Species"]])
#'
#' @export

get_dfs = function(col) {
  # Get degrees of freedom (df) for categorical variables
  ## df = number of groups - 1
  if (is.factor(col) | is.character(col)) {
    df = length(unique(col)) - 1
  }
  # Get df for numeric variables
  ## df = 1
  else {
    df = 1
  }
  return(df)
}
