#' get_dfs
#'
#' Calculates the degrees of freedom for a covariate in a linear regression model
#'
#' @param col column of the regression design matrix
#'
#' @return This function returns a numeric value indicating the degrees of freedom of a linear regression covariate.
#'
#' @examples
#' data(iris)
#' mod = lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species, data = iris)
#' get_dfs(mod$model[["Species"]])
#'
#' @export

get_dfs = function(col) {
  if (class(col) == "factor" | class(col) == "character") {
    df = length(unique(col)) - 1
  }
  else {
    df = 1
  }
  return(df)
}
