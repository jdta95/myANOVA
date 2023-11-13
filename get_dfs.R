get_dfs = function(col) {
  if (class(col) == "factor" | class(col) == "character") {
    df = length(unique(col)) - 1
  }
  else {
    df = 1
  }
}
