train_test <- function(asset_returns, size = 0.8, train = TRUE) {
  train_row = size * nrow(asset_returns)
  train_set <- 1: train_row
  if (train == TRUE) {
    return (asset_returns[train_set, ])
  } else {
    return (asset_returns[-train_set, ])
  }
}





asset_returnstrain <<- train_test(asset_returns, 0.8, train = TRUE)
asset_returnstest <<- train_test(asset_returns, 0.8, train = FALSE)
asset_returnstrain
asset_returnstest