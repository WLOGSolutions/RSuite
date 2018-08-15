library(testthat)

test_results <- data.frame(testthat::test_dir("."), stringsAsFactors = FALSE)
if (any(test_results$failed) || any(test_results$error)) {
  q(status = 1)
}
