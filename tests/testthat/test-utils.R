test_that("is alphanumeric", {
  expect_true(is_alphanumeric("abc"))
})

test_that("is not alphanumeric", {
  expect_false(is_alphanumeric("ab-!c"))
})

test_that("is quiet", {
  expect_silent(quiet_message(silent = T, message = "hello"))
})

test_that("is verbose", {
  suppressMessages(expect_equal(quiet_message(silent = F, message = "hello"),
                                message("hello")))
})

test_that("invalid length API key is invalid", {
  expect_false(is_valid_api_key("abc"))
})

test_that("invalid character API key is invalid", {
  expect_false(is_valid_api_key("ab-6e0a8ea4b7fe8a7fc16f93509363ca2092096"))
})

test_that("valid API key is validated", {
  skip_on_cran()
  expect_true(is_valid_api_key(TESTTHAT_API_TOKEN))
})
