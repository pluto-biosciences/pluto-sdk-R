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

test_that("clean_row converts NULLs to NA and keeps row length", {
  row <- list(Sample_ID = "s1", PC1 = NULL, PC2 = 2.5)
  cleaned <- clean_row(row)
  expect_equal(length(cleaned), 3)
  expect_equal(names(cleaned), c("Sample_ID", "PC1", "PC2"))
  expect_true(is.na(cleaned$PC1))
  expect_equal(cleaned$PC2, 2.5)
})

test_that("clean_row leaves rows without NULLs and empty rows unchanged", {
  row <- list(Sample_ID = "s1", PC1 = 1.1)
  expect_identical(clean_row(row), row)
  expect_identical(clean_row(list()), list())
})

test_that("data_response_to_df converts NULL cells to NA", {
  response_json <- list(
    count = 3,
    headers = list("Sample_ID", "PC1"),
    items = list(list(Sample_ID = "s1", PC1 = 1.1),
                 list(Sample_ID = "s2", PC1 = NULL),
                 list(Sample_ID = "s3", PC1 = 3.3))
  )
  df <- data_response_to_df(response_json)
  expect_equal(nrow(df), 3)
  expect_equal(names(df), c("Sample_ID", "PC1"))
  expect_equal(unlist(df$PC1), c(1.1, NA, 3.3))
})

test_that("arrow_response_to_df converts NULL values to NA", {
  response_json <- list(
    count = 3,
    headers = list("Gene_Symbol", "Adj_P_Value"),
    items = list(list("A", NULL, "C"),
                 list(0.01, NULL, 0.5))
  )
  df <- arrow_response_to_df(response_json)
  expect_equal(nrow(df), 3)
  expect_equal(names(df), c("Gene_Symbol", "Adj_P_Value"))
  expect_equal(unlist(df$Gene_Symbol), c("A", NA, "C"))
  expect_equal(unlist(df$Adj_P_Value), c(0.01, NA, 0.5))
})
