test_that("valid experiment response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_list_experiments()
  expect_equal(expt_resp$count, 115)
  expect_equal(length(expt_resp$items), expt_resp$count)
})

test_that("valid experiment response has correct structure with limit", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_list_experiments(limit=1)
  expect_equal(expt_resp$count, 115)
  expect_equal(length(expt_resp$items), 1)
})
