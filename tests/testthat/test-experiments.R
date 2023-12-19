test_that("valid experiment response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_get_experiments_all()
  expect_equal(expt_resp$count, NUM_ALL_EXPTS)
  expect_equal(length(expt_resp$items), expt_resp$count)
})

test_that("valid experiment response has correct structure with limit", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_get_experiments_all(limit=1)
  expect_equal(expt_resp$count, NUM_ALL_EXPTS)
  expect_equal(length(expt_resp$items), 1)
})

test_that("valid experiment details response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_get_experiment(experiment_id=TESTTHAT_EXPT_ID_RNASEQ)
  expect_equal(expt_resp$pluto_id, TESTTHAT_EXPT_ID_RNASEQ)
})
