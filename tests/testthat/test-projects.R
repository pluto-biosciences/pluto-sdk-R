test_that("valid project response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  proj_resp <- pluto_list_projects()
  expect_equal(proj_resp$count, 24)
  expect_equal(length(proj_resp$items), proj_resp$count)
})

test_that("valid project experiment response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_list_project_experiments(project_id = TESTTHAT_PROJ_ID)
  expect_equal(expt_resp$count, 2)
})

test_that("valid project experiment data frame has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_df <- pluto_read_project_experiments(project_id = TESTTHAT_PROJ_ID)
  expected_names <- c("experiment_id", "name", "organism", "experiment_type",
    "share_level", "status", "assay_data_units", "plot_count", "owner", "created_by")
  expect_equal(nrow(expt_df), 2)
  expect_equal(names(expt_df), expected_names)
})
