test_that("valid project response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  proj_resp <- pluto_get_projects()
  expect_equal(proj_resp$count, 3)
  expect_equal(length(proj_resp$items), proj_resp$count)
})

test_that("valid project details response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  proj_resp <- pluto_get_project(project_id = TESTTHAT_NONEMPTY_PROJ_ID)
  expect_equal(proj_resp$pluto_id, TESTTHAT_NONEMPTY_PROJ_ID)
})

test_that("valid project with experiments response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_get_project_experiments(project_id = TESTTHAT_NONEMPTY_PROJ_ID)
  expect_equal(expt_resp$count, NUM_PROJ_EXPTS)
})

test_that("valid project without experiments response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_resp <- pluto_get_project_experiments(project_id = TESTTHAT_EMPTY_PROJ_ID)
  expect_equal(expt_resp$count, 0)
})

test_that("valid project experiment data frame has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expt_df <- pluto_read_project_experiments(project_id = TESTTHAT_NONEMPTY_PROJ_ID)
  expected_names <- c("experiment_id", "name", "organism", "experiment_type",
    "share_level", "status", "assay_data_units", "plot_count", "owner", "created_by", "created_at")
  expect_equal(nrow(expt_df), NUM_PROJ_EXPTS)
  expect_equal(names(expt_df), expected_names)
})
