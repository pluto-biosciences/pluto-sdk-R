test_that("list analyses returns expected response - CUT&RUN example", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  all_analyses <- pluto_list_experiment_analyses(experiment_id = TESTTHAT_EXPT_ID)
  expect_equal(all_analyses$count, 6)
  expect_equal(length(all_analyses$items), all_analyses$count)
})

test_that("list analyses returns expected response - RNA-seq example", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  all_analyses <- pluto_list_experiment_analyses(experiment_id = TESTTHAT_EXPT_ID_RNASEQ)
  expect_equal(all_analyses$count, 2)
  expect_equal(length(all_analyses$items), all_analyses$count)
})

test_that("read experiment analyses returns expected data frame - CUT&RUN example", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  analysis_df <- pluto_read_experiment_analyses(experiment_id = TESTTHAT_EXPT_ID)
  expect_equal(nrow(analysis_df), 6)
  expect_equal(names(analysis_df), c('analysis_type', 'analysis_name', 'display_type',
                                     'share_level', 'status', 'is_archived'))
})
