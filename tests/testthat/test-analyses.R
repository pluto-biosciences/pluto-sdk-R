test_that("get analyses returns expected response - CUT&RUN example", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  all_analyses <- pluto_get_experiment_analyses(experiment_id = TESTTHAT_EXPT_ID_CUTANDRUN)
  expect_equal(all_analyses$count, NUM_ANALYSES_CUTANDRUN)
  expect_equal(length(all_analyses$items), all_analyses$count)
})

test_that("get analyses returns expected response - RNA-seq example", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  all_analyses <- pluto_get_experiment_analyses(experiment_id = TESTTHAT_EXPT_ID_RNASEQ)
  expect_equal(all_analyses$count, NUM_ANALYSES_RNASEQ)
  expect_equal(length(all_analyses$items), all_analyses$count)
})

test_that("read experiment analyses returns expected data frame - CUT&RUN example", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  analysis_df <- pluto_read_experiment_analyses(experiment_id = TESTTHAT_EXPT_ID_CUTANDRUN)
  expect_equal(nrow(analysis_df), NUM_ANALYSES_CUTANDRUN)
  expect_equal(names(analysis_df), c('analysis_type', 'analysis_name', 'display_type',
                                     'share_level', 'status', 'is_archived'))
})
