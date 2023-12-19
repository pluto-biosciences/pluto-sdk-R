test_that("plot results returns expected response - PCA", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  pca_results <- pluto_get_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                   plot_id = TESTTHAT_PLOT_ID_PCA,
                                   silent = T)
  expect_equal(pca_results$response_status_code, 200)
  expect_equal(nrow(pca_results$df), NUM_EXPT_SAMPLES_RNASEQ)
  expect_equal(names(pca_results$df),
               c('Sample_ID', paste0('PC', 1:5), 'group_id', 'group_name'))
})

test_that("plot results returns expected response - Differential binding", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  deg_results <- pluto_get_results(experiment_id = TESTTHAT_EXPT_ID_CUTANDRUN,
                                   plot_id = TESTTHAT_PLOT_ID_DB_VOLCANO_V2,
                                   silent = T)
  expect_equal(deg_results$response_status_code, 200)
  expect_equal(nrow(deg_results$df), NUM_DB_TARGETS_CUTANDRUN)
  expect_equal(names(deg_results$df),
               c('peak_id', 'Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
})

test_that("plot results returns expected response - Differential expression", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  deg_results <- pluto_get_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                   plot_id = TESTTHAT_PLOT_ID_DE_VOLCANO_V2,
                                   silent = T)
  expect_equal(deg_results$response_status_code, 200)
  expect_equal(nrow(deg_results$df), NUM_DE_TARGETS_RNASEQ)
  expect_equal(names(deg_results$df),
               c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
})

test_that("read plot results returns expected response - Differential expression", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  deg_results <- pluto_read_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                    plot_id = TESTTHAT_PLOT_ID_DE_VOLCANO_V2,
                                    silent = T)
  expect_s3_class(deg_results, 'data.frame')
  expect_equal(names(deg_results),
               c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
  expect_equal(nrow(deg_results), NUM_DE_TARGETS_RNASEQ)
})

test_that("download plot results returns expected response - Differential expression", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  temp_filename <- 'foo.csv'
  expect_false(file.exists(temp_filename))
  pluto_download_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                         plot_id = TESTTHAT_PLOT_ID_DE_VOLCANO_V2,
                         dest_filename = temp_filename)
  expect_true(file.exists(temp_filename))
  deg_results <- read.csv(temp_filename, stringsAsFactors = F)
  expect_true(all(c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value') %in% names(deg_results)))
  expect_equal(nrow(deg_results), NUM_DE_TARGETS_RNASEQ)
  file.remove(temp_filename)
})
