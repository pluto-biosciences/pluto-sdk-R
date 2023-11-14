test_that("plot results returns expected response - PCA", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  pca_results <- pluto_get_experiment_results(experiment_id = TESTTHAT_EXPT_ID,
                                              plot_id = TESTTHAT_PCA_PLOT_ID,
                                              silent = T)
  expect_equal(pca_results$status$status_code, 200)
  expect_equal(nrow(pca_results$df), 11)
  expect_equal(names(pca_results$df),
               c('Sample_ID', paste0('PC', 1:5), 'group_id', 'group_name'))
})

test_that("plot results returns expected response - Differential binding", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  deg_results <- pluto_get_experiment_results(experiment_id = TESTTHAT_EXPT_ID,
                                              plot_id = TESTTHAT_DB_VOLCANO_PLOT_V2_ID,
                                              silent = T)
  expect_equal(deg_results$status$status_code, 200)
  expect_equal(nrow(deg_results$df), 15627)
  expect_equal(names(deg_results$df),
               c('peak_id', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
})

test_that("plot results returns expected response - Differential expression", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  deg_results <- pluto_get_experiment_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                              plot_id = TESTTHAT_DE_VOLCANO_PLOT_V2_ID,
                                              silent = T)
  expect_equal(deg_results$status$status_code, 200)
  expect_equal(nrow(deg_results$df), 16193)
  expect_equal(names(deg_results$df),
               c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
})

test_that("read plot results returns expected response - Differential expression", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  deg_results <- pluto_read_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                    plot_id = TESTTHAT_DE_VOLCANO_PLOT_V2_ID,
                                    silent = T)
  expect_s3_class(deg_results, 'data.frame')
  expect_equal(names(deg_results),
               c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
})

test_that("download plot results returns expected response - Differential expression", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  temp_filename <- 'foo.csv'
  expect_false(file.exists(temp_filename))
  pluto_download_results(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                         plot_id = TESTTHAT_DE_VOLCANO_PLOT_V2_ID,
                         dest_filename = temp_filename)
  expect_true(file.exists(temp_filename))
  deg_results <- read.csv(temp_filename, stringsAsFactors = F)
  expect_true(all(c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value') %in% names(deg_results)))
  file.remove(temp_filename)
})
