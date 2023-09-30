test_that("plot details returns expected response - PCA / sample scatter plot", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  pca_details <- pluto_get_plot_details(experiment_id = TESTTHAT_EXPT_ID,
                                        plot_id = TESTTHAT_PCA_PLOT_ID)
  expect_equal(pca_details$status$status_code, 200)
  expect_equal(pca_details$plot_details$analysis_type, "principal_components")
  expect_equal(pca_details$plot_details$plot_type, "sample_scatter_plot")
})

test_that("plot details returns expected response - Differential binding / volcano v2", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  pca_details <- pluto_get_plot_details(experiment_id = TESTTHAT_EXPT_ID,
                                        plot_id = TESTTHAT_DB_VOLCANO_PLOT_V2_ID)
  expect_equal(pca_details$status$status_code, 200)
  expect_equal(pca_details$plot_details$analysis_type, "differential_binding")
  expect_equal(pca_details$plot_details$plot_type, "volcano_plot_v2")
})

test_that("plot results returns expected response - PCA", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  pca_results <- pluto_get_experiment_results(experiment_id = TESTTHAT_EXPT_ID,
                                              plot_id = TESTTHAT_PCA_PLOT_ID,
                                              table_type = "results",
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
                                              table_type = "results",
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
                                              table_type = "results",
                                              silent = T)
  expect_equal(deg_results$status$status_code, 200)
  expect_equal(nrow(deg_results$df), 16193)
  expect_equal(names(deg_results$df),
               c('Gene_Symbol', 'Average_Expression', 'Log2_Fold_Change',
                 'P_Value', 'Adj_P_Value'))
})

