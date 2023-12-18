test_that("get plots returns expected response - PCA / sample scatter plot", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  all_plots <- pluto_get_experiment_plots(experiment_id = TESTTHAT_EXPT_ID_RNASEQ)
  expect_equal(all_plots$response_status_code, 200)
  expect_equal(all_plots$count, length(all_plots$items))
})

test_that("plot details returns expected response - PCA / sample scatter plot", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  pca_details <- pluto_get_plot(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                plot_id = TESTTHAT_PLOT_ID_PCA)
  expect_equal(pca_details$status_code, 200)
  expect_equal(pca_details$plot_details$analysis_type, "principal_components")
  expect_equal(pca_details$plot_details$plot_type, "sample_scatter_plot")
})

test_that("plot details returns expected response - Differential expression / volcano v2", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  vol_details <- pluto_get_plot(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                plot_id = TESTTHAT_PLOT_ID_DE_VOLCANO_V2)
  expect_equal(vol_details$status_code, 200)
  expect_equal(vol_details$plot_details$analysis_type, "differential_expression")
  expect_equal(vol_details$plot_details$plot_type, "volcano_plot_v2")
})
