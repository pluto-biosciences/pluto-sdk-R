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
