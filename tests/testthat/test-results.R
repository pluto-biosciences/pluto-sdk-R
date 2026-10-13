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

mock_plot_GET <- function(display_type, data_resp){
  function(url_path, api_token = NULL){
    if (grepl("/data/", url_path)){
      return(c(data_resp, response_status_code = 200))
    }
    list(analysis_type = "differential_expression",
         analysis = list(name = "Analysis"),
         display = list(display_type = display_type, plot_title = "Plot"),
         response_status_code = 200)
  }
}

test_that("read plot results handles missing values - volcano v2", {
  data_resp <- list(
    count = 3,
    headers = list("Gene_Symbol", "Log2_Fold_Change", "Adj_P_Value"),
    items = list(list("A", "B", NULL),
                 list(1.5, -0.2, 0.1),
                 list(0.01, NULL, 0.5))
  )
  local_mocked_bindings(pluto_GET = mock_plot_GET("volcano_plot_v2", data_resp))
  deg_results <- pluto_read_results(experiment_id = "PLX123456",
                                    plot_id = "plot-uuid", silent = T)
  expect_s3_class(deg_results, 'data.frame')
  expect_equal(nrow(deg_results), 3)
  expect_equal(deg_results$Gene_Symbol, c("A", "B", NA))
  expect_equal(deg_results$Adj_P_Value, c(0.01, NA, 0.5))
})

test_that("read plot results handles missing values - sample scatter plot", {
  data_resp <- list(
    count = 3,
    headers = list("Sample_ID", "PC1"),
    items = list(list(Sample_ID = "s1", PC1 = 1.1),
                 list(Sample_ID = "s2", PC1 = NULL),
                 list(Sample_ID = "s3", PC1 = 3.3))
  )
  local_mocked_bindings(pluto_GET = mock_plot_GET("sample_scatter_plot", data_resp))
  pca_results <- pluto_read_results(experiment_id = "PLX123456",
                                    plot_id = "plot-uuid", silent = T)
  expect_equal(nrow(pca_results), 3)
  expect_equal(pca_results$Sample_ID, c("s1", "s2", "s3"))
  expect_equal(pca_results$PC1, c(1.1, NA, 3.3))
})

test_that("read plot results without missing values is unchanged", {
  data_resp <- list(
    count = 2,
    headers = list("Gene_Symbol", "Adj_P_Value"),
    items = list(list("A", "B"),
                 list(0.01, 0.5))
  )
  local_mocked_bindings(pluto_GET = mock_plot_GET("volcano_plot_v2", data_resp))
  deg_results <- pluto_read_results(experiment_id = "PLX123456",
                                    plot_id = "plot-uuid", silent = T)
  expect_equal(deg_results$Gene_Symbol, c("A", "B"))
  expect_equal(deg_results$Adj_P_Value, c(0.01, 0.5))
})
