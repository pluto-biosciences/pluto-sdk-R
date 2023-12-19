test_that("valid response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 10
  sample_data <- pluto_get_experiment_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                              table_type = "sample", limit = expected_rows,
                              silent = T)
  expect_equal(sample_data$status_code, 200)
  expect_equal(nrow(sample_data$df), expected_rows)
})

test_that("unauthorized response has correct structure", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_INVALID_API_TOKEN)
  sample_data <- pluto_get_experiment_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                              table_type = "sample", limit = 10, silent = T)
  expect_equal(sample_data$status_code, 401)
  expect_null(sample_data$df)
})

test_that("experiment doesn't exist", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  sample_data <- pluto_get_experiment_data(experiment_id = "PLXBAD", table_type = "sample",
                              limit = 10, silent = T)
  expect_equal(sample_data$status_code, 400)
  expect_null(sample_data$df)
})

test_that("nonpaginated limit works", {
  skip_on_cran()
  #skip(message = "Skipping long-running integration test")
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 10
  assay_data <- pluto_get_experiment_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                          table_type = "assay", limit = expected_rows,
                                          silent = F)
  expect_equal(assay_data$status_code, 200)
  expect_equal(nrow(assay_data$df), expected_rows)

  gene_names <- assay_data$df$gene_symbol
  expect_false(any(duplicated(gene_names)))
})

test_that("paginated limit works", {
  skip_on_cran()
  #skip(message = "Skipping long-running integration test")
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 10003
  assay_data <- pluto_get_experiment_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                          table_type = "assay", limit = expected_rows,
                                          silent = F)
  expect_equal(assay_data$status_code, 200)
  expect_equal(nrow(assay_data$df), expected_rows)

  gene_names <- assay_data$df$gene_symbol
  expect_false(any(duplicated(gene_names)))
})

test_that("paginated no limit works", {
  skip_on_cran()
  #skip(message = "Skipping long-running pagination integration test")
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- NUM_EXPT_TARGETS_RNASEQ
  assay_data <- pluto_get_experiment_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                          table_type = "assay",
                                          silent = F)
  expect_equal(assay_data$status_code, 200)
  expect_equal(nrow(assay_data$df), expected_rows)

  gene_names <- assay_data$df$gene_symbol
  expect_false(any(duplicated(gene_names)))
})

test_that("pluto_read_data sample data saves a data frame with paginated limit", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 10
  sample_data <- pluto_read_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                 table_type = "sample", limit = expected_rows,
                                 silent = T)
  expect_s3_class(sample_data, 'data.frame')
  expect_equal(nrow(sample_data), expected_rows)
})

test_that("pluto_read_data assay data saves a data frame with paginated limit", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 12
  assay_data <- pluto_read_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                table_type = "assay",
                                limit = expected_rows,
                                silent = T)
  expect_s3_class(assay_data, 'data.frame')
  expect_equal(nrow(assay_data), expected_rows)
})

test_that("pluto_read_data no limit works", {
  skip_on_cran()
  #skip(message = "Skipping long-running pagination integration test")
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- NUM_EXPT_TARGETS_RNASEQ
  assay_data <- pluto_read_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                                table_type = "assay",
                                silent = F)
  expect_equal(nrow(assay_data), expected_rows)
  gene_names <- assay_data$df$gene_symbol
  expect_false(any(duplicated(gene_names)))
})

test_that("pluto_download_sample_data downloads CSV", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- NUM_EXPT_SAMPLES_RNASEQ
  expected_filename <- "sample_foo.csv"
  pluto_download_sample_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                             dest_filename = expected_filename)
  expect_true(file.exists(expected_filename))
  df <- read.csv(expected_filename)
  expect_equal(nrow(df), expected_rows)
  file.remove(expected_filename)
})

test_that("pluto_download_assay_data downloads CSV", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- NUM_EXPT_TARGETS_RNASEQ
  expected_filename <- "assay_foo.csv"
  pluto_download_assay_data(experiment_id = TESTTHAT_EXPT_ID_RNASEQ,
                            dest_filename = expected_filename)
  expect_true(file.exists(expected_filename))
  df <- read.csv(expected_filename)
  expect_equal(nrow(df), expected_rows)
  file.remove(expected_filename)
})
