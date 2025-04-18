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

test_that("experiment ID not valid throws error", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expect_error(pluto_get_experiment_data(experiment_id = "PLXBAD", table_type = "sample",
                              limit = 10, silent = T))

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

test_that("pluto_read_seurat_object raw works", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_cells <- NUM_EXPT_CELLS_RAW_SCRNASEQ
  expected_cols <- NUM_EXPT_COLS_RAW_SCRNASEQ
  so <- pluto_read_seurat_object(experiment_id = TESTTHAT_EXPT_ID_SCRNASEQ,
                                 seurat_type = "raw",
                                 silent = FALSE)
  expect_equal(nrow(so@meta.data), expected_cells)
  expect_equal(ncol(so@meta.data), expected_cols)
})

test_that("pluto_read_seurat_object final works", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_cells <- NUM_EXPT_CELLS_FINAL_SCRNASEQ
  expected_cols <- NUM_EXPT_COLS_FINAL_SCRNASEQ_WA
  expected_annotation_sets <- CUSTOM_ANNOTATION_SETS
  expected_palettes <- NUM_COLOR_PALETTES
  expected_color_1 <- CLUSTER_COLOR_CHECK_1
  expected_color_2 <- CLUSTER_COLOR_CHECK_2
  so <- pluto_read_seurat_object(experiment_id = TESTTHAT_EXPT_ID_SCRNASEQ,
                                 seurat_type = "final",
                                 silent = FALSE)
  expect_equal(nrow(so$obj@meta.data), expected_cells)
  expect_equal(ncol(so$obj@meta.data), expected_cols)
  expect_in(expected_annotation_sets, colnames(so$obj@meta.data))
  expect_equal(length(so$colors), expected_palettes)
  expect_equal(so$colors$cell_types[[4]], expected_color_1)
  expect_equal(so$colors$default_clusters_res_0_4[[1]], expected_color_2)
})

test_that("pluto_download_seurat_object downloads raw RDS", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_filename <- "raw_seurat_object.rds"
  expected_cells <- NUM_EXPT_CELLS_RAW_SCRNASEQ
  expected_cols <- NUM_EXPT_COLS_RAW_SCRNASEQ
  pluto_download_seurat_object(experiment_id = TESTTHAT_EXPT_ID_SCRNASEQ,
                               seurat_type = "raw",
                               silent = FALSE,
                               dest_filename = expected_filename)
  expect_true(file.exists(expected_filename))
  so <- readRDS(expected_filename)
  expect_equal(nrow(so@meta.data), expected_cells)
  expect_equal(ncol(so@meta.data), expected_cols)
  file.remove(expected_filename)
})

test_that("pluto_download_seurat_object downloads final RDS", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_filename <- "final_seurat_object.rds"
  expected_cells <- NUM_EXPT_CELLS_FINAL_SCRNASEQ
  expected_cols <- NUM_EXPT_COLS_FINAL_SCRNASEQ_NA
  pluto_download_seurat_object(experiment_id = TESTTHAT_EXPT_ID_SCRNASEQ,
                               seurat_type = "final",
                               silent = FALSE,
                               dest_filename = expected_filename)
  expect_true(file.exists(expected_filename))
  so <- readRDS(expected_filename)
  expect_equal(nrow(so@meta.data), expected_cells)
  expect_equal(ncol(so@meta.data), expected_cols)
  file.remove(expected_filename)
})
