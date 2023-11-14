test_that("pluto_download_sample_data downloads CSV", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 11
  expected_filename <- "sample_foo.csv"
  pluto_download_sample_data(experiment_id = TESTTHAT_EXPT_ID,
                             dest_filename = expected_filename)
  expect_true(file.exists(expected_filename))
  df <- read.csv(expected_filename)
  expect_equal(nrow(df), expected_rows)
  file.remove(expected_filename)
})

test_that("pluto_download_assay_data downloads CSV", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN=TESTTHAT_API_TOKEN)
  expected_rows <- 15629
  expected_filename <- "assay_foo.csv"
  pluto_download_assay_data(experiment_id = TESTTHAT_EXPT_ID,
                            dest_filename = expected_filename)
  expect_true(file.exists(expected_filename))
  df <- read.csv(expected_filename)
  expect_equal(nrow(df), expected_rows)
  file.remove(expected_filename)
})
