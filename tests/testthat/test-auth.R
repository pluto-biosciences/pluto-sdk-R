test_that("failed login if invalid API key", {
  expect_error(pluto_login("badapikey"))
})

test_that("successful login if valid API key", {
  skip_on_cran()
  expect_true(pluto_login(TESTTHAT_API_TOKEN, silent = T))
})

test_that("successful login sets env variable", {
  skip_on_cran()
  Sys.setenv(PLUTO_API_TOKEN = "")
  pluto_login(TESTTHAT_API_TOKEN, silent = T)
  expect_equal(Sys.getenv("PLUTO_API_TOKEN"), TESTTHAT_API_TOKEN)
})

test_that("logout resets env variable to blank", {
  Sys.setenv(PLUTO_API_TOKEN = "")
  pluto_login(TESTTHAT_API_TOKEN, silent = T)
  expect_equal(Sys.getenv("PLUTO_API_TOKEN"), TESTTHAT_API_TOKEN)

  pluto_logout(silent = T)
  expect_equal(Sys.getenv("PLUTO_API_TOKEN"), "")
})

test_that("user is logged in", {
  Sys.setenv(PLUTO_API_TOKEN = "")
  pluto_login(TESTTHAT_API_TOKEN, silent = T)
  expect_true(pluto_is_logged_in())
})

test_that("user is logged out", {
  Sys.setenv(PLUTO_API_TOKEN = "")
  pluto_login(TESTTHAT_API_TOKEN, silent = T)
  pluto_logout(silent = T)
  expect_false(pluto_is_logged_in())
})
