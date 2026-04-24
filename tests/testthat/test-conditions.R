test_that("pluto_check_response passes through 2xx responses unchanged", {
  resp <- list(response_status_code = 200L, items = list())
  expect_silent(out <- pluto_check_response(resp))
  expect_identical(out, resp)
})

test_that("404 with resource_not_found code -> pluto_not_found_error", {
  resp <- list(
    response_status_code = 404L,
    code = "resource_not_found",
    message = "not here",
    details = NULL
  )
  expect_error(
    pluto_check_response(resp),
    class = "pluto_not_found_error"
  )
})

test_that("400 with invalid_object_id code -> pluto_not_found_error (not validation)", {
  resp <- list(
    response_status_code = 400L,
    code = "invalid_object_id",
    message = "bogus id",
    details = NULL
  )
  err <- tryCatch(
    pluto_check_response(resp),
    pluto_not_found_error = function(e) e
  )
  expect_s3_class(err, "pluto_not_found_error")
  expect_s3_class(err, "pluto_api_error")
  expect_equal(err$status_code, 400L)
  expect_equal(err$code, "invalid_object_id")
})

test_that("400 with other code stays as pluto_validation_error", {
  resp <- list(
    response_status_code = 400L,
    code = "invalid_body_attribute",
    message = "bad"
  )
  err <- tryCatch(
    pluto_check_response(resp),
    pluto_validation_error = function(e) e
  )
  expect_s3_class(err, "pluto_validation_error")
  expect_false(inherits(err, "pluto_not_found_error"))
})

test_that("401 maps to pluto_auth_error", {
  resp <- list(response_status_code = 401L, message = "bad token")
  err <- tryCatch(
    pluto_check_response(resp),
    pluto_auth_error = function(e) e
  )
  expect_s3_class(err, "pluto_auth_error")
})

test_that("403 maps to pluto_permission_error", {
  resp <- list(response_status_code = 403L, message = "no access")
  err <- tryCatch(
    pluto_check_response(resp),
    pluto_permission_error = function(e) e
  )
  expect_s3_class(err, "pluto_permission_error")
})

test_that("429 maps to pluto_rate_limit_error", {
  resp <- list(response_status_code = 429L)
  err <- tryCatch(
    pluto_check_response(resp),
    pluto_rate_limit_error = function(e) e
  )
  expect_s3_class(err, "pluto_rate_limit_error")
})

test_that("5xx maps to pluto_server_error", {
  for (status in c(500L, 502L, 503L, 504L, 599L)) {
    resp <- list(response_status_code = status)
    err <- tryCatch(
      pluto_check_response(resp),
      pluto_server_error = function(e) e
    )
    expect_s3_class(err, "pluto_server_error")
  }
})

test_that("pluto_is_not_found and pluto_is_api_error predicates work", {
  resp <- list(response_status_code = 404L, code = "resource_not_found")
  err <- tryCatch(
    pluto_check_response(resp),
    error = function(e) e
  )
  expect_true(pluto_is_not_found(err))
  expect_true(pluto_is_api_error(err))

  # A plain error is neither
  plain_err <- simpleError("boom")
  expect_false(pluto_is_not_found(plain_err))
  expect_false(pluto_is_api_error(plain_err))
})
