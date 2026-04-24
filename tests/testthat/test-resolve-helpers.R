test_that(".resolve_timeout uses env var when arg is NULL", {
  withr::with_envvar(c(PLUTO_TIMEOUT = "45"), {
    expect_equal(.resolve_timeout(NULL), 45)
  })
})

test_that(".resolve_timeout falls back to the package default", {
  withr::with_envvar(c(PLUTO_TIMEOUT = NA), {
    expect_equal(.resolve_timeout(NULL), .pluto_default_timeout)
  })
})

test_that(".resolve_timeout prefers explicit arg", {
  withr::with_envvar(c(PLUTO_TIMEOUT = "45"), {
    expect_equal(.resolve_timeout(10), 10)
  })
})

test_that(".resolve_timeout ignores malformed env values", {
  withr::with_envvar(c(PLUTO_TIMEOUT = "not-a-number"), {
    expect_equal(.resolve_timeout(NULL), .pluto_default_timeout)
  })
})

test_that(".resolve_max_retries uses env var when arg is NULL", {
  withr::with_envvar(c(PLUTO_MAX_RETRIES = "7"), {
    expect_equal(.resolve_max_retries(NULL), 7L)
  })
})

test_that(".resolve_max_retries clamps negative values to zero", {
  expect_equal(.resolve_max_retries(-5), 0L)
})

test_that(".resolve_max_retries falls back to the package default", {
  withr::with_envvar(c(PLUTO_MAX_RETRIES = NA), {
    expect_equal(.resolve_max_retries(NULL), .pluto_default_max_retries)
  })
})
