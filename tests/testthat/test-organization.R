test_that(".resolve_organization returns NULL when nothing is set", {
  withr::with_envvar(c(PLUTO_ORGANIZATION = NA), {
    expect_null(.resolve_organization(NULL))
  })
})

test_that(".resolve_organization returns env var when set", {
  withr::with_envvar(c(PLUTO_ORGANIZATION = "env-org"), {
    expect_equal(.resolve_organization(NULL), "env-org")
  })
})

test_that(".resolve_organization prefers explicit argument over env var", {
  withr::with_envvar(c(PLUTO_ORGANIZATION = "env-org"), {
    expect_equal(.resolve_organization("arg-org"), "arg-org")
  })
})

test_that(".resolve_organization treats empty string as unset", {
  expect_null(.resolve_organization(""))
})

test_that("pluto_use_organization sets and clears the env var", {
  withr::with_envvar(c(PLUTO_ORGANIZATION = NA), {
    pluto_use_organization("abc-123")
    expect_equal(Sys.getenv("PLUTO_ORGANIZATION"), "abc-123")
    expect_equal(pluto_current_organization(), "abc-123")

    pluto_use_organization(NULL)
    expect_equal(Sys.getenv("PLUTO_ORGANIZATION"), "")
    expect_null(pluto_current_organization())
  })
})

test_that("pluto_current_organization returns NULL when unset", {
  withr::with_envvar(c(PLUTO_ORGANIZATION = NA), {
    expect_null(pluto_current_organization())
  })
})
