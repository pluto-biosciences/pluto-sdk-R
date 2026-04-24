# Mock tests for the R workflow helpers.

test_that("pluto_list_workflows builds correct URL", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(items = list(), response_status_code = 200L)
    }
  )
  pluto_list_workflows("PLX076418")
  expect_equal(captured, "lab/experiments/PLX076418/workflows/")
})


test_that("pluto_list_workflows unwraps bare-list response", {
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      # Simulate bare JSON array with status code appended
      list(
        list(uuid = "w-1", name = "X", status = "completed"),
        list(uuid = "w-2", name = "Y", status = "accepted"),
        response_status_code = 200L
      )
    }
  )
  out <- pluto_list_workflows("PLX076418")
  expect_equal(length(out), 2)
  expect_equal(out[[1]]$uuid, "w-1")
  expect_equal(out[[2]]$status, "accepted")
})


test_that("pluto_get_workflow builds correct URL", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(uuid = "w-1", status = "completed", response_status_code = 200L)
    }
  )
  pluto_get_workflow("PLX076418", "w-1")
  expect_equal(captured, "lab/experiments/PLX076418/workflows/w-1/")
})


test_that("accept/archive/copy POST to the correct endpoints", {
  captured <- character()
  local_mocked_bindings(
    pluto_POST = function(url_path, body_data, ...) {
      captured <<- c(captured, url_path)
      list(message = "ok", response_status_code = 200L)
    }
  )
  pluto_accept_workflow("PLX076418", "w-1")
  pluto_archive_workflow("PLX076418", "w-1")
  pluto_copy_workflow("PLX076418", "w-1", list(name = "copy"))
  expect_equal(captured, c(
    "lab/experiments/PLX076418/workflows/w-1/accept/",
    "lab/experiments/PLX076418/workflows/w-1/archive/",
    "lab/experiments/PLX076418/workflows/w-1/copy/"
  ))
})


test_that("pluto_wait_for_workflow returns immediately for terminal statuses", {
  for (final_status in c("completed", "accepted", "failed")) {
    local_mocked_bindings(
      pluto_GET = function(url_path, ...) {
        list(uuid = "w-1", status = final_status, response_status_code = 200L)
      }
    )
    t0 <- Sys.time()
    wf <- pluto_wait_for_workflow(
      "PLX076418", "w-1", poll_interval = 5, timeout = 10
    )
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    expect_equal(wf$status, final_status)
    expect_lt(elapsed, 2)  # should not have slept
  }
})


test_that("pluto_wait_for_workflow transitions in_progress -> completed", {
  calls <- 0
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      calls <<- calls + 1
      status <- if (calls < 2) "in_progress" else "completed"
      list(uuid = "w-1", status = status, response_status_code = 200L)
    }
  )
  wf <- pluto_wait_for_workflow(
    "PLX076418", "w-1", poll_interval = 0.01, timeout = 10
  )
  expect_equal(wf$status, "completed")
  expect_gte(calls, 2)
})


test_that("pluto_wait_for_workflow raises on timeout", {
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      list(uuid = "w-1", status = "in_progress", response_status_code = 200L)
    }
  )
  expect_error(
    pluto_wait_for_workflow(
      "PLX076418", "w-1", poll_interval = 0.02, timeout = 0.05
    ),
    "Timed out"
  )
})


test_that("pluto_list_workflow_preprocesses builds correct URL", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(items = list(), response_status_code = 200L)
    }
  )
  pluto_list_workflow_preprocesses("PLX076418", "w-1")
  expect_equal(
    captured,
    "lab/experiments/PLX076418/workflows/w-1/preprocesses/"
  )
})
