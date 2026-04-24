# Mock unit tests for programs + experiment history.

test_that("pluto_list_programs builds correct URL", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(count = 0, next_url = NULL, previous = NULL, results = list(),
           response_status_code = 200L)
    }
  )
  pluto_list_programs(offset = 5, limit = 3, search = "cancer")
  expect_match(captured, "^programs/")
  expect_match(captured, "offset=5")
  expect_match(captured, "limit=3")
  expect_match(captured, "search=cancer")
})


test_that("pluto_get_program builds correct URL", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(uuid = "p-1", name = "X", response_status_code = 200L)
    }
  )
  pluto_get_program("abc-123")
  expect_equal(captured, "programs/abc-123/")
})


test_that("pluto_get_program_framework_summary builds correct URL", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(dimensions = list(), response_status_code = 200L)
    }
  )
  pluto_get_program_framework_summary("p-1")
  expect_equal(captured, "programs/p-1/framework/summary/")
})


test_that("module-level lookups dispatch to correct endpoints", {
  captured <- character()
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- c(captured, url_path)
      list(response_status_code = 200L)
    }
  )
  pluto_list_program_types()
  pluto_list_framework_templates()
  pluto_list_program_labels()
  pluto_get_program_stats()
  expect_equal(captured, c(
    "programs/program-types/",
    "programs/framework-templates/",
    "programs/labels/",
    "programs/stats/"
  ))
})


test_that("framework sub-resource helpers all dispatch correctly", {
  captured <- character()
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- c(captured, url_path)
      list(response_status_code = 200L)
    }
  )
  pluto_get_program_framework("p-1")
  pluto_list_program_framework_versions("p-1")
  pluto_list_program_coverage_gaps("p-1")
  pluto_list_program_next_steps("p-1")
  pluto_list_program_evidence_sources("p-1")
  pluto_list_program_assets("p-1")
  pluto_list_program_analysis_spaces("p-1")
  expect_equal(captured, c(
    "programs/p-1/framework/",
    "programs/p-1/framework/versions/",
    "programs/p-1/framework/coverage-gaps/",
    "programs/p-1/framework/next-steps/",
    "programs/p-1/framework/evidence-sources/",
    "programs/p-1/assets/",
    "programs/p-1/analysis-spaces/"
  ))
})


test_that("pluto_get_experiment_history sends no params when cursor absent", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(items = list(), next_cursor = NULL, response_status_code = 200L)
    }
  )
  pluto_get_experiment_history("PLX207753")
  expect_equal(captured, "lab/experiments/PLX207753/history/")
})


test_that("pluto_get_experiment_history passes cursor through query string", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(items = list(), response_status_code = 200L)
    }
  )
  pluto_get_experiment_history("PLX207753", cursor = "abc-cursor")
  expect_match(captured, "^lab/experiments/PLX207753/history/\\?cursor=abc-cursor$")
})


test_that(".pluto_as_list handles paginated, items envelope, and bare list", {
  expect_equal(.pluto_as_list(list(results = list(list(uuid = "r-1")))),
               list(list(uuid = "r-1")))
  expect_equal(.pluto_as_list(list(items = list(list(uuid = "i-1")))),
               list(list(uuid = "i-1")))
  expect_equal(.pluto_as_list(list()), list())
})
