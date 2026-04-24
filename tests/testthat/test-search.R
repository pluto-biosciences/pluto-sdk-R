test_that("pluto_search rejects empty query", {
  expect_error(pluto_search(""), "query is required")
  expect_error(pluto_search(NULL), "query is required")
})

test_that("pluto_search rejects invalid type / access", {
  expect_error(pluto_search("x", type = "bogus"))
  expect_error(pluto_search("x", access = "nope"))
})

test_that("pluto_search passes args through as query parameters", {
  captured <- NULL
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      captured <<- url_path
      list(count = 0, items = list(), response_status_code = 200L)
    }
  )
  pluto_search("BRCA1", type = "experiments",
               access = "shareable", offset = 5, limit = 3)
  expect_match(captured, "query=BRCA1")
  expect_match(captured, "type=experiments")
  expect_match(captured, "access=shareable")
  expect_match(captured, "offset=5")
  expect_match(captured, "limit=3")
})

test_that("pluto_search_all stops once count is reached", {
  # Pretend the endpoint has 25 matches total with pages of 10.
  pages <- list()
  page_index <- 0L
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      page_index <<- page_index + 1L
      offset <- as.integer(sub(".*offset=([0-9]+).*", "\\1", url_path))
      limit <- as.integer(sub(".*limit=([0-9]+).*", "\\1", url_path))
      items <- Map(function(i) list(uuid = sprintf("u-%d", i)),
                   seq(offset + 1, min(offset + limit, 25)))
      list(count = 25L, items = items, response_status_code = 200L)
    }
  )
  out <- pluto_search_all("x", type = "experiments",
                          page_size = 10, max_results = 100)
  expect_equal(length(out), 25)
})

test_that("pluto_search_all respects max_results cap", {
  local_mocked_bindings(
    pluto_GET = function(url_path, ...) {
      offset <- as.integer(sub(".*offset=([0-9]+).*", "\\1", url_path))
      limit <- as.integer(sub(".*limit=([0-9]+).*", "\\1", url_path))
      items <- Map(function(i) list(uuid = sprintf("u-%d", i)),
                   seq(offset + 1, offset + limit))
      list(count = 10000L, items = items, response_status_code = 200L)
    }
  )
  out <- pluto_search_all("x", page_size = 10, max_results = 15)
  expect_equal(length(out), 15)
})
