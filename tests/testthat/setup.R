library(httptest2)

# Read project .Renviron file to get test env vars
readRenviron("../../.Renviron")
TESTTHAT_API_TOKEN <- Sys.getenv("PLUTO_API_TOKEN")
TESTTHAT_INVALID_API_TOKEN <- Sys.getenv("INVALID_API_TOKEN")
TESTTHAT_EXPT_ID <- Sys.getenv("EXPERIMENT_ID")
