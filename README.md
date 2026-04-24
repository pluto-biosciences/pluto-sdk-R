# Pluto R package

<img src="https://cdn.bfldr.com/2Q1IPX6I/at/hqwgpb3vqscf83j375mfn9gf/Integrations_-_R" height="500">

<button class="pluto-button"><a href="https://share.hsforms.com/15Mj4CreYSoqKDoZDYBKpgg5c2ld" target="_blank" class="button-text-link">Subscribe for notifications about new releases :rocket:</a></button>

## Overview

`pluto` is the official R package for interacting with [Pluto](https://pluto.bio), the biological discovery platform. With a few lines of code, you can read data and results directly into your R scripts, run server-side analyses, wait on them, and push custom plots back to Pluto where they remain interactive and collaborative.

Not using Pluto yet? We'd be happy to show you the platform in action — [schedule a personalized demo](https://pluto.bio/get-info).

## Install

```r
# Install remotes if you don't already have it
install.packages("remotes")

# Install the pluto R package
remotes::install_github("pluto-biosciences/pluto-sdk-R")
```

Load into your scripts with `library(pluto)`.

## Authenticate

Create an API key in the Pluto UI ([help article](https://help.pluto.bio/en/articles/creating-your-api-token)), then set:

```r
# Option 1: set per-session and optionally save to .Renviron
pluto_login("YOUR_API_KEY")

# Option 2: put PLUTO_API_TOKEN in your .Renviron permanently
Sys.setenv(PLUTO_API_TOKEN = "YOUR_API_KEY")

# Target a different environment (optional)
Sys.setenv(PLUTO_ENV = "staging")       # or "development"
```

See `vignette("authentication")` for the full flow.

### Organizations

If your Pluto token has access to **multiple organizations**, you can
scope requests to a specific one. When no organization is set, the
backend falls back to the user's `default_organization` — enough for
single-org users.

```r
# Discover your organizations
orgs <- pluto_list_organizations()
for (o in orgs) cat(o$uuid, "-", o$name, "\n")

# Option 1: set at login time
pluto_login("YOUR_API_KEY", organization = "<org-uuid>")

# Option 2: switch mid-session
pluto_use_organization("<org-uuid>")
pluto_current_organization()          # -> "<org-uuid>"
pluto_use_organization(NULL)          # clear; fall back to user default

# Option 3: environment variable
Sys.setenv(PLUTO_ORGANIZATION = "<org-uuid>")

# Option 4: per-call override
pluto_GET("lab/experiments/", organization = "<other-org-uuid>")
```

A per-call `organization` argument beats the env var; the env var beats
the backend default. Passing an org the user isn't a member of returns
a 403 (`pluto_permission_error`).

## Quick reference

```r
library(pluto)

# List / fetch
projects <- pluto_get_projects_all()
experiments <- pluto_get_experiments_all()
experiment <- pluto_get_experiment("PLX207753")

# Data
samples <- pluto_read_sample_data("PLX207753")            # data.frame
counts  <- pluto_get_counts("PLX207753", kind = "cpm")    # raw | cpm | tpm_gene | tpm_transcript
assay   <- pluto_read_assay_data("PLX207753")

# Run an analysis, block on it, pull results
analyses <- pluto_get_experiment_analyses("PLX207753")
a <- pluto_wait_for_analysis("PLX207753", analyses$items[[1]]$uuid, progress = TRUE)
summary <- pluto_get_analysis_summary("PLX207753", a$uuid, limit = 5000)
```

Every HTTP call applies:

- a default 60s timeout (override with `PLUTO_TIMEOUT` or the `timeout` argument),
- automatic retries on 429/5xx with exponential backoff (override with `PLUTO_MAX_RETRIES` or `max_retries`),
- tolerant response parsing (204/empty bodies no longer break callers).

## Error handling

All HTTP helpers return the parsed response as a list (with a
`$response_status_code` field) by default — existing scripts keep
working. For robust pipelines, pass `strict = TRUE` to any base HTTP
helper, or call `pluto_check_response()` on a result, and failures raise
classed `rlang` conditions:

```r
tryCatch(
  pluto_GET("lab/experiments/BOGUS/", strict = TRUE),
  pluto_not_found_error = function(e) {
    message("Not found: ", e$code, " / status ", e$status_code)
    NULL
  }
)
```

Condition classes (each inherits from `pluto_error`):

- `pluto_api_error`         — any HTTP response error
- `pluto_auth_error`        — 401
- `pluto_permission_error`  — 403
- `pluto_not_found_error`   — 404, or 400 + `invalid_object_id` / `object_not_found`
- `pluto_validation_error`  — 400 / 409 / 422 (other)
- `pluto_rate_limit_error`  — 429
- `pluto_server_error`      — 5xx
- `pluto_connection_error`  — network-level failure

Helpers: `pluto_is_not_found(e)`, `pluto_is_api_error(e)`.

## Bulk RNA-seq, microarray, and proteomics

```r
# Counts / CPM / TPM (automatically selects the right ExperimentFile)
counts <- pluto_get_counts("PLX207753", kind = "raw")             # assay_data
cpm    <- pluto_get_counts("PLX207753", kind = "cpm")             # assay_data_cpm
tpm_g  <- pluto_get_counts("PLX207753", kind = "tpm_gene")
tpm_t  <- pluto_get_counts("PLX207753", kind = "tpm_transcript")

# Sample metadata
samples <- pluto_read_sample_data("PLX207753")
```

If no file of the requested kind exists, you'll get a clean
`pluto_not_found_error` rather than a raw HTTP failure.

## Epigenetics (ChIP-seq / ATAC-seq / CUT&RUN)

```r
# Discover
bigwigs <- pluto_list_bigwigs("PLX240899")
peaks   <- pluto_list_peaks("PLX240899")
bams    <- pluto_list_bam_files("PLX240899")

# Download
paths <- pluto_download_bigwigs("PLX240899", dest_dir = "./bw")
paths <- pluto_download_peaks("PLX240899", dest_dir = "./bed",
                              file_uuids = c("<uuid1>", "<uuid2>"))
```

## Single-cell (Seurat)

```r
# List and download Seurat .rds objects
files <- pluto_list_seurat_objects("PLX076418")
path  <- pluto_download_seurat_object("PLX076418", folder_path = "./sc",
                                      which = "final")   # final | raw
seurat_obj <- pluto_read_seurat_object("PLX076418")
```

See `vignette("scrnaseq_recipes")` for full workflows.

## Analyses

```r
# Listing and detail
analyses <- pluto_get_experiment_analyses("PLX207753")
a <- pluto_get_analysis("PLX207753", analyses$items[[1]]$uuid)

# Wait until the analysis finishes (polls pipeline_status until terminal:
# completed, failed, unprocessable, canceled)
a <- pluto_wait_for_analysis(
  "PLX207753", a$uuid,
  poll_interval = 5, timeout = 3600, progress = TRUE
)

# Results
if (identical(a$pipeline_status, "completed")) {
  summary <- pluto_get_analysis_summary("PLX207753", a$uuid, limit = 5000)
  stats   <- pluto_get_analysis_stats("PLX207753", a$uuid)
  url     <- pluto_get_analysis_signed_url("PLX207753", a$uuid, kind = "spreadsheet")
}
```

Timeout expiration raises an error; analysis failure does not — inspect
`$pipeline_status` instead.

## Biomarker sets

Curated collections of molecules (genes / proteins / metabolites):

```r
# List & fetch
sets <- pluto_list_biomarker_sets(limit = 100, search = "pathway")
bs   <- pluto_get_biomarker_set("<set-uuid>")

# Create + populate
new_set <- pluto_create_biomarker_set(list(
  name = "My candidates",
  target_type = "gene",                # gene | protein | metabolite | other
  organism_shortname = "human"
))
pluto_add_biomarker(new_set$uuid, list(
  name = "BRCA1",
  target_shortname = "BRCA1"
))

biomarkers <- pluto_list_biomarkers(new_set$uuid, limit = 1000)
pluto_download_biomarker_set(new_set$uuid, dest_filename = "./my_set.csv")

# Cleanup
pluto_archive_biomarker_set(new_set$uuid)
```

## Biomodel sets

Structured reference datasets with user-defined columns (cell lines,
patients, cohorts):

```r
sets <- pluto_list_biomodel_sets()
rows <- pluto_list_biomodels(sets[[1]]$uuid, limit = 100)   # {headers, items}
cols <- pluto_get_biomodel_columns(sets[[1]]$uuid)
```

## Search

Full-text across projects and experiments:

```r
# Returns {count, items}
results <- pluto_search("BRCA1", type = "experiments", access = "shareable")

# Paginate through every result up to max_results
all_hits <- pluto_search_all("pathway", type = "projects", max_results = 1000)
```

## Annotation sets (single-cell clusters)

```r
sets        <- pluto_list_annotation_sets("PLX076418")
a_set       <- pluto_get_annotation_set("PLX076418", sets[[1]]$uuid)
annotations <- pluto_list_annotations("PLX076418", sets[[1]]$uuid)
```

## Plots

```r
plots <- pluto_get_experiment_plots("PLX207753")
plot  <- pluto_get_plot("PLX207753", "<plot-uuid>")

# Push your own plot back to Pluto
pluto_create_external_plot(
  experiment_id = "PLX207753",
  name = "My custom plot",
  origin = "R",
  display_file_path = "plot.html",
  results_file_path = "results.csv",
  script_file_path = "script.R",
  methods = "markdown or path to a .md file"
)
```

See `vignette("rnaseq_recipes")` and `vignette("data_integration")` for
more examples.

## Example: bulk RNA-seq end-to-end

```r
library(pluto)

# 1. Pull data
exp     <- pluto_get_experiment("PLX207753")
samples <- pluto_read_sample_data("PLX207753")
counts  <- pluto_get_counts("PLX207753", kind = "cpm")

# 2. Drive a server-side analysis to completion
analyses <- pluto_get_experiment_analyses("PLX207753")
a <- pluto_wait_for_analysis("PLX207753", analyses$items[[1]]$uuid, progress = TRUE)

# 3. Pull DE results
if (identical(a$pipeline_status, "completed")) {
  summary <- pluto_get_analysis_summary("PLX207753", a$uuid,
                                        limit = 5000, sort_by = "-abs_log2_fold_change")
}
```

## Tips and tricks

When transferring a very large file (e.g. fetching a Seurat object),
you can circumvent network issues by changing your R options:

```r
options(download.file.method = "curl")
options(download.file.extra = "--retry 6 --retry-delay 5 --continue-at -")
```

## Working with an outsourced bioinformatics team?

With Pluto, your cross-functional team has the power to collaborate easily
in one place regardless of coding ability. The same applies to your
vendors as well. Instead of having your outsourced bioinformatics vendor
send you results by email or in folders where critical findings are
easily lost, **you can request that they deliver data and results
directly back into your Pluto lab space**. No training necessary on your
part — Pluto handles all onboarding and training for bioinformatics CROs
collaborating with clients in Pluto as part of the service. Chat with our
Customer Experience team to learn more.

## Questions?

We're here to help! Reach out to
[support@pluto.bio](mailto:support@pluto.bio) or email your Pluto
customer representative directly.

## Developers

### Using environment variables locally

When running tests and developing the `pluto` R package, you will need to
decrypt the encrypted `.Renviron` file in this repo.

Decrypt:

```
openssl enc -d -aes256 -base64 -in .Renviron.encrypted -out .Renviron
```

Encrypt after changes:

```
openssl enc -aes256 -base64 -in .Renviron -out .Renviron.encrypted
```

Check in the updated `.Renviron.encrypted` when opening a PR so that
other developers have the latest changes.

### Running tests

While iterating, use `devtools::load_all()` to pick up local changes.

```r
devtools::test()                                            # full suite
devtools::test_active_file("tests/testthat/test-projects.R")  # single file
```

Most tests are integration tests and require a valid API token and
UUIDs in `.Renviron`.

### Documentation & vignettes

```r
pkgdown::build_site()     # build docs site
devtools::document()      # refresh man pages from roxygen
```
