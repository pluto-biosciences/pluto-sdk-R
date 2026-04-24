# pluto 0.2.0

Large, additive release. No existing function signatures changed; every
new capability is opt-in. Validated live against `dev-api.pluto.bio` at
each step.

## Reliability hardening

- Every HTTP helper (`pluto_GET` / `_POST` / `_PUT` / `_PATCH` /
  `_DELETE`) applies a default 60s timeout and retries on 429/5xx with
  exponential backoff (POST retries only on 429 to avoid duplicate
  writes). Override via `PLUTO_TIMEOUT` / `PLUTO_MAX_RETRIES` env vars
  or per-call arguments.
- Added `pluto_PATCH()` and `pluto_DELETE()`.
- Tolerant parsing of 204 / empty bodies.
- New `strict = TRUE` opt-in on every base helper raises classed
  `rlang` conditions on non-2xx instead of returning the legacy
  response list. Default behavior is unchanged.
- Typed condition classes (each inherits from `pluto_error`):
  `pluto_api_error`, `pluto_auth_error`, `pluto_permission_error`,
  `pluto_not_found_error`, `pluto_validation_error`,
  `pluto_rate_limit_error`, `pluto_server_error`,
  `pluto_connection_error`.
- Backend codes `invalid_object_id`, `object_not_found`, and
  `resource_not_found` are routed to `pluto_not_found_error`
  regardless of HTTP status.
- Helpers: `pluto_check_response()`, `pluto_is_not_found()`,
  `pluto_is_api_error()`.

## Multi-org support

- `organization` argument on every base HTTP helper.
- `PLUTO_ORGANIZATION` env var.
- `pluto_login(api_key, organization = <uuid>)`.
- `pluto_list_organizations()`, `pluto_use_organization(uuid | NULL)`,
  `pluto_current_organization()`.

## Analyses

- **Fixed:** `pluto_get_experiment_analyses()` hit `/plots` instead of
  `/analyses` (copy-paste bug). The new `/analyses` response shape (a
  bare JSON array) is normalized to the `{count, items}` envelope
  `pluto_read_experiment_analyses()` expects.
- `pluto_read_experiment_analyses()` now tolerates both the flat
  `/analyses` shape and the old nested `/plots`-style shape.
- New: `pluto_get_analysis()`, `pluto_get_analysis_summary()`,
  `pluto_get_analysis_stats()`,
  `pluto_get_analysis_signed_url(kind = "spreadsheet" | "image")`.
- New: `pluto_wait_for_analysis(..., poll_interval, timeout, progress)`
  polls `pipeline_status` until terminal
  (`completed` / `failed` / `unprocessable` / `canceled`). Raises only
  on timeout; callers check `$pipeline_status` to distinguish success
  from failure.

## Bulk RNA-seq + epigenetics

- `pluto_get_counts(experiment_id, kind = "raw" | "cpm" | "tpm_gene" | "tpm_transcript")`
  returns a data.frame. Raw delegates to the existing assay-data path;
  other kinds discover the file via `/files/?data_type=<X>`, grab a
  signed URL, and parse CSV.
- `pluto_list_bigwigs()` / `pluto_download_bigwigs()`.
- `pluto_list_peaks()` / `pluto_download_peaks()` (consensus BED).
- `pluto_list_bam_files()`.
- `pluto_list_seurat_objects()`.

## Pipelines

- `pluto_list_pipelines()`, `pluto_list_pipeline_runs()`.
- Note: single-cell Workflow endpoints were intentionally NOT ported to
  R because the backend views are currently decorated with
  `authentication_classes((JWTCookieAuthentication,))` — they don't
  accept API-token auth. They can be added once the backend supports
  `AccessTokenAuthentication` for those views.

## Tier 2 coverage

- **Biomarker sets:** `pluto_list_biomarker_sets()`,
  `pluto_get_biomarker_set()`, `pluto_create_biomarker_set()`,
  `pluto_archive_biomarker_set()`, `pluto_list_biomarkers()`,
  `pluto_add_biomarker()`, `pluto_download_biomarker_set()`.
- **Biomodel sets:** `pluto_list_biomodel_sets()`,
  `pluto_get_biomodel_set()`, `pluto_create_biomodel_set()`,
  `pluto_archive_biomodel_set()`, `pluto_list_biomodels()` (returns
  `{headers, count, items}`), `pluto_get_biomodel_columns()`.
- **Search:** `pluto_search()`, `pluto_search_all()`.
- **Annotation sets:** `pluto_list_annotation_sets()`,
  `pluto_get_annotation_set()`, `pluto_list_annotations()`.

## Tests

- 48 mock unit tests added under `tests/testthat/`, covering the
  condition system, resolve-* helpers, organization flow, and search
  pagination. No API token or network required.

## Miscellaneous

- `DESCRIPTION` imports `rlang` and suggests `withr` (for tests).
- Switched `httr2::multi_req_perform` (removed in httr2 >= 1.0) to
  `httr2::req_perform_parallel` with a graceful fallback for older
  httr2 versions. Fixes a latent bug that would have broken paginated
  data fetches on modern httr2.

# pluto 0.1.0

## Minor improvements

- Simplified API request syntax for increased performance and improved maintainability
- Added new functions `pluto_create_external_plot()` and `pluto_update_external_plot()` to support simplified endpoints for creating and updating external plots
- Updated `pluto_add_experiment_plot()` and `pluto_update_external_plot()` to use the new simplified endpoints and support script file uploads, while maintaining backward compatibility with the old parameter names (`analysis_name` and `plot_methods`)

# pluto 0.0.1

## Major changes

- Added family of `pluto_get_*()` functions to retrieve data from the Pluto API

- Added family of `pluto_read_*()` functions to read Pluto data directly into a data.frame

- Added function `pluto_add_experiment_plot()` to upload HTML and image files to an Experiment in Pluto
