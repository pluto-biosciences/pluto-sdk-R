library(httptest2)

Sys.setenv(PLUTO_ENV="staging")

# Read project .Renviron file to get test env vars
TESTTHAT_API_TOKEN <- Sys.getenv("PLUTO_TEST_API_TOKEN")
TESTTHAT_INVALID_API_TOKEN <- Sys.getenv("PLUTO_INVALID_API_TOKEN")

# There is 1 project containing toy data,
# 1 project for testing creating expts and analyses,
# and 1 empty project
NUM_PROJS <- 3
TESTTHAT_EMPTY_PROJ_ID <- Sys.getenv("EMPTY_PROJECT_ID")
TESTTHAT_NONEMPTY_PROJ_ID <- Sys.getenv("NONEMPTY_PROJECT_ID")
NUM_ALL_EXPTS <- 7
NUM_PROJ_EXPTS <- 6

# RNA-seq test experiment
TESTTHAT_EXPT_ID_RNASEQ <- Sys.getenv("EXPERIMENT_ID_RNASEQ")
NUM_ANALYSES_RNASEQ <- 13
NUM_EXPT_SAMPLES_RNASEQ <- 153
NUM_EXPT_TARGETS_RNASEQ <- 15892
NUM_DE_TARGETS_RNASEQ <- 8270
TESTTHAT_PLOT_ID_PCA <- Sys.getenv("PLOT_ID_PCA")
TESTTHAT_PLOT_ID_DE_VOLCANO_V2 <- Sys.getenv("PLOT_ID_DE_VOLCANO_V2")

# CUT&RUN test experiment
TESTTHAT_EXPT_ID_CUTANDRUN <- Sys.getenv("EXPERIMENT_ID_CUTANDRUN")
NUM_ANALYSES_CUTANDRUN <- 2
TESTTHAT_PLOT_ID_DB_VOLCANO_V2 <- Sys.getenv("PLOT_ID_DB_VOLCANO_V2")
NUM_DB_TARGETS_CUTANDRUN <- 49916

# Experiment for creating/modifying analyses
TESTTHAT_EXPT_ID_MOD <- Sys.getenv("EXPERIMENT_ID_MOD")

# scRNA-seq test experiment
TESTTHAT_EXPT_ID_SCRNASEQ <- Sys.getenv("EXPERIMENT_ID_SCRNASEQ")
NUM_EXPT_CELLS_RAW_SCRNASEQ <- 7720
NUM_EXPT_COLS_RAW_SCRNASEQ <- 8
NUM_EXPT_CELLS_FINAL_SCRNASEQ <- 6969
NUM_EXPT_COLS_FINAL_SCRNASEQ_WA <- 21
NUM_EXPT_COLS_FINAL_SCRNASEQ_NA <- 16
CUSTOM_ANNOTATION_SETS <- c("cell_types",
                            "default_clusters_res_0_1",
                            "default_clusters_res_0_4",
                            "default_clusters_res_0_6",
                            "default_clusters_res_1_0")
NUM_COLOR_PALETTES <- 5
CLUSTER_COLOR_CHECK_1 <- "#e056fd"
CLUSTER_COLOR_CHECK_2 <- "#6366f1"
