#!/usr/bin/env Rscript

`%||%` <- function(x, y) if (!is.null(x) && length(x) > 0 && !is.na(x) && nzchar(x)) x else y

source_first_existing <- function(paths, local = TRUE) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop(
      "Could not find any of these files: ",
      paste(paths, collapse = ", "),
      call. = FALSE
    )
  }
  source(hit, local = local)
}


source_first_existing(c("dnam_functions.R", "R/dnam_functions.R"), local = FALSE)


message("Installing Required Packages...")
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}
renv::init()
source("renv/activate.R")
renv::restore(prompt = FALSE)


args <- commandArgs(trailingOnly = TRUE)
config_file <- "dnam_config.yml"

if (length(args) > 0) {
  if (startsWith(args[1], "--config=")) {
    config_file <- sub("^--config=", "", args[1])
  } else if (length(args) >= 2 && args[1] == "--config") {
    config_file <- args[2]
  }
}


if (!requireNamespace("yaml", quietly = TRUE)) {
  message("yaml package not found. Installing...")
  install_and_load_packages("yaml", "cran")
}

cfg <- yaml::read_yaml(config_file)

# normalize common fields
cfg$project_dir <- normalizePath(cfg$project_dir %||% getwd(), winslash = "/", mustWork = TRUE)
cfg$sample_sheet <- normalizePath(file.path(cfg$project_dir, cfg$sample_sheet), winslash = "/", mustWork = TRUE)
cfg$results_dir <- normalizePath(file.path(cfg$project_dir, cfg$output_dir %||% "results"), winslash = "/", mustWork = FALSE)

dir.create(cfg$results_dir, recursive = TRUE, showWarnings = FALSE)

saveRDS(cfg, file = file.path(cfg$project_dir, ".pipeline_config.rds"))
config_file <- "dnam_config.yml"


message("\n\nProject directory: ", cfg$project_dir)
message("Sample sheet: ", cfg$sample_sheet)
message("Output directory: ", cfg$results_dir)
message("Run name: ", cfg$run_name)
message("ncores: ", cfg$ncores,"\n\n")



if (!requireNamespace("targets", quietly = TRUE)) {
  message("targets package not found. Installing...")
  install_and_load_packages("targets", "cran")
}

targets::tar_make(script = "dnam_targets.R", store = paste0("dnam_targets-",cfg$run_name))
