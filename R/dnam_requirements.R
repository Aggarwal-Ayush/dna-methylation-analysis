
source_first_existing <- function(paths, local = TRUE) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("Could not find any of these files: ", paste(paths, collapse = ", "), call. = FALSE)
  }
  source(hit, local = local)
}

source_first_existing(c("dnam_functions.R", "R/dnam_functions.R"), local = FALSE)


# Libraries ---------------------------------------------------------------

install_and_load_packages("plotly", "cran", silent = TRUE) # conumee2 dependency
install_and_load_packages("hovestadtlab/conumee2",
                          "github", silent = TRUE,
                          subdir = "conumee2") # doesn't install without this argument

required_packages = 
  list(
    cran = c(
      "here",
      "targets",
      "tarchetypes",
      "crew",
      "crew.cluster",
      "visNetwork",
      "BiocManager",
      "ggplot2",
      "patchwork",
      "dplyr",
      "tidyr",
      "stringr",
      "reshape2",
      "scales",
      "qs2",
      "RColorBrewer",
      "parallel",
      "future",
      "pbapply",
      "plotly", 
      "stringr",
      "data.table",
      "kernlab",
      "caret",
      "shinyFiles",
      "gtools",
      "readxl",
      "viridis",
      "yaml"
    ),
    bioc = c(
      "sesame",
      "minfi",
      "DMRcate",
      "IlluminaHumanMethylationEPICv2manifest",
      "ComplexHeatmap",
      "circlize"
    ),
    github = c(
      "hovestadtlab/conumee2",
      "StaafLab/PureBeta"
    )
  )

required_packages = stack(required_packages)

install_and_load_packages(required_packages[, 1],
                          required_packages[, 2],
                          silent = TRUE)

