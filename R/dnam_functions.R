
# GENERAL HELPERS ---------------------------------------------------------

#' General helper functions useful for most scripts
#' 


#' Function to initialize an R script
#' Creates standard path variable based on initialize_project.R script
#' Creates script specific path variables
#' source commonly used custom R functions stored in R/
#' Set global seed value
#' 
#' @param script_name Name of the script. This is used to create script specific path variables
#' @param raw_data_dir Path of the raw data directory
#' @param processed_data_dir Path of the process data directory
#' @param output_data_dir Path of the output directory
#' @param define_project_dir logical specifying whether this R project is part of a multi-repo project, Defaults to TRUE
#' @param run Defines the run number of the script. Modify only when there are substantial changes to the script and 
#' output from different runs are needed for comparison. Defaults to 1
#' @param run_dir Defines the run directory to be created within script directory if run > 1
#' @return list of path variables

initialize_script = 
  function(
    script_name,
    repo_dir = NULL,
    raw_data_dir = file.path(repo_dir, "data", "raw"),
    processed_data_dir = file.path(repo_dir, "data", "processed"),
    output_dir = file.path(repo_dir, "output"),
    run = 1,
    run_dir = paste0("run_",run)
  ){
    
    script_name = script_name
    message("Running script: ",script_name,".R")
    
    if(!requireNamespace("here", quietly = TRUE)){
      message("'here' package not found. Installing now...")
      install.packages("here")
    }
    suppressPackageStartupMessages(library(here))
    
    if(is.null(repo_dir)){
      repo_dir = here::here()
    }
    
    repo_name = base::strsplit(repo_dir, "_")[[1]][2] #repo name after underscore
    
    # Create path variables
    raw_data_dir = file.path(repo_dir, "data", "raw")
    processed_data_dir = file.path(repo_dir, "data", "processed")
    output_dir = file.path(repo_dir, "output")
    
    script_dir = file.path(output_dir, script_name)
    
    # Create a new directory for subsequent script runs
    if(run > 1){
      script_dir = file.path(output_dir, script_name, run_dir)
      script_name = paste0(script_name,"__",run_dir)
      message("Script run: ", run_dir)
    }
    
    script_figures_dir = file.path(script_dir, "figures")
    
    plot_name_pre = 
      file.path(
        script_figures_dir,
        paste0(script_name,"__")
      )
    
    file_name_pre = 
      file.path(
        script_dir,
        paste0(script_name, "__")
      )
    
    # List of directories to create
    dirs_to_create = 
      c(raw_data_dir,
        processed_data_dir,
        output_dir,
        script_figures_dir)
    
    # Loop through the list and create each directory
    
    for (dir_path in dirs_to_create) {
      # Check if the directory already exists
      if (!dir.exists(dir_path)) {
        message(paste("\nCreating directory:", dir_path))
        # `recursive = TRUE` ensures parent directories are also created
        dir.create(dir_path, recursive = TRUE)
      } else {
        message(paste("\nDirectory already exists:", dir_path))
      }
    }
    
    
    # Custom R functions
    custom_rfuncs = 
      list.files(
        path = file.path(repo_dir, "R"), 
        pattern = "\\.[Rr]$", 
        full.names = TRUE
      )
    
    if(length(custom_rfuncs) > 0){
      
      message("\nSourcing custom R functions from R/")
      
      # Remove dependencies.R and initialize_script.R file
      files_to_remove = 
        c(file.path(repo_dir, "R", "dependencies.R"),
          file.path(repo_dir, "R", "initialize_script.R"))
      custom_rfuncs = 
        custom_rfuncs[!(custom_rfuncs %in% files_to_remove)]
      
      # Source custom R function
      lapply(custom_rfuncs, source)
    }
    
    # Set seed 
    seed_value = 123
    message("\nSetting seed to: ", seed_value)
    set.seed(seed_value)
    
    return(
      list(
        script_name = script_name,
        repo_name = repo_name,
        output_dir = output_dir,
        script_dir = script_dir,
        script_figures_dir = script_figures_dir,
        plot_name_pre = plot_name_pre,
        file_name_pre = file_name_pre,
        raw_data_dir = raw_data_dir,
        processed_data_dir = processed_data_dir
      )
    )
    
  }



#' Function to read a csv, xlsx, txt, or rds file
#' Takes in the file path and load the file based on its extension
#' 
#' @param file_path path of the file to load
#' @return object of the file in the environment

read_dynamic_file <- function(file_path, format = NULL) {
  
  if (!file.exists(file_path)) {
    stop("Input file not found at path: ", file_path)
  }
  
  # 1. --- Setup Paths and Determine Load Source ---
  tmp_dir <- Sys.getenv("TMPDIR")
  is_tmp_available <- tmp_dir != ""
  tmp_path <- file_path # Default fallback
  
  if (is_tmp_available) {
    message("\nStaging data to fast local scratch: ", tmp_dir)
    tmp_path = file.path(tmp_dir, basename(file_path))
    
    # 2. FILE COPY AND COPY CHECK
    message("  -> Copying file to scratch...")
    time_copy <- system.time({
      copy_success <- file.copy(file_path, tmp_path, overwrite = TRUE)
    })
    message("  File Copy Time: elapsed=", round(time_copy[3], 2), "s.")
    
    if (!copy_success) {
      warning("Warning: File copy to TMPDIR failed. Loading directly from source.")
      tmp_path = file_path
    }
  }
  
  extension <- ifelse(is.null(format), 
                      tolower(tools::file_ext(tmp_path)),
                      format)
  data <- NULL # Initialize data object
  
  # 3. --- Data Loading Logic with System Timing ---
  
  message("  -> Reading file content...")
  
  time_read <- system.time({
    if (extension == "csv") {
      if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
      data <- data.table::fread(tmp_path)
      
    } else if (extension == "xlsx") {
      if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
      data <- readxl::read_excel(tmp_path)
      
    } else if (extension == "txt") {
      data <- read.table(tmp_path, header = TRUE)
      
    } else if (extension == "qs2") {
      if (!requireNamespace("qs2", quietly = TRUE)) install.packages("qs2")
      data <- qs2::qs_read(tmp_path)
      
    } else if (extension == "rds") {
      data <- readRDS(tmp_path)
      
    } else if (grepl("rda|rdata", extension)) {
      # NOTE: load() assigns objects to the environment and returns the names
      data <- load(tmp_path, envir = .GlobalEnv)
      return(invisible(data))
      
    } else {
      warning(paste("Unsupported file type for:", tmp_path))
    }
  })
  
  # Print the timing result for the read operation
  message("  File Read Time: elapsed=", time_read[3], "s.")
  
  return(data)
}



#' Function to save an object (data frame, list, etc.) to a file, 
#' prioritizing writing to $TMPDIR for speed, and then copying to the final destination.
#'
#' @param object The R object to be saved, or a character string representing the object's name.
#' @param file_path The final, persistent path including the desired file name and extension (e.g., "output/data__results.xlsx").
#' @param copy_to_main Logical. If TRUE and TMPDIR is available, copies the file to file_path.
#' @param overwrite Logical. If FALSE and file already exist, doesn't copy the file and returns NULL
#' @return NULL (invisibly), executed for its side effect of writing a file.
#'
save_dynamic_file <- function(object, file_path, copy_to_main = TRUE, overwrite = FALSE) {
  
  # 1. Non-Overwrite Check 🛑
  if (file.exists(file_path) && !overwrite) {
    warning(paste("File already exists at FINAL destination:", file_path, "Skipping save operation."))
    return(invisible(NULL))
  }
  
  # --- Setup Temporary Paths ---
  if(copy_to_main){
    tmp_dir <- Sys.getenv("TMPDIR")
  } else {
    if (!requireNamespace("here", quietly = TRUE)) {
      message("The 'here' package is required for creating global scratch directory files. Installing now...")
      install.packages("here")
      library(here)
    }
    #' use global scratch for intermediate files so that they are not immediately
    #' deleted after a job fails and are available to resume the script.
    # NOTE: Added safety around here::here() in case the path is needed
    project_identifier <- tryCatch({
      basename(here::here())
    }, error = function(e) {
      warning("here::here() failed. Using basename(getwd()) as project identifier for scratch check.", call. = FALSE)
      return(basename(getwd()))
    })
    
    tmp_dir <- file.path("/c4/scratch",
                         Sys.getenv("USER"),
                         project_identifier)
  } 
  
  if (tmp_dir == "") {
    write_path <- file_path
    message(paste("TMPDIR not available. Writing directly to final path:", file_path))
  } else {
    tmp_filename <- basename(file_path)
    tmp_dest_dir <- file.path(tmp_dir, "data_save_temp")
    if (!dir.exists(tmp_dest_dir)) dir.create(tmp_dest_dir, recursive = TRUE)
    
    write_path <- file.path(tmp_dest_dir, tmp_filename)
    message(paste("TMPDIR available. Writing to fast scratch:", write_path))
  }
  
  
  if (is.character(object) && length(object) == 1) {
    if (exists(object, envir = .GlobalEnv)) {
      data_to_save <- get(object, envir = .GlobalEnv)
    } else {
      warning(paste("Object named '", object, "' not found in the global environment. Skipping save.", sep=""))
      return(invisible(NULL))
    }
  } else {
    data_to_save <- object
  }
  
  # --- 2. Write Data to Temporary Path (or Final Path if fallback) ---
  
  extension <- tolower(tools::file_ext(write_path))
  
  # START TIMING THE WRITE OPERATION
  time_write <- system.time({
    if (extension == "csv") {
      
      # CSV
      
      if (!requireNamespace("data.table", quietly = TRUE)) {
        message("The 'data.table' package is required for fast CSV writing. Installing now...")
        install.packages("data.table")
      }
      data.table::fwrite(data_to_save, write_path, row.names = TRUE)
      
    } else if (extension == "xlsx") {
      
      # XLSX
      
      if (!requireNamespace("writexl", quietly = TRUE)) {
        message("The 'writexl' package is required for .xlsx files. Installing now...")
        install.packages("writexl")
        library(writexl)
      }
      writexl::write_xlsx(list(Sheet1 = data_to_save), path = write_path)
      
    } else if (extension == "txt") {
      
      # TXT
      
      write.table(data_to_save, write_path, row.names = TRUE)
      
    } else if (extension == "qs2") {
      
      # QS2
      
      if (!requireNamespace("qs2", quietly = TRUE)) {
        message("The 'qs2' package is required for fast RDS writing. Installing now...")
        install.packages("qs2")
      }
      
      qs2::qs_save(data_to_save, write_path)
      
    } else if (extension == "rds") {
      
      # RDS
      
      saveRDS(data_to_save, write_path)
      
    } else if (grepl("rda", extension, ignore.case = TRUE)) { 
      
      # RDATA
      
      # convert the object to chr if its not
      save(list = as.character(object), file = write_path, envir = .GlobalEnv)
      
    } else {
      
      # NONE OF THE ABOVE
      
      warning(paste("Unsupported file type for saving:", write_path))
      return(invisible(NULL))
      
    }
  })
  
  message("Write Time (to scratch/final): elapsed=", time_write[3], "s.")
  
  # --- 3. Copy from TMPDIR to Final Destination (If TMPDIR was used) ---
  
  # Check if the write operation was successful
  if (!file.exists(write_path)) {
    warning("File write failed at the temporary location. Cannot proceed with copy.")
    return(invisible(NULL))
  }
  
  if (tmp_dir != "" && copy_to_main) {
    # Ensure the target directory structure exists before copying
    final_dir <- dirname(file_path)
    if (!dir.exists(final_dir)) dir.create(final_dir, recursive = TRUE)
    
    message(paste("Copying file from fast scratch to final location:", file_path))
    
    # START TIMING THE COPY OPERATION
    time_copy <- system.time(
      file.copy(from = write_path, to = file_path, overwrite = TRUE) 
    )
    message("Copy Time (to final): elapsed=", round(time_copy[3], 2), "s.")
    
    # Optional: Clean up the temporary file (good practice)
    file.remove(write_path) 
  }
  
  # 4. Confirmation message
  if (file.exists(file_path) || (tmp_dir != "" && !copy_to_main)) { # Check final path OR temporary path if checkpointing
    if (copy_to_main) {
      message(paste("Successfully saved object to final destination:", file_path))
    } else {
      message("Saved intermediate checkpoint file to temporary directory: ", write_path)
    }
  } else {
    warning(paste("Failed to save file to final destination:", file_path))
  }
  
  return(invisible(NULL))
}




#' Install and Load R Packages from Vectors
#'
#' This function takes vectors of package names and sources, installs any
#' missing packages, and then loads them into the R session.
#'
#' @param package_names A character vector of package names (e.g., "dplyr", "hadley/devtools").
#' @param package_sources A character vector of corresponding sources ("cran", "bioc", or "github").
install_and_load_packages <- function(
    package_names,
    package_sources = "cran",
    silent = FALSE,
    ...
) {
  
  msg <- function(...) {
    if (!silent) message(...)
  }
  
  get_pkg_name <- function(p) {
    parts <- strsplit(p, "/")[[1]]
    parts[length(parts)]
  }
  
  install_if_missing <- function(p, source) {
    
    package_name <- get_pkg_name(p)
    
    if (requireNamespace(package_name, quietly = TRUE)) {
      msg("Already installed: ", package_name, " ✓")
      return(list(success = TRUE, pkg = package_name))
    }
    
    msg("Installing package: ", package_name, " from ", source, " ...")
    
    ok <- tryCatch({
      
      if (source == "cran") {
        
        install.packages(
          p,
          dependencies = TRUE,
          quiet = TRUE,
          ...
        )
        
      } else if (source == "bioc") {
        
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
          install.packages(
            "BiocManager",
            repos = "https://cran.r-project.org",
            quiet = TRUE
          )
        }
        
        BiocManager::install(
          p,
          dependencies = TRUE,
          ask = FALSE,
          update = FALSE,
          quiet = TRUE,
          ...
        )
        
      } else if (source == "github") {
        
        if (!requireNamespace("remotes", quietly = TRUE)) {
          install.packages(
            "remotes",
            dependencies = TRUE,
            quiet = TRUE
          )
        }
        
        remotes::install_github(
          p,
          quiet = TRUE,
          ...
        )
        
      } else {
        
        stop("Unknown package source: ", source)
        
      }
      
      TRUE
      
    }, error = function(e) {
      
      msg("Failed to install ", package_name, ": ", e$message)
      FALSE
      
    })
    
    if (!ok || !requireNamespace(package_name, quietly = TRUE)) {
      msg("Failed to install: ", package_name)
      return(list(success = FALSE, pkg = package_name))
    }
    
    msg("Installed: ", package_name, " ✓")
    
    list(
      success = TRUE,
      pkg = package_name
    )
  }
  
  if (length(package_sources) == 1 && length(package_names) > 1) {
    package_sources <- rep(package_sources, length(package_names))
  }
  
  if (length(package_names) != length(package_sources)) {
    stop(
      "The 'package_names' and 'package_sources' vectors must be the same length."
    )
  }
  
  msg("Checking for all packages...")
  
  install_results <- mapply(
    install_if_missing,
    package_names,
    package_sources,
    SIMPLIFY = FALSE
  )
  
  available_packages <- vapply(
    install_results,
    function(x) if (x$success) x$pkg else NA_character_,
    character(1)
  )
  
  available_packages <- stats::na.omit(available_packages)
  
  failed_packages <- vapply(
    install_results,
    function(x) if (!x$success) x$pkg else NA_character_,
    character(1)
  )
  
  failed_packages <- stats::na.omit(failed_packages)
  
  msg("\nLoading available packages...")
  
  invisible(
    lapply(unique(available_packages), function(p) {
      
      if (requireNamespace(p, quietly = TRUE)) {
        
        suppressPackageStartupMessages(
          library(p, character.only = TRUE)
        )
        
        msg("Loaded: ", p, " ✓")
        
      } else {
        
        msg("Skipped loading (not available): ", p)
        
      }
    })
  )
  
  if (length(failed_packages) > 0) {
    
    msg("\n\n*************** WARNING ***************\n")
    msg("Could not install: ", paste(failed_packages, collapse = ", "))
    msg("\n*************** WARNING ***************\n\n")
    
  }
  
  
  # Create a static file to guarantee renv records dynamic dependencies.
  
  # Define the directory and file path
  dependency_dir <- "R"
  dependency_file <- file.path(dependency_dir, "dependencies.R") 
  
  # Check and create the R/ folder if it doesn't exist
  if (!dir.exists(dependency_dir)) {
    message(paste("Creating directory:", dependency_dir, "to store dependencies..."))
    dir.create(dependency_dir, recursive = TRUE)
  }
  
  # NEW LOGIC: Read existing dependencies and merge them
  
  # 1. Start with the packages required by the CURRENT run
  packages_to_record <- unique(available_packages)
  
  # 2. If the file exists, read it to extract previously recorded packages
  if (file.exists(dependency_file)) {
    existing_lines <- readLines(dependency_file)
    
    # Extract package names from lines starting with "library("
    # Uses regex to pull only the package name out of the library() call
    existing_packages <- sub("^library\\((.*)\\)$", "\\1", existing_lines[grep("^library\\(", existing_lines)])
    
    # 3. Merge the lists and get a final unique list
    packages_to_record <- unique(c(packages_to_record, existing_packages))
  }
  
  # 4. Write the final, complete, unique list back (overwriting the old file)
  writeLines(
    c(
      "# This file is generated by install_and_load_packages() to ensure renv finds dynamic dependencies.",
      "# DO NOT EDIT THIS FILE MANUALLY.",
      "",
      paste0("library(", packages_to_record, ")")
    ), 
    dependency_file
  )
  
  msg(paste0("\nGenerated dependency file: '", dependency_file, "' (Contains ", length(packages_to_record), " unique dependencies)"))
  
  msg("\nAll required packages are now ready to use!")
  
  
  invisible(
    list(
      installed = unique(available_packages),
      failed = unique(failed_packages)
    )
  )
}


# Test
# install_and_load_packages(c("ggplot2"))
# install_and_load_packages(c("ggplot2","sesame"),c("cran","bioc"))




# Function to find a specific directory in a parent path, up to a
# specified number of levels.

#' Find a Directory in a Parent Path
#'
#' This function iteratively searches for a directory in the parent
#' directories of a starting path. It is much faster than a recursive
#' search when you only need to go up a few levels.
#'
#' @param target_dir The name of the directory to find.
#' @param start_path The starting path for the search. Defaults to the
#'                   current working directory.
#' @param max_levels The maximum number of levels to search up the
#'                   directory tree.
#'
#' @return The full path to the found directory, or NULL if not found.
find_parent_dir <- function(target_dir, start_path = here::here(), max_levels = 5) {
  # Start at the provided path
  current_path <- start_path
  
  for (i in 1:max_levels) {
    # Move up one level
    parent_path <- dirname(current_path)
    
    # Construct the full path to the target directory
    full_path <- file.path(parent_path, target_dir)
    
    # Check if the directory exists and return if found
    if (dir.exists(full_path)) {
      return(full_path)
    }
    
    # If the parent path is the same, we've reached the root of the file system
    if (parent_path == current_path) {
      break
    }
    
    # Update the current path for the next iteration
    current_path <- parent_path
  }
  
  # Return NULL if the directory was not found
  warning(paste("Directory '", target_dir, "' not found within", max_levels, "levels."))
  return(NULL)
}


mylapply <- function(
    X,
    FUN,
    ...,
    n_cores = NULL,
    reserve_fraction = 1,
    export_objects = NULL,
    verbose = TRUE
) {
  
  has_parallel <- requireNamespace("parallel", quietly = TRUE)
  
  if (!has_parallel) {
    if (verbose) {
      message("'parallel' package not available. Using lapply().")
    }
    return(lapply(X, FUN, ...))
  }
  
  if (is.null(n_cores)) n_cores <- 1
  
  if (requireNamespace("parallelly", quietly = TRUE)) {
    usable_cores <- parallelly::availableCores()
  } else {
    usable_cores <- parallel::detectCores(logical = TRUE)
  }
  
  workers <- min(
    n_cores,
    length(X),
    as.integer(floor(usable_cores / reserve_fraction))
  )
  
  if (workers <= 1L) {
    if (verbose) {
      message("Using lapply() (1 worker).")
    }
    return(lapply(X, FUN, ...))
  }
  
  if (.Platform$OS.type == "windows") {
    if (verbose) {
      message("Using parLapply() with ", workers, " workers.")
    }
    
    result <- tryCatch({
      cl <- parallel::makeCluster(workers)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      
      if (!is.null(export_objects)) {
        parallel::clusterExport(
          cl,
          varlist = export_objects,
          envir = parent.frame()
        )
      }
      
      parallel::parLapply(
        cl,
        X,
        FUN,
        ...
      )
    }, error = function(e) {
      if (verbose) {
        message(
          "Parallel execution failed (",
          conditionMessage(e),
          "). Falling back to lapply()."
        )
      }
      lapply(X, FUN, ...)
    })
    
    return(result)
  }
  
  if (verbose) {
    message("Using mclapply() with ", workers, " workers.")
  }
  
  result <- tryCatch({
    parallel::mclapply(
      X,
      FUN,
      ...,
      mc.cores = workers,
      mc.preschedule = FALSE
    )
  }, error = function(e) {
    if (verbose) {
      message(
        "Parallel execution failed (",
        conditionMessage(e),
        "). Falling back to lapply()."
      )
    }
    lapply(X, FUN, ...)
  })
  
  result
}




# DNAM PROCESSING ---------------------------------------------------------

# Sesame ------------------------------------------------------------------

normalize_sample_sheet <- function(df, sample_id_col = "Sample_ID", sample_sex_col = "Sex") {
  if (!"Basename" %in% names(df)) {
    stop("Sample sheet must contain a 'Basename' column with IDAT basenames.", call. = FALSE)
  }
  
  if (!sample_id_col %in% names(df)) {
    message("Column '", sample_id_col, "' not found; creating it from Basename.")
    df[[sample_id_col]] <- basename(df$Basename)
  }
  
  if (!sample_sex_col %in% names(df)) {
    message("Column '", sample_sex_col, "' not found; sex mismatch QC will be skipped.")
    df[[sample_sex_col]] <- NA_character_
  }
  
  df[[sample_id_col]] <- as.character(df[[sample_id_col]])
  df[[sample_sex_col]] <- as.character(df[[sample_sex_col]])
  df$Basename <- as.character(df$Basename)
  
  if (anyDuplicated(df[[sample_id_col]]) > 0) {
    stop("Sample IDs in column '", sample_id_col, "' must be unique.", call. = FALSE)
  }
  
  df
}


#' Run Sesame Pipeline
#' sample_sheet must contain 'Basename' column with the idat file path
#' may optionally contain a column with sample ID's
run_sesame_pipeline <- function(
    sample_sheet, 
    ncores = 1,
    sample_id_col = NULL,
    out_path = file.path("data/processed/dnam_sesame-sigdfs.qs2")
) {
  # Ensure sesame cache is initialized
  sesameDataCache()
  
  if(is.character(sample_sheet)) sample_sheet <- data.table::fread(sample_sheet)
  
  allcores <- parallelly::availableCores()
  usable_cores <- min(ncores, allcores)
  
  ssets <- openSesame(
    sample_sheet[["Basename"]],
    prep = "QCDPB",
    prep_args = NULL,
    manifest = NULL,
    func = NULL,
    BPPARAM = BiocParallel::MulticoreParam(min(nrow(sample_sheet), usable_cores)),
    platform = "",
    min_beads = 1
  )
  
  head(names(ssets))
  all(identical(names(ssets), sample_sheet[["filename"]]))
  
  if (!is.null(sample_id_col)) {
    # Changing name to sample ID
    names(ssets) <- sample_sheet[["Sample_ID"]]
    head(names(ssets))
  }
  
  if (!is.null(out_path)) {
    save_dynamic_file(
      ssets,
      out_path,
      overwrite = TRUE
    )
  }
  
  return(ssets)
}


get_sesame_qc_df <- function(
    sigdf_list,
    infer_sex = TRUE,
    qc_funs = c("detection", "numProbes", "intensity", "channel", "dyeBias", "betas"),
    out_csv = NULL
) {
  if (!is.list(sigdf_list) || length(sigdf_list) == 0) {
    stop("`sigdf_list` must be a non-empty list of SigDF objects.")
  }
  
  if (is.null(names(sigdf_list)) || any(names(sigdf_list) == "")) {
    names(sigdf_list) <- paste0("sample_", seq_along(sigdf_list))
  }
  
  qc_list <- lapply(sigdf_list, function(sdf) {
    sesame::sesameQC_calcStats(sdf, funs = qc_funs)
  })
  
  if (!all(vapply(qc_list, inherits, logical(1), "sesameQC"))) {
    stop("All elements of `qc_list` must inherit from class 'sesameQC'.")
  }
  
  qc_df <- do.call(rbind, lapply(seq_along(qc_list), function(i) {
    x <- as.data.frame(qc_list[[i]], check.names = FALSE)
    x$Sample_ID <- names(qc_list)[i]
    x
  }))
  qc_df <- qc_df[, c("Sample_ID", setdiff(colnames(qc_df), "Sample_ID")), drop = FALSE]
  
  if (infer_sex) {
    beta_list <- lapply(sigdf_list, getBetas)
    infered_sex <- sapply(beta_list, inferSex)
    infered_sex <- as.data.frame(infered_sex)
    infered_sex$Sample_ID <- rownames(infered_sex)
    
    qc_df <- left_join(qc_df, infered_sex, by = "Sample_ID")
  }
  
  if (!is.null(out_csv)) {
    data.table::fwrite(qc_df, out_csv)
  }
  
  qc_df
}


flag_sesame_qc_samples <- function(
    qc_df,
    metadata_df,
    sample_col = "Sample_ID",
    metadata_sex_col = "Sex",
    qc_sex_col = "infered_sex",
    detection_col = "frac_dt",
    intensity_col = "mean_intensity_MU",
    dye_bias_col = "RGdistort",
    detection_cutoff = NULL,
    intensity_cutoff = NULL,
    dye_bias_cutoff = NULL,
    iqr_mult = 1.5,
    out_path = NULL
) {
  qc_df <- as.data.frame(qc_df, check.names = FALSE)
  metadata_df <- as.data.frame(metadata_df, check.names = FALSE)
  
  required_qc <- c("Sample_ID", detection_col, intensity_col, dye_bias_col)
  missing_qc <- setdiff(required_qc, colnames(qc_df))
  if (length(missing_qc) > 0) {
    stop("Missing required QC columns: ", paste(missing_qc, collapse = ", "))
  }
  
  if (!sample_col %in% colnames(metadata_df)) {
    stop("`sample_col` not found in `metadata_df`.")
  }
  
  if (!metadata_sex_col %in% colnames(metadata_df)) {
    metadata_sex_col <- grep("sex", colnames(metadata_df), ignore.case = T, value = T)
    if (length(metadata_sex_col) == 1) {
      warning(metadata_sex_col, " not found in `metadata_df`. \nFound ", metadata_sex_col, " instead and using that.")
    } else stop("`metadata_sex_col` not found in `metadata_df`.")
  }
  
  qc_df$Sample_ID <- as.character(qc_df$Sample_ID)
  metadata_df[[sample_col]] <- as.character(metadata_df[[sample_col]])
  
  merged <- merge(
    qc_df,
    metadata_df,
    by.x = "Sample_ID",
    by.y = sample_col,
    all.x = TRUE,
    sort = FALSE
  )
  
  norm_sex <- function(x) {
    x <- toupper(trimws(as.character(x)))
    out <- rep(NA_character_, length(x))
    out[grepl("^F", x)] <- "F"
    out[grepl("^M", x)] <- "M"
    out
  }
  
  fence_flag <- function(x, direction = c("low", "high"), cutoff = NULL) {
    direction <- match.arg(direction)
    x <- suppressWarnings(as.numeric(x))
    
    if (all(is.na(x))) return(rep(NA, length(x)))
    
    if (is.null(cutoff)) {
      q1 <- stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)
      q3 <- stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)
      iqr <- stats::IQR(x, na.rm = TRUE)
      cutoff <- if (direction == "low") {
        q1 - iqr_mult * iqr
      } else {
        q3 + iqr_mult * iqr
      }
    }
    
    if (direction == "low") {
      x < cutoff
    } else {
      x > cutoff
    }
  }
  
  # Standardize / coerce
  merged$detection_rate <- as.numeric(merged[[detection_col]])
  merged$intensity <- as.numeric(merged[[intensity_col]])
  merged$dye_bias <- as.numeric(merged[[dye_bias_col]])
  
  # If detection is stored as fraction (0-1), convert to percent
  if (max(merged$detection_rate, na.rm = TRUE) <= 1) {
    merged$detection_rate <- merged$detection_rate * 100
  }
  
  # Flags
  merged$flag_detection <- fence_flag(
    merged$detection_rate,
    direction = "low",
    cutoff = detection_cutoff
  )
  
  merged$flag_intensity <- fence_flag(
    merged$intensity,
    direction = "low",
    cutoff = intensity_cutoff
  )
  
  merged$flag_dye_bias <- fence_flag(
    merged$dye_bias,
    direction = "high",
    cutoff = dye_bias_cutoff
  )
  
  if (qc_sex_col %in% colnames(merged)) {
    merged$reported_sex <- norm_sex(merged[[metadata_sex_col]])
    merged$inferred_sex <- norm_sex(merged[[qc_sex_col]])
    merged$flag_sex_mismatch <- !is.na(merged$reported_sex) &
      !is.na(merged$inferred_sex) &
      merged$reported_sex != merged$inferred_sex
  } else {
    merged$reported_sex <- norm_sex(merged[[metadata_sex_col]])
    merged$inferred_sex <- NA_character_
    merged$flag_sex_mismatch <- NA
  }
  
  # Overall flag + reasons
  merged$any_flag <- with(
    merged,
    isTRUE(flag_detection) | isTRUE(flag_intensity) | isTRUE(flag_dye_bias) | isTRUE(flag_sex_mismatch)
  )
  
  merged$flag_reason <- apply(
    merged[, c("flag_detection", "flag_intensity", "flag_dye_bias", "flag_sex_mismatch"), drop = FALSE],
    1,
    function(x) {
      reasons <- character(0)
      if (isTRUE(x[1])) reasons <- c(reasons, "low_detection")
      if (isTRUE(x[2])) reasons <- c(reasons, "low_intensity")
      if (isTRUE(x[3])) reasons <- c(reasons, "high_dye_bias")
      if (isTRUE(x[4])) reasons <- c(reasons, "sex_mismatch")
      if (length(reasons) == 0) "" else paste(reasons, collapse = ";")
    }
  )
  
  out <- merged[, c(
    "Sample_ID",
    detection_col,
    intensity_col,
    dye_bias_col,
    metadata_sex_col,
    "reported_sex",
    "inferred_sex",
    "detection_rate",
    "intensity",
    "dye_bias",
    "flag_detection",
    "flag_intensity",
    "flag_dye_bias",
    "flag_sex_mismatch",
    "any_flag",
    "flag_reason"
  ), drop = FALSE]
  
  out <- out[order(!out$any_flag, out$Sample_ID), , drop = FALSE]
  rownames(out) <- NULL
  
  if (!is.null(out_path)) {
    data.table::fwrite(out, out_path)
  }
  
  out
}


make_sesame_qc_visuals <- function(
    flag_df,
    out_pdf = NULL,
    detection_col = "detection_rate",
    intensity_col = "intensity",
    dye_bias_col = "dye_bias",
    flag_col = "any_flag",
    reason_col = "flag_reason",
    sample_col = "Sample_ID",
    bins = 30
) {
  flag_df <- as.data.frame(flag_df, check.names = FALSE)
  
  required <- c(sample_col, detection_col, intensity_col, dye_bias_col, flag_col, reason_col)
  missing <- setdiff(required, colnames(flag_df))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  flag_df[[flag_col]] <- as.logical(flag_df[[flag_col]])
  
  # Tidy flag reasons
  flag_long <- flag_df[flag_df[[flag_col]] %in% TRUE, , drop = FALSE]
  if (nrow(flag_long) > 0) {
    flag_long <- tidyr::separate_rows(
      flag_long,
      !!rlang::sym(reason_col),
      sep = ";"
    )
    flag_long[[reason_col]] <- trimws(flag_long[[reason_col]])
    flag_long <- flag_long[flag_long[[reason_col]] != "", , drop = FALSE]
  } else {
    flag_long <- data.frame(reason = character(0))
    colnames(flag_long) <- c(reason_col)
  }
  
  # Histograms
  p_detection <- ggplot2::ggplot(
    flag_df,
    ggplot2::aes(x = .data[[detection_col]], fill = .data[[flag_col]])
  ) +
    ggplot2::geom_histogram(bins = bins, alpha = 0.75, position = "identity") +
    ggplot2::labs(
      x = "Detection rate",
      y = "Count",
      fill = "Flagged",
      title = "Detection rate distribution"
    ) +
    ggplot2::theme_bw()
  
  p_intensity <- ggplot2::ggplot(
    flag_df,
    ggplot2::aes(x = .data[[intensity_col]], fill = .data[[flag_col]])
  ) +
    ggplot2::geom_histogram(bins = bins, alpha = 0.75, position = "identity") +
    ggplot2::labs(
      x = "Intensity",
      y = "Count",
      fill = "Flagged",
      title = "Intensity distribution"
    ) +
    ggplot2::theme_bw()
  
  p_dye <- ggplot2::ggplot(
    flag_df,
    ggplot2::aes(x = .data[[dye_bias_col]], fill = .data[[flag_col]])
  ) +
    ggplot2::geom_histogram(bins = bins, alpha = 0.75, position = "identity") +
    ggplot2::labs(
      x = "Dye bias",
      y = "Count",
      fill = "Flagged",
      title = "Dye bias distribution"
    ) +
    ggplot2::theme_bw()
  
  # Flag counts by reason
  if (nrow(flag_long) > 0) {
    reason_counts <- as.data.frame(table(flag_long[[reason_col]]), stringsAsFactors = FALSE)
    colnames(reason_counts) <- c("reason", "n")
    p_reasons <- ggplot2::ggplot(reason_counts, ggplot2::aes(x = reorder(reason, n), y = n)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = NULL,
        y = "Flagged samples",
        title = "Flag counts by reason"
      ) +
      ggplot2::theme_bw()
  } else {
    p_reasons <- ggplot2::ggplot() +
      ggplot2::theme_void() +
      ggplot2::ggtitle("Flag counts by reason")
  }
  
  # Table of flagged samples
  flagged_table <- flag_df[flag_df[[flag_col]] %in% TRUE, , drop = FALSE]
  flagged_table <- flagged_table[order(flagged_table[[sample_col]]), , drop = FALSE]
  
  # Combined plot
  combined <- p_detection / p_intensity / p_dye / p_reasons
  
  if (!is.null(out_pdf)) {
    grDevices::pdf(out_pdf, width = 11, height = 14)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(combined)
    if (nrow(flagged_table) > 0) {
      grid::grid.newpage()
      gridExtra::grid.table(flagged_table[, intersect(
        c(sample_col, detection_col, intensity_col, dye_bias_col, reason_col),
        colnames(flagged_table)
      ), drop = FALSE])
    }
  }
  
  invisible(list(
    combined_plot = combined,
    detection_plot = p_detection,
    intensity_plot = p_intensity,
    dye_bias_plot = p_dye,
    reason_plot = p_reasons,
    flagged_table = flagged_table
  ))
}


#' Advanced filtering for EPICv2 and Sex Chromosomes
#' @param ssets List of SigDFs
#' @param keep_nv Logical; keep the new EPICv2 variant probes?
#' @param remove_sex_chroms Logical; remove probes on X and Y?
filter_sesame_probes <- function(ssets, 
                                 keep_rs = FALSE, 
                                 keep_ch = FALSE, 
                                 keep_nv = FALSE,
                                 keep_sex_chroms = FALSE) {
  
  lapply(ssets, function(sdf) {
    
    rownames(sdf) <- sdf[["Probe_ID"]]
    
    probe_ids <- sdf[["Probe_ID"]]
    
    # 1. Handle Probe Types
    patterns <- c("^cg")
    if (keep_rs) patterns <- c(patterns, "^rs")
    if (keep_ch) patterns <- c(patterns, "^ch")
    if (keep_nv) patterns <- c(patterns, "^nv")
    
    keep_regex <- paste(patterns, collapse = "|")
    sdf_filtered <- sdf[grep(keep_regex, probe_ids), ]
    
    # 2. Handle Sex Chromosomes
    if (!keep_sex_chroms) {
      
      # Filter out X and Y based on the address info
      sex_probes <- 
        sesameData_getProbesByRegion(
          chrm = c("chrX","chrY"),
          platform = sdfPlatform(sdf)
        ) %>%
        as.data.frame()
      
      sex_probes <- rownames(sex_probes)
      
      sdf_filtered <- sdf_filtered[!(rownames(sdf_filtered) %in% sex_probes), ]
    }
    
    return(sdf_filtered)
  })
}


# Extract and Filter Betas
process_betas <- function(ssets, mask_threshold = 1, collapse = FALSE, ncores = 1, out_path = NULL) {
  
  allcores <- parallelly::availableCores()
  usable_cores <- min(ncores, ceiling(allcores / 10), length(ssets))
  
  # Get raw betas
  beta_values <- openSesame(
    ssets,
    prep = "QCDPB",
    prep_args = NULL,
    manifest = NULL,
    func = getBetas,
    BPPARAM = BiocParallel::MulticoreParam(usable_cores),
    platform = "",
    min_beads = 1
  )
  
  # Filter probes masked in > X% of samples
  mask_pct <- rowMeans(is.na(beta_values))
  beta_values_filtered <- beta_values[mask_pct <= mask_threshold, ]
  
  if (collapse) {
    beta_values_filtered <- betasCollapseToPfx(beta_values_filtered)
  }
  
  if (!is.null(out_path)) save_dynamic_file(beta_values_filtered, out_path, copy_to_main = TRUE, overwrite = TRUE)
  
  beta_values_filtered
}


get_control_ssets <- function(ssets, platform = c("HM450", "EPIC", "EPICv2"), use_default = TRUE, custom_ssets = NULL) {
  
  infer_platform <- if(is.data.frame(ssets)) sdfPlatform(ssets) else sdfPlatform(ssets[[1]])
  if (is.null(platform)) platform <- infer_platform
  
  if (use_default) {
    if (platform != infer_platform) {
      warning("Platform infered from data: ", infer_platform, " is different from the one provided: ", platform)
    }
    platform <- match.arg(platform)
    platform <- ifelse(platform == "EPICv2", "EPIC", platform)
    
    sesame_data_controls <- sesameDataList("normal")
    sesame_data_controls <- sesame_data_controls %>%
      dplyr::mutate(control_name = case_when(
        str_detect(Title, "EPIC") ~ "EPIC",
        str_detect(Title, "BLCA") ~ "450k_BLCA",
        str_detect(Title, "PAAD") ~ "450k_PAAD",
        TRUE ~ Title
      ))
    
    sesame_data_control_flt <- sesame_data_controls %>%
      dplyr::filter(str_detect(Title, platform))
    
    if (nrow(sesame_data_control_flt) == 0) {
      if (is.null(custom_ssets)) {
        stop("No normal data found for the given/infered platform: ", platform)
      } else {
        warning("No normal data found for the given/infered platform: ", platform, "\nUsing the provided custom normal only.")
      }
    }
    
    sesame_data_control_names <- setNames(sesame_data_control_flt$Title, 
                                          sesame_data_control_flt$control_name)
    
    sesame_controls <- lapply(sesame_data_control_names, sesameDataGet)
    
    for (nm in names(sesame_controls)) {
      attr(sesame_controls[[nm]], "control_name") <- nm
    }
    
  } else sesame_controls <- NULL
  
  if (is.character(custom_ssets)) {
    custom_controls <- lapply(custom_ssets, read_dynamic_file)
    custom_ssets_names <- basename(tools::file_path_sans_ext(custom_ssets))
    names(custom_controls) <- custom_ssets_names
    
    for (nm in names(custom_controls)) {
      attr(custom_controls[[nm]], "control_name") <- nm
    }
  } else custom_controls <- custom_ssets
  
  if (is.null(custom_ssets)) {
    control_list <- sesame_controls
  } else if (is.null(sesame_controls)) {
    control_list <- custom_controls
  } else control_list <- c(sesame_controls, custom_controls)
  
  control_list
}


# SVM ---------------------------------------------------------------------

svm_classifier <- function(beta_values, platform = c("HM450", "EPICv2"), out_path = NULL){
  
  platform <- match.arg(platform)
  platform <- ifelse(platform == "HM450", "450k", platform)
  
  base_dir <- dir("data", "SVM_classifier", full.names = TRUE)
  svm_probes_txt <- sprintf("probes_%s.txt", platform)
  svm_3group_rds <- sprintf("svm_Linear_%s.rds", platform)
  svm_4group_rds <- sprintf("svm_Linear_%s_4groups.rds", platform)
  
  probes_v2 = read.table(file.path(base_dir, svm_probes_txt), quote = "") %>%
    t() %>%
    as.character()
  
  # Removing suffix _BC/_TC
  probes_v2 <- str_remove(probes_v2, "_.*")
  
  svm_Linear_EPICV2 = readRDS(file.path(base_dir, svm_3group_rds))
  svm_Linear_EPICV2_4groups = readRDS(file.path(base_dir, svm_4group_rds))
  
  
  newdat_v2 = as.data.frame(t(beta_values[rownames(beta_values) %in% probes_v2, ])) #double check
  newdat_v2[is.na(newdat_v2)] = 0   #replace NA with 0 for SVM classifier
  head(names(newdat_v2))
  # checking if all the probes in classifier are present in the data
  if(!all(match(names(newdat_v2), probes_v2))){
    stop("Some probes from the SVM classifier are missing from the beta values matrix")
  }
  
  
  p = factor()
  for (i in rownames(newdat_v2)) {
    p = c(p, predict(svm_Linear_EPICV2, newdata = newdat_v2[i,]))
  }
  
  p2 = factor()
  for (i in rownames(newdat_v2)) {
    p2 = c(p2, predict(svm_Linear_EPICV2_4groups, newdata = newdat_v2[i,]))
  }
  
  sample_sheet_dnam_groups = data.frame(Sample_ID = rownames(newdat_v2), 
                                        "DNA_methylation_group" = p, 
                                        "DNA_methylation_subgroup" = p2)
  
  
  if (!is.null(out_path)) fwrite(sample_sheet_dnam_groups, out_path)
  
  return(sample_sheet_dnam_groups)
}


# Conumee 2 ---------------------------------------------------------------

infer_genome_build <- function(
    ssets_tumor, ssets_control
) {
  tumor_platform <- if(is.data.frame(ssets_tumor)) sdfPlatform(ssets_tumor) else sdfPlatform(ssets_tumor[[1]])
  tumor_platform <- ifelse(tumor_platform == "HM450", "450k", tumor_platform)
  
  control_platform <- if(is.data.frame(ssets_control)) sdfPlatform(ssets_control) else sdfPlatform(ssets_control[[1]])
  control_platform <- ifelse(control_platform == "HM450", "450k", control_platform)
  
  array_type = unique(c(control_platform, tumor_platform))
  
  if (identical(array_type, "EPICv2")) {
    return("hg38")
  } else return("hg19")
}

get_detail_regions <- function(
    genome_build, genes_to_annotate = "default", detail_regions_path = NULL
) {
  if (genome_build == "hg19") {
    data("detail_regions")
  }
  
  if (genome_build == "hg38") {
    data("detail_regions.hg38")
  }
  
  # Custom
  if (!is.null(detail_regions_path) & !"default" %in% genes_to_annotate) {
    new_detail_regions <- read_dynamic_file(detail_regions_path)
    common_genes <- intersect(genes_to_annotate, new_detail_regions$name)
    if (length(common_genes) == 0) {
      message("No genes to annotate found. Reverting to default...")
    } else {
      detail_regions <- new_detail_regions[new_detail_regions$name %in% common_genes]
    }
  }
  
  detail_regions
  
}

run_conumee <- function(
    ssets_tumor, ssets_control, ncores = 1, out_path = NULL, ...
) {
  
  tumor_platform <- if(is.data.frame(ssets_tumor)) sdfPlatform(ssets_tumor) else sdfPlatform(ssets_tumor[[1]])
  tumor_platform <- ifelse(tumor_platform == "HM450", "450k", tumor_platform)
  
  control_platform <- if(is.data.frame(ssets_control)) sdfPlatform(ssets_control) else sdfPlatform(ssets_control[[1]])
  control_platform <- ifelse(control_platform == "HM450", "450k", control_platform)
  
  array_type = unique(c(control_platform, tumor_platform))
  
  anno = CNV.create_anno(
    bin_minprobes = 15,
    bin_minsize = 50000,
    bin_maxsize = 5e+06,
    array_type = array_type,
    ...
  )
  
  anno@args$array_type <- array_type
  
  inorm <- CNV.load(do.call(cbind, lapply(ssets_control, totalIntensities)))
  
  sample_ids <- names(ssets_tumor)
  n_samples  <- length(sample_ids)
  
  
  process_sample <- function(sample_id) {
    itum <- conumee2::CNV.load(sesame::totalIntensities(ssets_tumor[[sample_id]]))
    y <- conumee2::CNV.fit(itum, inorm, anno)
    names(y) <- sample_id
    y <- conumee2::CNV.bin(y)
    y <- conumee2::CNV.detail(y)
    y <- conumee2::CNV.segment(y)
    y <- conumee2::CNV.focal(y)
    y
  }
  
  cna_list <- mylapply(
    sample_ids,
    process_sample,
    n_cores = ncores
  )
  
  names(cna_list) <- sample_ids
  
  # combine fit
  cna <- cna_list[[1]]
  if (length(cna_list) > 1) {
    for (i in 2:length(cna_list)) {
      cna <- CNV.combine(cna, cna_list[[i]])
    }
  }
  
  # calculate bin and details on combined object
  cna <- CNV.bin(cna)
  cna <- CNV.detail(cna)
  
  # combine segments
  cna@seg$summary = lapply(cna_list, function(x){x@seg$summary[[1]]})
  cna@seg$p = lapply(cna_list, function(x){x@seg$p[[1]]})
  
  # combine focal details
  details2combine <- setdiff(names(cna@detail), c("ratio", "probes"))
  for (d in details2combine) {
    cna@detail[[d]] <- lapply(cna_list, function(x){x@detail[[d]][[1]]})
  }
  
  if (!is.null(out_path)) {
    save_dynamic_file(cna, out_path, overwrite = TRUE)
  }
  
  return(cna)
}


get_conumee_segments <- function(cna_data, csv_path = NULL) {
  
  seg_list <- CNV.write(cna_data, what = "segments")
  seg_df <- do.call(rbind, seg_list)
  
  if (!is.null(csv_path)) {
    if (isTRUE(file.info(csv_path)$isdir)) {
      out_path <- file.path(csv_path, paste0("conumee2_cna-segments-combined_.csv"))
    } else out_path <- csv_path
    
    data.table::fwrite(x = seg_df, file = out_path)
  }
  
  seg_df
  
}


# Modified CNV.summaryplot that plots for different gain and loss threshold in one plot
.cumsum0 <- function(x, left = TRUE, right = FALSE, n = NULL) {
  xx <- c(0, cumsum(as.numeric(x)))
  if (!left)
    xx <- xx[-1]
  if (!right)
    xx <- head(xx, -1)
  names(xx) <- n
  xx
}

CNV.summaryplot.mod <- function(
    object,
    set_par = TRUE,
    main = NULL,
    output = "local",
    directory = getwd(),
    width = 12,
    height = 6,
    res = 720,
    threshold_gain = 0.1,
    threshold_loss = -0.1,
    ...
) {
  
  if (set_par) {
    mfrow_original <- par()$mfrow
    mar_original <- par()$mar
    oma_original <- par()$oma
  }
  
  if (ncol(object@fit$ratio) <= 1) {
    stop("Please use multiple query samples to create a summaryplot")
  }
  
  if (output == "pdf") {
    p_names <- paste(directory, "/", "genome_summaryplot", ".pdf", sep = "")
    pdf(p_names, width = width, height = height)
    par(mfrow = c(1, 1), mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))
  }
  
  if (output == "png") {
    p_names <- paste(directory, "/", "genome_summaryplot", ".png", sep = "")
    png(p_names, units = "in", width = width, height = height, res = res)
    par(mfrow = c(1, 1), mar = c(4, 4, 4, 4), oma = c(0, 0, 0, 0))
  }
  
  # Get segment-level output instead of symmetric thresholded output
  y <- rbindlist(CNV.write(object, what = "segments", ...))
  
  # Find the segment mean column robustly
  seg_col_candidates <- c(
    "Segment_Mean", "Segment.Mean", "segment.mean",
    "seg.mean", "mean"
  )
  seg_col <- intersect(seg_col_candidates, colnames(y))[1]
  
  if (is.na(seg_col) || length(seg_col) == 0L) {
    stop("Could not find a segment mean / ratio column in CNV.write output.")
  }
  
  # Apply asymmetric thresholds
  y$Alteration <- "balanced"
  y$Alteration[y[[seg_col]] >= threshold_gain] <- "gain"
  y$Alteration[y[[seg_col]] <= threshold_loss] <- "loss"
  
  message("creating summaryplot")
  
  segments.i <- GRanges(
    seqnames = y$chrom,
    ranges = IRanges(y$loc.start, y$loc.end)
  )
  segments_chromosomes <- GRanges(
    seqnames = object@anno@genome$chr,
    ranges = IRanges(start = 1, end = object@anno@genome$size)
  )
  segments <- c(segments.i, segments_chromosomes)
  d_segments <- as.data.frame(GenomicRanges::disjoin(segments))
  
  overview <- as.data.frame(matrix(nrow = 0, ncol = 4))
  for (i in 1:nrow(d_segments)) {
    x <- d_segments[i, ]
    involved_segements <- y[
      y$chrom == x$seqnames &
        y$loc.start <= x$start &
        y$loc.end >= x$end,
    ]
    balanced <- sum(involved_segements$Alteration == "balanced")
    gain <- sum(involved_segements$Alteration == "gain")
    loss <- sum(involved_segements$Alteration == "loss")
    overview <- rbind(overview, c(as.character(x$seqnames), balanced, gain, loss))
  }
  
  colnames(overview) <- c("disjoined_segment", "count_balanced", "count_gains", "count_losses")
  overview$count_balanced <- as.numeric(overview$count_balanced)
  overview$count_gains <- as.numeric(overview$count_gains)
  overview$count_losses <- as.numeric(overview$count_losses)
  
  d_segments$gains <- overview$count_gains / length(unique(y$ID)) * 100
  d_segments$losses <- overview$count_losses / length(unique(y$ID)) * 100
  d_segments$balanced <- overview$count_balanced / length(unique(y$ID)) * 100
  
  segments_pl <- d_segments[rep(1:nrow(d_segments), each = 2), ]
  odd_indexes <- seq(1, nrow(segments_pl), 2)
  even_indexes <- seq(2, nrow(segments_pl), 2)
  segments_pl$xpos <- NA
  segments_pl$xpos[odd_indexes] <- segments_pl$start[odd_indexes]
  segments_pl$xpos[even_indexes] <- segments_pl$end[even_indexes]
  
  segments_pl$seqnames <- factor(segments_pl$seqnames, levels = object@anno@genome$chr)
  segments_pl <- segments_pl[order(segments_pl$seqnames, segments_pl$start), ]
  
  par(mfrow = c(1, 1), mgp = c(4, 1, 0), mar = c(4, 8, 4, 4), oma = c(0, 0, 0, 0))
  plot(
    NA,
    xlim = c(0, sum(as.numeric(object@anno@genome$size))),
    ylim = c(-100, 100),
    xaxs = "i",
    xaxt = "n",
    yaxt = "n",
    xlab = NA,
    ylab = "percentage of samples exhibiting the CNV [%]",
    main = main,
    cex = 1.5,
    cex.lab = 1,
    cex.axis = 1,
    cex.main = 1.5
  )
  
  abline(v = .cumsum0(object@anno@genome$size, right = TRUE), col = "grey")
  abline(v = .cumsum0(object@anno@genome$size) + object@anno@genome$pq, col = "grey", lty = 2)
  axis(
    1,
    at = .cumsum0(object@anno@genome$size) + object@anno@genome$size / 2,
    labels = object@anno@genome$chr,
    las = 2
  )
  
  axis(2, las = 2, lty = 1, at = seq(0, 100, 20), labels = abs(seq(0, 100, 20)), cex.axis = 1)
  axis(2, las = 2, lty = 1, at = seq(-100, 0, 20), labels = abs(seq(-100, 0, 20)), cex.axis = 1)
  
  chr <- object@anno@genome$chr
  chr.cumsum0 <- .cumsum0(object@anno@genome[chr, "size"], n = chr)
  
  polygon(
    as.numeric(chr.cumsum0[match(segments_pl$seqnames, names(chr.cumsum0))]) + segments_pl$xpos,
    segments_pl$gains,
    col = "#F16729",
    lwd = 1.5
  )
  polygon(
    as.numeric(chr.cumsum0[match(segments_pl$seqnames, names(chr.cumsum0))]) + segments_pl$xpos,
    -segments_pl$losses,
    col = "darkblue",
    lwd = 1.5
  )
  
  if (is.element(output, c("pdf", "png"))) {
    dev.off()
    message("file was created")
  }
  
  if (set_par) {
    par(mfrow = mfrow_original, mar = mar_original, oma = oma_original)
  }
  
  NULL
}


plot_cna_summary <- function(cna_data, output_path) {
  pdf(output_path, width = 8*2, height = 6)
  
  # Density plot
  cna_segments_dt <- rbindlist(CNV.write(cna_data, what = "segments"))
  
  custom_breaks <- function(limits) {
    # rounding to 1 decimal place
    lo <- floor(limits[1]*10)/10
    hi <- ceiling(limits[2]*10)/10
    
    # Fine region
    fine_min <- max(lo, -0.5)
    fine_max <- min(hi,  0.5)
    
    fine <- if (fine_min <= fine_max) {
      seq(fine_min, fine_max, by = 0.1)
    } else numeric(0)
    
    # Coarse regions
    left <- if (lo < -0.5) seq(lo, -0.5, by = 0.5) else numeric(0)
    right <- if (hi > 0.5) seq(0.5, hi, by = 0.5) else numeric(0)
    
    # Combine + deduplicate + sort
    sort(unique(c(left, fine, right)))
  }
  
  print(
    ggplot(cna_segments_dt,
           aes(x=seg.mean)) + 
      geom_density(linewidth = 1) + 
      # geom_vline(xintercept = c(-0.25,0.1),
      #            linetype = 2,
      #            color = "red") + 
      scale_x_continuous(breaks = custom_breaks) + 
      theme_bw() + 
      theme(axis.title = element_text(size = 18),
            axis.text = element_text(size = 14),
            panel.widths = unit(8*1.7,"in"),
            panel.heights = unit(6*0.8,"in"))
  )
  
  # Summary plots at different thresholds
  for (thr in seq(0, 0.5, by = 0.05)) {
    CNV.summaryplot(cna_data, threshold = thr)
    title(paste0("Threshold: ", thr))
  }
  
  # Heatmap
  CNV.heatmap(cna_data)
  
  # Genome plots for each sample
  for (i in names(cna_data)) {
    CNV.genomeplot(cna_data[i])
  }
  
  dev.off()
  output_path
}


# Binary CNA --------------------------------------------------------------

#' Function for binary CNA calling using DNA methylation segmentation data
#'
#' @param seg_data data frame or path for csv containing segmentation data.
#'        Must contain 'ID' (Sample ID), 'chrom', 'loc.start', 'loc.end', 'seg.mean' columns.
#'        If a directory path is provided, all .csv files within it will be read and combined.
#' @param genome_build version of human genome used. Supported: 'hg19' (default) and 'hg38'.
#' @param loss_cutoff Threshold for calling a loss (e.g., -0.25). Should be negative.
#' @param gain_cutoff Threshold for calling a gain (e.g., 0.25). Should be positive.
#' @param chr_cutoff Minimum segment width (as a percentage of the arm's total width) required
#'        to call a loss or gain for that arm. Should be between 0 and 100.
#' @return A matrix of binary CNA calls (1 for gain/loss, 0 otherwise), with
#'         samples in rows and chromosome arms (e.g., '1p_loss', '1q_gain') in columns.
#' @export
dnam_call_binary_cna <- function(
    seg_data,
    genome_build = "hg19",
    loss_cutoff = -0.25,
    gain_cutoff = 0.15,
    chr_cutoff = 5,
    out_dir = NULL) {
  
  # === INITIAL CHECKS AND SETUP ===
  # ADDED 'gtools' for correct alphanumeric (chromosome) sorting
  required_packages <- c("dplyr", "tidyr", "stringr", "UCSC.utils", "gtools")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required but not installed. Please install it.", pkg))
    }
  }
  
  # Set scientific notation preference
  options(scipen = 9)
  message("✅ Setting up environment and checking inputs...")
  
  # Load data if seg_data is a file path or a directory path
  if (is.character(seg_data)) {
    file_info <- file.info(seg_data)
    
    if (is.na(file_info$isdir)) {
      stop("Path specified for 'seg_data' does not exist.")
    } else if (file_info$isdir) {
      # Case 1: Directory specified. Load and combine all CSVs.
      message(sprintf("📂 Loading segmentation data from directory: %s (combining all CSVs)...", seg_data))
      seg_files <- list.files(seg_data, pattern = "\\.csv$", full.names = TRUE)
      
      if (length(seg_files) == 0) {
        stop(sprintf("No CSV segmentation files found in directory: %s", seg_data))
      }
      
      seg_data_list <- lapply(seg_files, read.csv)
      # Use dplyr::bind_rows for safe combination
      seg_data <- dplyr::bind_rows(seg_data_list)
      
    } else {
      # Case 2: Single file path specified.
      message(sprintf("📄 Loading segmentation data from file: %s...", seg_data))
      seg_data <- read.csv(seg_data)
    }
  }
  
  # Post-load check for empty data frame
  if (is.data.frame(seg_data) && nrow(seg_data) == 0) {
    stop("Segmentation data is empty after loading.")
  }
  
  # Check required columns
  required_cols <- c("ID", "chrom", "loc.start", "loc.end", "seg.mean")
  if (!all(required_cols %in% colnames(seg_data))) {
    stop(sprintf("Input 'seg_data' must contain all required columns: %s.", paste(required_cols, collapse = ", ")))
  }
  
  # Check cutoff signs and range
  if (loss_cutoff > 0) {
    warning("Argument 'loss_cutoff' should typically be negative (e.g., -0.25). Absolute value will be used.")
    loss_cutoff <- -abs(loss_cutoff)
  }
  if (gain_cutoff < 0) {
    warning("Argument 'gain_cutoff' should typically be positive (e.g., 0.25). Absolute value will be used.")
    gain_cutoff <- abs(gain_cutoff)
  }
  if (chr_cutoff < 0 || chr_cutoff > 100) {
    stop("Argument 'chr_cutoff' must be a percentage between 0 and 100.")
  }
  
  # === 1. PROCESS CYTOBAND DATA TO GET ARM COORDINATES ===
  message("⚙️ Fetching and processing cytoband data...")
  
  # Fetch cytoband data, rename, and process for arm boundaries
  cytoband <- UCSC.utils::fetch_UCSC_track_data(genome_build, "cytoBand") |>
    dplyr::rename(chrom = "chrom", chromStart = "chromStart", chromEnd = "chromEnd", band = "name") |>
    dplyr::mutate(
      arm = stringr::str_remove_all(band, "[0-9\\.]"),
      # Convert UCSC 0-based start to 1-based start
      loc.start_arm = chromStart + 1,
      loc.end_arm = chromEnd
    ) |>
    # Summarize to find the true min/max coordinates of each arm
    dplyr::group_by(chrom, arm) |>
    dplyr::summarise(
      arm_start = min(loc.start_arm),
      arm_end = max(loc.end_arm),
      .groups = 'drop'
    )
  
  # Determine the centromere split point (used to partition segments)
  centromere_split <- cytoband |>
    # Filter for autosomes and sex chromosomes with arms
    dplyr::filter(stringr::str_detect(chrom, "^chr(\\d{1,2}|X|Y)$")) |>
    dplyr::group_by(chrom) |>
    dplyr::summarise(
      p_end = max(arm_end[arm == "p"]),
      q_start = min(arm_start[arm == "q"]),
      .groups = 'drop'
    )
  
  if (nrow(centromere_split) == 0) {
    stop("Failed to determine centromere coordinates for the specified genome build. Check 'UCSC.utils::fetch_UCSC_track_data'.")
  }
  
  # === 2. PROCESS SEGMENTATION DATA AND MAP TO ARMS ===
  message("🔄 Mapping segments to chromosome arms...")
  
  processed_seg_data <- seg_data |>
    # CRITICAL: Standardize chromosome names (e.g., "1" to "chr1") to match UCSC data format for joining.
    # This removes the "chr" prefix if present, then adds it back, ensuring consistency.
    dplyr::mutate(chrom = paste0("chr", stringr::str_remove(chrom, "chr"))) |>
    
    # Calculate width early
    dplyr::mutate(width = loc.end - loc.start + 1) |>
    
    # Join with centromere data
    dplyr::inner_join(centromere_split, by = "chrom", relationship = "many-to-one") |>
    
    # Determine arm(s) covered by the segment (conservative approximation)
    dplyr::mutate(
      arm_p = dplyr::if_else(loc.end <= p_end | loc.start < q_start, "p", NA_character_),
      arm_q = dplyr::if_else(loc.start >= q_start | loc.end > p_end, "q", NA_character_),
      arm_call = paste(arm_p, arm_q, sep = ",") |> stringr::str_remove_all("NA,|,NA")
    ) |>
    
    # Separate segments that span both p and q (e.g., arm_call = 'p,q')
    tidyr::separate_rows(arm_call, sep = ",") |>
    dplyr::filter(!is.na(arm_call) & arm_call != "") |>
    dplyr::rename(arm = arm_call) |>
    
    # Group by Sample ID, Chromosome, and Arm
    dplyr::group_by(ID, chrom, arm) |>
    
    # Calculate percent width of each segment within the arm
    dplyr::mutate(percent_width = width * 100 / sum(width)) |>
    
    # === CALL BINARY CNA (LOSS/GAIN) ===
    dplyr::mutate(
      # Loss/Gain is called if seg.mean meets cutoff
      loss = dplyr::if_else(seg.mean <= loss_cutoff, 1L, 0L),
      gain = dplyr::if_else(seg.mean >= gain_cutoff, 1L, 0L),
    )
  
  # === 3. SUMMARIZE AND CONVERT TO MATRIX (REFACTORED) ===
  message("📊 Summarizing calls and creating output matrix...")
  
  # 3.1 Summarize: Get a single binary call (1 or 0) per Sample-Arm combination
  aggregated_calls <- processed_seg_data |>
    
    # Group by Sample ID, Chromosome, and Arm
    dplyr::group_by(ID, chrom, arm) |>
    
    dplyr::summarise(
      # Percent chr arm Loss/Gain if Loss/Gain segment contributes >= chr_cutoff % of arm width
      percent_chr_loss = sum(percent_width[loss == 1]),
      percent_chr_gain = sum(percent_width[gain == 1]),
      
      loss = dplyr::if_else(percent_chr_loss >= chr_cutoff, 1L, 0L),
      gain = dplyr::if_else(percent_chr_gain >= chr_cutoff, 1L, 0L),
      .groups = 'drop'
    ) |>
    # Remove 'chr' prefix for final column names (e.g., 'chr1' -> '1')
    dplyr::mutate(chrom = stringr::str_remove(chrom, "chr"))
  
  # 3.2 Pivot Loss Calls (Wide Format)
  loss_wide <- aggregated_calls |>
    dplyr::select(ID, chrom, arm, loss) |>
    tidyr::pivot_wider(
      id_cols = ID,
      names_from = c(chrom, arm),
      values_from = loss,
      names_sep = "" # Names will look like '1p', '1q', etc.
    ) |>
    # Add '_loss' suffix to the pivoted loss columns
    dplyr::rename_with(~ paste0(.x, "_loss"), .cols = -ID)
  
  # 3.3 Pivot Gain Calls (Wide Format)
  gain_wide <- aggregated_calls |>
    dplyr::select(ID, chrom, arm, gain) |>
    tidyr::pivot_wider(
      id_cols = ID,
      names_from = c(chrom, arm),
      values_from = gain,
      names_sep = ""
    ) |>
    # Add '_gain' suffix to the pivoted gain columns
    dplyr::rename_with(~ paste0(.x, "_gain"), .cols = -ID)
  
  # 3.4 Join Loss and Gain Calls
  # Use full_join to ensure all samples/arms are included, even if only loss/gain was present.
  final_calls_df <- dplyr::full_join(loss_wide, gain_wide, by = "ID")
  
  # 4. Final Matrix Conversion and Cleanup
  final_calls_mat <- final_calls_df |>
    dplyr::select(-ID) |>
    as.matrix()
  
  
  # Set row names (Sample IDs)
  rownames(final_calls_mat) <- final_calls_df$ID
  
  # Sorting step
  message("🔠 Sorting rows (Samples) and columns (Arms) using mixed-order sort...")
  
  # Sort columns (Arm names) using gtools::mixedsort for correct chromosome ordering (1, 2, 10, X, Y)
  final_calls_mat <- final_calls_mat[, gtools::mixedsort(colnames(final_calls_mat)), drop = FALSE]
  
  # Sort rows (Sample IDs) using gtools::mixedsort for robustness against numeric IDs (ID_1, ID_10, ID_2)
  final_calls_mat <- final_calls_mat[gtools::mixedsort(rownames(final_calls_mat)), , drop = FALSE]
  
  attr(final_calls_mat, "control") <- unique(seg_data$control)
  attr(final_calls_mat, "genome_build") <- genome_build
  attr(final_calls_mat, "loss_cutoff") <- loss_cutoff
  attr(final_calls_mat, "gain_cutoff") <- gain_cutoff
  attr(final_calls_mat, "chr_cutoff") <- chr_cutoff
  
  message("🎉 Analysis complete. Returning binary CNA call matrix.")
  
  if (!is.null(out_dir)) {
    out_path <- file.path(
      out_dir,
      sprintf("dnam_cna_calls_%s_L%s-G%s.csv",
              attr(final_calls_mat, "control"),loss_cutoff,gain_cutoff)
    )
    
    data.table::fwrite(final_calls_mat, out_path)
  }
  
  return(final_calls_mat)
}


#' Merges arm-level binary CNA calls into a single numeric code per arm.
#'
#' This function takes a matrix of binary (0/1) CNA calls (e.g., '1p_loss', 'Xq_gain')
#' and merges the corresponding 'loss' and 'gain' columns for each arm into a
#' single column (e.g., '1p') using a custom numeric coding scheme.
#'
#' **Special Handling:** If columns represent whole chromosome calls (e.g., '1_loss'),
#' the values are duplicated to create corresponding 'p' and 'q' arm calls (e.g., '1p_loss' and '1q_loss').
#'
#' @param cna_matrix A matrix of binary CNA calls (0 or 1). Rows must have Sample IDs
#'        as names, and columns must be named in the format '[ChrArm]_[type]'
#'        (e.g., '1p_loss', 'Xq_gain', or '1_loss').
#' @return A matrix with samples in rows and chromosome arms in columns (e.g., 'chr1p', 'chrXq').
#'         The values are integer codes:
#'         -2 = Loss only
#'         -1 = Both Loss and Gain (Complex/Conflicting)
#'          0 = Neither Loss nor Gain (Normal)
#'          1 = Gain only
#' @export
aggregate_binary_cna <- function(cna_matrix) {
  
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(gtools)
  
  message("✅ Starting merge of binary calls into single arm-level codes...")
  
  # Initial validation check (relaxed to allow whole chromosome calls)
  if (is.null(rownames(cna_matrix)) || !any(stringr::str_detect(colnames(cna_matrix), "_(loss|gain)$"))) {
    stop("Input matrix must have rownames (Sample IDs) and columns must include calls named '*_(loss|gain)'.")
  }
  
  # --- NEW STEP: Handle whole chromosome calls (e.g., '1_loss' -> '1p_loss' and '1q_loss') ---
  original_cols <- colnames(cna_matrix)
  
  # Identify columns that end in _loss or _gain but DO NOT contain 'p' or 'q' before the underscore
  is_whole_chr <- stringr::str_detect(original_cols, "_(loss|gain)$") & 
    !stringr::str_detect(original_cols, "[pq]_(loss|gain)$")
  
  whole_chr_cols <- original_cols[is_whole_chr]
  
  if (length(whole_chr_cols) > 0) {
    message(sprintf("Found %d whole chromosome calls. Duplicating to p and q arms...", length(whole_chr_cols)))
    
    # Matrix for whole chromosome calls
    whole_chr_matrix <- cna_matrix[, whole_chr_cols, drop = FALSE]
    
    # Create new column names for p and q arms
    new_cols_p <- stringr::str_replace(whole_chr_cols, "(_loss|_gain)$", "p\\1")
    new_cols_q <- stringr::str_replace(whole_chr_cols, "(_loss|_gain)$", "q\\1")
    
    # Create p and q arm matrices by duplicating data
    p_arm_matrix <- whole_chr_matrix
    colnames(p_arm_matrix) <- new_cols_p
    
    q_arm_matrix <- whole_chr_matrix
    colnames(q_arm_matrix) <- new_cols_q
    
    # Identify arm-specific columns to keep
    arm_specific_cols <- original_cols[!is_whole_chr]
    arm_specific_matrix <- cna_matrix[, arm_specific_cols, drop = FALSE]
    
    # Combine: Arm-specific + duplicated p-arm + duplicated q-arm
    cna_matrix <- cbind(arm_specific_matrix, p_arm_matrix, q_arm_matrix)
    
    # Ensure all values remain 0/1 integers after cbind (which might coerce to numeric)
    cna_matrix <- round(cna_matrix)
  }
  # --- END NEW STEP ---
  
  # 1. Convert the matrix output back to a data frame for tidyverse processing
  cna_df <- as.data.frame(cna_matrix)
  cna_df$ID <- rownames(cna_df)
  
  # 2. Pivot to long format and parse the column names
  arm_calls_long <- cna_df %>%
    # Move all call columns into rows
    tidyr::pivot_longer(
      cols = -ID,
      names_to = "arm_call",
      values_to = "value"
    ) %>%
    # Parse the arm name and the call type
    dplyr::mutate(
      # Extract the base arm name (e.g., '1p', 'Xq')
      arm = stringr::str_remove(arm_call, "_(loss|gain)$"),
      # Remove chr prefix if present
      arm = stringr::str_remove(arm, "chr"),
      # # Ensure the final arm name includes the 'chr' prefix (e.g., '1p' -> 'chr1p')
      # arm = paste0("chr", stringr::str_remove(arm, "^chr")),
      # Extract just the type ('loss' or 'gain')
      type = stringr::str_extract(arm_call, "(loss|gain)")
    )
  
  # 3. Pivot wider to get loss and gain side-by-side for each arm
  arm_calls_merged <- arm_calls_long %>%
    tidyr::pivot_wider(
      id_cols = c(ID, arm),
      names_from = type,
      values_from = value,
      values_fill = 0L # Crucially, fill NA/missing calls with 0 (Normal)
    ) %>%
    
    # 4. Apply the custom merge logic
    dplyr::mutate(
      merged_call = dplyr::case_when(
        loss == 1 & gain == 0 ~ -2L, # Loss only
        loss == 1 & gain == 1 ~ -1L, # Both (Complex/Conflicting)
        loss == 0 & gain == 1 ~ 1L,  # Gain only
        TRUE ~ 0L                    # Neither (Normal)
      )
    )
  
  # 5. Pivot back to the final wide matrix
  merged_arm_matrix <- arm_calls_merged %>%
    tidyr::pivot_wider(
      id_cols = ID,
      names_from = arm,
      values_from = merged_call,
      values_fill = 0L # Fills any entirely unrepresented arms with 0
    ) %>%
    tibble::column_to_rownames("ID") %>%
    as.matrix()
  
  # 6. Apply mixed-sort for proper ordering
  # Sort rows (Sample IDs)
  message("Sorting sample ID rows...")
  merged_arm_matrix <- merged_arm_matrix[gtools::mixedsort(rownames(merged_arm_matrix)), , drop = FALSE]
  
  # Sort columns (Chromosome Arms)
  message("Sorting chromosome arm columns...")
  merged_arm_matrix <- merged_arm_matrix[, gtools::mixedsort(colnames(merged_arm_matrix)), drop = FALSE]
  
  message("🎉 Merge complete. Result matrix (Code: -2=Loss, -1=Both, 0=Normal, 1=Gain).")
  
  return(merged_arm_matrix)
}


#' Get binary CNA matrix for a particular loss and gain cutoff
#' @param loss_cutoff negative numeric
#' @param gain_cutoff positive numeric
#' @aggregate logical weather to aggregate loss and gain calls
#' @return A matrix of binary CNA calls
#' 
#' @export
add_bulk_cna_calls <- function(
    cna_calls_mat,  
    bulk_cna_calls = NULL,
    aggregate_mat = FALSE
){
  
  if(aggregate_mat) cna_calls_mat <- aggregate_binary_cna(cna_calls_mat)
  
  if(!is.null(bulk_cna_calls)){
    
    if (!all(colnames(bulk_cna_calls) %in% colnames(cna_calls_mat))) {
      stop("All column names of bulk_cna_calls should match column name of mat")
    }
    
    # MAKE IT MORE ROBUST BY INCLUDING MERGE_BY AND REMOVING T()
    cna_calls_mat <- cna_calls_mat %>%
      t() %>% # chr to rows and sample to columns
      data.frame(check.names = F) %>%
      merge(
        data.frame(t(bulk_cna_calls), check.names = F),
        by = 0,
        all = TRUE
      ) %>%
      dplyr::arrange(order(mixedorder(Row.names))) %>% # order rows
      dplyr::select(mixedsort(names(.))) %>% # order columns
      data.frame(row.names = "Row.names", check.names = F) %>%
      as.matrix()
  }
  
  # replace NA with 0
  cna_calls_mat[is.na(cna_calls_mat)] = 0
  
  cna_calls_mat
  
}


#' Plot CNA calls as a heatmap
#'
#' Expected CNA states in `mat`:
#'   -2 = Loss, -1 = Both, 0 = Normal, 1 = Gain
#'
#' @param mat Numeric matrix (or data frame) with values in {-2, -1, 0, 1, NA}.
#' @param metadata_df Optional data frame with one row per column in `mat`.
#' @param annotation_colors Optional named list of annotation color mappings,
#'   one element per metadata column. For categorical annotations, supply a
#'   named vector; for numeric annotations, a function is allowed.
#' @param cna_colors Named vector of colors for CNA states -2, -1, 0, 1.
#' @param column_split Either:
#'   (1) a character vector of one or more metadata column names in `metadata_df`,
#'   or (2) a vector of length `ncol(mat)` used directly to split heatmap columns.
#'   If more than one metadata column name is supplied, one heatmap is generated
#'   per split column.
#' @return A drawn ComplexHeatmap object, or a list of drawn heatmaps when
#'   multiple `column_split` metadata columns are supplied.
cna_heatmap <- function(
    mat,
    metadata_df = NULL,
    annotation_colors = NULL,
    cna_colors = c(`-2` = "blue2", `-1` = "purple2", `0` = "grey", `1` = "red2"),
    name = "CNA",
    cluster_columns = TRUE,
    cluster_rows = FALSE,
    column_split = NULL,
    show_column_dend = FALSE,
    show_row_dend = FALSE,
    show_column_names = FALSE,
    border = TRUE,
    border_gp = grid::gpar(col = "black"),
    gap = grid::unit(0.1, "mm"),
    rect_gp = grid::gpar(col = "white"),
    column_names_gp = grid::gpar(fontsize = 8),
    show_heatmap_legend = TRUE,
    width = grid::unit(500, "mm"),
    height = grid::unit(170, "mm"),
    heatmap_title = NULL,
    filename = NULL,
    ...
) {
  
  mat <- as.matrix(mat)
  storage.mode(mat) <- "numeric"
  
  allowed <- c(-2, -1, 0, 1)
  bad_vals <- setdiff(unique(stats::na.omit(as.vector(mat))), allowed)
  if (length(bad_vals) > 0) {
    stop(
      "`mat` may only contain -2, -1, 0, 1, or NA. Invalid values: ",
      paste(bad_vals, collapse = ", ")
    )
  }
  
  
  if (is.null(names(cna_colors)) || !all(as.character(allowed) %in% names(cna_colors))) {
    stop("`cna_colors` must be a named vector with names: -2, -1, 0, 1.")
  }
  
  
  make_annotation_col <- local({
    palette_pool <- c(pals::cols25(25), pals::alphabet(26), pals::alphabet2(26), scales::hue_pal(l = 65, c = 100)(100))
    next_idx <- 1L
    
    function(x, spec = NULL) {
      x_non_na <- stats::na.omit(x)
      
      if (is.numeric(x)) {
        brks <- unique(stats::quantile(
          x_non_na,
          probs = seq(0, 1, length.out = 11),
          names = FALSE
        ))
        if (length(brks) < 2) {
          rng <- range(x_non_na, na.rm = TRUE)
          brks <- if (diff(rng) == 0) c(rng[1] - 1, rng[2] + 1) else rng
        }
        cols <- grDevices::hcl.colors(length(brks), "Viridis")
        return(circlize::colorRamp2(brks, cols))
      }
      
      lev <- unique(as.character(x_non_na))
      
      if (!is.null(spec)) {
        if (is.function(spec)) return(spec)
        if (is.vector(spec) && !is.null(names(spec)) && all(lev %in% names(spec))) {
          return(spec[lev])
        }
      }
      
      n <- length(lev)
      if (next_idx + n - 1L > length(palette_pool)) {
        stop("Not enough annotation colors available. Supply `annotation_colors` for some columns.")
      }
      
      cols <- palette_pool[next_idx:(next_idx + n - 1L)]
      next_idx <<- next_idx + n
      stats::setNames(cols, lev)
    }
  })
  
  top_annotation <- NULL
  
  if (!is.null(metadata_df)) {
    metadata_df <- as.data.frame(metadata_df, check.names = FALSE)
    
    metadata_df <- metadata_df[match(colnames(mat), rownames(metadata_df)), ]
    
    if (nrow(metadata_df) != ncol(mat)) {
      stop("`metadata_df` must have exactly one row per column in `mat`.")
    }
    
    if (!is.null(rownames(metadata_df)) && !is.null(colnames(mat)) &&
        all(colnames(mat) %in% rownames(metadata_df))) {
      metadata_df <- metadata_df[colnames(mat), , drop = FALSE]
    }
    
    annotation_columns <- names(metadata_df)
    
    if (!is.null(column_split)) {
      if (is.character(column_split) && length(column_split) == 1) {
        annotation_columns <- setdiff(names(metadata_df), column_split)
        column_split <- metadata_df[[column_split]]
      }
    }
    
    anno_cols <- lapply(annotation_columns, function(nm) { 
      spec <- if (!is.null(annotation_colors)) annotation_colors else NULL 
      make_annotation_col(metadata_df[[nm]], spec = spec) 
    }) 
    names(anno_cols) <- annotation_columns
    
    top_annotation <- ComplexHeatmap::HeatmapAnnotation(
      df = metadata_df[,annotation_columns],
      col = anno_cols,
      na_col = "black",
      show_annotation_name = TRUE,
      border = TRUE,
      gp = grid::gpar(col = "white")
    )
  }
  
  
  set.seed(123)
  h <- ComplexHeatmap::Heatmap(
    mat = mat,
    name = name,
    col = cna_colors,
    cluster_columns = cluster_columns,
    cluster_rows = cluster_rows,
    column_split = column_split,
    show_column_dend = show_column_dend,
    show_row_dend = show_row_dend,
    show_column_names = show_column_names,
    column_names_gp = column_names_gp,
    top_annotation = top_annotation,
    border = border,
    border_gp = border_gp,
    gap = gap,
    rect_gp = rect_gp,
    show_heatmap_legend = show_heatmap_legend,
    heatmap_legend_param = list(
      at = allowed,
      labels = c("Loss", "Both", "Normal", "Gain"),
      title = name
    ),
    width = width,
    height = height
  )
  
  if (!is.null(filename)) {
    grDevices::pdf(filename, width = 8 * 4, height = 6 * 2)
    on.exit(grDevices::dev.off(), add = TRUE)
    
    set.seed(123)
    ComplexHeatmap::draw(
      h,
      column_title = heatmap_title,
      merge_legend = TRUE
    )
  }
  
  return(h)
}


# Tumor Purity ------------------------------------------------------------

get_purebeta_purities_mat <- function(purebeta_purities, out_path = NULL) {
  purity_list <- purebeta_purities
  names(purity_list) <- sapply(purity_list, function(x) x[["reference"]])
  
  purity_df <- rbindlist(lapply(purity_list, function(x) {
    mat <- x$`Estimated_1-Purities`
    purity_cols <- grep("purity|bound", colnames(mat))
    
    mat[, purity_cols] <- sapply(
      mat[, purity_cols, drop = FALSE],
      as.numeric
    )
    
    mat[["purity"]] <- 1 - mat[["estimate_1-purity"]]
    
    mat$reference <- x[["reference"]]
    mat
  })) %>% as.data.frame()
  
  colnames(purity_df)[1] <- "sample"
  colnames(purity_df)[3] <- "estimate_1.purity"
  
  if (!is.null(out_path)) data.table::fwrite(purity_df, out_path, row.names = TRUE)
  
  purity_df
}


#' Plot purity and no of cpg used from purebeta estimated purities targets
#' 
#' @param purity_list purity list obtained from purebeta_purities_targets
#' @param out_path file path to save the plot pdf
#' @return out_path
#' 
purebeta_plots <- function(purity_list, out_path){
  
  names(purity_list) <- sapply(purity_list, function(x) x[["reference"]])
  
  purity_df <- as.data.frame(lapply(purity_list, function(x){
    x$`Estimated_1-Purities`
  }))
  
  
  purity_mat <- sapply(purity_df[, grep("purity|bound", colnames(purity_df))],
                       as.numeric)
  purity_mat <- 1 - purity_mat
  rownames(purity_mat) <- purity_df[[1]]
  colnames(purity_mat)[grep("purity", colnames(purity_mat))] <- 
    paste0(names(purity_list),".purity")
  
  
  no_of_cpg_used = sapply(purity_list,
                          function(x){
                            lengths(x$Used_CpGs)
                          })
  
  no_of_cpg_used_melt = reshape2::melt(no_of_cpg_used)
  
  
  h_data <- t(purity_mat)[grep("purity", colnames(purity_mat)), ]
  set.seed(123)
  h1 <- ComplexHeatmap::Heatmap(
    h_data,
    col = colorRamp2(c(0,0.5,1),c("blue","white","red")),
    name = "Tumor\nPurity",
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    height = unit(7*nrow(h_data),"mm"),
    width = if(ncol(h_data) > 50) NULL else unit(7*ncol(h_data),"mm")
  )
  
  
  h_data <- t(purity_mat)
  set.seed(123)
  h2 <- ComplexHeatmap::Heatmap(
    h_data,
    col = colorRamp2(c(0,0.5,1),c("blue","white","red")),
    name = "Tumor\nPurity",
    show_row_dend = FALSE,
    show_column_dend = FALSE,
    height = unit(7*nrow(h_data),"mm"),
    width = if(ncol(h_data) > 50) NULL else unit(7*ncol(h_data),"mm")
  )
  
  
  p1 <- ggplot(no_of_cpg_used_melt,
               aes(x = Var2, y = value)) + 
    geom_violin(aes(fill = Var2)) + 
    geom_boxplot(width = 0.2) + 
    scale_x_discrete("") + 
    scale_y_continuous("Number of cpgs used") + 
    theme_bw() + 
    theme(axis.title = element_text(size = 24),
          axis.text = element_text(size = 20),
          legend.position = "none",
          panel.widths = unit(100,"mm"),
          panel.heights = unit(100,"mm"))
  
  pdf(out_path,
      width = 8*2, height = 6*2)
  
  set.seed(123)
  draw(h1)
  set.seed(123)
  draw(h2)
  print(p1)
  
  dev.off()
  
  invisible(list(h1,h1,p1))
}


#' Gives an average purity values across the provided reference
#' or all reference if NULL
get_tumor_purities <- function(
    purebeta_purities_mat,
    reference = NULL,
    out_path = NULL
) {
  df <- as.data.frame(purebeta_purities_mat, check.names = FALSE)
  
  required_cols <- c("sample", "purity", "reference")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  if (!is.null(reference)) {
    df <- df[df$reference %in% reference, , drop = FALSE]
    if (nrow(df) == 0) {
      stop("No rows matched the provided reference.")
    }
  }
  
  out <- aggregate(
    purity ~ sample,
    data = df,
    FUN = mean,
    na.rm = TRUE
  )
  
  colnames(out)[colnames(out) == "purity"] <- "tumor_purity"
  
  out <- out[gtools::mixedorder(out$sample), ]
  
  if (!is.null(out_path)) {
    data.table::fwrite(out, out_path)
  }
  
  out
}

