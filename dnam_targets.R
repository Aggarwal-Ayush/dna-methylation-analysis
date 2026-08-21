# DNA methylation pipeline -------------------------------------------------
# Configuration is read from environment variables. See run_pipeline.R.

message("\n\n********* RUNNING MAIN PIPELINE *********\n\n")

source_first_existing <- function(paths, local = TRUE) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("Could not find any of these files: ", paste(paths, collapse = ", "), call. = FALSE)
  }
  source(hit, local = local)
}

source_first_existing(c("dnam_functions.R", "R/dnam_functions.R"), local = FALSE)
source_first_existing(c("dnam_requirements.R", "R/dnam_requirements.R"), local = FALSE)

cfg <- readRDS(".pipeline_config.rds")



analysis_dir <- file.path(cfg$results_dir, cfg$run_name)
dir.create(analysis_dir, recursive = TRUE, showWarnings = FALSE)

get_filepath <- function(fname) {
  file.path(analysis_dir, paste0(cfg$run_name, "__", fname))
}


tar_option_set(
  packages = str_remove(required_packages$values, ".*/"), # from requirements.R
  format = "qs", # This automatically uses qs2/qs for intermediate objects
  memory = "transient",
  garbage_collection = TRUE,
  error = "abridge",
  seed = cfg$seed
)


# * SESAME ------------------------------------------------------------------

sesame_targets <- list(
  # Sample sheet
  tar_file_read(
    sample_sheet_raw,
    cfg$sample_sheet,
    read_dynamic_file(file_path = !!.x) %>% data.frame(check.names = F)
  ),
  
  tar_target(
    sample_sheet,
    normalize_sample_sheet(sample_sheet_raw, cfg$sample_id_col, cfg$sample_sex_col)
  ),
  
  # idats to ssets
  tar_target(
    dnam_sigdf,
    run_sesame_pipeline(
      sample_sheet = sample_sheet,
      ncores = cfg$ncores,
      sample_id_col = cfg$sample_id_col,
      out_path = get_filepath("sigdfs.qs2")
    )
  ),
  
  # beta values
  tar_target(
    dnam_betas_standard, 
    process_betas(
      dnam_sigdf, 
      mask_threshold = 1, 
      ncores = cfg$ncores, 
      collapse = FALSE,
      out_path = get_filepath("betas_standard.csv")
    )
  ),
  
  tar_target(
    dnam_betas_masked,
    if (cfg$mask_threshold < 1) {
      process_betas(
        dnam_sigdf, 
        mask_threshold = cfg$mask_threshold, 
        ncores = cfg$ncores, 
        collapse = FALSE,
        out_path = get_filepath("betas_masked.csv")
      )
    } else {
      NULL
    }
  ),
  
  tar_target(
    dnam_betas_collapsed, 
    betasCollapseToPfx(
      dnam_betas_standard,
      BPPARAM = BiocParallel::MulticoreParam(max(1L, min(cfg$ncores, parallelly::availableCores())))
    )
  ),
  
  tar_target(
    dnam_betas_collapsed_file, 
    if (cfg$collapse_betas) {
      save_dynamic_file(dnam_betas_collapsed, get_filepath("betas_collapsed.csv"),
                        overwrite = TRUE)
    } else {
      NULL
    }, format = "file"
  ),
  
  # QC
  tar_target(
    dnam_sigdf_qc,
    get_sesame_qc_df(
      sigdf_list = dnam_sigdf,
      infer_sex = TRUE,
      qc_funs = c("detection", "numProbes", "intensity", "channel", "dyeBias", "betas"),
      out_csv = get_filepath("sesame-qc.csv")
    )
  ),
  
  tar_target(
    dnam_sigdf_qc_flagged,
    flag_sesame_qc_samples(
      qc_df = dnam_sigdf_qc,
      metadata_df = sample_sheet,
      sample_col = cfg$sample_id_col,
      metadata_sex_col = cfg$sample_sex_col,
      qc_sex_col = "infered_sex",
      detection_col = "frac_dt_mk",
      intensity_col = "mean_intensity_MU",
      dye_bias_col = "RGdistort",
      detection_cutoff = NULL,
      intensity_cutoff = NULL,
      dye_bias_cutoff = NULL,
      iqr_mult = 1.5,
      out_path = get_filepath("sesame-qc_flagged.csv")
    )
  ),
  
  tar_target(
    dnam_sigdf_qc_plots,
    make_sesame_qc_visuals(
      flag_df = dnam_sigdf_qc_flagged,
      out_pdf = get_filepath("sesame-qc_plots.pdf"),
      detection_col = "detection_rate",
      intensity_col = "intensity",
      dye_bias_col = "dye_bias",
      flag_col = "any_flag",
      reason_col = "flag_reason",
      sample_col = cfg$sample_id_col,
      bins = 30
    )
  ),
  
  # SNP heatmap
  tar_target(
    dnam_qc_snp_heatmap,
    {
      beta_vals <- dnam_betas_standard
      # SNP probes
      snp_idx <- grepl("^rs", rownames(beta_vals))
      betas_snp <- beta_vals[snp_idx, , drop = FALSE]
      
      # optional: keep most variable SNPs
      vars <- matrixStats::rowVars(betas_snp, na.rm = TRUE)
      betas_snp <- na.omit(betas_snp)
      
      pdf(get_filepath("sesame-qc_snp-heatmap.pdf"), width = 70, height = 6)
      
      set.seed(123)
      h <- ComplexHeatmap::draw(
        ComplexHeatmap::Heatmap(
          betas_snp,
          name = "Beta",
          cluster_rows = TRUE,
          cluster_columns = TRUE,
          show_row_names = FALSE,
          heatmap_width = grid::unit(min(ncol(betas_snp), 140),"cm"),
          show_row_dend = F,
          show_column_dend = F
        )
      )
      
      dev.off()
      
      h
    }
  )
)


control_targets <- list(
  tar_target(
    dnam_control_list,
    get_control_ssets(
      ssets = dnam_sigdf,
      platform = cfg$platform,
      use_default = cfg$use_default_normal,
      custom_ssets = cfg$custom_normal
    )
  )
)


# * CONUMEE 2 ---------------------------------------------------------------

conumee_targets <- list(
  # conumee cna
  tar_target(
    dnam_conumee2_cna_data,
    {
      data(detail_regions)
      dnam_control <- dnam_control_list[[1]]
      control_name <- attr(dnam_control, "control_name")
      run_conumee(
        ssets_tumor = dnam_sigdf,
        ssets_control = dnam_control,
        detail_regions = detail_regions,
        ncores = cfg$ncores,
        out_path = get_filepath(sprintf("conumee2_cna-data--%s.qs2",control_name))
      )
    },
    pattern = map(dnam_control_list),
    iteration = "list"
  ),
  
  tar_target(
    dnam_conumee2_segments,
    {
      cna_data <- dnam_conumee2_cna_data
      get_conumee_segments(
        cna_data = cna_data,
        csv_path = get_filepath(sprintf("conumee2_cna-segments--%s.csv", cna_data@name))
      )
    },
    pattern = map(dnam_conumee2_cna_data),
    iteration = "list"
  ),
  
  tar_target(
    dnam_conumee2_cna_plots,
    {
      cna_data <- dnam_conumee2_cna_data
      out_path <- get_filepath(sprintf("conumee2_cna-report-%s.pdf", cna_data@name))
      plot_cna_summary(cna_data, out_path)
      out_path
    },
    pattern = map(dnam_conumee2_cna_data),
    format = "file"
  ),
  
  tar_target(
    dnam_conumee2_cna_summaryplots,
    {
      cna_data <- dnam_conumee2_cna_data
      lt <- cfg$segment_cut_loss
      gt <- cfg$segment_cut_gain
      out_path <- get_filepath(
        sprintf("conumee2_summary-plot--%s-loss%s-gain%s.pdf", 
                cna_data@name, lt, gt)
      )
      pdf(out_path, width = 8*2, height = 6)
      CNV.summaryplot.mod(
        cna_data,
        threshold_gain = gt,
        threshold_loss = lt
      )
      dev.off()
      out_path
    },
    pattern = map(dnam_conumee2_cna_data),
    format = "file"
  )
)


# * SVM CLASSIFIER ----------------------------------------------------------

svm_targets <- list(
  if (cfg$run_classifier) {
    tar_target(
      dnam_groups,
      svm_classifier(
        beta_values = dnam_betas_collapsed,
        platform = cfg$platform,
        out_path = get_filepath("meningioma_classification.csv")
      )
    )
  } else {
    NULL
  }
)


# * COMBINED ----------------------------------------------------------------


dnam_targets <- c(
  sesame_targets,
  control_targets,
  conumee_targets,
  svm_targets
)

dnam_targets
