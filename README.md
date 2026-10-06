# DNA methylation pipeline

## One-command run

```bash
Rscript run_pipeline.R \
  --config dnam_config.yml
```


## Configuration parameters

The pipeline is configured through `dnam_config.yml`. Paths may be specified relative to `project_dir`.

### Project and input paths

- **`project_dir`**: Root directory of the analysis project. Use `"."` when running from the project root.
- **`sample_sheet`**: Path to the sample metadata file. It must contain a `Basename` column pointing to IDAT basenames.
- **`results_dir`**: Directory where pipeline results are written.
- **`run_name`**: Name of the analysis run. Results are organized under `<results_dir>/<run_name>/`.

### Sample sheet columns

- **`sample_id_col`**: Column containing unique sample identifiers. The supplied default is `"Sample_ID"`. If absent, the pipeline can derive IDs from `Basename`.
- **`sample_sex_col`**: Column containing reported sample sex. The supplied default is `"Sex"` and it is used for sex-mismatch QC. If absent, sex-mismatch QC is skipped.

### Differential methylation (optional)

- **`condition_col`**: Sample sheet column defining groups (default `"condition"`). If the column is absent, differential analysis is skipped.
- **`condition_reference`**: Baseline level; each other level is compared against it. `null` uses the first level alphabetically.
- **`condition_covariates`**: Optional list of additional sample sheet columns to adjust for (e.g. `["Sex"]`).

Probes are tested with limma on M-values (`run_differential_methylation()`), each group needs at least 2 samples, and samples with a blank condition are excluded. Results (`logFC` on the M-value scale, `delta_beta`, `P.Value`, `adj.P.Val`) are written to `<run_name>__differential-methylation.csv`, with one file per contrast when there are more than two groups.

A PDF, `<run_name>__differential-methylation_heatmaps.pdf`, is also written with three ComplexHeatmap pages per comparison (using only the two compared groups): the top 10,000 probes per condition by `logFC` (largest logFC in each direction), probes with `P.Value <= 0.05`, and probes with `adj.P.Val <= 0.05`. Columns are annotated and split by condition; rows are annotated and split by the condition with higher methylation (sign of `logFC`). Row clustering is skipped (rows ordered by P-value) for heatmaps with more than 20,000 probes. The top-N cap is the `top_n` argument of `plot_differential_heatmaps()`.

### Execution

- **`ncores`**: Number of CPU cores available to parallelized steps. Set this according to the workstation or HPC allocation.
- **`seed`**: Random seed for reproducible operations.

### DNA methylation preprocessing

- **`mask_threshold`**: Controls probe masking when beta values are calculated. The threshold represents the minimum fraction of samples in which a probe must remain unmasked to be retained in the final beta-value matrix. Lower values are less stringent and retain more probes, while higher values require probes to pass masking criteria in a larger fraction of samples. A value of 1 requires a probe to be unmasked in all samples.
- **`collapse_betas`**: Logical (`true`/`false`) controlling whether collapsed beta values are written.
- **`run_classifier`**: Logical (`true`/`false`) controlling whether the methylation classifier is run.

### Array platform and normal controls

- **`platform`**: DNA methylation array platform. Set to `null` when platform selection should be left unspecified for downstream helper logic; otherwise supply the platform identifier expected by those functions.
- **`use_default_normal`**: Logical (true/false) controlling whether normal control samples provided through the sesameData Bioconductor package are used for copy-number analysis. For samples profiled using the Illumina Infinium MethylationEPIC BeadChip (EPIC) or Illumina Infinium MethylationEPIC v2.0 BeadChip (EPICv2), the sesameData EPIC (EH6841 - EPIC.5.SigDF.normal) normal controls are used. For samples profiled using the Illumina Infinium HumanMethylation450 BeadChip (HM450/450K), the sesameData HM450 (HM450.10.TCGA.BLCA.normal and HM450.10.TCGA.PAAD.normal) normal controls are used.
- **`custom_normal`**: Optional custom normal/control dataset for copy-number analysis. Custom normal IDAT files can first be processed through this same DNA methylation pipeline to generate the required processed normal data. The resulting normal dataset can then be supplied here as the custom control input. Set custom_normal: null when no custom normal dataset should be used.

### CNA calling

- **`chr_length_cut`**: Chromosome-length cutoff used by the CNA workflow. The supplied configuration uses `5`.
- **`segment_cut_loss`**: CNA segment threshold for losses. The supplied configuration uses `-0.25`.
- **`segment_cut_gain`**: CNA segment threshold for gains. The supplied configuration uses `0.15`.

Use the density plot in the generated CNA report to assess whether the gain and loss thresholds are appropriate for a particular dataset.

## Example configuration

```yaml
project_dir: "."
sample_sheet: "data/sample_sheet_test.csv"
results_dir: "results"
run_name: "test"

sample_id_col: "Sample_ID"
sample_sex_col: "Sex"

ncores: 1
seed: 123

mask_threshold: 1
collapse_betas: true
run_classifier: true

platform: null
use_default_normal: true
custom_normal: null

chr_length_cut: 5
segment_cut_loss: -0.25
segment_cut_gain: 0.15
```

## Sample sheet requirements

At minimum, the sample sheet needs a `Basename` column that points to the IDAT basenames.

If `Sample_ID` is not present, it will be created from `Basename`.

If `Sex` is not present, sex mismatch QC is skipped automatically.

## Output

All outputs are written under:

`results/<run_name>/`

with a manifest CSV listing the main artifacts.
