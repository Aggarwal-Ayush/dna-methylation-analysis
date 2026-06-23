# DNA methylation pipeline

## One-command run

```bash
Rscript run_pipeline.R \
  --sample_sheet data/processed/sample_sheet.csv \
  --project_dir . \
  --output_dir output \
  --run_name dnam_analysis \
  --sample_id_col Sample_ID \
  --sample_sex_col Sex \
  --ncores 8
```

## Optional flags

`--mask_threshold`  
Default: `1`

`--collapse_betas`  
Default: `FALSE`

`--run_classifier`  
Default: `FALSE`

`--platform`  
Default: `EPICv2`

`--seed`  
Default: `123`

## Sample sheet requirements

At minimum, the sample sheet needs a `Basename` column that points to the IDAT basenames.

If `Sample_ID` is not present, it will be created from `Basename`.

If `Sex` is not present, sex mismatch QC is skipped automatically.

## Output

All outputs are written under:

`output/<run_name>/`

with a manifest CSV listing the main artifacts.
