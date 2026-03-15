# **Merge two survey waves**

This function merges a new survey wave into an old one while preserving
variable labels and allowing flexible control over which variables are
retained.

It provides two options for which variables to retain:

- `"New and shared old"` (default): keeps all variables from the new
  dataset, plus shared variables from the old dataset, and any
  additional old-only variables specified in `keep_old`.

- `"All"`: keeps all variables from both datasets.

Variable labels are reapplied after merging:

- For variables present in the new dataset, labels are taken from
  `new_data`.

- For old-only variables (when included), labels are taken from
  `old_data`.

## Usage

``` r
merge_waves(
  new_data,
  old_data,
  vars = c("New and shared old", "All"),
  keep_old = NULL
)
```

## Arguments

- new_data:

  A data frame for the new wave.

- old_data:

  A data frame for the old wave.

- vars:

  Character string, either `"New and shared old"` (default) or `"All"`.

- keep_old:

  (Optional) character vector of old-only variables to keep when
  `vars = "New and shared old"`.

## Value

A merged data frame with harmonized variable labels.

## Examples

``` r
# Merge new data and shared variables from old data
merged_data <- merge_waves(new_data = new_data, old_data = old_data, vars = "New and shared old")
#> Error: These packages are required but not installed: dplyr
```
