# **One-hot encode categorical variables**

This function converts one or more categorical variables in a data frame
into one-hot encoded variables.

## Usage

``` r
one_hot_encode(data, vars, keep_all = TRUE)
```

## Arguments

- data:

  A data frame containing the variables to encode.

- vars:

  Variables to one-hot encode.

- keep_all:

  Logical; if `TRUE`, returns full dataset; if `FALSE`, returns only
  one-hot encoded variables.

## Value

A data frame with one-hot encoded variables.

- If `keep_all = TRUE`, the full dataset is returned with one-hot
  encoded variables replacing originals.

- If `keep_all = FALSE`, only one-hot encoded variables are returned.

## Details

- Binary factors labeled `Unchecked`/`Checked` are encoded as a single
  0–1 variable.

- Multi-category factors are expanded into multiple one-hot encoded
  variables (one per level).

- Original variable labels are preserved and extended to the generated
  one-hot encoded variables.

## Examples

``` r
# One-hot encode region and Q1_1 while keeping all original variables
data <- one_hot_encode(data = data, c(region, Q1_1))
#> Error: These packages are required but not installed: dplyr, tidyselect, stringr

# One-hot encode region and Q1_1, returning only the encoded columns
data_new <- one_hot_encode(data = data, c(region, Q1_1), keep_all = FALSE)
#> Error: These packages are required but not installed: dplyr, tidyselect, stringr
```
