# **Combine split variables into a single variable**

This function merges two variables that represent split versions of the
same question (e.g., `Q1_1` asked to one group and `Q1_2` to another)
into a single combined variable. The function fills in missing values
from one variable with non-missing values from the other, preserving
factor levels and SPSS-style attributes.

## Usage

``` r
combine(data, var_1, var_2, new_name = NULL, new_label = NULL)
```

## Arguments

- data:

  A data frame containing the variables to combine.

- var_1:

  First variable to merge.

- var_2:

  Second variable to merge.

- new_name:

  (Optional) Name for the new variable. If omitted, defaults to
  `var_1_var_2_COMB`.

- new_label:

  (Optional) SPSS-style variable label. If omitted, the label from
  `var_1` is copied.

## Value

A modified data frame with a new combined variable placed immediately
after `var_2`.

## Details

- If both variables have valid values for the same row, `var_1` takes
  precedence (a warning is issued).

- Factor levels are inherited from `var_1`.

- Existing variables with the same name are overwritten.

To apply this function across multiple pairs of split variables, you can
map it using the code block below:

    pairs <- list(
      list("Q1_1", "Q1_2"),
      list("Q2_1", "Q2_2")
    )

    dat <- reduce(
      pairs,
      function(d, vars) {
        combine(d, !!sym(vars[[1]]), !!sym(vars[[2]]))
      },
      .init = dat
    )

## Examples

``` r
# Combine Q1_1 and Q2_2
data <- combine(data = survey_data, var_1 = Q1_1, var_2 = Q1_2)
#> Error: These packages are required but not installed: dplyr
```
