# **Identify straightliners in grid questions**

This function flags respondents who selected the same response across
all items in a grid question.

## Usage

``` r
strline(data, x)
```

## Arguments

- data:

  A data frame containing the survey data.

- x:

  One or more variable prefixes identifying the grid question(s).

## Value

The original data frame with an additional logical column named
`prefix_strline`, indicating whether each respondent provided identical
responses across all items in the grid.

## Details

- The function automatically detects all variables that begin with the
  specified prefix followed by an underscore.

- It excludes multiple response sets (e.g., binary `0`/`1` or
  `Unchecked`/`Checked` grids).

## Examples

``` r
# Identify straightliners for one grid question
data <- strline(data = survey_data, x = Q1)
#> Error: These packages are required but not installed: dplyr, stringr

# Identify straightliners for multiple grid questions
data <- strline(data = survey_data, c(Q1, Q2))
#> Error: These packages are required but not installed: dplyr, stringr
```
