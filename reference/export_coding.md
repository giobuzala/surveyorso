# **Export a coding workbook**

This function creates an Excel-based coding workbook for open-ended
survey questions.

- To export a workbook for a single variable, set `x = var`.

- To export for multiple variables at once, provide a vector, e.g.,
  `x = c(var1, var2, var3)`.

- Optionally, you can include a `filter` variable that will appear in
  the workbook for easier sorting or filtering.

## Usage

``` r
export_coding(data, x, path = "Data", id_var, filter = NULL)
```

## Arguments

- data:

  A data frame containing the survey responses. Must include a
  respondent ID (`id_var`) column and the variables specified in `x`.

- x:

  A single variable or multiple variables specifying the open-ended
  question(s) to export.

- path:

  File path where the workbook(s) will be saved. Defaults to `"Data"`.

- id_var:

  ID variable in the dataset.

- filter:

  (Optional) A single variable used to group or filter responses in the
  exported workbook.

## Value

One or more Excel workbooks saved to the specified path.

## Details

Each workbook includes:

- A `Coding Workbook` sheet containing responses, along with columns for
  codes and bins.

- A `Codes` sheet pre-populated with standard codes (`97` = `Other`,
  `98` = `None`, `99` = `Don't know`) and space for custom codes.

The workbook also includes Excel formulas for bin lookups, counts, and
percentages, with conditional formatting to highlight invalid codes.

**NOTE:** The formula in cell D2 of the first sheet must be filled down
to the end of the column. Populating the formula programmatically in R
causes Excel to slow down.

## Examples

``` r
# Export coding workbook for Q5
export_coding(data = survey_data, x = Q5)
#> Error: These packages are required but not installed: dplyr, tidyselect, openxlsx

# Export separate coding workbooks for Q5 and Q6
export_coding(data = survey_data, x = c(Q5, Q6))
#> Error: These packages are required but not installed: dplyr, tidyselect, openxlsx
```
