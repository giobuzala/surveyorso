# **Import a coding workbook and merge coded variable into the dataset**

This function imports a coded Excel workbook for a specific open-ended
question and merges processed coded variables back into the dataset.

## Usage

``` r
import_coding(data, x, path = "Data", id_var)
```

## Arguments

- data:

  A data frame containing at least the `id_var` and original question
  column.

- x:

  A variable to be imported (e.g., `Q5`).

- path:

  Directory containing the workbook. Defaults to `"Data"`.

- id_var:

  ID variable in the dataset.

## Value

A data frame merged with new coded columns. If unknown codes are
detected, the function prints a table of respondent IDs and problematic
codes and then stops with an error.

## Details

This function handles one question at a time.

## Examples

``` r
# Import coding workbook for Q5 and merge the results into the dataset
data <- import_coding(data = survey_data, x = Q5, path = "Data", id_var = id_var)
#> Error in import_coding(data = survey_data, x = Q5, path = "Data", id_var = id_var): could not find function "import_coding"
```
