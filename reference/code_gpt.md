# **Code open-ended survey responses using the OpenAI API**

This function submits open-ended survey responses to the OpenAI API and
returns a dataset with the original responses, assigned codes, and bins
from a provided code list.

- If the `theme_list` argument includes a `Description` column, its text
  is incorporated as additional coding instructions.

- Additional custom instructions can also be supplied through the
  `instructions` argument.

## Usage

``` r
code_gpt(
  data,
  x,
  theme_list,
  id_var,
  n = NULL,
  batch_size = 100,
  model = "gpt-4o",
  instructions = NULL
)
```

## Arguments

- data:

  A data frame containing the survey data.

- x:

  The open-ended variable to be coded.

- theme_list:

  A data frame with at least two columns: `Code` and `Bin`.

- id_var:

  The respondent ID variable.

- n:

  Optional integer; number of responses to code. Defaults to all rows.
  Useful for testing coding quality without coding every response.

- batch_size:

  Integer; number of responses per API call. Defaults to `100`.

- model:

  Character string; the OpenAI model to use. Defaults to `gpt-4o`.

- instructions:

  Optional string; additional instructions for coding.

## Value

A table with respondent IDs, responses, codes, and bins.

## Details

Requires an OpenAI API key, which can be generated at
`https://platform.openai.com/`, to be set in your R session using
`Sys.setenv(OPENAI_API_KEY="...")`.

Use
[`code_gpt_to_df()`](https://giobuzala.github.io/surveyorso/reference/code_gpt_to_df.md)
to update the dataset with results or
[`code_gpt_to_excel()`](https://giobuzala.github.io/surveyorso/reference/code_gpt_to_excel.md)
to export coded responses to Excel.

## Examples

``` r
# Code Q5
Q5_coded <- code_gpt(data = survey_data, x = Q5, theme_list = theme_list, id_var = id_var, model = "gpt-4o")
#> Error: These packages are required but not installed: dplyr, stringr
```
