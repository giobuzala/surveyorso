# **Generate a thematic code list using the OpenAI API**

This function analyzes open-ended survey responses and automatically
generates a set of thematic codes with descriptions. Additional custom
instructions can be supplied through the `instructions` argument.

## Usage

``` r
theme_gpt(
  data,
  x,
  n = NULL,
  sample = NULL,
  model = "gpt-4o",
  instructions = NULL
)
```

## Arguments

- data:

  A data frame containing the survey data.

- x:

  The open-ended variable to analyze.

- n:

  Integer; number of themes to return. If `NULL` (default), the model
  determines an appropriate number of themes based on the responses.

- sample:

  Optional integer specifying the number of responses to sample for
  analysis. If `NULL`, all valid responses are used.

- model:

  Character string; the OpenAI model to use. Defaults to `gpt-4o`.

- instructions:

  Optional string; additional instructions for coding.

## Value

A table containing the generated thematic code list and their
description. Standard codes (`Other`, `None`, `Don't know`) are included
automatically.

## Details

Requires an OpenAI API key, which can be generated at
`https://platform.openai.com/`, to be set in your R session using
`Sys.setenv(OPENAI_API_KEY="...")`.

The output is a tibble with three columns:

- `Code`: A unique numeric code for each theme (standard codes 97–99 are
  added automatically).

- `Bin`: Short label for the theme, written in sentence case.

- `Description`: A one-sentence summary describing the theme's content.

Standard codes are included automatically:

- `97` = `Other`

- `98` = `None`

- `99` = `Don't know`

Use this function to create a `theme_list` for input into
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md),
or copy and paste it into an Excel coding workbook.

**Note:** It’s best to review and refine the generated codes before
using them in
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md).

## Examples

``` r
# Generate a theme list for Q5
theme_list <- theme_gpt(data = survey_data, x = Q5)
#> Error: These packages are required but not installed: dplyr, stringr
```
