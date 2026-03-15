# **Process `code_gpt()` output to the dataset**

This function takes a
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
result table and merges binary `Unchecked`/`Checked` code variables back
into the original survey dataset, preserving labels and metadata.

## Usage

``` r
code_gpt_to_df(coded_tbl)
```

## Arguments

- coded_tbl:

  A dataset returned by
  [`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
  with coding results.

## Value

The original dataset specified in
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
with factor-coded variables (levels: `Unchecked`, `Checked`) appended
for each code.

## Details

Use this function immediately after
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
in a pipeline, and save the result as a dataset.

For example:

    data <- code_gpt(data = survey_data, x = Q5, theme_list = theme_list) 
      code_gpt_to_df()

This will append binary `coded_` variables for each code in the list.

## Examples

``` r
# Code Q5 and add to the dataset
data <- code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%
  code_gpt_to_df()
#> Error in code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%     code_gpt_to_df(): could not find function "%>%"
```
