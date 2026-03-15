# **Write `code_gpt()` results to Excel workbook**

This function inserts the coded responses (`Code(s)` column) from a
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
result table into the corresponding Excel coding workbook created for
that question.

## Usage

``` r
code_gpt_to_excel(coded_tbl, path = "Data")
```

## Arguments

- coded_tbl:

  A dataset returned by
  [`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
  with coding results.

- path:

  Directory containing the coding workbook. Defaults to `"Data"`.

## Value

The Excel workbook is updated in place.

## Details

- The function locates the workbook named `X Coding Workbook.xlsx`,
  matches coded responses by respondent ID, and writes the codes into
  the `Code(s)` of the coding sheet.

- [`import_coding()`](https://giobuzala.github.io/surveyorso/reference/import_coding.md)
  function can be used to import and process the coding workbook.

Use this function immediately after
[`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)
in a pipeline to write the coded results directly to the corresponding
Excel workbook.

You do not need to save it as a dataset, as the workbook is updated in
place.

For example:

    code_gpt(data = survey_data, x = Q5, theme_list = theme_list) 
      code_gpt_to_excel()

This will update `Q5 Coding Workbook` in place with the coded responses,
which can then be imported using
[`import_coding()`](https://giobuzala.github.io/surveyorso/reference/import_coding.md).

## Examples

``` r
# Update "Q5 Coding Workbook" in place with the coded responses
code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%
  code_gpt_to_excel()
#> Error in code_gpt(data = survey_data, x = Q5, theme_list = theme_list) %>%     code_gpt_to_excel(): could not find function "%>%"
```
