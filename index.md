# surveyorso

`surveyorso` is an R package providing a unified toolkit for end-to-end
survey research workflows.

It is designed for applied survey analysts who want fast, consistent,
and reproducible ways to clean data, manage metadata, tabulate results,
run advanced analytics, and prepare outputs for reporting, with built-in
support for SPSS-style metadata such as data types, variable labels, and
value levels.

## Installation

Install from GitHub using `remotes`:

``` r
install.packages("remotes")
remotes::install_github("giobuzala/surveyorso")
```

Alternatively, install using `pak`:

``` r
install.packages("pak")
pak::pak("giobuzala/surveyorso")
```

## Functions

### Tabulation

- Single response questions:
  [`tab()`](https://giobuzala.github.io/surveyorso/reference/tab.md),
  [`tab_all()`](https://giobuzala.github.io/surveyorso/reference/tab_all.md)
- Multiple response (pick-any) questions:
  [`tab_mr()`](https://giobuzala.github.io/surveyorso/reference/tab_mr.md),
  [`tab_mr_all()`](https://giobuzala.github.io/surveyorso/reference/tab_mr_all.md)
- Grid questions:
  [`tab_grid()`](https://giobuzala.github.io/surveyorso/reference/tab_grid.md),
  [`tab_grid_all()`](https://giobuzala.github.io/surveyorso/reference/tab_grid_all.md),
  [`tab_grid_t()`](https://giobuzala.github.io/surveyorso/reference/tab_grid_t.md),
  [`tab_grid_t_all()`](https://giobuzala.github.io/surveyorso/reference/tab_grid_t_all.md)

### Tracking

- Merging and stacking waves:
  [`merge_waves()`](https://giobuzala.github.io/surveyorso/reference/merge_waves.md)

### Statistical

- Standardization and normalization:
  [`standardize()`](https://giobuzala.github.io/surveyorso/reference/standardize.md),
  [`normalize()`](https://giobuzala.github.io/surveyorso/reference/normalize.md)
- Feature engineering:
  [`one_hot_encode()`](https://giobuzala.github.io/surveyorso/reference/one_hot_encode.md)
- Correlation tables:
  [`cor_matrix()`](https://giobuzala.github.io/surveyorso/reference/cor_matrix.md)
- Correlation heatmaps:
  [`cor_heatmap()`](https://giobuzala.github.io/surveyorso/reference/cor_heatmap.md)

### Segmentation

- K-means grid search:
  [`kmeans_grid()`](https://giobuzala.github.io/surveyorso/reference/kmeans_grid.md)

### Data quality

- Straightlining detection:
  [`strline()`](https://giobuzala.github.io/surveyorso/reference/strline.md)
- Low-quality open-ended response detection (API-based):
  [`lowqual_gpt()`](https://giobuzala.github.io/surveyorso/reference/lowqual_gpt.md)

### Open-ended coding

This package supports both:

- Excel workbooks for human coding
  ([`export_coding()`](https://giobuzala.github.io/surveyorso/reference/export_coding.md)
  →
  [`import_coding()`](https://giobuzala.github.io/surveyorso/reference/import_coding.md)),
  and
- AI-assisted workflows to generate themes and assign codes
  ([`theme_gpt()`](https://giobuzala.github.io/surveyorso/reference/theme_gpt.md)
  →
  [`code_gpt()`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md)).

To use the AI-assisted functions, set your OpenAI API key in the current
R session or store it in a local `.Renviron` file for persistent use.

``` r
# Set your OpenAI API key in the current R session
Sys.setenv(OPENAI_API_KEY="your_api_key")

# 1) Create a draft theme list
theme_list <- theme_gpt(survey_data, x = Q5)

# 2a) Code responses and append binary-coded variables directly to the dataset
survey_data_coded <- code_gpt(survey_data, x = Q5, theme_list = theme_list) %>%
  code_gpt_to_df()

# 2b) Or write codes into an Excel coding workbook created by export_coding() for human review
code_gpt(survey_data, x = Q5, theme_list = theme_list) %>%
  code_gpt_to_excel(path = "Data")
```

### Miscellaneous

- Combining variables:
  [`combine()`](https://giobuzala.github.io/surveyorso/reference/combine.md)

## Documentation

All functions are documented with examples using `roxygen2`. Browse help
pages such as
[`?tab`](https://giobuzala.github.io/surveyorso/reference/tab.md),
[`?standardize`](https://giobuzala.github.io/surveyorso/reference/standardize.md),
[`?strline`](https://giobuzala.github.io/surveyorso/reference/strline.md),
and
[`?code_gpt`](https://giobuzala.github.io/surveyorso/reference/code_gpt.md).
