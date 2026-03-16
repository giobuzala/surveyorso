# surveyorso

`surveyorso` is an R package providing a unified toolkit for end-to-end survey research workflows.

It is designed for applied survey analysts who want fast, consistent, and reproducible ways to clean data, manage metadata, tabulate results, run advanced analytics, and prepare outputs for reporting, with built-in support for SPSS-style metadata such as data types, variable labels, and value levels.

## Installation

Install from GitHub using `remotes`:

```r
install.packages("remotes")
remotes::install_github("giobuzala/surveyorso")
```

Alternatively, install using `pak`:

```r
install.packages("pak")
pak::pak("giobuzala/surveyorso")
```

## Functions

### Tabulation

- **Single response questions**: `tab()`, `tab_all()`
- **Multiple response (pick-any) questions**: `tab_mr()`, `tab_mr_all()`
- **Grid questions**: `tab_grid()`, `tab_grid_all()`, `tab_grid_t()`, `tab_grid_t_all()`

### Tracking

- **Merging and stacking waves**: `merge_waves()`

### Statistical

- **Standardization and normalization**: `standardize()`, `normalize()`
- **Feature engineering**: `one_hot_encode()`
- **Correlation tables**: `cor_matrix()`
- **Correlation heatmaps**: `cor_heatmap()`

### Segmentation

- **K-means grid search**: `kmeans_grid()`

### Data quality

- **Straightlining detection**: `strline()`
- **Low-quality open-ended response detection (API-based)**: `lowqual_gpt()`

### Open-ended coding

This package supports both:

- **Excel workbooks** for human coding (`export_coding()` → `import_coding()`), and
- **AI-assisted workflows** to generate themes and assign codes (`theme_gpt()` → `code_gpt()`).

To use the AI-assisted functions, set your OpenAI API key in the current R session or store it in a local `.Renviron` file for persistent use.

```r
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

- **Combining variables**: `combine()`

## Documentation

All functions are documented with examples using `roxygen2`. Browse help pages such as `?tab`, `?standardize`, `?strline`, and `?code_gpt`.

## License

MIT License
