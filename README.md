# surveyorso

`surveyorso` is an R package providing a unified toolkit for end-to-end survey research workflows.

It is designed for applied survey analysts who want fast, consistent, and reproducible ways to clean data, manage metadata, tabulate results, run advanced analytics, and prepare outputs for reporting, with built-in support for SPSS-style metadata such as data types, variable labels, and value levels.

`surveyorso` is intentionally opinionated. While it reflects common survey research workflows, it may require customization for specific projects. The package is designed to be forked, extended, and adapted to fit different survey research environments.

## Installation

Install from GitHub:

```r
install.packages("remotes")
remotes::install_github("giobuzala/surveyorso")
```

Alternatively, using `pak`:

```r
install.packages("pak")
pak::pak("giobuzala/surveyorso")
```

## What’s included

### Tabulation

- **Single response**: `tab()`, `tab_all()`
- **Multiple response (pick-any)**: `tab_mr()`, `tab_mr_all()`
- **Grid questions**: `tab_grid()`, `tab_grid_all()`, `tab_grid_t()`, `tab_grid_t_all()`

### Tracking (longitudinal)

Use `merge_waves()` to merge a new wave into an old wave while preserving labels and giving you control over which variables to keep.

```r
# merged <- merge_waves(new_data = wave2, old_data = wave1, vars = "New and shared old")
```

### Data preparation utilities

- **Standardization / normalization**: `standardize()`, `normalize()`
- **Feature engineering**: `one_hot_encode()`
- **Convenience**: `combine()`

### Correlations and visualization

- **Correlation tables**: `cor_matrix()`
- **Correlation heatmaps**: `cor_heatmap()`

```r
num_data <- survey_data[, c("Q3_1", "Q3_2")]
cor_matrix(num_data, half = FALSE)
```

### Segmentation

Use `kmeans_grid()` to run a grid of k-means solutions (optionally including PCA variants) and compare fit metrics across configurations.

```r
# kmeans_grid(num_data, k_range = 2:4)
```

### Data quality checks

- **Straightlining in grids**: `strline()` (adds a `*_strline` flag per grid prefix)
- **Low-quality open-ended responses (API)**: `lowqual_gpt()` (adds a `*_lowqual` flag)

### Open-ended coding (Excel + API)

This package supports both:

- **Excel workbooks** for human coding (`export_coding()` → `import_coding()`), and
- **GPT-assisted workflows** to propose themes and assign codes (`theme_gpt()` / `code_gpt()`).

To use the GPT-assisted functions, set your API key in the current R session:

```r
Sys.setenv(OPENAI_API_KEY = "YOUR_KEY_HERE")

# 1) Create a draft theme list
theme_list <- theme_gpt(survey_data, x = Q5, sample = 200)

# 2a) Code responses and append binary coded variables back to the dataset
survey_data_coded <- code_gpt(survey_data, x = Q5, theme_list = theme_list) |>
  code_gpt_to_df()

# 2b) Or write codes into an Excel coding workbook created by export_coding()
code_gpt(survey_data, x = Q5, theme_list = theme_list) |>
  code_gpt_to_excel(path = "Data")
```

## Documentation

All functions are documented with examples using `roxygen2`. Browse help pages such as `?tab`, `?tab_mr`, `?tab_grid`, and `?merge_waves`.

## Getting help

- **Issues / requests**: please use the GitHub issue tracker at `https://github.com/giobuzala/surveyorso/issues`

## License

MIT License
