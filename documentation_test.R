# This script rebuilds the package documentation and reloads the package for local testing.
# It also provides quick access to all function help pages to verify that roxygen comments were rendered correctly.

devtools::document() # Rebuild roxygen2 documentation
devtools::load_all() # Reload the package locally for testing

#### Open help pages to verify documentation
## Tabulation functions ----
?tab
?print.tab_result
?tab_all
?tab_mr
?print.tab_mr_result
?tab_mr_all
?tab_grid
?print.tab_grid_result
?tab_grid_all
?tab_grid_t
?print.tab_grid_t_result
?tab_grid_t_all

## Tracking functions ----
?merge_waves

## Statistical functions ----
?standardize
?normalize
?one_hot_encode
?cor_matrix
?cor_heatmap

## Data quality functions ----
?strline
?lowqual_gpt

## Open-ended coding functions ----
## Miscellaneous functions ----
?combine
