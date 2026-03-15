# **Identify low-quality open-ended responses using the OpenAI API**

This function flags low-quality open-ended survey responses using the
OpenAI API. Responses are evaluated for signs of gibberish, nonsense,
random text, irrelevant words, or other indicators of poor data quality.

## Usage

``` r
lowqual_gpt(data, x, batch_size = 100, model = "gpt-4o")
```

## Arguments

- data:

  A data frame containing the survey data.

- x:

  The open-ended variable to evaluate for response quality.

- batch_size:

  Integer; number of responses per API call. Defaults to `100`.

- model:

  Character string; the OpenAI model to use. Defaults to `gpt-4o`.

## Value

A data frame with an additional variable named `x_lowqual` containing
the classification results.

## Details

Requires an OpenAI API key, which can be generated at
`https://platform.openai.com/`, to be set in your R session using
`Sys.setenv(OPENAI_API_KEY="...")`.

The model classifies each response as low-quality (`1`) or valid (`0`).

A response is flagged as low-quality if it is:

- Gibberish or random characters

- Off-topic or meaningless

- Contains only emojis or irrelevant text

A response is considered valid if it is interpretable, relevant, and
meaningful.

The function appends a new column to the dataset with the results.

## Examples

``` r
# Identify low-quality respondents in question Q5
data <- lowqual_gpt(data = survey_data, x = Q5)
#> Error: These packages are required but not installed: dplyr, stringr
```
