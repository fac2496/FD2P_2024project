# FD2P 2024 — Fast Food Nutrition App

An R Shiny app that lets you build a virtual fast food basket and tracks your nutritional intake against UK dietary guidelines, personalised by age and gender.

## Requirements

- R (≥ 4.0)
- Packages: `shiny`, `dplyr`, `shinyjs`

Install packages in R:
```r
install.packages(c("shiny", "dplyr", "shinyjs"))
```

## How to run

```r
shiny::runApp("code/app2.R")
```

Or open `code/app2.R` in RStudio and click **Run App**.

## Project structure

```
code/
  dataprep.R        # Data cleaning pipeline (run once to regenerate processed_data/)
  app2.R            # Main Shiny app
  app.R             # Earlier version
raw_data/           # Source CSVs (Kaggle fast food dataset + UK nutrition guidelines)
processed_data/     # Cleaned CSVs used by the app
```

## Data sources

- Fast food nutrition data: [Kaggle — Fast Food Nutrition Menu](https://www.kaggle.com/datasets/joebeachcapital/fast-food)
- Dietary guidelines: UK government recommended daily intake values