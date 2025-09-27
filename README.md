# github

# DATA2x02 Survey Explorer (Shiny App)

This repository contains the code and documentation for the **DATA2x02 Survey Explorer**, a Shiny web application built in R for exploring and analyzing the DATA2x02 class survey dataset.

The app allows users to visualize missingness, explore one-variable distributions, and perform statistical hypothesis testing interactively.

## Live Demo

-   **ShinyApps.io deployment**: [<https://ivychen08.shinyapps.io/MyShinyAPP/>]\
-   **GitHub Repository**: [<https://github.com/Ivy-chen08/githubshinyapp>]

------------------------------------------------------------------------

## Features

### 1. Data Visualization

**A. Missingness overview**:

1) Top-10 missing variables table.\
2) Missingness matrix.

**B. One-variable distributions**:

1) Histograms for numeric variables.\
2) Bar plots for categorical variables.\

**C. Bedtime demo**:

1) Specialized parsing of bedtime strings.\
2) Visualization as a circular histogram.

### 2. Statistical Tests

**A. Two-sample t-test**

1) Welch or Student’s t automatically chosen.\
2) Flexible visualization styles (violin/boxplot/density/mean±CI).\
3) Optional raw points overlay.\
4) Displays group means, SDs, effect size (Cohen’s d).

**B. Chi-square: Independence**

1) Automatically chooses Pearson, Fisher, or Monte Carlo test.\
2) Outputs contingency table with expected counts & standardized residuals.\
3) Visualizations: stacked proportions + residual heatmap.

**C. Chi-square: Goodness of Fit**

1) Tests observed category proportions against uniform expectation.\
2) Displays observed vs expected counts.\
3) Residual bar chart and summary table.

## Usage

Run locally in RStudio:

\`\`\`r

shiny::runApp("app.R")

Upload your own .xlsx or .csv dataset, oruse the bundled cleaned_survey.xlsx (checked by default).

## Dependencies

install.packages(c("shiny", "bslib", "thematic", "readxl", "readr", "dplyr", "tidyr", "stringr", "forcats", "janitor", "ggplot2", "gt", "visdat", "hms", "rlang"))

## Capabilities

Supports multiple visualization styles for flexible exploration.

Automates statistical decisions (Welch vs t-test, Pearson vs Fisher/Monte Carlo).

Provides both numerical results and visual outputs for easier interpretation.

Uses a pre-cleaned dataset to reduce noise and increase reliability.

Deployed on shinyapps.io for direct web access.

## **Limitations**

Only supports t-tests and chi-square tests.

No automatic diagnostics for normality or variance homogeneity, assumptions noted only in text.

Optimized for small/medium datasets, large datasets may slow down rendering.

GitHub commit history is compressed because the repo was connected after development was mostly complete.
