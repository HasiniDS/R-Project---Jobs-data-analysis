# Week 11 Data Science Jobs Analysis Project

## Project Overview
This project completes the Week 11 jobs mini-project using the cleaned data science jobs dataset. It brings together descriptive analysis, one hypothesis test, one linear regression model, one decision tree classification model, and one k-means clustering analysis in a single organised workflow.

The project is designed to be presentation-ready and easy to review. The scripts run in order, save the main tables and figures automatically, and finish with a short summary script that prints the key findings, cautions, and suggested slide order.

## Data Files
- `Data/Raw/Tabular_DS_Jobs.csv`
- `Data/Clean/Jobs Clean.csv`

## Script Order
- `Scripts/00 Run Entire Project.R`
- `Scripts/01 Data Quality Review.R`
- `Scripts/02 Descriptive Figures.R`
- `Scripts/03 Hypothesis Test Salary by Location Match.R`
- `Scripts/04 Linear Regression Average Salary.R`
- `Scripts/05 Classification Tree Job Category.R`
- `Scripts/06 K Means Clustering Job Profiles.R`
- `Scripts/Summary.R`

## Outputs
Main outputs are saved in:
- `Outputs/Tables`
- `Outputs/Figures`

The project produces:
- data quality and descriptive summary tables
- regression, classification, and clustering result tables
- presentation-ready analytical figures

## How to Run
1. Open the `Project Week 11` folder in R or RStudio.
2. Run `Scripts/00 Run Entire Project.R`.
3. The runner checks required files, clears old outputs, runs all scripts in order, and confirms that the expected tables and figures were created.

## Main Analytical Sections
- Hypothesis test: Welch t-test comparing average salary by `same_state`
- Linear regression: average salary model using company and skill variables
- Classification: decision tree for the main job groups
- Clustering: k-means clustering with elbow, silhouette, and interpretation

## Interpretation Notes
- The salary difference by `same_state` is statistically detectable but small.
- The linear regression fit is weak and should be treated as descriptive.
- The decision tree is affected by class imbalance, especially for smaller groups.
- The k-means result is exploratory rather than a fixed truth.

## Presentation Use
The figures in `Outputs/Figures` and the summary objects printed by `Scripts/Summary.R` are the main materials to use when building presentation slides.
