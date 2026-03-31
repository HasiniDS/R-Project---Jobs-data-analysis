# Jobs Analyse

## Assignment Context
This project finalises the Week 11 group activity described in the module email and the file titled `Week 11 BIG Task`.
It builds on the Week 5 jobs dataset work and is organised as a clear presentation-ready hand-in for a group presentation to the teaching team.
The analysis covers one hypothesis test, one linear regression model, one classification model, one clustering method, interpretation, audience-focused communication, and reflection.

## Project Title
Jobs Analyse: Week 11 modelling, interpretation, and communication using the cleaned data science jobs dataset.

## File Structure
```text
Project Week 11/
|-- Data/
|   |-- Raw/
|   |   `-- Data Science Jobs Dataset - Raw.csv
|   `-- Clean/
|       `-- Data Science Jobs Dataset - Clean.csv
|-- Scripts/
|   |-- 00 Run Entire Project.R
|   |-- 01 Data Quality Review.R
|   |-- 02 Descriptive Figures.R
|   |-- 03 Hypothesis Test - Salary by Location Match.R
|   |-- 04 Linear Regression - Average Salary.R
|   |-- 05 Classification Tree - Job Category.R
|   |-- 06 K Means Clustering - Job Profiles.R
|   `-- 07 Written Summary and Presentation Notes.R
|-- Outputs/
|   |-- Tables/
|   |-- Figures/
|   `-- Models/
|-- Presentation/
|   |-- Week 11 Key Findings and Interpretation.md
|   `-- Week 11 Presentation Speaking Notes.md
`-- README.md
```

## Required Packages
- dplyr
- ggplot2
- tidyr
- readr
- broom
- car
- rpart
- rpart.plot
- cluster
- forcats
- scales

## How to Run the Project
1. Open the `Project Week 11` folder in R or RStudio.
2. Run `Scripts/00 Run Entire Project.R` from top to bottom.
3. Check the `Outputs` and `Presentation` folders for the saved results.

## What a Lecturer Can Review Quickly
- `Presentation/Week 11 Key Findings and Interpretation.md` for the short analytical summary.
- `Presentation/Week 11 Presentation Speaking Notes.md` for the audience-specific speaking notes.
- `Outputs/Tables/` for the saved statistical results.
- `Outputs/Figures/` for the supporting visuals used in the presentation.

## Output Description
- `Outputs/Tables/` contains dataset checks, descriptive summaries, model tables, and clustering summaries.
- `Outputs/Figures/` contains the analytical visualisations used in the report and presentation.
- `Outputs/Models/` stores the saved regression, tree, and clustering models.
- `Presentation/` contains a key findings summary and presentation notes for different audiences.
