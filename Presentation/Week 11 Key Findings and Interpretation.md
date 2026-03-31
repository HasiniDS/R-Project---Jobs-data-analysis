# Week 11 Key Findings

## Project Aim
This project analysed the cleaned data science jobs dataset to test one hypothesis, fit one linear regression model, build one classification model, and explore job groupings with clustering.

## Dataset Overview
The analysis used 648 cleaned job postings with 28 variables. The largest job category was `data scientist` with 436 postings, which confirms that the dataset is imbalanced.
The `seniority` field was mostly recorded as `na`, so it was described but not used as a main modelling target. Company ratings of 0 were kept because they represent unrated firms rather than missing values.

## Main Hypothesis Test Finding
Average salary differed between the same-state groups. Postings in the same state as headquarters were 7.44 thousand USD lower on average, with a small effect size (Cohen's d = -0.19).

## Main Regression Finding
The linear regression model was weak overall (R-squared = 0.028, adjusted R-squared = 0.006; model p = 0.211). Only limited coefficient evidence emerged, with same-state postings being the clearest single signal. This model is best treated as a descriptive check rather than a strong explanatory model.

## Main Classification Finding
The decision tree reached an overall test accuracy of 0.775, which was only slightly above the majority-class baseline of 0.757. The most influential variables were company_age_imputed, rating, python, but the weakest recall was for `data engineer`, so the smaller classes remained difficult to separate.

## Main Clustering Finding
The final k-means solution used k = 2. The highest-paying cluster was Cluster 2 (mean salary 125.0 thousand USD), while Cluster 1 had the lowest mean salary at 123.2 thousand USD. The clearest differences between clusters were in spark and hadoop, which suggests an engineering-heavy cluster and a broader general cluster. This should still be treated as exploratory rather than definitive.

## Limitations
- `job_simp` was highly imbalanced, which affected both the classification results and the interpretation of grouped summaries.
- `company_age` included missing values, so a simple median imputation was used in the modelling stages to keep the sample size stable.
- `seniority` was too sparse to support reliable modelling.
- The regression model was descriptive rather than causal, and the clustering solution should be treated as exploratory.

## Conclusion
Across the project, salary patterns were related to job type and some skill indicators, but the dataset still contained substantial unexplained variation. The most defensible conclusion is that the models provide useful structure for understanding the jobs market, while their practical predictions should be treated with caution.
