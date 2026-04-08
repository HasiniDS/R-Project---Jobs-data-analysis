# Week 11 Data Science Jobs Analysis Project

## Key Findings

### Project Aim
This project uses the cleaned jobs dataset to complete the Week 11 brief through one hypothesis test, one linear regression model, one decision tree classifier, one k-means clustering analysis, supporting figures, and clear audience-focused interpretation.

### Dataset Overview
The analysis used 648 cleaned job postings and 28 variables from `Jobs Clean.csv`. The largest job group was `data scientist` with 436 postings, so the class distribution was clearly imbalanced.
The `seniority` field was mostly recorded as `na`, so it was described but not used as a main modelling target. Ratings of 0 were retained because they represent unrated companies rather than missing values.

### Hypothesis Test
Average salary differed between the same-state groups. Postings in the same state as headquarters were 7.44 thousand USD lower on average, with a small effect size (Cohen's d = -0.19).

### Linear Regression
The linear regression model was weak overall (R-squared = 0.028, adjusted R-squared = 0.006; model p = 0.211), so it should be treated as descriptive rather than strongly predictive.

### Classification Tree
The decision tree reached 0.775 test accuracy, only slightly above the majority-class baseline of 0.757. The weakest recall was for data engineer, so class imbalance remained a clear limitation.
avg_salary is included here because the classifier is describing observed differences between completed job postings. A cleaner deployment-style alternative would remove avg_salary if job type had to be predicted before salary information is available.

### K-Means Clustering
The final k-means solution used k = 2. Cluster 2 had the higher mean salary at 125.0 thousand USD, but the clearer separation came from differences in spark and hadoop.
k = 2 was retained because it had the strongest silhouette width, the elbow curve started to flatten after two clusters, and the two-cluster solution remained easiest to interpret as a broad skill-mix split.
The clustering result should still be treated as exploratory rather than definitive.

### Limitations
- `job_simp` was highly imbalanced, which affected the classification results and limited the reliability of smaller classes.
- `company_age` required median imputation to keep the modelling sample stable.
- `seniority` was too sparse to support reliable modelling.
- The regression remained weak overall, so it should not be treated as a strong predictive model.
- The clustering solution is an exploratory grouping rather than proof of fixed job categories.

### Conclusion
The project gives a useful, well-supported summary of broad salary and role patterns in the jobs dataset, but its modelling results should be used cautiously rather than as final predictions.
