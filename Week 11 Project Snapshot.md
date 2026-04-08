# Week 11 Data Science Jobs Analysis Project

- Project aim: Complete the Week 11 brief with one hypothesis test, one regression model, one classification tree, one k-means clustering analysis, and clear communication.
- Dataset overview: 648 cleaned postings from `Jobs Clean.csv`, with a strongly imbalanced `job_simp` distribution led by `data scientist`.
- Hypothesis test: Average salary differed between the same-state groups. Postings in the same state as headquarters were 7.44 thousand USD lower on average, with a small effect size (Cohen's d = -0.19).
- Regression: The linear regression model was weak overall (R-squared = 0.028, adjusted R-squared = 0.006; model p = 0.211), so it should be treated as descriptive rather than strongly predictive.
- Classification: The decision tree reached 0.775 test accuracy, only slightly above the majority-class baseline of 0.757. The weakest recall was for data engineer, so class imbalance remained a clear limitation.
- Clustering: The final k-means solution used k = 2. Cluster 2 had the higher mean salary at 125.0 thousand USD, but the clearer separation came from differences in spark and hadoop.
- Biggest limitation: The largest limitation is the strong class imbalance in job_simp, combined with sparse seniority data and a weak salary regression fit.
- Final takeaway: The project gives a useful, well-supported summary of broad salary and role patterns in the jobs dataset, but its modelling results should be used cautiously rather than as final predictions.
