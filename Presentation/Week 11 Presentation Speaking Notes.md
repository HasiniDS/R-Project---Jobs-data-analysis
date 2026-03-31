# Week 11 Presentation Notes

## 1. Technical Audience
**Opening**
This analysis used the cleaned data science jobs dataset to apply a Welch t-test, one linear regression model, one decision tree classifier, and one k-means clustering model.

**Key findings**
- The hypothesis test result was: Average salary differed between the same-state groups. Postings in the same state as headquarters were 7.44 thousand USD lower on average, with a small effect size (Cohen's d = -0.19).
- The regression explained 0.028 of the variance in `avg_salary`, so it should be treated as a weak descriptive model.
- The classification tree reached 0.775 test accuracy, only slightly above the majority-class baseline of 0.757.
- The clustering stage selected k = 2 using elbow and silhouette checks, and the strongest separation came from skill-mix differences rather than salary alone.

**What the models mean**
The test suggests whether salary differs across a simple geographic grouping. The regression shows conditional salary associations. The tree shows which variables help separate job types. The clustering result gives an exploratory structure for role patterns.

**Limitations**
- Seniority was too sparse for serious modelling use.
- `job_simp` imbalance affected the classifier.
- The regression left a large share of salary variation unexplained.
- Clustering should not be interpreted as proof of fixed job types.

**Recommendations**
- Add richer predictors such as experience level, location detail, and employer characteristics in future work.
- Consider class-balancing strategies or alternative classifiers if job-type prediction becomes the main objective.

## 2. Stakeholder Audience
**Opening**
This project looked at what the cleaned jobs data can tell us about salary patterns, job categories, and broad role groupings.

**Key findings**
- Average salary differed between the same-state groups. Postings in the same state as headquarters were 7.44 thousand USD lower on average, with a small effect size (Cohen's d = -0.19).
- The salary model was weak overall and explained only a small part of the picture (R-squared = 0.028).
- The job-type classifier was easiest for the largest class and less reliable for smaller role groups, even though overall accuracy was 0.775 and the majority-class baseline was 0.757.
- The final k-means solution used k = 2. The highest-paying cluster was Cluster 2 (mean salary 125.0 thousand USD), while Cluster 1 had the lowest mean salary at 123.2 thousand USD. The clearest differences between clusters were in spark and hadoop, which suggests an engineering-heavy cluster and a broader general cluster. This should still be treated as exploratory rather than definitive.

**What the models mean**
The results are useful for understanding broad patterns in the jobs market. They help compare role types and skills, but they are not detailed enough to make precise salary predictions for individual vacancies.

**Limitations**
- Some variables were missing or weakly populated.
- Smaller job categories were under-represented.
- The clustering output is best used as a planning aid rather than a final segmentation.

**Recommendations**
- Focus salary benchmarking on the larger and clearer job categories first.
- Improve future data collection for seniority and employer details.

## 3. Non-Technical Decision-Makers
**Opening**
The dataset was used to look for simple patterns in salary, job type, and skills across data science vacancies.

**Key findings**
- Average salary differed between the same-state groups. Postings in the same state as headquarters were 7.44 thousand USD lower on average, with a small effect size (Cohen's d = -0.19).
- The salary model was weak, so there is still a lot we cannot explain from this dataset alone.
- The job classifier worked better for common roles than uncommon ones, so it should not be treated as a perfect sorting tool.
- The cluster analysis grouped jobs into 2 broad patterns, mainly separating roles by skill mix and only slightly by salary.

**What the models mean**
These results are helpful for spotting trends. They are less suitable for making exact predictions about a single company or vacancy.

**Limitations**
- Some fields were incomplete.
- The dataset had many more data scientist roles than other job types.
- The clusters are exploratory groupings rather than fixed categories.

**Recommendations**
- Use the findings to guide discussion and planning, not as final evidence on their own.
- Collect more balanced and detailed data before making high-stakes decisions.
