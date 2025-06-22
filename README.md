# EDA-using-Shiny-App

## 0. Dashboard
https://deokjaeshiny.shinyapps.io/final_project/

To check the SHINY App dashboard, please visit the link above.

## 1. Introduction

This report presents an analysis of a GPA dataset containing information from 2,000 students across seven columns. The study was conducted by 덕재, 요셉, and 정웅. The primary research questions addressed in this analysis are:

1. **How can students live a happy life without stress?**
2. **How can students achieve high grades?**

The following sections detail the dataset description, research methodology, analysis findings, and conclusions.

## 2. Dataset Description

The dataset includes the following variables:
- **Study_Hours_Per_Day:** Ranges between 5 and 10 hours with a mean of approximately 7.4 hours. Given the high average study time, it is presumed that the dataset represents high school students rather than college students.
- **Extracurricular_Hours_Per_Day:** Interpreted as the time spent on hobbies. This variable ranges from 0 to 4 hours with an average of 2 hours.
- **Additional Variables:** The dataset also contains columns related to sleep, social activities, physical activity, GPA, and stress.

### Data Quality Considerations
- **Physical Activity:** The data shows an average of over 4 hours of daily physical activity and a maximum value of 13 hours. These values are implausible (even for professional athletes) and have been flagged as invalid; hence, physical activity is excluded from further analysis.
- **Stress Levels:** The stress variable is not evenly distributed. To address this, stress levels were initially categorized into binary classes (“High” vs. “Low Moderate”). However, further analysis shows that the low-stress group does not correlate well with study time and sleep time, posing limitations on interpreting these results.

## 3. Research Questions and Methodology

### 3.1 Reducing Stress

To explore the first research question—how to reduce stress—a logistic regression analysis was conducted using various predictors:

- **Study Hours:** The logistic regression model indicates that the probability of experiencing high stress increases as study hours increase. This result was confirmed by both the logistic curve and the LOESS curve.
- **Sleep Hours:** Analysis reveals that lower sleep hours are associated with a higher probability of stress.
- **Social and Extracurricular Hours:** Logistic regression and density plots suggest that neither social activity nor extracurricular activity are significant predictors of stress levels when considered individually. However, density plots for social activities, which include three stress-level groups (high, moderate, and low), indicate that increased social activity might be associated with lower stress.

#### Findings on Stress Reduction
- **Key Predictors:** Increased study hours and reduced sleep are strongly linked to higher stress.
- **Social Activity:** While not statistically significant in regression analysis, density plots hint that engaging in social activities could have a mitigating effect on stress.
- **Caution:** Given the disconnect observed in stress data—especially within the low-stress group—the results should be generalized with caution.

### 3.2 Achieving High GPA

The second research question focuses on how students can achieve high grades. The following analytical approaches were undertaken:

- **Simple Linear Regression:** Initial regression models using GPA as the response variable show that study hours have a significant linear relationship with GPA. Other predictors (sleep, social, and extracurricular hours) did not show a significant impact.
- **Multiple Regression Analysis:** Although additional predictors were considered in various combinations using R, none outperformed the simple regression model that solely uses study hours.
- **Interaction Effects:** A scatter plot of study hours versus GPA grouped by stress levels showed a consistent trend across different stress groups, indicating no significant interaction between study hours and stress on GPA.

#### In-Depth Analysis on High Study Hours
- **Focused Subset:** The analysis further focused on the top 25% of students in terms of study hours (500 students).
- **Sleep Patterns:** Within this group, density plots of GPA distributions based on sleep quantiles were compared. Students with longer sleep durations (indicated by a yellow distribution) tended to show a slightly higher density toward higher GPAs compared to those with shorter sleep durations (indicated by a purple distribution). Although the differences might not be statistically significant, the findings suggest that excessive reduction in sleep is not beneficial.
- **Social and Extracurricular Activities:** Further examination of GPA distribution by social and extracurricular hours revealed no significant relationship. Variability in GPA did not correspond to changes in the amount of time spent on these activities.

## 4. Conclusions

Based on the analysis of the dataset, the following conclusions can be drawn:

- **Stress Reduction:**  
  - **Main Findings:** Higher study hours and reduced sleep are clearly associated with increased stress.  
  - **Recommendations:** To reduce stress, it is advisable for students to balance their study schedules by studying less, getting more sleep, and engaging in social interactions.
  - **Caveat:** The stress data exhibits disconnects (particularly within the low-stress group), so these findings should be interpreted with caution.

- **Achieving High Grades:**  
  - **Primary Predictor:** Study hours are the dominant predictor of GPA.  
  - **Sleep Consideration:** Although studying is crucial, securing sufficient sleep (ideally 6–8 hours) may contribute to more stable and improved academic performance.  
  - **Other Factors:** Social and extracurricular activities do not show a significant direct impact on GPA, suggesting that the primary focus for academic achievement should be on effective study habits coupled with adequate sleep.

## 5. Final Remarks

The analysis confirms the intuitive notion that diligent study is critical to academic success while also emphasizing the importance of adequate sleep for stress reduction and consistent academic performance. Despite some data quality issues (especially regarding the physical activity and stress variables), these findings provide actionable insights for students aiming to achieve both well-being and academic excellence.
