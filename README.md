# Engagement in Care Before and After the COVID-19 Pandemic Among People with HIV

## 🔍 Overview
Regular and consistent care is critical in managing symptoms and transmission in people with HIV. However, in the US, engagement in care is typically low in the US and may have been further exacerbated by the COVID-19 pandemic. As a result, Analysis Group, in partnership with Gilead, undertook a study to understand the impact of the COVID-19 pandemic among people with HIV and what factors may be associated with patients falling out of the care spectrum.

## 📊 Analysis Details
- (1) Quantifying Disengagement in Care: A patient was defined as engaged in care if they had HIV-related visits, CD4 count/HIV viral load tests, or treatment adherence during the period of interest. Engagement in care in the year pre vs post COVID-19 was compared using logistic regression. See Programs 1.0 - 2.3. Study population was identified using SAS/SQL (program not shown here). 
- (2) Features Associated with Disengagement: Using the treatment adherence definition of engagement in care, all patients who were engaged in care pre-pandemic were included in the analysis and post-COVID engagement was used to define the two cohorts: falling out of care and retained in care. A LASSO model was then used to predict patients falling out of care and feature importance was reported to identify predictors associated with disengagement. 

## 🧠 Methods
- Data cleaning and preparation
- Logistic regression for quantifying the impact of COVID-19
- Regularized regression (LASSO) for variable selection
- Model tuning using cross-validation
- Interpretation and visualization of key predictors

## 🎯 Outcomes
- Quantified decrease in likelihood of engagement in care
- Identified top risk factors contributing to drop-off in care
- Insights can be used to improve clinical interventions and target at-risk groups

## 📂 Contents
- `scripts/`: Cleaning, modeling, and visualization scripts
- `slides/`: Final deck summarizing findings and recommendations (PDF)
- `README.md`: Overview of the project
