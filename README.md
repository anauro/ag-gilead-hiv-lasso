# Engagement in Care Before and After the COVID-19 Pandemic Among People with HIV
_A data science study conducted at Analysis Group in partnership with Gilead_

## 🔍 Overview
Regular and consistent care is critical for managing HIV symptoms and reducing transmission. In the U.S., engagement in care has historically been suboptimal and may have worsened due to the COVID-19 pandemic. In partnership with Gilead, our team at Analysis Group conducted a retrospective real-world evidence study to (1) quantify the pandemic's impact on retention in care among people with HIV and (2) identify risk factors associated with falling out of the care continuum.

## 📊 Analysis Details
This study had two key components:

### (1) Quantifying Drop-Off in Care
Patients were defined as retained in care if they had HIV-related provider visits, CD4/viral load tests, or treatment adherence within the observation window. We compared engagement in the year before vs. after COVID-19 using logistic regression.  

_See scripts 1.0–2.3 for details._
> *Note: Study population identification (SQL/SAS) is excluded from this repo.*

### (2) Identifying Predictors of Disengagement
Using the treatment adherence-based definition of engagement, we focused on patients who were engaged pre-pandemic and classified them based on post-COVID outcomes (retained in care vs. falling out of care). A LASSO model was used to predict disengagement and surface the most important contributing features.

_See scripts 3.0–4.1 for details._

## 🧠 Methods & Tools
- **Languages**: R
- **Techniques**: Logistic regression, LASSO (regularized regression), cross-validation  
- **Workflow**: Data cleaning → feature engineering → model tuning → interpretation & reporting

## 🎯 Outcomes
- Quantified statistically significant decrease in HIV care engagement post-COVID
- Identified key predictive features (e.g., comorbidities, visit history, demographic flags)
- Enabled actionable insights to support targeted clinical outreach and intervention

## 📂 Contents
- `scripts/`: Code
- `README.md`: Overview of the project

| Program | Description |
|--------|-------------|
| `1.0 Demographics` | Define study population demographics such as age, sex, and health plan type |
| `1.1 Drugs and Labs` | Identify relevant medications and lab tests |
| `1.2 HRU and Costs` | Identify all-cause and HIV-related inpatient, emergency, and outpatient visits |
| `1.3 Comorbidities` | Identify Elixhauser, Quan-CCI, and HIV-related comorbidities |
| `1.4 Produce Baseline` | Produce baseline summary statistics to link to Excel |
| `2.0 STR and MTR Adjustment` | Adjust overlapping pharmacy claims to calculate medication adherence |
| `2.1 Retention in Care` | Create retention in care variables using three definitions |
| `2.2 Produce Retention in Care` | Produce retention in care summary statistics to link to Excel |
| `2.3 Comparison of Retention in Care` | Run logistic regression to quantify drop-off |
| `3.0 Cohort Selection` | Identify cohort used for secondary analysis |
| `3.1 Flowchart` | Calculate patient counts for patient selection flow chart |
| `3.2 Produce Baseline Overall` | Produce baseline summary statistics for all patients to link to Excel |
| `3.3 Produce Baseline Cohorts` | Produce baseline summary statistics by cohort to link to Excel |
| `4.0 LASSO Model` | Train LASSO model to predict falling out of care and important features |
| `4.1 Forest Plot` | Create feature importance forest plot |

> 📌 This study is currently under peer review. Final paper will be linked when published.
