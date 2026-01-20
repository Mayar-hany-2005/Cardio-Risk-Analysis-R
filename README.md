# Cardio-Risk-Analysis-R
<p align="center">
  <img src="imagesheart_banner.png" width="100%" alt="Heart Disease Analysis Banner">
</p>

# 🩺 Heart Disease Diagnostic Analysis & Predictive Modeling

## 📌 Project Overview
This project presents a comprehensive clinical data analysis and machine learning framework designed to diagnose heart disease and assess its severity. Using the "Heart Disease UCI" dataset, I implemented a full data science pipeline in **R**—from advanced preprocessing and statistical testing to building robust predictive models.

The study combines demographic information with biometric measurements to investigate the most critical factors influencing heart disease risk across multiple global sources (Cleveland, Hungary, Switzerland, and Long Beach VA).

## 🚀 Key Features
* **End-to-End Pipeline:** Full data lifecycle including cleaning (handling missing values), feature transformation, and categorical encoding.
* **Advanced Visualizations:** Utilizing custom themes to create Ridge plots, Violin plots, and Correlation matrices for intuitive medical insights.
* **Statistical Validation:** Performed T-Tests, Chi-Square tests, and ANOVA to validate the significance of clinical markers like Age, Gender, and Chest Pain types.

## 📊 Critical Clinical Insights
* **The "Silent" Risk:** My analysis revealed that **53.6%** of patients with heart disease were **Asymptomatic**, highlighting the danger of "Silent Ischemia" where no pain is felt.
* **Top Predictors:** Clinical data confirms that **Chest Pain Type (cp)** and **ST Depression (oldpeak)** are the most powerful indicators of heart disease presence.
* **Age Vulnerability:** Risk prevalence rises significantly after age 60, with the highest concentration of diagnosed cases in the 55-65 age bracket.

## 🤖 Predictive Models & Performance
I evaluated three distinct algorithms to determine the most reliable diagnostic tool:
1.  **Random Forest (Top Performer):** Delivered the best predictive accuracy and sensitivity, crucial for minimizing missed diagnoses in clinical settings.
2.  **Logistic Regression:** Achieved a solid **81% accuracy** in binary classification (Healthy vs. Sick).
3.  **Decision Tree:** Provided high interpretability, offering clear rule-based logic that can be easily explained to medical professionals.

## 🛠️ Tech Stack & Libraries
- **Language:** R
- **Key Libraries:** `tidyverse`, `ggplot2`, `randomForest`, `rpart`, `corrplot`, `ggridges`, `patchwork`.

## 📁 Project Structure
- `scripts/`: Contains the full R script (`R Dr.hend.R`) for data cleaning, EDA, and modeling.
- `report/`: A detailed PDF report (`report mayar.pdf`) containing full clinical interpretations.
- `images/`: High-resolution plots and project banners.

---
**Developed by:** Mayar Hany  
**Field:** Data Science | Applied Multivariate Analysis
