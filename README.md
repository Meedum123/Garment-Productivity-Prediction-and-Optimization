# Garment-Productivity-Prediction-and-Optimization
# 📊 Garment Factory Productivity Prediction and Optimization

![Python](https://img.shields.io/badge/Python-3.8+-blue)
![R](https://img.shields.io/badge/R-Statistics-blue)
![ML](https://img.shields.io/badge/ML-scikit--learn-yellow)
![Status](https://img.shields.io/badge/Status-Completed-brightgreen)

---

## 📚 Overview

This project investigates productivity patterns in a garment manufacturing factory using statistical analysis, unsupervised learning, and predictive modeling. After uncovering strong clustering by department (Sewing vs Finishing), separate models were built for each to better understand performance dynamics and suggest department-specific improvements.

---

## 🎯 Objectives

- Analyze production-level data to uncover key productivity drivers  
- Apply unsupervised learning to detect natural segmentation in the data  
- Build regression and tree-based models to predict actual productivity  
- Compare global vs segmented modeling strategies (by department)  
- Provide actionable recommendations for performance optimization  

---

## 🧪 Project Highlights

- Applied **Factor Analysis for Mixed Data (FAMD)** to identify two potential clusters in mixed categorical-numeric data.  
- Validated the clusters using **K-Means** and **Cramér’s V**, confirming they represent **Sewing** and **Finishing** departments.  
- Built predictive models on both the **combined dataset** and **department-wise splits**:  
  - 📄 Full Dataset R² ≈ 49.7%  
  - ✂️ Sewing Department: **80.7% R²** (Random Forest)  
  - ✂️ Finishing Department: **26.8% R²** (XGBoost)  
- Delivered department-specific suggestions based on key variables like incentives, SMV, and target productivity.

---

## 🛠️ Tech Stack

- **Languages**: Python, R  
- **Statistical Methods**: Chi-Square Test, Cramér’s V, FAMD  
- **Machine Learning Models**: Ridge, Lasso, Elastic Net, Random Forest, XGBoost  
- **Clustering**: K-Means  
- **Libraries**: pandas, scikit-learn, seaborn, matplotlib, `prince` (FAMD)  
- **Visualization**: Seaborn, Matplotlib  

---

## 📈 Results Summary

| Model Scope           | Model Used     | R² Score |
|-----------------------|----------------|----------|
| Combined Dataset      | Random Forest  | 49.7%    |
| Sewing Department     | Random Forest  | 80.7%    |
| Finishing Department  | XGBoost        | 26.8%    |

---

## 💡 Recommendations

- **Sewing Department**: Introduce incentives tied to SMV and track daily target deviations more closely.  
- **Finishing Department**: Reassess workload balance and investigate non-linearity in productivity vs. time series indicators.
- Use department-specific models instead of generalized ones to ensure better accuracy and actionable insights.

## Contributors
- Samudika Wanasinghe
- Pasindu Gamage
