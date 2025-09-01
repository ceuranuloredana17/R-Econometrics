# 📊 Econometrics in R

This repository focuses on **statistical modeling, regression analysis, and hypothesis testing** using **R programming**.  
The goal is to **analyze and compare datasets** (e.g., farm data) with econometric models such as **Lasso Regression**, **Ridge Regression**, **Elastic Net**, and **Multiple Linear Regression**, while validating the underlying assumptions with rigorous statistical tests.

---

## 🚀 **Key Features**

- **Data Processing**
  - Handles missing values and prepares data for regression models.

- **Regression Models**
  - **Lasso Regression** (feature selection)  
  - **Ridge Regression** (regularization)  
  - **Elastic Net** (combination of L1 and L2 penalties)  
  - **Multiple Linear Regression** (baseline, interpretable model)  

- **Model Evaluation**
  - Root Mean Square Error (**RMSE**) for performance comparison.  
  - Results evaluated across farm sizes (**small, medium, large**).  

- **Diagnostic & Statistical Tests**
  - Multicollinearity → **Variance Inflation Factor (VIF)**  
  - Heteroscedasticity → **Breusch-Pagan, White Tests**  
  - Linearity checks  
  - Normality of residuals → **Jarque-Bera Test**  
  - Autocorrelation → **Durbin-Watson Test**  
  - Robust corrections → **Weighted Least Squares (WLS), Generalized Least Squares (GLS)**  

- **Model Comparison**
  - Residual comparison with **t-tests**  
  - Performance comparison with **ANOVA**  

---

## 🔧 **Requirements**

- R (≥ 4.0.0)  
- Required libraries:  

```R
install.packages(c("glmnet", "caret", "stats", "sandwich", "MASS", 
                   "lmtest", "tseries", "olsrr", "car", "nlme"))

📂 Project Structure
Econometrie/
│
├── Data/
│   └── Cleaned_Dairy_Dataset.csv      # Clean dataset for regression analysis
│
├── Scripts/
│   ├── process_farm_data.R            # Main script: preprocessing + model building
│   └── compare_farm_models.R          # Model comparison (ANOVA, t-tests, RMSE)
│
├── Results/
│   └── output_summary.txt             # Key results exported from R scripts
│
└── README.md                          # Documentation


