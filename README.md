# CSSC-2025

This repository contains code and documentation for predicting incident atrial fibrillation (AF) using electronic health records (EHR) and electrocardiogram (ECG) features. The project was developed as part of the 2025 SSC Poster Presentation, leveraging a synthetic cohort of ~100,000 patients without prior AF diagnosis.

🔍 Objective
To develop and evaluate predictive models that estimate the risk of new-onset atrial fibrillation using structured health data, aiding early detection and preventive care strategies.

🧠 Methods
We leveraged and compared two survival analysis techniques in R, including:

Survival Gradient Boosting (via gbm)

Random Survival Forests (via randomForestSRC)

🧪 Evaluation
Model performance was assessed using:

Concordance index (C-index)

Feature Importance Plots

Time-dependent AUC

Sensitivity and Specificity
