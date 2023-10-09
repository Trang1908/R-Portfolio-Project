# Project Overview

In this project, we analyse the 'house-data.csv' dataset to gain valuable insights into the factors that influence house prices. These insights can prove valuable for real estate professionals, home buyers, and sellers.

# Project Steps

## Dataset Summaries

We start by providing numerical and graphical summaries of the dataset to get an initial understanding of its characteristics.
House Condition Classification

Next, we categorise houses based on their overall condition (OverallCond) into the following groups:

- Poor: If the overall condition is rated between 1 and 3.
- Average: If the overall condition falls between 4 and 6.
- Good: If the overall condition is rated between 7 and 10.
  
To predict the overall condition (OverallCond) of a house, we employ a **Multinomial Logistic Regression**. Additionally, we utilise the **Naive Bayes Classification** method, which yields improved predictions compared to **Multinomial Logistic Regression** within this dataset.

## House Price Prediction

We explore two methods for predicting house prices: **Random Forest Regression** and **Support Vector Machine (SVM)**. Our analysis reveals that the **Random Forest Regression** model with **Feature Selection** outperforms other models, exhibiting a lower **Mean Squared Error (MSE)** and higher variance when compared to the model without feature selection.

To estimate the test error associated with fitting models by **Random Forest** and **SVM**, we employ **Cross-validation** and **Bootstrap** techniques. Notably, **10-fold cross-validation** yields a lower **Root Mean Squared Error (RMSE)** compared to **Bootstrap**, with the **SVM**  model outperforming **Random Forest**.

In conclusion, for future house price predictions, we recommend fitting the **SVM** model with feature selection and employing **10-fold cross-validation** to estimate the test error effectively.

## Additional Investigation

As a final step, we explore **Principal Component Analysis (PCA)** to visualise the most important real features that influence the appraisal of a house's sale price.
