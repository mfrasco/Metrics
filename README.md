# Metrics

![Build Status](https://travis-ci.org/mfrasco/Metrics.png)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/Metrics)](https://cran.r-project.org/package=Metrics)
[![Downloads](http://cranlogs.r-pkg.org/badges/Metrics)](https://cran.rstudio.com/web/packages/Metrics/)

## How to Install this Package

This package is distributed from CRAN. From the R prompt, run `install.packages("Metrics")`.

## Metrics Repo

This repository contains the code for the **Metrics** package in R. **Metrics** was created by Ben Hamner, and its original home is located [at this github repo](https://github.com/benhamner/Metrics/). That repo contains packages for common machine learning metrics in several programming languages, not just R. However, on 2017-04-21, CRAN orphaned the R package. In an effort to revive the status of the R package, I cloned the original and created this repo. Since then, I have added new metrics, improved documentation, and fixed bugs. This repository will be the home of active development on the **Metrics** R package moving forward.

## Community Feedback

If you notice anything wrong with the **Metrics** package or have any ideas on how to improve it, please create an issue in this github repository that describes your issue. I also welcome improvements to this package via a pull request. This is a simple R package, which makes it perfect for first time open source contributors. [Here is a guide](https://opensource.guide/how-to-contribute/) that walks you through how to make an open source contribution.

## What Metrics are Included in this Package?

All functions in the **Metrics** package take at least two arguments: `actual` and `predicted`. In the table below, I abbreviate `actual` as $x$ and `predicted` as $y$ for the sake of mathematical brevity.

| Metric Type | Metric Name | Function Name | Formula |
| ---- | ------------------------ | ---- | ------------------------------- |
| regression | Squared Error | se | ![equation](https://latex.codecogs.com/gif.latex?%28x_i%20-%20y_i%29%20%5E%202) |
| regression | Mean Squared Error | mse | $\frac{1}{n} \sum_{i=1}^n (x_i - y_i) ^ 2$ |
| regression | Root Mean Squared Error | rmse | $\sqrt{\frac{1}{n} \sum_{i=1}^n (x_i - y_i)^ 2}$ |
| regression | Absolute Error | ae | $\lvert x_i - y_i \rvert$ |
| regression | Mean Absolute Error | mae | $\frac{1}{n} \sum_{i=1}^n \lvert x_i - y_i \rvert$ |
| regression | Absolute Percent Error | ape | $\frac{\lvert x_i - y_i \rvert}{x_i}$  |
| regression | Mean Absolute Percent Error | mape | $\frac{1}{n} \sum_{i=1}^n \frac{\lvert x_i - y_i \rvert}{x_i}$ |
| regression | Symmetric Mean Absolute Percent Error | smape | $\frac{2}{n} \sum_{i=1}^n  \frac{\lvert x_i - y_i \rvert}{\lvert x_i \rvert + \lvert y_i \rvert}$ |
| regression | Squared Log Error | sle | $\big(\ln(1 + x_i) - \ln(1 + y_i)\big) ^ 2$ |
| regression | Mean Squared Log Error | msle | $\frac{1}{n} \sum_{i=1}^n \big(\ln(1 + x_i) - \ln(1 + y_i)\big) ^ 2$ |
| regression | Root Mean Squared Log Error | rmsle | $\sqrt{\frac{1}{n} \sum_{i=1}^n \big(\ln(1 + x_i) - \ln(1 + y_i)\big) ^ 2}$ |
| regression | Relative Squared Error | rse | $\Big[\sum_{i=1}^n (x_i - y_i) ^ 2\Big] / \Big[\sum_{i=1}^n (x_i - \bar{x}) ^ 2\Big]$ |
| regression | Root Relative Squared Error | rse | $\sqrt{\Big[\sum_{i=1}^n (x_i - y_i) ^ 2\Big] / \Big[\sum_{i=1}^n (x_i - \bar{x}) ^ 2\Big]}$ |
| regression | Relative Absolute Error | rse | $\Big[\sum_{i=1}^n \lvert x_i - y_i \rvert\Big] / \Big[\sum_{i=1}^n \lvert x_i - \bar{x} \rvert\Big]$ |
| time series |  Mean Absolute Scaled Error | mase | $\Big[\sum_{i=1}^n \lvert x_i - y_i \rvert\Big] / \Big[\frac{n}{n - m} \sum_{i=1+m}^n \lvert x_i - x_{i-m} \rvert \Big]$|
| classification | Classification Error | ce | $\frac{1}{n} \sum_{i=1}^n I(x_i \neq y_i)$ |
| classification | Accuracy | accuracy | $\frac{1}{n} \sum_{i=1}^n I(x_i = y_i)$ |
| classification | F1 Score | f1 | $\frac{2 * \text{precision} * \text{recall}}{\text{precision} + \text{recall}}$ |
| binary classification | Area Under ROC Curve | auc | $\int_0^1 [1 - G_1(G^{-1}_0(1 - v))] dv$. `help(auc)` for details. |
| binary classification | Log Loss | ll | $x_i * \ln(y_i) + (1 - x_i) * \ln(1 - y_i)$ |
| binary classification | Mean Log Loss | logloss | $\frac{1}{n} \sum_{i=1}^n x_i * \ln(y_i) + (1 - x_i) * \ln(1 - y_i)$ |
