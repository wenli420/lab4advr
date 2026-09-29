# Advanced R Lab4 Package

[![Version](https://img.shields.io/badge/version-0.1.0-blue)](https://github.com/wenli420/lab4advr)
[![License](https://img.shields.io/badge/license-MIT-green)](https://github.com/wenli420/lab4advr/blob/main/LICENSE)
[![R](https://img.shields.io/badge/R-package-blue)](https://www.r-project.org/)
[![R-CMD-check](https://github.com/wenli420/lab4advr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wenli420/lab4advr/actions/workflows/R-CMD-check.yaml)
[![GitLab Pipeline](https://img.shields.io/badge/GitLab-Pipeline-orange?logo=gitlab)](https://gitlab.liu.se/qiayu183/mylinreg/-/pipelines)

Implement a custom linear regression model in pure R.

## Version 0.1.0

Implement a custom linear regression model in pure R.

## Peer Repository

[Qianwen Yu's mylinreg repository](https://gitlab.liu.se/qiayu183/mylinreg)


## 1. Project Structure

```text
lab4advr/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── LICENSE
├── LICENSE.md
├── lab4advr.Rproj
├── R/
│   ├── linreg.R
│   ├── linreg_qr.R
│   └── linreg_method.R
├── man/
├── tests/
├── vignettes/
└── .github/
```

## 2. Purpose and Functionality

This R package is created for Advanced R Lab 4 assignment.

#### It implements a custom linear regression model in pure R:
#### 2.1 GitHub Actions continuous integration setup for automatic package testing
#### 2.2 S3 `linreg` formula class to store all regression-related outputs
#### 2.3 Custom S3 methods for model printing, summary and visualization
#### 2.4 QR-decomposition-based algorithm to compute regression coefficients
#### 2.5 Vignette introduction as user guide for demonstration
The `R/` directory contains three main implementation files:

- `linreg.R` – main linear regression model
- `linreg_qr.R` – QR decomposition and regression coefficient calculation
- `linreg_method.R` – S3 methods for the `linreg` class


## 3. Installation

```r
devtools::install()
```

## 4. Run Example

```r
library(lab4advr)

model <- linreg(
  Petal.Length ~ Species,
  data = iris
)

print(model)
summary(model)
plot(model)
```

## 5. Issues
1. Calculating t‑values and p‑values
2. The issue with custom generic functions
3. The NAMESPACE file must be updated
4. Using match.call() to store the original call
5. Using medians in plotting to reduce the impact of extreme values
6. Package must be installed locally before writing the introduction
7. S3 method compatibility issue
8. Incorrect dependency declaration
9. Missing documentation for ellipsis argument
