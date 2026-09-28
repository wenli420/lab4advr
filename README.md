# Advanced R Lab4 Package

![Version](https://img.shields.io/badge/version-0.1.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)
![R](https://img.shields.io/badge/R-package-blue)

Implement a custom linear regression model in pure R.

## Version 0.1.0

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

## 3. License

![License](https://img.shields.io/badge/license-MIT-green)

This project is licensed under the MIT License.

See the [LICENSE](https://gitlab.liu.se/qiayu183/lab4advr/-/blob/main/LICENSE?ref_type=heads) file for full terms.

## 4. Pipeline Status

- [GitLab pipeline status](https://gitlab.liu.se/qiayu183/lab4advr/-/pipelines)

- Note: we also test GitHub Actions  
  [https://github.com/wenli420/lab4advr/actions](https://github.com/wenli420/lab4advr/actions)

## 5. Installation

```r
devtools::install()
```

## 6. Run Example

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

## 7. Issues

##### 1. Calculating t-values and p-values
##### 2. Custom generic functions
##### 3. The `NAMESPACE` file must be updated
##### 4. Store original function call
##### 5. Plot handling outliers
##### 6. Vignette local-install requirement
