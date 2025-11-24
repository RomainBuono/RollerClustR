# RollerClustR

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%203-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0)

**RollerClustR** is an R package for clustering **variables** (not observations). It implements three complementary approaches for analyzing relationships between variables:

- **VAR_CAH**: Hierarchical Agglomerative Clustering
- **VARCLUS**: Divisive clustering with PCA
- **TandemVarClust**: Tandem analysis (MCA + HAC) for mixed data

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("BryanMentos/RollerClustR")
```

---

## Quick Start

### Example 1: Hierarchical Clustering (VAR_CAH)

```r
library(RollerClustR)
data(iris)

# Create and fit model in one command
model <- roller_clust(
  X = iris[, 1:4],
  method = "var_cah",
  K = 2
)

# Display results
model$summary()

# Access variable clusters
print(model$Groupes)
# Sepal.Length Sepal.Width Petal.Length Petal.Width 
#            1           2            1           1
```

### Example 2: Illustrative Variables with `predict()`

```r
# Create new variables for prediction
new_vars <- data.frame(
  SumPetals = iris$Petal.Length + iris$Petal.Width,
  RatioPetals = iris$Petal.Length / iris$Petal.Width
)

# Predict their cluster membership
predictions <- model$predict(new_vars)

# View results
print(predictions$cluster)
# SumPetals RatioPetals 
#         1           1

print(predictions$scores)
#             Cluster_1 Cluster_2
# SumPetals      0.9845    0.3512
# RatioPetals    0.8923    0.2134
```

**Understanding predict() output:**
- `$cluster`: Assigned cluster for each new variable
- `$scores`: Correlation with each cluster's synthetic variable
- `$best_score`: Highest correlation score
- `$second_best_score`: Second-best correlation (for ambiguity assessment)

### Example 3: Categorical Data (TandemVarClust)

```r
# Create mixed data
iris_mixed <- iris
iris_mixed$Size <- cut(iris$Sepal.Length, breaks = 3, 
                       labels = c("Small", "Medium", "Large"))

# Tandem clustering
model_tandem <- roller_clust(
  X = iris_mixed[, c(1:4, 6)],
  method = "tandem",
  K = 3
)

model_tandem$summary()
```

---

## Core Features

### Unified Interface

The `roller_clust()` function provides a single entry point:

```r
model <- roller_clust(
  X = data,           # Data (data.frame or matrix)
  method = "var_cah", # Method: "var_cah", "varclus", "tandem"
  K = 2,              # Number of clusters
  scale = TRUE        # Standardization
)
```

### Essential Methods

```r
# Fit the model (already done by roller_clust)
model$fit(X)

# Detailed summary
model$summary()

# Prediction for illustrative variables
predictions <- model$predict(newdata)

# Change number of clusters (automatically refits)
model$K <- 3
```

### Active Properties

```r
# Number of clusters
model$K                    # Read
model$K <- 4              # Write (refits model)

# Variable groups
groups <- model$Groupes  # Named vector of assignments
```

---

## Method Selection Guide

| Method | Data Type | Approach | Best For |
|--------|-----------|----------|----------|
| **VAR_CAH** | Numeric | Agglomerative | Initial exploration, dendrograms |
| **VARCLUS** | Numeric | Divisive | Complex hierarchical structures |
| **TandemVarClust** | Mixed | MCA + HAC | Categorical or mixed variables |

---

## Complete Documentation

- **Full User Guide**: See `USERGUIDE.md`
- **Function Documentation**: `?roller_clust`, `?VAR_CAH`, `?VARCLUS`, `?TandemVarClust`
- **Vignettes**: `browseVignettes("RollerClustR")`

---

## Citation

If you use **RollerClustR** in your research, please cite:

```
Mentos Vendetta, B. (2025). RollerClustR: Variable Clustering in R.
R package version 1.0.0. https://github.com/BryanMentos/RollerClustR
```

---

## License

GPL-3 © Bryan Mentos Vendetta

---

## Support

- **Issues**: [GitHub Issues](https://github.com/BryanMentos/RollerClustR/issues)
- **Questions**: Open a discussion on GitHub
