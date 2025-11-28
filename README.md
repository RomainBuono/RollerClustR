<div align="center">

<img src="img/Logo_RollerClusteR.jpg" width="400px" alt="RollerClustR Logo"/>

# RollerClustR

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%203-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0)

### Variable Clustering for R

</div>

**RollerClustR** is an R package for clustering **variables** (not observations). It implements three complementary approaches for analyzing relationships between variables:

- **VAR_CAH**: Hierarchical Agglomerative Clustering
- **VAR_KMEANS**: K-means with Principal Components (Vigneau & Qannari algorithm)
- **TandemVarClust**: Tandem analysis (MCA + HAC) for mixed data

---

## Installation

```r
# Install from GitHub
# install.packages("devtools")
devtools::install_github("RomainBuono/RollerClustR")
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

### Example 2: K-means Variable Clustering (VAR_KMEANS)

```r
# K-means clustering with 1st principal components
model_km <- roller_clust(
  X = iris[, 1:4],
  method = "var_kmeans",
  K = 2,
  n_init = 20  # Multiple random initializations
)

model_km$summary()

# Access homogeneity (mean r²)
print(model_km$Homogeneite)
# [1] 0.7845
```

### Example 3: Illustrative Variables with `predict()`

```r
# Create new variables for prediction
new_vars <- data.frame(
  SumPetals = iris$Petal.Length + iris$Petal.Width,
  RatioPetals = iris$Petal.Length / iris$Petal.Width
)

# Predict their cluster membership
predictions <- model$predict(new_vars)

# View results
print(predictions$SumPetals$cluster)
# [1] 1

print(predictions$SumPetals$best_score)
# [1] 0.9682  # r² with cluster center
```

**Understanding predict() output:**
- `$cluster`: Assigned cluster for each new variable
- `$scores`: r² (squared correlation) with each cluster's center
- `$best_score`: Highest r² score
- `$second_best_score`: Second-best r² (for ambiguity assessment)

### Example 4: Categorical Data (TandemVarClust)

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
  X = data,              # Data (data.frame or matrix)
  method = "var_cah",    # Method: "var_cah", "var_kmeans", "tandem"
  K = 2,                 # Number of clusters
  scale = TRUE           # Standardization
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

# Quality metrics
model$Homogeneite         # Mean homogeneity (r² for VAR_KMEANS)
model$WithinClusterInertia  # Sum of r² (VAR_KMEANS) or inertia (VAR_CAH)
```

---

## Method Selection Guide

| Method | Data Type | Approach | Best For |
|--------|-----------|----------|----------|
| **VAR_CAH** | Numeric | Agglomerative | Initial exploration, dendrograms |
| **VAR_KMEANS** | Numeric | Iterative K-means | Optimized partitioning, maximizing r² |
| **TandemVarClust** | Mixed | MCA + HAC | Categorical or mixed variables |

### Algorithm Details

**VAR_CAH**: Hierarchical agglomerative clustering based on variable correlations. Builds a dendrogram bottom-up.

**VAR_KMEANS**: Implements Vigneau & Qannari algorithm using K-means with cluster centers represented by first principal components. Maximizes sum of squared correlations (r²).

**TandemVarClust**: Combines Multiple Correspondence Analysis (MCA) with Hierarchical Agglomerative Clustering (HAC) for categorical and mixed data.

---

## Complete Documentation

- **Full User Guide**: See `USERGUIDE.md`
- **Function Documentation**: `?roller_clust`, `?VAR_CAH`, `?VAR_KMEANS`, `?TandemVarClust`
- **Vignettes**: `browseVignettes("RollerClustR")`

---

## Citation

If you use **RollerClustR** in your research, please cite:

```
Romain Buono. (2025). RollerClustR: Variable Clustering in R.
R package version 1.0.0. https://github.com/RomainBuono/RollerClustR
```

**Key References**:

- Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012). ClustOfVar: An R Package for the Clustering of Variables. *Journal of Statistical Software*, 50(13), 1-16.

---

## License

GPL-3 © Romain Buono

---

## Support

- **Issues**: [GitHub Issues](https://github.com/RomainBuono/RollerClustR/issues)
- **Questions**: Open a discussion on GitHub
