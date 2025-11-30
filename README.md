<div align="center">
  
<img src="img/Logo_RollerClusteR.jpg" width="400px" alt="RollerClustR Logo"/>

# RollerClustR

[![R](https://img.shields.io/badge/R-%3E%3D%204.0-blue.svg)](https://www.r-project.org/)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%203-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0)

### Variable Clustering for R

</div>

**RollerClustR** is an R package for clustering **variables** (not observations). It implements three complementary approaches for analyzing relationships between variables:

- **VAR_CAH**: Hierarchical Agglomerative Clustering (HAC)
- **VAR_KMEANS**: K-means with Principal Components (Vigneau & Qannari algorithm)
- **TandemVarClust**: Tandem analysis (AFDM + HAC) for mixed data

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
set.seed(123)  # For reproducibility
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
set.seed(123)  # For reproducibility
model_km <- roller_clust(
  X = iris[, 1:4],
  method = "var_kmeans",
  K = 2,
  n_init = 20  # Multiple random initializations
)

model_km$summary()

# Access homogeneity (mean r²)
print(model_km$Homogeneite)
# [1] 0.9424
```

### Example 3: Illustrative Variables with `predict()`

```r
# Create new variables for prediction
set.seed(456)
new_vars <- data.frame(
  SumPetals = iris$Petal.Length + iris$Petal.Width,
  RatioPetals = iris$Petal.Length / (iris$Petal.Width + 0.1)
)

# Predict their cluster membership
predictions <- model_km$predict(new_vars)

# View results
print(predictions$SumPetals$cluster)
# [1] 1

print(predictions$SumPetals$best_score)
# [1] 0.9733  # r² with cluster center
```

**Understanding predict() output:**
- `$cluster`: Assigned cluster for each new variable
- `$scores`: r² (squared correlation) with each cluster's center
- `$best_score`: Highest r² score
- `$second_best_score`: Second-best r² (for ambiguity assessment)

### Example 4: Categorical Data (TandemVarClust)

```r
# Create mixed data
set.seed(789)
iris_mixed <- iris[, 1:4]
iris_mixed$Size <- cut(iris$Sepal.Length, breaks = 3, 
                       labels = c("Small", "Medium", "Large"))
iris_mixed$Species <- iris$Species

# Tandem clustering
model_tandem <- roller_clust(
  X = iris_mixed,
  method = "tandem",
  K = 3,
  n_bins = 5
)

model_tandem$summary()

# Predict illustrative variable (new approach with AFDM projection)
new_cat <- data.frame(
  Color = sample(c("Red", "Blue", "Green"), 150, replace = TRUE)
)

pred_result <- model_tandem$predict(new_cat)
print(pred_result$Color$cluster)  # Assigned cluster
print(pred_result$Color$distances)  # Distances to cluster centers
print(pred_result$Color$cramers_v)  # Association strength
```

**TandemVarClust predict() features:**
- **AFDM Projection**: Projects new variables into factorial space
- **Distance-based assignment**: Calculates distances to cluster centers
- **Statistical metrics**: Chi², Cramér's V, contingency tables, Dice scores
- **Rich interpretation**: Multiple perspectives on variable-cluster association

---

## Shiny Application

Launch the interactive Shiny interface for exploring clustering results:

```r
library(RollerClustR)
library(shiny)

# Launch the Shiny app (assuming app files are in inst/shiny/)
runApp(system.file("shiny", package = "RollerClustR"))
```

The Shiny app provides:
- Data import and preprocessing
- Interactive algorithm configuration
- Visual results exploration (dendrograms, factorial maps, silhouette)
- Prediction of illustrative variables
- Session history and export functionality

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

**TandemVarClust**: Combines Multiple Correspondence Analysis (MCA) with Hierarchical Agglomerative Clustering (HAC) for categorical and mixed data. The `predict()` method uses AFDM projection to assign new variables based on distances to cluster centers in factorial space, enriched with statistical association metrics (Chi², Cramér's V, Dice).

---

## Complete Documentation

- **Full User Guide**: See `USERGUIDE.md`
- **Technical Notices**: Detailed algorithm documentation in `/doc`
- **Function Documentation**: `?roller_clust`, `?VAR_CAH`, `?VAR_KMEANS`, `?TandemVarClust`
- **Vignettes**: `browseVignettes("RollerClustR")`

---

## Citation

If you use **RollerClustR** in your research, please cite:

```
Romain Buono, Nico Dena, Mohamed Habib Bah. (2025). RollerClustR: Variable Clustering in R.
R package version 1.0.0. https://github.com/RomainBuono/RollerClustR
```

**Key References**:

- Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012). ClustOfVar: An R Package for the Clustering of Variables. *Journal of Statistical Software*, 50(13), 1-16.
- Vigneau, E., & Qannari, E. M. (2003). Clustering of variables around latent components. *Communications in Statistics-Simulation and Computation*, 32(4), 1131-1150.

---

## License

GPL-3 © Romain BUONO, Nico DENA, Mohamed Habib BAH

---

## Support

- **Issues**: [GitHub Issues](https://github.com/RomainBuono/RollerClustR/issues)
- **Questions**: Open a discussion on GitHub
