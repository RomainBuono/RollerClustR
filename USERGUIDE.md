# RollerClustR User Guide

**Version**: 1.0.0  
**Authors**: Romain Buono, Nico Dena,Mohamed Habib Bah  
**Date**: November 2025

---

## Table of Contents

1. [Introduction](#introduction)
2. [Installation and Setup](#installation-and-setup)
3. [Normal User Guide](#normal-user-guide)
   - [Quick Start](#quick-start)
   - [The Three Clustering Methods](#the-three-clustering-methods)
   - [Typical Workflow: fit() and summary()](#typical-workflow-fit-and-summary)
   - [Illustrative Variables: predict()](#illustrative-variables-predict)
   - [Modifying the Number of Clusters](#modifying-the-number-of-clusters)
4. [Advanced User Guide](#advanced-user-guide)
   - [Direct R6 Class Usage](#direct-r6-class-usage)
   - [Advanced Methods by Algorithm](#advanced-methods-by-algorithm)
   - [Fine-Tuning Parameters](#fine-tuning-parameters)
   - [In-Depth Results Analysis](#in-depth-results-analysis)
5. [Shiny Application](#shiny-application)
6. [Detailed Use Cases](#detailed-use-cases)
7. [Interpreting Outputs](#interpreting-outputs)
8. [FAQ and Troubleshooting](#faq-and-troubleshooting)

---

## Introduction

**RollerClustR** is an R package dedicated to **variable clustering**. Unlike classical methods that group individuals, RollerClustR identifies groups of similar or redundant variables in your data.

### Why Cluster Variables?

- **Dimensionality reduction**: Identify groups of correlated variables
- **Variable selection**: Choose one representative per cluster
- **Multicollinearity detection**: Spot redundancies
- **Interpretability**: Understand latent data structure

### The Three Algorithms

| Algorithm | Data Type | Principle | Recommended Use |
|-----------|-----------|-----------|-----------------|
| **VAR_CAH** | Numeric | Hierarchical agglomerative | Exploration, visual dendrograms |
| **VAR_KMEANS** | Numeric | K-means with 1st PCs (Vigneau & Qannari) | Optimized partitioning, maximizing r² |
| **TandemVarClust** | Mixed | MCA + HAC on modalities | Categorical or mixed data |

---

## Installation and Setup

### Installation

```r
# From GitHub (recommended)
install.packages("devtools")
devtools::install_github("RomainBuono/RollerClustR")

# Load package
library(RollerClustR)
```

### Dependencies

RollerClustR requires:
- R >= 4.0
- Packages: `R6`, `stats`, `graphics`, `FactoMineR`

---

## Normal User Guide

This section is for users who want to use RollerClustR via the simplified `roller_clust()` interface.

### Quick Start

The main interface is the **`roller_clust()`** function that handles the entire workflow:

```r
library(RollerClustR)
data(iris)

# Clustering in one line
set.seed(123)  # For reproducibility
model <- roller_clust(
  X = iris[, 1:4],      # Data (columns = variables to cluster)
  method = "var_cah",   # Method choice
  K = 2,                # Desired number of clusters
  scale = TRUE          # Standardization (recommended)
)
```

**What does `roller_clust()` do?**
1. Creates an object of the appropriate class (VAR_CAH, VAR_KMEANS, or TandemVarClust)
2. Automatically calls `$fit(X)` to fit the model
3. Returns a fitted, ready-to-use model

### The Three Clustering Methods

#### Method 1: VAR_CAH (Hierarchical Agglomerative Clustering)

**Principle**: Bottom-up approach starting with individual variables and progressively merging them.

```r
model_cah <- roller_clust(
  X = iris[, 1:4],
  method = "var_cah",
  K = 2,
  scale = TRUE
)
```

**When to use**:
- Initial exploration
- Visualizing dendrograms
- Understanding progressive merging
- Relatively small number of variables (< 50)

**Key features**:
- Creates synthetic variables via PCA for each cluster
- Provides homogeneity metrics
- Allows tree visualization with `model_cah$get_tree()`

#### Method 2: VAR_KMEANS (K-means with Principal Components)

**Principle**: Iterative reallocation algorithm where cluster centers are represented by first principal components (Vigneau & Qannari algorithm). Maximizes sum of squared correlations (r²).

```r
model_km <- roller_clust(
  X = iris[, 1:4],
  method = "var_kmeans",
  K = 3,
  n_init = 20,          # Number of random initializations
  max_iter = 100        # Maximum iterations per run
)
```

**When to use**:
- Optimized variable partitioning
- When K is known in advance
- Need for reproducible results (with set.seed)
- Medium to large datasets

**Key features**:
- Cluster centers are 1st principal components
- Maximizes sum of r² (squared correlations)
- Multiple random initializations to avoid local optima
- Provides homogeneity as mean r²

#### Method 3: TandemVarClust (MCA + HAC for Mixed Data)

**Principle**: Combines Multiple Correspondence Analysis with Hierarchical Clustering on modalities.

```r
# Create mixed data (numeric + categorical)
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
```

**When to use**:
- Categorical variables
- Mixed data (numeric + categorical)
- Need for modality-level clustering
- Contingency table analysis

**Key features**:
- Discretizes numeric variables into bins
- Operates at modality level (not variable level)
- Uses Dice index for observation assignment
- **New**: AFDM projection for illustrative variables with distance-based assignment

### Typical Workflow: fit() and summary()

#### Step 1: Fit the Model

`roller_clust()` automatically fits for you, but you can also do it manually:

```r
set.seed(123)
model <- VAR_CAH$new(K = 2)
model$fit(iris[, 1:4])
```

#### Step 2: Display Summary

```r
model$summary()
```

**Example output (VAR_CAH)**:
```
===================================================================
   VAR_CAH - Hierarchical Variable Clustering Summary
===================================================================

Number of clusters (K): 2
Number of variables: 4
Scaling applied: TRUE
Clustering method: Complete linkage (on correlations)

Within-cluster inertia (W): 0.453
Between-cluster inertia (B): 3.547
Total inertia (T): 4.000
Variance explained by clustering: 88.68%

Global homogeneity: 0.774

-------------------------------------------------------------------
Cluster Composition and Homogeneity:
-------------------------------------------------------------------

Cluster 1 (3 variables):
  Sepal.Length, Petal.Length, Petal.Width
  Homogeneity (mean |cor|): 0.863

Cluster 2 (1 variable):
  Sepal.Width
  Homogeneity (mean |cor|): 1.000
===================================================================
```

**Example with VAR_KMEANS**:
```r
# Create and fit VAR_KMEANS model
set.seed(123)  # For reproducibility
model_km <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 20)
model_km$summary()
```

**Example output (VAR_KMEANS)**:
```
===========================================================
   VAR_KMEANS - K-Means with Principal Components
            (Vigneau & Qannari Algorithm)
===========================================================

Algorithm: K-Means with 1st Principal Components (PCA)
Optimization criterion: Maximize sum of squared correlations (r²)
Cluster centers: 1st principal component of each cluster
Number of initializations (n_init): 20 
Maximum iterations: 100 
Convergence tolerance: 1e-06 
Data standardization: TRUE 
Number of clusters (K): 2 
Number of variables: 4 

=== Clustering Quality Metrics ===

Sum of r² (criterion): 3.7697 
Mean homogeneity (r²): 0.9424 
Proportion of variance explained: 94.24%
Convergence status: TRUE 
Number of iterations: 12

=== Cluster Details ===

Cluster 1 (1 variable):
  Variables: Sepal.Width
  Mean r² (homogeneity): 1.0000

Cluster 2 (3 variables):
  Variables: Sepal.Length, Petal.Length, Petal.Width
  Mean r² (homogeneity): 0.9283
===========================================================
```

### Illustrative Variables: predict()

The `predict()` method assigns new variables to existing clusters **without refitting the model**. This is useful for:
- Testing if new variables fit into the existing structure
- Feature selection based on cluster membership
- Understanding relationships with external variables

**Important**: New variables must be measured on the **same observations** as the training data.

#### predict() for VAR_CAH and VAR_KMEANS

```r
# Example with VAR_KMEANS
set.seed(123)
model_km <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 20)

# Create illustrative variables (on same observations!)
set.seed(456)
new_vars <- data.frame(
  SumPetals = iris$Petal.Length + iris$Petal.Width,
  RatioPetals = iris$Petal.Length / (iris$Petal.Width + 0.1),
  SepalArea = iris$Sepal.Length * iris$Sepal.Width
)

# Predict cluster membership
predictions <- model_km$predict(new_vars)

# Examine results for each variable
print(predictions$SumPetals)
```

**Output structure**:
```
$cluster
[1] 2

$scores
   Cluster1    Cluster2
1 0.3456789  0.9733210

$best_score
[1] 0.9733210

$second_best_score
[1] 0.3456789

$ambiguity
[1] 0.6276421  # Difference between best and second-best
```

**Interpretation**:
- `cluster`: Assigned cluster (here: Cluster 2)
- `scores`: r² with each cluster center (squared correlation)
- `best_score`: Highest r² (0.97 → very strong fit to Cluster 2)
- `second_best_score`: Second-highest r² (0.35 → weak fit to Cluster 1)
- `ambiguity`: Difference between best and second-best (0.63 → clear assignment, no ambiguity)

**Decision rules**:
- `best_score > 0.7`: Strong fit, confident assignment
- `best_score 0.5-0.7`: Moderate fit, reasonable assignment
- `best_score < 0.5`: Weak fit, variable doesn't fit well in any cluster
- `ambiguity < 0.2`: Ambiguous assignment (similar scores for multiple clusters)

#### predict() for TandemVarClust (Updated Methodology)

**New in version 1.0.0**: TandemVarClust uses **AFDM projection** for illustrative variable assignment, providing richer statistical analysis.

```r
# Create and fit TandemVarClust model
set.seed(789)
iris_mixed <- iris[, 1:4]
iris_mixed$Size <- cut(iris$Sepal.Length, 3, labels = c("S", "M", "L"))
iris_mixed$Species <- iris$Species

model_tandem <- roller_clust(iris_mixed, method = "tandem", K = 3, n_bins = 5)

# Create illustrative categorical variable
set.seed(100)
illus_var <- data.frame(
  Color = sample(c("Red", "Blue", "Green"), 150, replace = TRUE)
)

# Predict
pred_result <- model_tandem$predict(illus_var)
print(pred_result$Color)
```

**Output structure** (TandemVarClust):
```
$cluster
[1] 2

$n_modalities
[1] 3

$modality_clusters
 Red Blue Green 
   2    1     2

$distances
 Cluster1  Cluster2  Cluster3
2.3456    1.2345    3.4567

$contingency
        Cluster1 Cluster2 Cluster3
Red          15       25       10
Blue         20       15       15
Green        10       30       10

$chi2_test
	Pearson's Chi-squared test

data:  contingency
X-squared = 12.345, df = 4, p-value = 0.0123

$cramers_v
[1] 0.2456

$significant
[1] TRUE

$dice_scores
     Cluster1  Cluster2  Cluster3
[1,]  0.234    0.567     0.199
[2,]  0.456    0.234     0.310
...
```

**New prediction methodology**:

1. **AFDM Projection**: Each modality of the illustrative variable is projected into the factorial space established during training
2. **Distance Calculation**: Euclidean distances are computed between projected modalities and cluster centers
3. **Assignment**: Variable assigned to cluster with minimum average distance
4. **Statistical Metrics**: Chi², Cramér's V, and contingency tables quantify association strength

**Interpretation**:
- `cluster`: Assigned cluster based on minimum distance (here: Cluster 2)
- `n_modalities`: Number of categories in the illustrative variable (3: Red, Blue, Green)
- `modality_clusters`: Cluster assignment for each modality individually
- `distances`: Average distances to each cluster center (lower = closer fit)
- `contingency`: Cross-tabulation of variable modalities × observation clusters
- `chi2_test`: Test of independence (p < 0.05 → significant association)
- `cramers_v`: Normalized association measure (0-1 scale, 0.2456 = moderate association)
- `significant`: Boolean flag for statistical significance (p < 0.05)
- `dice_scores`: Matrix of Dice similarity scores (observations × clusters)

**Key advantages of new methodology**:
- **Coherent with clustering**: Uses same factorial space as original clustering
- **Multiple perspectives**: Distance-based assignment + statistical association metrics
- **Quantified uncertainty**: Cramér's V measures association strength
- **Rich interpretation**: Contingency tables show distribution patterns

**Decision rules** (TandemVarClust):
- `significant = TRUE` + `cramers_v > 0.3`: Strong association with clustering structure
- `significant = TRUE` + `cramers_v 0.1-0.3`: Moderate association
- `significant = FALSE` or `cramers_v < 0.1`: Weak/no association, variable independent of clustering

### Modifying the Number of Clusters

All three algorithms support dynamic K modification:

```r
# Initial clustering
set.seed(123)
model <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)

# Change K without refitting from scratch
model$K <- 3
model$summary()  # Shows new 3-cluster solution

# Or use refit_with_k() method
model$refit_with_k(4)
```

**What happens internally**:
- **VAR_CAH**: Cuts dendrogram at new level (fast, no recomputation)
- **VAR_KMEANS**: Re-runs K-means with new K (uses stored data)
- **TandemVarClust**: Cuts dendrogram at new level, recomputes observation assignments

---

## Advanced User Guide

For users who want to leverage the full power of R6 classes.

### Direct R6 Class Usage

Instead of using `roller_clust()`, you can instantiate classes directly:

```r
# VAR_CAH
set.seed(123)
model_cah <- VAR_CAH$new(K = 3, scale = TRUE, method_cah = "ward.D2")
model_cah$fit(iris[, 1:4])
model_cah$summary()

# VAR_KMEANS
set.seed(456)
model_km <- VAR_KMEANS$new(K = 3, n_init = 50, max_iter = 200, tol = 1e-6)
model_km$fit(iris[, 1:4])

# TandemVarClust
set.seed(789)
iris_mixed <- iris[, 1:4]
iris_mixed$Species <- iris$Species
model_tandem <- TandemVarClust$new(K = 3, n_bins = 7, n_factors = 5)
model_tandem$fit(iris_mixed)
```

### Advanced Methods by Algorithm

#### VAR_CAH Advanced Methods

```r
set.seed(123)
model_cah <- roller_clust(iris[, 1:4], method = "var_cah", K = 3)

# Get hierarchical tree
tree <- model_cah$get_tree()
plot(tree)

# Get variables in specific cluster
vars_cluster_1 <- model_cah$get_cluster_variables(1)
print(vars_cluster_1)
# [1] "Sepal.Length" "Petal.Length" "Petal.Width"

# Get cluster for specific variable
cluster_of_sepal_width <- model_cah$get_variable_cluster("Sepal.Width")
print(cluster_of_sepal_width)
# [1] 2

# Calculate cluster homogeneity
homogeneity <- model_cah$get_cluster_homogeneity(1)
print(homogeneity)
# [1] 0.863  # Mean absolute correlation within cluster 1
```

#### VAR_KMEANS Advanced Methods

```r
set.seed(123)
model_km <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 20)

# Access cluster centers (1st PCs)
centers <- model_km$get_cluster_centers()
print(dim(centers))
# [1] 2 150  # 2 clusters × 150 observations

# Get convergence information
convergence <- model_km$get_convergence_info()
print(convergence)
# $converged
# [1] TRUE
# $n_iter
# [1] 12
# $criterion_value
# [1] 3.7697

# Calculate within-cluster sum of r²
within_ss <- model_km$get_within_cluster_ss()
print(within_ss)
# [1] 3.7697
```

#### TandemVarClust Advanced Methods

```r
set.seed(789)
iris_mixed <- iris[, 1:4]
iris_mixed$Species <- iris$Species
model_tandem <- roller_clust(iris_mixed, method = "tandem", K = 3, n_bins = 5)

# Get disjunctive table (modality-level data)
disj_table <- model_tandem$DisjunctiveTable
print(dim(disj_table))
# [1] 150  29  # 150 obs × 29 modalities (4 vars × 5 bins + 3 species)

# Get factorial coordinates (modalities in MCA space)
factorial_coords <- model_tandem$FactorialCoords
print(dim(factorial_coords))
# [1] 29  28  # 29 modalities × 28 factorial axes

# Get variance explained by factorial axes
var_explained <- model_tandem$VarianceExplained
print(var_explained[1:5])
# [1] 18.5 15.2 12.8 10.3  8.7  # Percentage variance for first 5 axes

# Get variable summary (aggregation from modalities to variables)
var_summary <- model_tandem$get_variable_summary()
print(var_summary)
#       variable n_modalites cluster_principal purity
# 1  Petal.Width           5                 1   1.00
# 2 Petal.Length           5                 1   0.80
# ...

# Get modalities of a specific variable
mod_petal <- model_tandem$get_modalities_of_variable("Petal.Width")
print(mod_petal)
#   modalite cluster
# 1     bin1       1
# 2     bin2       1
# 3     bin3       1
# 4     bin4       1
# 5     bin5       1

# Get all modalities in a specific cluster
mod_cluster_1 <- model_tandem$get_modalities_of_cluster(1)
print(head(mod_cluster_1))
#       variable modalite
# 1 Petal.Length     bin1
# 2 Petal.Length     bin2
# ...
```

### Fine-Tuning Parameters

#### VAR_CAH Parameters

```r
model <- VAR_CAH$new(
  K = 3,                  # Number of clusters
  scale = TRUE,           # Standardize variables
  method_cah = "ward.D2"  # Linkage: "ward.D2", "complete", "average", "single"
)
```

**Linkage methods**:
- `"ward.D2"`: Minimizes within-cluster variance (default, recommended)
- `"complete"`: Maximum distance between clusters (creates compact clusters)
- `"average"`: Average distance (balanced approach)
- `"single"`: Minimum distance (can create elongated clusters, use with caution)

#### VAR_KMEANS Parameters

```r
model <- VAR_KMEANS$new(
  K = 3,              # Number of clusters
  n_init = 50,        # Number of random initializations (higher = more robust)
  max_iter = 200,     # Maximum iterations per run
  tol = 1e-6,         # Convergence tolerance
  scale = TRUE        # Standardize variables
)
```

**Tuning advice**:
- Increase `n_init` for more robust results (20-50 recommended)
- Increase `max_iter` if convergence warnings appear (default 100 usually sufficient)
- Decrease `tol` for stricter convergence (default 1e-6 is good balance)

#### TandemVarClust Parameters

```r
model <- TandemVarClust$new(
  K = 3,                  # Number of clusters
  n_bins = 5,             # Bins for numeric discretization
  n_factors = NULL,       # Number of MCA factors to retain (NULL = all)
  method_cah = "ward.D2", # Linkage method
  scale = TRUE            # Standardize numeric variables before discretization
)
```

**n_bins tuning**:
- `n_bins = 3`: Coarse discretization (more robust, less information)
- `n_bins = 5`: Default (good balance)
- `n_bins = 7-10`: Fine discretization (more information, risk of overfitting)

**n_factors tuning**:
- `NULL` (default): Retain all factors (no dimensionality reduction)
- `3-5`: Moderate reduction (faster computation, may lose some structure)
- Rule of thumb: Retain factors explaining ≥ 5% variance

### In-Depth Results Analysis

#### Accessing All Results

All models store comprehensive results:

```r
# Get cluster assignments
clusters <- model$Groupes
# Named vector: variable names → cluster IDs

# Get number of clusters
K <- model$K

# Get within-cluster inertia (lower = tighter clusters)
within_inertia <- model$WithinClusterInertia

# Get homogeneity (higher = more coherent clusters)
homogeneity <- model$Homogeneite
```

#### Plotting Results

```r
# VAR_CAH: Dendrogram
set.seed(123)
model_cah <- roller_clust(iris[, 1:4], method = "var_cah", K = 3)
plot(model_cah$get_tree(), 
     main = "Variable Clustering Dendrogram",
     xlab = "Variables", ylab = "Height")
rect.hclust(model_cah$get_tree(), k = 3, border = "red")

# TandemVarClust: Factorial map
set.seed(789)
iris_mixed <- iris[, 1:4]
iris_mixed$Species <- iris$Species
model_tandem <- roller_clust(iris_mixed, method = "tandem", K = 3)

# Extract factorial coordinates and clusters
coords <- model_tandem$FactorialCoords
clusters <- model_tandem$Groupes

# Plot first two axes
plot(coords[, 1], coords[, 2],
     col = clusters, pch = 19,
     xlab = paste0("Dim 1 (", round(model_tandem$VarianceExplained[1], 1), "%)"),
     ylab = paste0("Dim 2 (", round(model_tandem$VarianceExplained[2], 1), "%)"),
     main = "Factorial Map of Modalities")
legend("topright", legend = paste("Cluster", 1:3), col = 1:3, pch = 19)
```

---

## Shiny Application

RollerClustR includes an interactive Shiny application for exploring clustering results without programming.

### Launching the App

```r
library(RollerClustR)
library(shiny)

# Launch the Shiny app
runApp(system.file("shiny", package = "RollerClustR"))

# Or if app files are in a custom location:
# runApp("path/to/shiny/app")
```

### App Features

The Shiny application provides a comprehensive graphical interface organized in several tabs:

#### 1. Data Management
- **Import**: Load CSV, Excel (.xlsx, .xls), or use built-in R datasets (iris, mtcars, USArrests, swiss, state.x77, airquality)
- **Generate**: Create synthetic data with controllable parameters (noise, correlation structure)
- **Preprocess**: Handle missing values (omit, impute by median/mode/mean)
- **Preview**: Interactive table with sorting and filtering

#### 2. Configuration
- **Algorithm Selection**: Choose between VAR_CAH, VAR_KMEANS, TandemVarClust with contextual descriptions
- **Variable Selection**: Interactive interface to select active variables
- **Parameters**:
  - Number of clusters (K) with automatic detection option
  - Standardization toggle
  - Algorithm-specific settings (linkage method, n_init, n_bins, n_factors)

#### 3. Clustering & Results
- **Launch**: Execute clustering with progress bar
- **Summary**: Textual summary with metrics (Silhouette, Davies-Bouldin, Dunn, Calinski-Harabasz)
- **Visualizations**:
  - Interactive dendrogram (VAR_CAH, TandemVarClust) with plotly
  - Silhouette plot by variable
  - Factorial maps (TandemVarClust)
  - Correlation matrix heatmap reorganized by clusters
- **Tables**: Detailed cluster composition with per-variable metrics

#### 4. Diagnostics
- **Quality Metrics**: Global clustering quality indicators
- **Bootstrap Stability**: Robustness assessment via resampling
- **Discriminant Variables**: Identification of most contributive variables

#### 5. Prediction
- **Import Illustrative Variables**: Load new variables or generate test cases
- **Automatic Assignment**: Apply model's predict() method
- **Results Display**:
  - For VAR_CAH/VAR_KMEANS: Correlation scores by cluster
  - For TandemVarClust: Contingency tables, Chi², Cramér's V, Dice scores
- **Visualization**: Position new variables in cluster space

#### 6. History & Session Management
- **Auto-save**: Each analysis automatically saved with metadata
- **Annotations**: Add notes to sessions
- **Multi-session**: Maintain multiple analyses for comparison
- **Reload**: Restore previous sessions with full model and data
- **Export/Import**: Save sessions as RDS files for sharing

#### 7. Export
- **Tables**: Export results as CSV or Excel
- **Graphics**: Save dendrograms and plots as PNG or PDF (high resolution)
- **HTML Report**: Auto-generate comprehensive report with all results
- **R6 Model**: Export fitted model object for programmatic reuse

### Typical Shiny Workflow

1. **Load Data**: Import your dataset or use a built-in example
2. **Configure**: Select algorithm, set K, choose active variables
3. **Cluster**: Launch analysis and view results
4. **Explore**: Navigate through visualizations and tables
5. **Predict**: Test illustrative variables
6. **Compare**: Try different K values or algorithms, save to history
7. **Export**: Download report and/or model for further analysis

### Technical Notes

- **State Management**: App uses reactiveValues for global state tracking
- **R6 Integration**: Seamless conversion between R6 objects and Shiny-compatible structures
- **Error Handling**: Robust validation with contextual error messages
- **Responsiveness**: Progress indicators for long computations

---

## Detailed Use Cases

### Use Case 1: Exploratory Data Analysis

**Problem**: Understanding correlation structure in a multivariate dataset.

```r
data(mtcars)

# Cluster variables
set.seed(123)
model <- roller_clust(mtcars, method = "var_cah", K = 4)
model$summary()

# Visualize dendrogram
plot(model$get_tree())

# Interpret clusters
for (k in 1:4) {
  cat("\nCluster", k, ":\n")
  vars <- model$get_cluster_variables(k)
  print(vars)
  
  homog <- model$get_cluster_homogeneity(k)
  cat("Homogeneity:", round(homog, 3), "\n")
}
```

**Outcome**: Identify groups of related variables (e.g., power-related vs economy-related).

### Use Case 2: Feature Selection for Machine Learning

**Problem**: Reduce 100 correlated features to 20 representative ones.

```r
# Simulate high-dimensional data
set.seed(456)
n <- 200
p <- 100
X <- matrix(rnorm(n * p), nrow = n)
colnames(X) <- paste0("Var", 1:p)

# Cluster variables
model <- roller_clust(X, method = "var_kmeans", K = 20, n_init = 30)

# Select one representative per cluster (highest homogeneity with cluster center)
selected_features <- c()
for (k in 1:20) {
  vars_in_k <- model$get_cluster_variables(k)
  
  # Calculate r² of each variable with cluster center
  center_k <- model$get_cluster_centers()[k, ]
  correlations <- apply(X[, vars_in_k, drop = FALSE], 2, function(v) cor(v, center_k)^2)
  
  # Select variable with highest r²
  best_var <- names(which.max(correlations))
  selected_features <- c(selected_features, best_var)
}

print(selected_features)
# [1] "Var3"  "Var7"  "Var12" ... (20 features)

# Use selected features in downstream analysis
X_reduced <- X[, selected_features]
```

**Benefit**: Systematic feature reduction with interpretability.

### Use Case 3: Mixed Data Customer Segmentation

**Problem**: Customer database with demographics (categorical) and behavior (numeric).

```r
# Simulate customer data
set.seed(789)
n_customers <- 500
customers <- data.frame(
  Age = sample(18:70, n_customers, replace = TRUE),
  Income = rnorm(n_customers, 50000, 15000),
  Education = sample(c("High School", "Bachelor", "Master", "PhD"), n_customers, replace = TRUE),
  Region = sample(c("North", "South", "East", "West"), n_customers, replace = TRUE),
  Membership = sample(c("Bronze", "Silver", "Gold"), n_customers, replace = TRUE),
  Churn = sample(c("Yes", "No"), n_customers, replace = TRUE, prob = c(0.2, 0.8))
)

# TandemVarClust for mixed data (exclude Churn)
mixed_data <- customers[, c("Age", "Income", "Education", "Region", "Membership")]
model <- roller_clust(mixed_data, method = "tandem", K = 4, n_bins = 5)

# Examine modality clusters
model$summary()
var_summary <- model$get_variable_summary()
print(var_summary)

# Illustrative analysis: Does "Churn" status relate to clusters?
churn_analysis <- model$predict(data.frame(Churn = customers$Churn))

if (churn_analysis$Churn$significant) {
  cat("Churn is significantly associated with the clustering!\n")
  cat("Cramér's V:", round(churn_analysis$Churn$cramers_v, 3), "\n\n")
  print(churn_analysis$Churn$contingency)
}
```

**Benefit**: Understand how demographic/behavioral variables group in customer base and identify churn patterns.

---

## Interpreting Outputs

### Understanding Homogeneity

**For VAR_CAH**: Homogeneity is mean absolute correlation within clusters.

```
Cluster 1 (3 variables): Homogeneity = 0.85
  → Variables in Cluster 1 have average |correlation| of 0.85
  → High internal coherence

Cluster 2 (2 variables): Homogeneity = 0.62
  → Moderate coherence
```

**For VAR_KMEANS**: Homogeneity is mean r² (squared correlation) within clusters.

```
Cluster 1 (3 variables): Homogeneity (mean r²) = 0.85
  → Variables in Cluster 1 have average r² of 0.85 with cluster center (1st PC)
  → Very high internal coherence (85% variance explained on average)

Cluster 2 (2 variables): Homogeneity (mean r²) = 0.62
  → Moderate coherence (62% variance explained)
```

**Quality benchmarks** (for r² in VAR_KMEANS):
- Homogeneity > 0.7: Good cluster
- Homogeneity 0.5-0.7: Acceptable cluster
- Homogeneity < 0.5: Weak cluster (reconsider K)

### Predict Output Interpretation

See detailed sections above for:
- [VAR_CAH/VAR_KMEANS predict() interpretation](#predict-for-var_cah-and-var_kmeans)
- [TandemVarClust predict() interpretation](#predict-for-tandemvarclust-updated-methodology)

**Key principle**: High scores/strong associations indicate that the illustrative variable "belongs" to that cluster conceptually.

---

## FAQ and Troubleshooting

### Q: When should I use which method?

**A**:
- **All numeric variables + want dendrogram visualization** → VAR_CAH
- **All numeric variables + optimized partitioning** → VAR_KMEANS
- **Categorical or mixed data** → TandemVarClust
- **Not sure** → Try VAR_CAH first (most straightforward)

### Q: How do I choose K?

**A**:
- **VAR_CAH**: Plot dendrogram (`plot(model$get_tree())`), look for natural cuts
- **VAR_KMEANS**: Try multiple K values, compare homogeneity (higher = better)
- **TandemVarClust**: Examine scree plot of MCA eigenvalues, use elbow method
- **General**: Try multiple K values, compare homogeneity/interpretability
- **Shiny app**: Use automatic K detection feature

### Q: What if predict() gives low scores for all clusters?

**A**: The new variable doesn't fit well into any existing cluster. This could mean:
- The variable captures a different dimension
- The original clustering may need more clusters
- The variable is noisy or poorly related to original variables

### Q: Can I use predict() on a different dataset?

**A**: No. `predict()` requires the **same observations** as training data. It predicts which cluster new **variables** belong to, not new observations.

### Q: How do I handle missing values?

**A**: Preprocess your data before clustering:
```r
# Remove observations with any NA
X_clean <- na.omit(X)

# Or impute missing values
library(mice)
X_imputed <- complete(mice(X, method = "pmm", m = 1))

# Then cluster
model <- roller_clust(X_clean, method = "var_cah", K = 3)
```

### Q: My TandemVarClust has too many modalities, what should I do?

**A**: Reduce `n_bins` for numeric discretization:
```r
# Instead of n_bins = 7
model <- roller_clust(X, method = "tandem", K = 3, n_bins = 3)
# Fewer bins → fewer modalities → more parsimonious model
```

### Q: How is VAR_KMEANS different from standard K-means?

**A**: VAR_KMEANS clusters **variables** (not observations) using the Vigneau & Qannari algorithm. Cluster centers are represented by 1st principal components, and the algorithm maximizes sum of squared correlations (r²) instead of minimizing distances.

### Q: What changed in TandemVarClust's predict() method?

**A**: Version 1.0.0 introduced **AFDM projection** for illustrative variables:
- **Old**: Direct Dice coefficient calculation (had structural bias issues)
- **New**: Projects variables into factorial space, calculates distances to cluster centers, provides Chi², Cramér's V, and contingency tables
- **Benefit**: Methodologically coherent with clustering, richer statistical interpretation

### Q: Can I save and reload models?

**A**: Yes, use standard R serialization:
```r
# Save
saveRDS(model, "my_model.rds")

# Load
model <- readRDS("my_model.rds")
model$summary()  # Works!
```

### Q: Why does VAR_KMEANS give different results each time?

**A**: VAR_KMEANS uses random initialization. Use `set.seed()` for reproducibility:
```r
set.seed(123)
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 3, n_init = 20)
# Results will be identical on each run with same seed
```

### Q: How do I launch the Shiny app?

**A**: 
```r
library(RollerClustR)
library(shiny)
runApp(system.file("shiny", package = "RollerClustR"))
```

If the app doesn't launch, ensure Shiny and all dependencies are installed:
```r
install.packages(c("shiny", "shinydashboard", "plotly", "DT"))
```

---

## Additional Resources

- **Package documentation**: `help(package = "RollerClustR")`
- **Function help**: `?roller_clust`, `?VAR_CAH`, `?VAR_KMEANS`, `?TandemVarClust`
- **Technical notices**: Detailed algorithm documentation in `/doc` directory
- **GitHub repository**: https://github.com/RomainBuono/RollerClustR
- **Report issues**: https://github.com/RomainBuono/RollerClustR/issues

**Key References**:
- Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012). ClustOfVar: An R Package for the Clustering of Variables. *Journal of Statistical Software*, 50(13), 1-16.
- Vigneau, E., & Qannari, E. M. (2003). Clustering of variables around latent components. *Communications in Statistics-Simulation and Computation*, 32(4), 1131-1150.

---

**End of User Guide**
