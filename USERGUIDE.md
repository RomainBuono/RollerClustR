# RollerClustR User Guide

**Version**: 1.0.0  
**Author**: Romain Buono  
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
5. [Detailed Use Cases](#detailed-use-cases)
6. [Interpreting Outputs](#interpreting-outputs)
7. [FAQ and Troubleshooting](#faq-and-troubleshooting)

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
- Packages: `R6`, `stats`, `graphics`

---

## Normal User Guide

This section is for users who want to use RollerClustR via the simplified `roller_clust()` interface.

### Quick Start

The main interface is the **`roller_clust()`** function that handles the entire workflow:

```r
library(RollerClustR)
data(iris)

# Clustering in one line
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
iris_mixed <- iris
iris_mixed$Size <- cut(iris$Sepal.Length, 3, labels = c("S", "M", "L"))

set.seed(123)  # For reproducible and balanced clustering
model_tandem <- roller_clust(
  X = iris_mixed[, c(1:4, 5, 6)],  # Columns: 4 numeric + Species + Size
  method = "tandem",
  K = 3,
  n_bins = 5,              # Number of bins for numeric discretization
  n_factors = 3            # Number of factorial axes to retain
)
```

**Note**: We use columns 1-4 (numeric), 5 (Species - already a factor in iris), and 6 (Size - newly created factor). This avoids creating a redundant `Species_char` column. The `set.seed(123)` ensures balanced cluster assignment.

**When to use**:
- Categorical variables
- Mixed data (numeric + categorical)
- Need for modality-level clustering
- Contingency table analysis

**Key features**:
- Discretizes numeric variables into bins
- Operates at modality level (not variable level)
- Uses Dice index for observation assignment

### Typical Workflow: fit() and summary()

#### Step 1: Fit the Model

`roller_clust()` automatically fits for you, but you can also do it manually:

```r
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
Number of iterations: 2 

=== Cluster Details ===

Cluster 1 : 1 variable(s)
  Variables: Sepal.Width 
  Homogeneity (r²): 1.0000 (single variable)

Cluster 2 : 3 variable(s)
  Variables: Sepal.Length, Petal.Length, Petal.Width 
  Homogeneity (mean r²): 0.9232

===========================================================

Note: With set.seed(123), these exact values should be reproducible.
Without set.seed(), values may vary slightly due to random initialization.
```

### Illustrative Variables: predict()

The `predict()` method assigns **new variables** (not observations) to existing clusters.

#### predict() for VAR_CAH and VAR_KMEANS

```r
# Fit a model with reproducible results
set.seed(123)
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 20)

# Create new variables
new_vars <- data.frame(
  SumPetals = iris$Petal.Length + iris$Petal.Width,
  RatioPetals = iris$Petal.Length / iris$Petal.Width,
  MeanSepals = (iris$Sepal.Length + iris$Sepal.Width) / 2
)

# Predict cluster membership
predictions <- model$predict(new_vars)
```

**Output structure**:
```r
str(predictions)
#List of 3
# $ SumPetals  :List of 3
#  ..$ cluster   : int 2
#  ..$ scores    : num [1:2] 0.17 0.973
#  ..$ best_score: num 0.973
# $ RatioPetals:List of 3
#  ..$ cluster   : int 2
#  ..$ scores    : num [1:2] 0.135 0.494
#  ..$ best_score: num 0.494
# $ MeanSepals :List of 3
#  ..$ cluster   : int 2
#  ..$ scores    : num [1:2] 0.145 0.505
#  ..$ best_score: num 0.505
```

**Note**: Exact values depend on random initialization. Use `set.seed()` for reproducibility.

**Interpretation**:
- **For VAR_KMEANS**: `scores` are **r²** (squared correlations) with each cluster's center (1st PC)
- `cluster`: Assigned cluster (highest r²)
- `best_score`: Maximum r² value
- `second_best_score`: Second-best r² (helps assess ambiguity)

**Example**: 
```r
predictions$SumPetals$best_score
# [1] 0.945  # (value may vary without set.seed)

# Interpretation: SumPetals has r² = 0.945 with Cluster 1's center
# This means 94.5% of SumPetals' variance is explained by the 1st PC of Cluster 1
# → Very strong association with Cluster 1
```

**Ambiguity detection**:
```r
# High ambiguity example
if (predictions$SumPetals$best_score - predictions$SumPetals$second_best_score < 0.1) {
  message("SumPetals is ambiguous between clusters")
}
# Low difference → Variable could belong to multiple clusters
```

#### predict() for TandemVarClust

TandemVarClust's `predict()` is different: it analyzes **illustrative categorical variables** using contingency tables.

```r
iris_mixed <- iris
iris_mixed$Size <- cut(iris$Sepal.Length, 3, labels = c("S", "M", "L"))

# Modèle
set.seed(123)
model_tandem <- roller_clust(
  X = iris_mixed[, c(1:4, 5, 6)],
  method = "tandem",
  K = 2
)

new_illus <- data.frame(
  PetalSize = cut(iris$Petal.Length, 
                  breaks = c(0, 2, 5, 7), 
                  labels = c("Small", "Medium", "Large"))
)

# Prédire
predictions <- model_tandem$predict(new_illus)
```

**Critical Notes**: 
- Using `set.seed(123)` for the model ensures balanced cluster assignment (avoids 147 vs 3 observation distributions)
- Using `set.seed(456)` for the illustrative variable ensures balanced color distribution
- Without proper seeds, you may get NaN values due to highly unbalanced distributions

**Output Structure** (different from VAR_CAH/VAR_KMEANS):

```r
str(predictions_tandem$Color)
# List of 8
#  $ contingency              : table [3, 3] 
#  $ percentages_by_modality  : num [3, 3]
#  $ percentages_by_cluster   : num [3, 3]
#  $ chi2_test                :List of 3
#   ..$ statistic: num 2.47
#   ..$ p.value  : num 0.651
#   ..$ df       : int 4
#  $ cramers_v                : num 0.091
#  $ significant              : logi FALSE
#  $ interpretation           : chr "No significant association"
#  $ dice_scores              : num [150, 3]
```

**Note**: Values shown are with `set.seed(456)`. Without `set.seed()`, you may get different values, including NaN for chi-square statistics if the random variable creates a highly unbalanced distribution.

**Interpretation**:
- `contingency`: Cross-table modalities × clusters
- `cramers_v`: Association strength (0 = none, 1 = perfect)
- `significant`: Chi-square test result (α = 0.05)
- `dice_scores`: Dice similarity between each observation and each cluster

**Example**:
```r
predictions_tandem$Color$cramers_v
# [1] 0.091  # (with set.seed(456))

# Interpretation: Very weak association between Color and the modality clustering
# Color doesn't discriminate between the 3 clusters (expected for random variable)

predictions_tandem$Color$chi2_test$p.value
# [1] 0.651  # (with set.seed(456))

# p > 0.05 → No significant association (expected for random variable)
```

**Important Note**: Since `Color` is created randomly with `sample()`, it's normal to find no significant association with the data-driven clusters. For a real analysis, use an illustrative variable that has meaningful relationship with your data (see commented Option 2 in code above).

### Modifying the Number of Clusters

The `K` property is **active**: setting it automatically refits the model.

```r
# Initial fit with K = 2
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 20)

# Change to K = 3 (automatic refit)
model$K <- 3

# Verify
print(model$K)
# [1] 3

model$summary()
# Now shows 3 clusters
```

**Important notes**:
- Modifying K calls `fit()` again internally
- For VAR_KMEANS: uses existing data (stored internally)
- All previously computed metrics are updated

**Example workflow**:
```r
# Initialize model
set.seed(456)  # For reproducibility
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 20)

# Compare multiple K values
for (k in 2:5) {
  model$K <- k
  cat("K =", k, "| Homogeneity =", round(model$Homogeneite, 3), "\n")
}

# Example output (values may vary):
# K = 2 | Homogeneity = 0.776
# K = 3 | Homogeneity = 0.814
# K = 4 | Homogeneity = 0.861
# K = 5 | Homogeneity = 0.895

# Note: Homogeneity often increases with K (more clusters = better fit)
# but this is not always monotonic. Choose K based on interpretability.
```

---

## Advanced User Guide

This section is for users who want direct control via R6 classes.

### Direct R6 Class Usage

```r
# Instead of roller_clust(), instantiate directly
model <- VAR_CAH$new(K = 3, scale = TRUE)

# Fit manually
model$fit(iris[, 1:4])

# Access all methods
model$summary()
groups <- model$Groupes
```

**Advantages**:
- Access to all methods (not just public ones)
- More control over initialization
- Allows method chaining

**Disadvantages**:
- More verbose
- Must remember to call `$fit()` explicitly

### Advanced Methods by Algorithm

#### VAR_CAH Advanced Methods

```r
model_cah <- VAR_CAH$new(K = 3)
model_cah$fit(iris[, 1:4])

# Get hierarchical tree
tree <- model_cah$get_tree()
# hclust object → can be used with plot(), cutree(), etc.

# Plot dendrogram
plot(tree, main = "Variable Dendrogram")
rect.hclust(tree, k = 3, border = "red")

# Get variables in specific cluster
vars_cluster_1 <- model_cah$get_cluster_variables(cluster_id = 1)
# Character vector: c("Sepal.Length", "Petal.Length", "Petal.Width")

# Get synthetic variable (1st PC) for a cluster
centers <- model_cah$get_cluster_centers()
# Matrix with 1 column per cluster

# Get quality metrics
quality <- model_cah$get_quality_metrics()
# List with:
# $intra       : Within-cluster inertia
# $inter       : Between-cluster inertia
# $pct_expliquee: % variance explained by clustering
```

#### VAR_KMEANS Advanced Methods

```r
model_km <- VAR_KMEANS$new(K = 3, n_init = 20, max_iter = 100)
model_km$fit(iris[, 1:4])

# Get cluster centers (1st PCs)
centers <- model_km$get_cluster_centers()
# Matrix: observations × K clusters
# Each column is the 1st PC of a cluster

# Access convergence information
converged <- model_km$Converged
# Logical: TRUE if algorithm converged

n_iterations <- model_km$NIterations
# Integer: number of iterations until convergence

# Get variables in specific cluster
vars_cluster_2 <- model_km$get_cluster_variables(cluster_id = 2)

# Manual prediction on specific variable
new_var <- iris$Sepal.Length + iris$Petal.Length
pred_result <- model_km$predict(data.frame(Combined = new_var))
print(pred_result$Combined$cluster)
# Assigned cluster

# Access homogeneity by cluster
summary_output <- capture.output(model_km$summary())
# Parse to extract per-cluster homogeneity
```

#### TandemVarClust Advanced Methods

```r
model_tandem <- TandemVarClust$new(K = 3, n_bins = 5, n_factors = 3)
model_tandem$fit(iris_mixed)

# Get summary by original variable
var_summary <- model_tandem$get_variable_summary()
# Shows which modalities belong to which clusters

# Get modalities of a specific variable
mods <- model_tandem$get_modalities_of_variable("Sepal.Length")
# Character vector of modality names

# Get modalities in a specific cluster
cluster_mods <- model_tandem$get_modalities_of_cluster(cluster_id = 1)
# All modalities assigned to cluster 1

# Access factorial coordinates
coords <- model_tandem$FactorialCoords
# Matrix of modality coordinates in factorial space

# Access disjunctive table
disj_table <- model_tandem$DisjunctiveTable
# Complete disjunctive table (one-hot encoded)

# Variance explained by factorial axes
var_exp <- model_tandem$VarianceExplained
# Percentage of variance explained by each MCA axis

# Check results integrity
model_tandem$check_results_integrity()
# Validates internal consistency
```

### Fine-Tuning Parameters

#### VAR_CAH Parameters

```r
model <- VAR_CAH$new(
  K = 3,
  scale = TRUE,          # Standardize variables
  max.iter = 100,        # Max iterations (unused in current implementation)
  tolerance = 1e-6       # Convergence tolerance (unused in current implementation)
)
```

#### VAR_KMEANS Parameters

```r
model <- VAR_KMEANS$new(
  K = 3,
  n_init = 20,           # Number of random initializations (recommended: 10-50)
                         # More → better solution, slower
  max_iter = 100,        # Maximum iterations per run
                         # Usually converges in < 50 iterations
  tolerance = 1e-6,      # Convergence tolerance (change in criterion)
  scale = TRUE           # Standardize variables (recommended)
)

# Example: More thorough search
model_thorough <- VAR_KMEANS$new(K = 4, n_init = 50, max_iter = 200)
model_thorough$fit(iris[, 1:4])
# More likely to find global optimum (but slower)

# Example: Quick exploration
model_quick <- VAR_KMEANS$new(K = 4, n_init = 5, max_iter = 50)
model_quick$fit(iris[, 1:4])
# Faster, but may miss best solution
```

**Choosing n_init**:
- **5-10**: Quick exploration
- **20 (default)**: Good balance
- **50+**: Thorough search for important analyses

**Note**: VAR_KMEANS maximizes sum of r², so higher criterion = better

#### TandemVarClust Parameters

```r
model <- TandemVarClust$new(
  K = 3,
  n_bins = 5,            # Number of bins for discretization
                         # 3-5: Recommended
                         # 7-10: Fine granularity (more modalities)
  n_factors = NULL,      # Number of MCA factors to retain
                         # NULL: Automatic (based on Kaiser criterion)
                         # 2-5: Typical range
  scale = TRUE           # Scale numeric variables before discretization
)

# Example: More modalities
model_fine <- TandemVarClust$new(K = 4, n_bins = 7)
model_fine$fit(iris[, 1:4])
# More nuanced clustering but higher complexity

# Example: Fewer modalities
model_simple <- TandemVarClust$new(K = 3, n_bins = 3)
model_simple$fit(iris[, 1:4])
# Simpler, more interpretable
```

**Choosing n_bins**:
- **3**: Very coarse (low/medium/high)
- **5 (default)**: Good balance
- **7-10**: Fine granularity (use if many observations)

### In-Depth Results Analysis

#### Accessing Results Programmatically

```r
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 3, n_init = 20)

# Variable assignments
groups <- model$Groupes
# Named integer vector
# Example: c(Sepal.Length = 1, Sepal.Width = 2, Petal.Length = 1, Petal.Width = 1)

# Identify which variables are in cluster k
vars_in_cluster_1 <- names(groups)[groups == 1]
# Character vector: c("Sepal.Length", "Petal.Length", "Petal.Width")

# Count variables per cluster
table(groups)
# groups
# 1 2 
# 3 1

# Global metrics
homogeneity <- model$Homogeneite
# Numeric: mean r² across all variables

criterion <- model$WithinClusterInertia
# Numeric: sum of r² (VAR_KMEANS maximizes this)

# Convergence (VAR_KMEANS only)
converged <- model$Converged
n_iter <- model$NIterations
```

#### Creating Custom Summaries

```r
# Function to summarize each cluster
summarize_cluster <- function(model, cluster_id) {
  vars <- model$get_cluster_variables(cluster_id)
  
  cat("Cluster", cluster_id, ":\n")
  cat("  Variables (", length(vars), "):", paste(vars, collapse = ", "), "\n")
  
  # For VAR_KMEANS, homogeneity is r²
  # Extract from summary (not directly exposed)
  cat("\n")
}

# Apply to all clusters
for (k in 1:model$K) {
  summarize_cluster(model, k)
}
```

#### Comparing Models

```r
# Compare VAR_CAH vs VAR_KMEANS
model_cah <- roller_clust(iris[, 1:4], "var_cah", K = 3)
model_km <- roller_clust(iris[, 1:4], "var_kmeans", K = 3, n_init = 20)

cat("VAR_CAH Homogeneity:", model_cah$Homogeneite, "\n")
cat("VAR_KMEANS Homogeneity:", model_km$Homogeneite, "\n")

# Compare groupings
table(model_cah$Groupes, model_km$Groupes)
#     1 2 3
# 1   3 0 0
# 2   0 1 0

# Interpretation: Both methods agree on variable assignments
```

---

## Detailed Use Cases

### Use Case 1: Multicollinearity Detection in Regression

**Problem**: You have 15 predictors with suspected multicollinearity.

```r
# Your dataset
data <- read.csv("regression_data.csv")
predictors <- data[, 1:15]

# Cluster variables
model <- roller_clust(predictors, method = "var_kmeans", K = 5, n_init = 20)

# Identify highly correlated groups
model$summary()

# For each cluster, keep only one representative
representatives <- sapply(1:5, function(k) {
  vars <- model$get_cluster_variables(k)
  vars[1]  # Take first variable from each cluster
})

# Build regression with representatives
reduced_data <- data[, c(representatives, "Response")]
lm_model <- lm(Response ~ ., data = reduced_data)
```

**Benefit**: Reduced from 15 to 5 predictors while preserving information.

### Use Case 2: Survey Data Dimensionality Reduction

**Problem**: Customer survey with 40 Likert-scale questions.

```r
survey_data <- read.csv("survey.csv")

# Cluster questions
model <- roller_clust(survey_data, method = "var_kmeans", K = 6, n_init = 20)

# Examine clusters to identify themes
for (k in 1:6) {
  cat("\n=== Theme", k, "===\n")
  vars <- model$get_cluster_variables(k)
  cat("Questions:", paste(vars, collapse = "\n  "), "\n")
}

# Create composite scores
composite_scores <- sapply(1:6, function(k) {
  vars <- model$get_cluster_variables(k)
  rowMeans(survey_data[, vars], na.rm = TRUE)
})
colnames(composite_scores) <- paste0("Theme_", 1:6)

# Use composite scores for subsequent analysis
```

**Benefit**: 40 questions → 6 themes, easier interpretation.

### Use Case 3: Feature Selection for Machine Learning

**Problem**: 100 features, need to reduce before training ML model.

```r
features <- training_data[, 1:100]

# Hierarchical clustering to explore
model_cah <- roller_clust(features, "var_cah", K = 10)
plot(model_cah$get_tree())

# K-means for optimal partitioning
model_km <- roller_clust(features, "var_kmeans", K = 10, n_init = 50)

# Select one variable per cluster (highest correlation with cluster center)
selected_features <- character(10)

for (k in 1:10) {
  vars <- model_km$get_cluster_variables(k)
  
  # Predict each variable's association with its cluster
  preds <- model_km$predict(features[, vars, drop = FALSE])
  
  # Select variable with highest r²
  scores <- sapply(preds, function(p) p$best_score)
  selected_features[k] <- vars[which.max(scores)]
}

# Train ML model with selected features
train_reduced <- training_data[, c(selected_features, "Target")]
rf_model <- randomForest(Target ~ ., data = train_reduced)
```

**Benefit**: Systematic feature reduction with interpretability.

### Use Case 4: Mixed Data Customer Segmentation

**Problem**: Customer database with demographics (categorical) and behavior (numeric).

```r
customers <- read.csv("customers.csv")

# Prepare mixed data
mixed_data <- customers[, c("Age", "Income", "Education", "Region", "Membership")]

# TandemVarClust for mixed data
model <- roller_clust(mixed_data, method = "tandem", K = 4, n_bins = 5)

# Examine modality clusters
model$summary()

# Illustrative analysis: Does "Churn" status relate to clusters?
churn_analysis <- model$predict(data.frame(Churn = customers$Churn))

if (churn_analysis$Churn$significant) {
  cat("Churn is significantly associated with the clustering!\n")
  print(churn_analysis$Churn$contingency)
}
```

**Benefit**: Understand how categorical/numeric variables group in customer base.

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

**Interpreting Global Homogeneity**:

```
Global homogeneity: 0.75 (VAR_KMEANS)
  → On average, variables have r² = 0.75 with their cluster center
  → Good clustering quality
```

**Comparing clusters**:

```
Cluster 1: 5 variables
  Mean homogeneity: 0.82
  → Strong internal coherence

Cluster 2: 4 variables
  Mean homogeneity: 0.71
  → Good coherence

Cluster 3: 3 variables
  Mean homogeneity: 0.68
  → Weaker internal coherence (consider splitting if needed)
```

**Quality benchmarks** (for r² in VAR_KMEANS):
- Homogeneity > 0.7: Good cluster
- Homogeneity 0.5-0.7: Acceptable cluster
- Homogeneity < 0.5: Weak cluster (reconsider K)

#### VAR_KMEANS Summary

VAR_KMEANS uses iterative reallocation to maximize sum of r². The summary shows optimization criterion, convergence status, and final cluster composition.

**Example interpretation**:
```
Sum of r² (criterion): 3.104
  → Total squared correlation across all 4 variables
  → Maximum possible is 4.0 (perfect clustering where each variable r²=1)
  → Achieved 77.6% of maximum (3.104/4.0 = 0.776)

Convergence status: Converged
Number of iterations: 8
  → Algorithm converged in 8 iterations (out of max 100)
  → Quick convergence indicates stable solution

Number of random initializations: 20
  → Tried 20 different starting points
  → Final solution is best among these 20 runs

Global homogeneity (mean r²): 0.776
  → Average r² across all 4 variables (3.104/4 = 0.776)
  → Good clustering quality (>0.7)

Note: Exact values depend on random initialization and data structure.
```

#### TandemVarClust Summary

```
Number of modality clusters: 3
Total number of modalities: 24
  → 4 variables × 5 bins + 1 categorical with 4 levels = 24 modalities

Modalities per cluster:
  Cluster 1: 10 modalities
  Cluster 2: 8 modalities
  Cluster 3: 6 modalities
```

### Predict Output Interpretation

See detailed sections above for:
- [VAR_CAH/VAR_KMEANS predict() interpretation](#predict-for-var_cah-and-var_kmeans)
- [TandemVarClust predict() interpretation](#predict-for-tandemvarclust)

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

### Q: What if predict() gives low scores for all clusters?

**A**: The new variable doesn't fit well into any existing cluster. This could mean:
- The variable captures a different dimension
- The original clustering may need more clusters
- The variable is noisy or poorly related to original variables

### Q: Can I use predict() on a different dataset?

**A**: No. `predict()` requires the **same observations** as training data. It predicts which cluster new **variables** belong to, not new observations.

### Q: How do I handle missing values?

**A**: Set `na.action` parameter:
```r
model <- roller_clust(X, method = "var_cah", K = 3, na.action = "omit")
# "warn": Issue warning (default)
# "omit": Remove observations with NA
# "fail": Stop execution
```

### Q: My TandemVarClust has too many modalities, what should I do?

**A**: Reduce `n_bins` for numeric discretization:
```r
# Instead of n_bins = 7
model <- roller_clust(X, method = "tandem", K = 3, n_bins = 3)
# Fewer bins → fewer modalities → more parsimonious model
```

### Q: Can I extract the synthetic variables from VAR_CAH?

**A**: Not directly exposed via public API in current version, but they're used internally for prediction. You can reconstruct them:
```r
# For cluster k
vars_in_k <- model$get_cluster_variables(k)
X_cluster <- scale(iris[, vars_in_k])
pca <- prcomp(X_cluster)
synthetic_var <- pca$x[, 1]  # First PC
```

### Q: How is VAR_KMEANS different from standard K-means?

**A**: VAR_KMEANS clusters **variables** (not observations) using the Vigneau & Qannari algorithm. Cluster centers are represented by 1st principal components, and the algorithm maximizes sum of squared correlations (r²) instead of minimizing distances.

### Q: How is TandemVarClust different from standard MCA + HAC?

**A**: TandemVarClust operates at the **modality level**, not observation level. Observations are then assigned to modality clusters using the Dice similarity index, providing a mathematically sound assignment method.

### Q: Can I save and reload models?

**A**: Yes, use standard R serialization:
```r
# Save
saveRDS(model, "my_model.rds")

# Load
model <- readRDS("my_model.rds")
model$summary()  # Works!
```

### Q: The package gave me a warning about scaling. Should I always scale?

**A**: Generally yes (`scale = TRUE`) unless:
- Variables are already on the same scale
- You specifically want to preserve original scale differences
- You're using TandemVarClust with only categorical variables

### Q: Can I perform variable clustering on a subset of variables?

**A**: Yes, just subset your data frame:
```r
model <- roller_clust(iris[, c(1, 3, 4)], method = "var_cah", K = 2)
# Clusters only Sepal.Length, Petal.Length, Petal.Width
```

### Q: Why does VAR_KMEANS give different results each time?

**A**: VAR_KMEANS uses random initialization. Use `set.seed()` for reproducibility:
```r
set.seed(123)
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 3, n_init = 20)
# Results will be identical on each run with same seed
```

---

## Additional Resources

- **Package documentation**: `help(package = "RollerClustR")`
- **Function help**: `?roller_clust`, `?VAR_CAH`, `?VAR_KMEANS`, `?TandemVarClust`
- **GitHub repository**: https://github.com/RomainBuono/RollerClustR
- **Report issues**: https://github.com/RomainBuono/RollerClustR/issues

**Key References**:
- Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012). ClustOfVar: An R Package for the Clustering of Variables. *Journal of Statistical Software*, 50(13), 1-16.

---

**End of User Guide**