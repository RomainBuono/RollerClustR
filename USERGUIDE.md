# RollerClustR User Guide

**Version**: 1.0.0  
**Author**: Bryan Mentos Vendetta  
**Date**: November 2024

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

**RollerClustR** is an R package dedicated to **variable clustering** (not observation clustering). Unlike classical methods that group individuals, RollerClustR identifies groups of similar or redundant variables in your data.

### Why Cluster Variables?

- **Dimensionality reduction**: Identify groups of correlated variables
- **Variable selection**: Choose one representative per cluster
- **Multicollinearity detection**: Spot redundancies
- **Interpretability**: Understand latent data structure

### The Three Algorithms

| Algorithm | Data Type | Principle | Recommended Use |
|-----------|-----------|-----------|-----------------|
| **VAR_CAH** | Numeric | Hierarchical agglomerative | Exploration, visual dendrograms |
| **VARCLUS** | Numeric | Divisive with PCA | Complex structures, optimized splitting |
| **TandemVarClust** | Mixed | MCA + HAC on modalities | Categorical or mixed data |

---

## Installation and Setup

### Installation

```r
# From GitHub (recommended)
install.packages("devtools")
devtools::install_github("BryanMentos/RollerClustR")

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
1. Creates an object of the appropriate class (VAR_CAH, VARCLUS, or TandemVarClust)
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

#### Method 2: VARCLUS (Divisive Clustering with PCA)

**Principle**: Top-down approach starting with all variables and recursively splitting.

```r
model_vc <- roller_clust(
  X = iris[, 1:4],
  method = "varclus",
  K = 3,                    # K is often ignored - algorithm determines optimal K
  stop_eigenvalue = 1.0     # Stopping criterion (λ₂ threshold)
)
```

**When to use**:
- Complex hierarchical structures
- Need for automatic cluster determination
- PCA-based interpretability
- Medium to large datasets

**Key features**:
- Uses Varimax rotation
- Automatic stopping based on eigenvalue threshold
- Recursive splitting process

#### Method 3: TandemVarClust (MCA + HAC for Mixed Data)

**Principle**: Combines Multiple Correspondence Analysis with Hierarchical Clustering on modalities.

```r
# Create mixed data
iris_mixed <- iris
iris_mixed$Species_char <- as.character(iris$Species)
iris_mixed$Size <- cut(iris$Sepal.Length, 3, labels = c("S", "M", "L"))

model_tandem <- roller_clust(
  X = iris_mixed[, c(1:4, 6, 7)],
  method = "tandem",
  K = 3,
  n_bins = 5,              # Number of bins for numeric discretization
  n_factors = 3            # Number of factorial axes to retain
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

### Typical Workflow: fit() and summary()

#### Step 1: Fit the Model

With `roller_clust()`, fitting is automatic:

```r
model <- roller_clust(X = iris[, 1:4], method = "var_cah", K = 2)
# Model is already fitted!
```

For advanced users using R6 classes directly:

```r
model <- VAR_CAH$new(K = 2)
model$fit(iris[, 1:4])  # Explicit fit
```

**What happens during fit()**:
- Data validation and cleaning
- Missing value handling (based on `na.action` parameter)
- Scaling/standardization (if `scale = TRUE`)
- Algorithm execution
- Cluster evaluation and quality metrics computation

#### Step 2: Examine Results with summary()

```r
model$summary()
```

**Example output for VAR_CAH**:

```
===============================================================
   VAR_CAH - Hierarchical Variable Clustering Summary
===============================================================

Number of clusters      : 2
Number of variables     : 4
Overall homogeneity     : 0.8523

Details by Cluster:
  Cluster Nb_Vars Mean_Homogeneity
1       1       3           0.8912
2       2       1           0.7134

Cluster 1: 3 variables
  Sepal.Length, Petal.Length, Petal.Width
  Mean intra-cluster correlation: 0.8912

Cluster 2: 1 variable
  Sepal.Width
```

**Understanding summary() output**:
- **Number of clusters**: Your specified K
- **Number of variables**: Total variables in dataset
- **Overall homogeneity**: Average intra-cluster correlation (higher is better)
- **Per-cluster details**: 
  - Number of variables in each cluster
  - Mean correlation within cluster
  - Variable names

### Illustrative Variables: predict()

The `predict()` method assigns **new variables** to existing clusters without refitting the entire model. This is useful for:
- Testing if a derived variable belongs to an existing cluster
- Analyzing illustrative variables
- Validating cluster stability

#### predict() for VAR_CAH and VARCLUS

**Example**:

```r
# Fit model on original variables
model <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)

# Create new derived variables
new_vars <- data.frame(
  SumPetals = iris$Petal.Length + iris$Petal.Width,
  RatioPetals = iris$Petal.Length / iris$Petal.Width,
  SumSepals = iris$Sepal.Length + iris$Sepal.Width
)

# Predict cluster membership
predictions <- model$predict(new_vars)
```

**Output Structure**:

```r
str(predictions)
# List of 4
#  $ cluster          : Named int [1:3] 1 1 1
#  $ scores           : num [1:3, 1:2] 0.985 0.892 0.654 0.351 0.213 ...
#  $ best_score       : num [1:3] 0.985 0.892 0.654
#  $ second_best_score: num [1:3] 0.351 0.213 0.478
```

**Interpreting predict() output**:

1. **`$cluster`**: Assigned cluster for each new variable
   ```r
   predictions$cluster
   # SumPetals RatioPetals SumSepals 
   #         1           1         1
   ```

2. **`$scores`**: Correlation matrix with cluster synthetic variables
   ```r
   predictions$scores
   #             Cluster_1 Cluster_2
   # SumPetals      0.9845    0.3512
   # RatioPetals    0.8923    0.2134
   # SumSepals      0.6541    0.4782
   ```
   - **Interpretation**: Higher score = stronger association
   - **Range**: [0, 1] (absolute correlations)
   - **Assignment rule**: Highest score determines cluster

3. **`$best_score`**: Confidence metric
   ```r
   predictions$best_score
   # SumPetals RatioPetals SumSepals 
   #    0.9845      0.8923    0.6541
   ```
   - Values close to 1 indicate strong assignment
   - Values < 0.6 suggest weak cluster affiliation

4. **`$second_best_score`**: Ambiguity assessment
   ```r
   predictions$second_best_score
   # SumPetals RatioPetals SumSepals 
   #    0.3512      0.2134    0.4782
   ```
   - **Gap analysis**: `best_score - second_best_score`
   - Large gap (> 0.3) indicates clear assignment
   - Small gap (< 0.1) suggests ambiguous assignment

**Example interpretation**:

```r
# SumPetals analysis
# best_score = 0.9845, second_best = 0.3512
# Gap = 0.6333 → Strong, unambiguous assignment to Cluster 1

# SumSepals analysis
# best_score = 0.6541, second_best = 0.4782
# Gap = 0.1759 → Weaker, more ambiguous assignment
```

#### predict() for TandemVarClust

For categorical illustrative variables, `predict()` performs association analysis:

```r
# Fit tandem model
model_tandem <- roller_clust(iris_mixed[, c(1:4, 6)], method = "tandem", K = 3)

# Create illustrative categorical variable
new_cat <- data.frame(
  ColorGroup = sample(c("Red", "Blue", "Green"), 150, replace = TRUE)
)

# Predict association
predictions_tandem <- model_tandem$predict(new_cat)
```

**Output Structure** (different from VAR_CAH/VARCLUS):

```r
str(predictions_tandem)
# List of 1
#  $ ColorGroup:List of 7
#   ..$ contingency            : table [1:3, 1:3]
#   ..$ percentages_by_modality: num [1:3, 1:3]
#   ..$ percentages_by_cluster : num [1:3, 1:3]
#   ..$ chi2_test              : 'htest' object
#   ..$ cramers_v              : num 0.234
#   ..$ significant            : logi TRUE
#   ..$ dice_scores            : num [1:150, 1:3]
```

**Interpreting TandemVarClust predict() output**:

1. **Contingency table**: Cross-tabulation of modalities vs clusters
   ```r
   predictions_tandem$ColorGroup$contingency
   #       Cluster_1 Cluster_2 Cluster_3
   # Red          15        12        23
   # Blue         18        20        12
   # Green        17        18        15
   ```

2. **Chi-square test**: Tests independence between variable and clusters
   ```r
   predictions_tandem$ColorGroup$chi2_test
   # X-squared = 5.234, df = 4, p-value = 0.264
   ```
   - p < 0.05: Significant association (variable discriminates clusters)
   - p ≥ 0.05: No significant association

3. **Cramer's V**: Effect size measure [0, 1]
   ```r
   predictions_tandem$ColorGroup$cramers_v
   # [1] 0.234
   ```
   - < 0.1: Weak association
   - 0.1-0.3: Moderate association
   - > 0.3: Strong association

4. **Percentages by cluster**: How each cluster distributes across modalities
   ```r
   predictions_tandem$ColorGroup$percentages_by_cluster
   #       Cluster_1 Cluster_2 Cluster_3
   # Red       30.0      24.0      46.0
   # Blue      36.0      40.0      24.0
   # Green     34.0      36.0      30.0
   ```

### Modifying the Number of Clusters

You can dynamically change K after fitting, which automatically refits the model:

```r
model <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)
model$summary()  # 2 clusters

# Change to 3 clusters
model$K <- 3
model$summary()  # 3 clusters - model refitted automatically!
```

**What happens**:
- Model stores original data
- Refits clustering with new K
- Updates all metrics and assignments
- Previous predictions become invalid (rerun if needed)

---

## Advanced User Guide

This section is for users who need fine-grained control and want to use R6 class methods directly.

### Direct R6 Class Usage

Instead of `roller_clust()`, you can instantiate classes directly:

```r
# Create object without fitting
model <- VAR_CAH$new(K = 3, scale = TRUE)

# Fit explicitly
model$fit(iris[, 1:4])

# Access advanced methods
tree <- model$get_tree()
rep_var <- model$get_representative_variable(cluster_id = 1)
```

**Advantages**:
- More control over initialization
- Access to class-specific methods
- Better for programming/automation

### Advanced Methods by Algorithm

#### VAR_CAH Advanced Methods

```r
model_cah <- VAR_CAH$new(K = 3)
model_cah$fit(iris[, 1:4])

# Get hierarchical tree (hclust object)
tree <- model_cah$get_tree()
plot(tree)

# Get variables in a specific cluster
vars_cluster1 <- model_cah$get_cluster_variables(cluster_id = 1)
# [1] "Sepal.Length" "Petal.Length" "Petal.Width"

# Get most representative variable of a cluster
# (highest correlation with cluster's synthetic variable)
rep_var <- model_cah$get_representative_variable(cluster_id = 1)
# [1] "Petal.Length"

# Compute inertia decomposition
inertia <- model_cah$inertie()
# $totale      : Total variance
# $intra       : Within-cluster inertia
# $inter       : Between-cluster inertia
# $pct_expliquee: % variance explained by clustering
```

#### VARCLUS Advanced Methods

```r
model_vc <- VARCLUS$new(stop_eigenvalue = 1.0)
model_vc$fit(iris[, 1:4])

# Access division tree structure
tree <- model_vc$get_tree()

# Get iteration history
iterations <- model_vc$get_iterations()
# Data frame with splitting history

# Examine specific cluster's PCA
cluster_vars <- names(model_vc$Groupes)[model_vc$Groupes == 1]
# Perform custom analysis on these variables
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

#### VARCLUS Parameters

```r
model <- VARCLUS$new(
  stop_eigenvalue = 1.0  # Stop splitting when λ₂ < threshold
                         # Lower value → more clusters
                         # Higher value → fewer clusters
)

# Example: More aggressive splitting
model_fine <- VARCLUS$new(stop_eigenvalue = 0.7)
model_fine$fit(iris[, 1:4])
# Likely produces more clusters
```

**Choosing stop_eigenvalue**:
- **1.0 (default)**: Standard Kaiser criterion
- **0.7-0.9**: More clusters, finer granularity
- **> 1.0**: Fewer clusters, broader groupings

#### TandemVarClust Parameters

```r
model <- TandemVarClust$new(
  K = 3,
  n_bins = 5,           # Discretization bins for numeric variables
  method_cah = "ward.D2", # Linkage method: "ward.D2", "complete", "average", etc.
  n_factors = NULL,     # Factorial axes to retain (NULL = all axes with λ > 1/p)
  scale = TRUE,         # Standardize before discretization
  na_action = "omit"    # "omit" or "fail"
)
```

**Parameter guidance**:
- **n_bins**: 
  - 3-5: Coarse discretization, more robust to outliers
  - 5-7: Standard choice
  - 7-10: Fine discretization, captures more nuance
  
- **method_cah**:
  - "ward.D2": Minimizes within-cluster variance (default, usually best)
  - "complete": Max distance between clusters
  - "average": Average linkage (UPGMA)

- **n_factors**:
  - NULL: Automatic (keeps axes with eigenvalue > 1/p)
  - Integer: Manual selection (e.g., 3 for first 3 axes)

### In-Depth Results Analysis

#### Analyzing Cluster Quality

```r
model <- roller_clust(iris[, 1:4], method = "var_cah", K = 3)

# Overall homogeneity
# Higher = variables within clusters are more similar
overall_homog <- mean(sapply(1:3, function(k) {
  vars <- model$get_cluster_variables(k)
  if (length(vars) > 1) {
    cor_matrix <- cor(iris[, vars])
    mean(cor_matrix[upper.tri(cor_matrix)])
  } else {
    NA
  }
}), na.rm = TRUE)

# Silhouette-like analysis for variables
# Compare within-cluster vs between-cluster similarities
```

#### Comparing Methods

```r
# Fit all three methods
model_cah <- roller_clust(iris[, 1:4], "var_cah", K = 3)
model_vc <- roller_clust(iris[, 1:4], "varclus", K = 3)

# Compare groupings
table(model_cah$Groupes, model_vc$Groupes)

# Compare predictions on same new variables
new_vars <- data.frame(Sum = iris[,1] + iris[,3])
pred_cah <- model_cah$predict(new_vars)
pred_vc <- model_vc$predict(new_vars)

# Agreement?
pred_cah$cluster == pred_vc$cluster
```

---

## Detailed Use Cases

### Use Case 1: Feature Selection for Regression

```r
# Problem: Too many predictors, multicollinearity issues
data(mtcars)

# Step 1: Cluster variables
model <- roller_clust(mtcars[, -1], method = "var_cah", K = 4)
model$summary()

# Step 2: Select one representative per cluster
representatives <- sapply(1:4, function(k) {
  model$get_representative_variable(k)
})

# Step 3: Build regression with selected variables
selected_vars <- representatives
formula_str <- paste("mpg ~", paste(selected_vars, collapse = " + "))
lm_model <- lm(as.formula(formula_str), data = mtcars)
summary(lm_model)
```

### Use Case 2: Survey Data Reduction

```r
# Problem: 50 survey questions, need to reduce to key themes

# Cluster questions
model <- roller_clust(survey_data, method = "varclus", stop_eigenvalue = 1.0)

# Examine cluster composition
for (k in unique(model$Groupes)) {
  cat("\n\nTheme", k, ":\n")
  vars <- names(model$Groupes)[model$Groupes == k]
  cat(paste(vars, collapse = "\n"))
}

# Create theme scores (average of variables in cluster)
theme_scores <- sapply(unique(model$Groupes), function(k) {
  vars <- names(model$Groupes)[model$Groupes == k]
  rowMeans(survey_data[, vars], na.rm = TRUE)
})
colnames(theme_scores) <- paste0("Theme_", unique(model$Groupes))
```

### Use Case 3: Mixed Data Market Segmentation

```r
# Problem: Customer data with demographics (categorical) and behavior (numeric)

model <- roller_clust(
  customer_data,
  method = "tandem",
  K = 4,
  n_bins = 5
)

# Examine modality clusters
for (k in 1:4) {
  cat("\n\nSegment", k, ":\n")
  mods <- model$get_modalities_of_cluster(k)
  cat(paste(mods, collapse = "\n"))
}

# Profile new customers
new_customers <- data.frame(
  PurchaseFrequency = factor(c("High", "Low", "Medium")),
  Region = factor(c("North", "South", "East"))
)

profile <- model$predict(new_customers)
# Examine chi-square tests and Cramer's V for association strength
```

---

## Interpreting Outputs

### Summary Output Interpretation

#### VAR_CAH Summary

```
Number of clusters: 3
Number of variables: 10
Overall homogeneity: 0.78

Cluster 1: 4 variables
  Mean homogeneity: 0.85
  → Strong internal coherence

Cluster 2: 3 variables
  Mean homogeneity: 0.72
  → Moderate internal coherence

Cluster 3: 3 variables
  Mean homogeneity: 0.68
  → Weaker internal coherence (consider splitting if needed)
```

**Quality benchmarks**:
- Homogeneity > 0.7: Good cluster
- Homogeneity 0.5-0.7: Acceptable cluster
- Homogeneity < 0.5: Weak cluster (reconsider K)

#### VARCLUS Summary

VARCLUS determines K automatically based on eigenvalue criterion. The summary shows the recursive splitting process and final cluster composition.

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
- [VAR_CAH/VARCLUS predict() interpretation](#predict-for-var_cah-and-varclus)
- [TandemVarClust predict() interpretation](#predict-for-tandemvarclust)

**Key principle**: High scores/strong associations indicate that the illustrative variable "belongs" to that cluster conceptually.

---

## FAQ and Troubleshooting

### Q: When should I use which method?

**A**:
- **All numeric variables + want dendrogram visualization** → VAR_CAH
- **All numeric variables + complex structure** → VARCLUS
- **Categorical or mixed data** → TandemVarClust
- **Not sure** → Try VAR_CAH first (most straightforward)

### Q: How do I choose K?

**A**:
- **VAR_CAH**: Plot dendrogram (`plot(model$get_tree())`), look for natural cuts
- **VARCLUS**: Let algorithm decide (it uses eigenvalue criterion)
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

---

## Additional Resources

- **Package documentation**: `help(package = "RollerClustR")`
- **Function help**: `?roller_clust`, `?VAR_CAH`, `?VARCLUS`, `?TandemVarClust`
- **GitHub repository**: https://github.com/BryanMentos/RollerClustR
- **Report issues**: https://github.com/BryanMentos/RollerClustR/issues

---

**End of User Guide**
