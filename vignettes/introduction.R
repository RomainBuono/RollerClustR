## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(RollerClustR)

## ----basic_usage--------------------------------------------------------------
# Load iris data
data(iris)

# Clustering with VAR_CAH
result <- roller_clust(
  X = iris[, 1:4],
  method = "var_cah",
  K = 2,
  scale = TRUE
)

# Display summary
result$summary()

# Access groups
print(result$Groupes)

## ----r6_usage-----------------------------------------------------------------
# Create a VAR_CAH object
model <- VAR_CAH$new(K = 2, scale = TRUE)

# Fit on data
model$fit(iris[, 1:4])

# Display summary
model$summary()

# Modify number of clusters
model$K <- 3

## ----iris_exploration---------------------------------------------------------
# View data structure
str(iris[, 1:4])

# Correlation matrix
cor(iris[, 1:4])

## ----iris_clustering----------------------------------------------------------
# Method 1: VAR_CAH (hierarchical)
model_cah <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)
cat("\n=== VAR_CAH ===\n")
print(model_cah$Groupes)

# Method 2: VAR_KMEANS (iterative optimization)
model_kmeans <- roller_clust(
  X = iris[, 1:4], 
  method = "var_kmeans", 
  K = 2,
  n_init = 10  # 10 random initializations for robustness
)
cat("\n=== VAR_KMEANS ===\n")
print(model_kmeans$Groupes)
cat("Within-cluster inertia:", model_kmeans$WithinClusterInertia, "\n")
cat("Converged:", model_kmeans$Converged, "\n")
cat("Iterations:", model_kmeans$NIterations, "\n")

## ----iris_comparison----------------------------------------------------------
# Compare assignments
comparison <- data.frame(
  Variable = names(iris[, 1:4]),
  VAR_CAH = model_cah$Groupes,
  VAR_KMEANS = model_kmeans$Groupes
)
print(comparison)

# Agreement between methods
cat("\nContingency table:\n")
print(table(model_cah$Groupes, model_kmeans$Groupes))

## ----kmeans_optimization------------------------------------------------------
# Compare different numbers of initializations
results <- data.frame(
  n_init = c(1, 5, 10, 20),
  inertia = numeric(4),
  iterations = numeric(4)
)

for (i in 1:4) {
  model <- roller_clust(
    iris[, 1:4], 
    method = "var_kmeans", 
    K = 2,
    n_init = results$n_init[i]
  )
  results$inertia[i] <- model$WithinClusterInertia
  results$iterations[i] <- model$NIterations
}

print(results)
cat("\nNote: More initializations generally lead to better (lower) inertia\n")

## ----iris_mixed_example-------------------------------------------------------
# Create mixed data from iris
iris_mixed <- iris
iris_mixed$Size <- cut(iris$Sepal.Length, 
                       breaks = 3, 
                       labels = c("Small", "Medium", "Large"))
iris_mixed$PetalSize <- cut(iris$Petal.Length,
                             breaks = 3,
                             labels = c("Short", "Medium", "Long"))

# Select numeric and categorical variables
data_mixed <- iris_mixed[, c("Sepal.Width", "Petal.Width", "Size", "PetalSize")]

# Use TandemVarClust for mixed data
model_tandem <- roller_clust(
  X = data_mixed,
  method = "tandem",
  K = 2,
  n_bins = 3
)

cat("\n=== TandemVarClust - Modality Clustering ===\n")
print(model_tandem$Groupes)

cat("\n=== Summary by Original Variable ===\n")
print(model_tandem$get_variable_summary())

## ----tandem_predict-----------------------------------------------------------
# After fitting the model, analyze Species as an illustrative variable
model_tandem <- TandemVarClust$new(K = 2, n_bins = 3)
model_tandem$fit(data_mixed)

# Analyze Species association with clusters
results <- model_tandem$predict(newdata = data.frame(Species = iris$Species))

cat("\n=== Species Association with Clusters ===\n")
cat("Cramer's V:", results$Species$cramers_v, "\n")
cat("p-value (Chi²):", results$Species$chi2_test$p.value, "\n")
cat("Significant association:", results$Species$significant, "\n")

# Contingency table
cat("\nContingency table:\n")
print(results$Species$contingency)

## ----predict_example----------------------------------------------------------
# Fit model on iris data
model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2, n_init = 10)

# Create a new variable (linear combination)
new_var <- data.frame(
  NewVariable = iris$Sepal.Length + iris$Petal.Length
)

# Predict cluster assignment
prediction <- model$predict(new_var)

cat("New variable assigned to cluster:", prediction$NewVariable$cluster, "\n")
cat("Correlation with cluster center:", prediction$NewVariable$best_score, "\n")
cat("\nScores with all clusters:\n")
print(prediction$NewVariable$scores)

## ----dynamic_k----------------------------------------------------------------
# Create a model with K=2
model <- VAR_CAH$new(K = 2)
model$fit(iris[, 1:4])

cat("With K=2:\n")
print(model$Groupes)

# Change K (automatically refits)
model$K <- 3

cat("\nAfter modification to K=3:\n")
print(model$Groupes)

## ----dynamic_k_kmeans,eval=FALSE----------------------------------------------
# # VAR_KMEANS also supports dynamic K modification
# model_kmeans <- VAR_KMEANS$new(K = 2, n_init = 10)
# model_kmeans$fit(iris[, 1:4])
# 
# cat("With K=2:\n")
# print(model_kmeans$Groupes)
# cat("Inertia:", model_kmeans$WithinClusterInertia, "\n\n")
# 
# # Change K
# model_kmeans$K <- 3
# 
# cat("After modification to K=3:\n")
# print(model_kmeans$Groupes)
# cat("Inertia:", model_kmeans$WithinClusterInertia, "\n")

## ----dynamic_k_tandem---------------------------------------------------------
# TandemVarClust uses refit_with_k() to change K
model_tandem <- TandemVarClust$new(K = 2, n_bins = 3)
model_tandem$fit(data_mixed)

cat("With K=2:\n")
cat("Number of clusters:", model_tandem$K, "\n")

# Change K
model_tandem$refit_with_k(3)

cat("\nAfter refit_with_k(3):\n")
cat("Number of clusters:", model_tandem$K, "\n")

## ----na_handling--------------------------------------------------------------
# Create data with NA
iris_na <- iris[, 1:4]
iris_na[1:5, 1] <- NA

# Option 1: Warning (default)
model_warn <- roller_clust(iris_na, na.action = "warn")

# Option 2: Remove observations with NA
model_omit <- roller_clust(iris_na, na.action = "omit")

# Option 3: Stop if NA
# model_fail <- roller_clust(iris_na, na.action = "fail")  # Generates error

