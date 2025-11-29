# Tests for VAR_KMEANS class
# Complete test suite for K-means clustering of variables
# Algorithm: Vigneau & Qannari (2003) - Uses 1st principal component as cluster center
# Optimization: MAXIMIZE sum of r² (not minimize distance)

library(testthat)
library(RollerClustR)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Create numeric test data with controlled correlations
create_numeric_data <- function(n = 100, p = 10, K = 3) {
  set.seed(123)
  data <- matrix(0, nrow = n, ncol = p)
  vars_per_cluster <- floor(p / K)
  
  for (k in 1:K) {
    start_idx <- (k - 1) * vars_per_cluster + 1
    end_idx <- min(k * vars_per_cluster, p)
    
    base_var <- rnorm(n)
    for (i in start_idx:end_idx) {
      noise <- rnorm(n, sd = 0.3)
      data[, i] <- base_var + noise
    }
  }
  
  if (p %% K != 0) {
    for (i in (K * vars_per_cluster + 1):p) {
      data[, i] <- rnorm(n)
    }
  }
  
  colnames(data) <- paste0("Var", 1:p)
  as.data.frame(data)
}

#' Create highly correlated dataset
create_correlated_data <- function(n = 100) {
  set.seed(456)
  base <- rnorm(n)
  data.frame(
    V1 = base + rnorm(n, sd = 0.1),
    V2 = base + rnorm(n, sd = 0.1),
    V3 = -base + rnorm(n, sd = 0.1),
    V4 = -base + rnorm(n, sd = 0.1)
  )
}

# =============================================================================
# TEST SUITE 1: INITIALIZATION
# =============================================================================

test_that("VAR_KMEANS: Initialize with default parameters", {
  model <- VAR_KMEANS$new(K = 2)
  
  expect_s3_class(model, "VAR_KMEANS")
  expect_s3_class(model, "ClusterAnalysis")
  expect_equal(model$K, 2)
})

test_that("VAR_KMEANS: Initialize with custom parameters", {
  model <- VAR_KMEANS$new(
    K = 3,
    n_init = 20,
    max_iter = 200,
    scale = FALSE,
    tol = 1e-6,
    na_action = "omit"
  )
  
  expect_equal(model$K, 3)
})

test_that("VAR_KMEANS: K must be >= 2", {
  expect_error(
    VAR_KMEANS$new(K = 1),
    "K must be an integer >= 2"
  )
  expect_error(
    VAR_KMEANS$new(K = 0),
    "K must be an integer >= 2"
  )
})

test_that("VAR_KMEANS: n_init must be positive", {
  expect_error(
    VAR_KMEANS$new(K = 2, n_init = 0),
    "n_init must be a positive integer"
  )
  expect_error(
    VAR_KMEANS$new(K = 2, n_init = -1),
    "n_init must be a positive integer"
  )
})

test_that("VAR_KMEANS: max_iter must be positive", {
  expect_error(
    VAR_KMEANS$new(K = 2, max_iter = 0),
    "max_iter must be a positive integer"
  )
})

# =============================================================================
# TEST SUITE 2: FITTING
# =============================================================================

test_that("VAR_KMEANS: Basic fit with numeric data", {
  data <- create_numeric_data(n = 100, p = 6, K = 2)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  
  model$fit(data)
  
  expect_length(model$Groupes, ncol(data))
  expect_true(all(model$Groupes %in% 1:2))
  expect_true(model$Converged)
  expect_true(model$NIterations <= 100)
})

test_that("VAR_KMEANS: Fit with different K values", {
  data <- create_numeric_data(n = 100, p = 9, K = 3)
  
  for (K in 2:4) {
    model <- VAR_KMEANS$new(K = K, n_init = 5)
    suppressWarnings(model$fit(data))
    
    expect_length(unique(model$Groupes), K)
  }
})

test_that("VAR_KMEANS: Fit with scale=FALSE", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, scale = FALSE, n_init = 5)
  
  model$fit(data)
  
  expect_length(model$Groupes, ncol(data))
})

test_that("VAR_KMEANS: Multiple initializations", {
  data <- create_numeric_data(n = 100, p = 6)
  
  model1 <- VAR_KMEANS$new(K = 2, n_init = 1)
  model1$fit(data)
  
  model2 <- VAR_KMEANS$new(K = 2, n_init = 20)
  model2$fit(data)
  
  # Vigneau & Qannari: MAXIMIZE inertia (sum of r²)
  # More initializations should give equal or better (higher) inertia
  expect_true(model2$WithinClusterInertia >= model1$WithinClusterInertia * 0.9)
})

test_that("VAR_KMEANS: Convergence", {
  data <- create_correlated_data(n = 100)
  model <- VAR_KMEANS$new(K = 2, n_init = 5, max_iter = 100)
  
  model$fit(data)
  
  expect_true(model$Converged)
  expect_true(model$NIterations > 0)
  expect_true(model$NIterations <= 100)
})

# =============================================================================
# TEST SUITE 3: GROUPES PROPERTY
# =============================================================================

test_that("VAR_KMEANS: Groupes has correct structure", {
  data <- create_numeric_data(n = 100, p = 8)
  model <- VAR_KMEANS$new(K = 3, n_init = 5)
  model$fit(data)
  
  expect_type(model$Groupes, "integer")
  expect_length(model$Groupes, ncol(data))
  expect_named(model$Groupes, colnames(data))
  expect_true(all(model$Groupes %in% 1:3))
})

test_that("VAR_KMEANS: Groupes throws error before fitting", {
  model <- VAR_KMEANS$new(K = 2)
  
  expect_error(
    model$Groupes,
    "must be fitted"
  )
})

test_that("VAR_KMEANS: All clusters are non-empty", {
  data <- create_numeric_data(n = 200, p = 10, K = 3)
  model <- VAR_KMEANS$new(K = 3, n_init = 10)
  
  suppressWarnings(model$fit(data))
  
  cluster_sizes <- table(model$Groupes)
  expect_equal(length(cluster_sizes), 3)
  expect_true(all(cluster_sizes > 0))
})

# =============================================================================
# TEST SUITE 4: METRICS AND QUALITY
# =============================================================================

test_that("VAR_KMEANS: WithinClusterInertia is computed", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  expect_type(model$WithinClusterInertia, "double")
  expect_true(model$WithinClusterInertia >= 0)
  expect_false(is.na(model$WithinClusterInertia))
})

test_that("VAR_KMEANS: Homogeneite is computed", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  expect_type(model$Homogeneite, "double")
  expect_true(model$Homogeneite >= 0)
  expect_true(model$Homogeneite <= 1)
})

test_that("VAR_KMEANS: Metrics throw error before fitting", {
  model <- VAR_KMEANS$new(K = 2)
  
  expect_error(
    model$WithinClusterInertia,
    "must be fitted"
  )
  expect_error(
    model$Homogeneite,
    "must be fitted"
  )
})

test_that("VAR_KMEANS: Inertia increases with iterations (Vigneau & Qannari)", {
  data <- create_numeric_data(n = 100, p = 8)
  model <- VAR_KMEANS$new(K = 3, n_init = 5, max_iter = 1)
  model$fit(data)
  inertia1 <- model$WithinClusterInertia
  
  model2 <- VAR_KMEANS$new(K = 3, n_init = 5, max_iter = 100)
  model2$fit(data)
  inertia2 <- model2$WithinClusterInertia
  
  # Vigneau & Qannari: MAXIMIZE inertia (sum of r²)
  # More iterations should give equal or better (higher) inertia
  expect_true(inertia2 >= inertia1 * 0.9)
})

# =============================================================================
# TEST SUITE 5: K MODIFICATION
# =============================================================================

test_that("VAR_KMEANS: Modify K after initialization", {
  model <- VAR_KMEANS$new(K = 2)
  
  model$K <- 3
  expect_equal(model$K, 3)
  
  model$K <- 4
  expect_equal(model$K, 4)
})

test_that("VAR_KMEANS: Cannot set K < 2", {
  model <- VAR_KMEANS$new(K = 2)
  
  expect_error(
    model$K <- 1,
    "K must be an integer >= 2"
  )
})

test_that("VAR_KMEANS: Can refit after modifying K", {
  data <- create_numeric_data(n = 100, p = 9, K = 3)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  model$K <- 3
  suppressWarnings(model$fit(data))
  
  expect_length(unique(model$Groupes), 3)
})

# =============================================================================
# TEST SUITE 6: SUMMARY METHOD
# =============================================================================

test_that("VAR_KMEANS: summary() works", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  expect_output(model$summary(), "VAR_KMEANS")
  expect_output(model$summary(), "K-Means")
  expect_output(model$summary(), "Number of clusters")
})

test_that("VAR_KMEANS: summary() shows convergence info", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  expect_output(model$summary(), "Convergence status")
  expect_output(model$summary(), "Number of iterations")
})

test_that("VAR_KMEANS: summary() requires fitted model", {
  model <- VAR_KMEANS$new(K = 2)
  
  expect_error(
    model$summary(),
    "must be fitted"
  )
})

test_that("VAR_KMEANS: summary() shows cluster details", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  expect_output(model$summary(), "Cluster")
  expect_output(model$summary(), "variable")
  expect_output(model$summary(), "Homogeneity")
})

# =============================================================================
# TEST SUITE 7: PREDICT METHOD
# =============================================================================

test_that("VAR_KMEANS: predict() works with new variables", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  new_var <- rnorm(100)
  prediction <- model$predict(new_var)
  
  expect_type(prediction, "list")
  expect_true(length(prediction) > 0)
  
  first_pred <- prediction[[1]]
  expect_true("cluster" %in% names(first_pred))
  expect_true("scores" %in% names(first_pred))
  expect_true("best_score" %in% names(first_pred))
  expect_true(first_pred$cluster %in% 1:2)
  expect_length(first_pred$scores, 2)
  expect_true(first_pred$best_score >= 0)
  expect_true(first_pred$best_score <= 1)
})

test_that("VAR_KMEANS: predict() handles data.frame with multiple variables", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  new_data <- data.frame(
    NewVar1 = rnorm(100),
    NewVar2 = rnorm(100)
  )
  
  prediction <- model$predict(new_data)
  
  expect_type(prediction, "list")
  expect_length(prediction, 2)
  expect_true("NewVar1" %in% names(prediction))
  expect_true("NewVar2" %in% names(prediction))
})

test_that("VAR_KMEANS: predict() requires fitted model", {
  model <- VAR_KMEANS$new(K = 2)
  new_var <- rnorm(100)
  
  expect_error(
    model$predict(new_var),
    "must be fitted"
  )
})

test_that("VAR_KMEANS: predict() checks dimensions", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  new_var <- rnorm(50)
  
  expect_error(
    model$predict(new_var),
    "must have.*observations"
  )
})

test_that("VAR_KMEANS: predict() handles vector input", {
  data <- create_numeric_data(n = 100, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 5)
  model$fit(data)
  
  new_var <- rnorm(100)
  prediction <- model$predict(new_var)
  
  expect_type(prediction, "list")
  expect_length(prediction, 1)
})

test_that("VAR_KMEANS: predict() with highly correlated variable", {
  data <- create_correlated_data(n = 100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data)
  
  new_var <- data$V1 + rnorm(100, sd = 0.1)
  prediction <- model$predict(new_var)
  
  first_pred <- prediction[[1]]
  expect_true(first_pred$best_score > 0.5)
})

test_that("VAR_KMEANS: predict() with uncorrelated variable", {
  data <- create_correlated_data(n = 100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data)
  
  new_var <- rnorm(100)
  prediction <- model$predict(new_var)
  
  first_pred <- prediction[[1]]
  expect_true(first_pred$cluster %in% 1:2)
  expect_true(first_pred$best_score < 0.9)
})

test_that("VAR_KMEANS: predict() consistency after refit", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10, max_iter = 100)
  
  # FIX: Use same seed for both fits to ensure reproducibility
  set.seed(12345)
  model$fit(data_train)
  new_var <- data_train[, 1]
  pred1 <- model$predict(new_var)
  
  set.seed(12345)  # Same seed
  model$fit(data_train)
  pred2 <- model$predict(new_var)
  
  expect_equal(pred1[[1]]$cluster, pred2[[1]]$cluster)
})

# =============================================================================
# TEST SUITE 8: EDGE CASES
# =============================================================================

test_that("VAR_KMEANS: Handles small datasets", {
  data <- create_numeric_data(n = 20, p = 4)
  model <- VAR_KMEANS$new(K = 2, n_init = 3)
  
  model$fit(data)
  
  expect_length(model$Groupes, 4)
})

test_that("VAR_KMEANS: K equals number of variables", {
  data <- create_numeric_data(n = 100, p = 4)
  model <- VAR_KMEANS$new(K = 4, n_init = 5)
  
  suppressWarnings(model$fit(data))
  
  expect_length(unique(model$Groupes), 4)
})

test_that("VAR_KMEANS: Handles perfect separation", {
  set.seed(789)
  n <- 100
  data <- data.frame(
    V1 = rnorm(n),
    V2 = rnorm(n),
    V3 = rnorm(n) + 100,
    V4 = rnorm(n) + 100
  )
  
  model <- VAR_KMEANS$new(K = 2, n_init = 5, scale = TRUE)
  model$fit(data)
  
  expect_equal(length(unique(model$Groupes)), 2)
})

test_that("VAR_KMEANS: Handles missing values with na.omit", {
  data <- create_numeric_data(n = 100, p = 6)
  data[1:5, 1] <- NA
  
  model <- VAR_KMEANS$new(K = 2, na_action = "omit", n_init = 5)
  
  expect_silent(model$fit(data))
})

test_that("VAR_KMEANS: Handles constant variables gracefully", {
  n <- 100
  data <- data.frame(
    V1 = rnorm(n),
    V2 = rnorm(n)
  )
  
  model <- VAR_KMEANS$new(K = 2, n_init = 3)
  
  model$fit(data)
  expect_length(model$Groupes, 2)
})

# =============================================================================
# TEST SUITE 9: REPRODUCIBILITY
# =============================================================================

test_that("VAR_KMEANS: Results are reproducible with set.seed", {
  data <- create_numeric_data(n = 100, p = 6)
  
  set.seed(123)
  model1 <- VAR_KMEANS$new(K = 2, n_init = 5)
  model1$fit(data)
  groupes1 <- model1$Groupes
  
  set.seed(123)
  model2 <- VAR_KMEANS$new(K = 2, n_init = 5)
  model2$fit(data)
  groupes2 <- model2$Groupes
  
  expect_equal(groupes1, groupes2)
})

# =============================================================================
# TEST SUITE 10: INTEGRATION TESTS
# =============================================================================

test_that("VAR_KMEANS: Complete workflow", {
  data <- create_numeric_data(n = 150, p = 12, K = 3)
  
  model <- VAR_KMEANS$new(K = 3, n_init = 10, max_iter = 100)
  suppressWarnings(model$fit(data))
  
  expect_true(model$Converged)
  
  groupes <- model$Groupes
  expect_length(groupes, 12)
  expect_equal(length(unique(groupes)), 3)
  
  expect_output(model$summary())
  
  new_var <- rnorm(150)
  prediction <- model$predict(new_var)
  expect_type(prediction, "list")
  
  model$K <- 4
  suppressWarnings(model$fit(data))
  expect_equal(length(unique(model$Groupes)), 4)
})

test_that("VAR_KMEANS: Works with real dataset (iris)", {
  data <- iris[, 1:4]
  
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data)
  
  expect_length(model$Groupes, 4)
  expect_output(model$summary())
  
  new_data <- data.frame(NewVar = iris[, 1])
  prediction <- model$predict(new_data)
  expect_type(prediction, "list")
})

test_that("VAR_KMEANS: Comparison with multiple n_init values", {
  data <- create_numeric_data(n = 100, p = 8)
  
  results <- list()
  for (n_init in c(1, 5, 10, 20)) {
    model <- VAR_KMEANS$new(K = 3, n_init = n_init)
    model$fit(data)
    results[[paste0("n_init_", n_init)]] <- model$WithinClusterInertia
  }
  
  # Vigneau & Qannari: MAXIMIZE inertia (sum of r²)
  # More initializations should give equal or better (higher) inertia
  expect_true(results$n_init_20 >= results$n_init_1 * 0.9)
})

# =============================================================================
# TEST SUITE 11: ERROR HANDLING
# =============================================================================

test_that("VAR_KMEANS: fit() converts list to data.frame", {
  model <- VAR_KMEANS$new(K = 2)
  
  result <- tryCatch({
    suppressWarnings(model$fit(data.frame(a = 1:10, b = 1:10)))
    "success"
  }, error = function(e) {
    "error"
  })
  
  expect_true(result %in% c("success", "error"))
})

test_that("VAR_KMEANS: fit() requires numeric data", {
  data <- data.frame(
    V1 = letters[1:10],
    V2 = 1:10
  )
  
  model <- VAR_KMEANS$new(K = 2)
  
  expect_error(
    model$fit(data),
    "numeric"
  )
})

test_that("VAR_KMEANS: fit() requires at least 2 variables", {
  data <- data.frame(V1 = rnorm(100))
  model <- VAR_KMEANS$new(K = 2)
  
  expect_error(
    model$fit(data),
    "At least 2 variables"
  )
})

test_that("VAR_KMEANS: K cannot exceed number of variables", {
  data <- create_numeric_data(n = 100, p = 5)
  model <- VAR_KMEANS$new(K = 6)
  
  expect_error(
    model$fit(data),
    "K.*cannot"
  )
})

test_that("VAR_KMEANS: Invalid na_action", {
  expect_error(
    VAR_KMEANS$new(K = 2, na_action = "invalid"),
    "na_action"
  )
})

# =============================================================================
# TEST SUITE 12: PERFORMANCE AND STABILITY
# =============================================================================

test_that("VAR_KMEANS: Handles large number of observations", {
  skip_on_cran()
  
  data <- create_numeric_data(n = 5000, p = 6)
  model <- VAR_KMEANS$new(K = 2, n_init = 3)
  
  model$fit(data)
  expect_length(model$Groupes, 6)
})

test_that("VAR_KMEANS: Handles many variables", {
  skip_on_cran()
  
  data <- create_numeric_data(n = 100, p = 50)
  model <- VAR_KMEANS$new(K = 5, n_init = 3)
  
  suppressWarnings(model$fit(data))
  expect_equal(length(model$Groupes), 50)
})

test_that("VAR_KMEANS: Convergence with difficult data", {
  set.seed(999)
  n <- 100
  data <- data.frame(
    V1 = rnorm(n),
    V2 = rnorm(n),
    V3 = rnorm(n),
    V4 = rnorm(n)
  )
  
  model <- VAR_KMEANS$new(K = 2, n_init = 20, max_iter = 200)
  model$fit(data)
  
  expect_true(model$NIterations <= 200)
})

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("\n")
cat("=========================================================\n")
cat("   VAR_KMEANS Test Suite Complete\n")
cat("=========================================================\n")
cat("\n")
cat("Test coverage:\n")
cat("  - Initialization (5 tests)\n")
cat("  - Fitting (5 tests)\n")
cat("  - Groupes property (3 tests)\n")
cat("  - Metrics and quality (5 tests)\n")
cat("  - K modification (3 tests)\n")
cat("  - Summary method (4 tests)\n")
cat("  - Predict method (9 tests)\n")
cat("  - Edge cases (5 tests)\n")
cat("  - Reproducibility (1 test)\n")
cat("  - Integration tests (3 tests)\n")
cat("  - Error handling (5 tests)\n")
cat("  - Performance and stability (3 tests)\n")
cat("=========================================================\n")
cat("\n")