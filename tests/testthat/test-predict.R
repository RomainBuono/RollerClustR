# ==============================================================================
# TESTS FOR PREDICT() METHOD
# Tests covering the three algorithms: VAR_CAH, VAR_KMEANS, TandemVarClust
# ==============================================================================

library(testthat)
library(RollerClustR)

# ==============================================================================
# TEST DATA
# ==============================================================================

# Dataset for numeric variables
create_numeric_data <- function(n = 100) {
  set.seed(123)
  data.frame(
    var1 = rnorm(n),
    var2 = rnorm(n, mean = 0, sd = 1),
    var3 = rnorm(n, mean = 5, sd = 2),
    var4 = rnorm(n, mean = 10, sd = 3)
  )
}

# Mixed dataset for TandemVarClust
create_mixed_data <- function(n = 100) {
  set.seed(456)
  data.frame(
    cat1 = sample(c("A", "B", "C"), n, replace = TRUE),
    cat2 = sample(c("X", "Y", "Z"), n, replace = TRUE),
    num1 = rnorm(n),
    num2 = runif(n, 0, 10)
  )
}

# ==============================================================================
# TESTS FOR VAR_CAH$predict()
# ==============================================================================

test_that("VAR_CAH predict() works with a single variable", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 2)
  model$fit(data_train)
  
  # New variable to predict
  new_var <- data.frame(new1 = rnorm(100))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Checks
  expect_type(pred, "list")
  expect_true("new1" %in% names(pred))
  expect_true("cluster" %in% names(pred$new1))
  expect_true("scores" %in% names(pred$new1))
  expect_length(pred$new1$cluster, 1)
  expect_length(pred$new1$scores, 2)
  expect_true(pred$new1$cluster %in% 1:2)
})

test_that("VAR_CAH predict() works with multiple variables", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 3)
  model$fit(data_train)
  
  # New variables to predict
  new_vars <- data.frame(
    new1 = rnorm(100),
    new2 = rnorm(100, mean = 5),
    new3 = rnorm(100, mean = 10)
  )
  
  # Prediction
  pred <- model$predict(new_vars)
  
  # Checks
  expect_length(pred, 3)
  expect_true(all(c("new1", "new2", "new3") %in% names(pred)))
  expect_true(all(sapply(pred, function(x) x$cluster) %in% 1:3))
  expect_true(all(sapply(pred, function(x) x$best_score) >= 0))
  expect_true(all(sapply(pred, function(x) x$best_score) <= 1))
})

test_that("VAR_CAH predict() handles dimension errors", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 2)
  model$fit(data_train)
  
  # New variable with wrong number of observations
  new_var_wrong <- data.frame(new1 = rnorm(50))
  
  # Should throw an error
  expect_error(
    model$predict(new_var_wrong),
    "must have"
  )
})

test_that("VAR_CAH predict() requires a fitted model", {
  # Unfitted model
  model <- VAR_CAH$new(K = 2)
  new_var <- data.frame(new1 = rnorm(100))
  
  # Should throw an error
  expect_error(
    model$predict(new_var),
    "must be fitted"
  )
})

test_that("VAR_CAH predict() accepts a vector", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 2)
  model$fit(data_train)
  
  # New variable as vector
  new_var_vec <- rnorm(100)
  
  # Prediction
  pred <- model$predict(new_var_vec)
  
  # Checks
  expect_type(pred, "list")
  expect_length(pred, 1)
})

test_that("VAR_CAH predict() handles missing values", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 2)
  model$fit(data_train)
  
  # New variable with NA
  new_var_na <- data.frame(new1 = c(rnorm(90), rep(NA, 10)))
  
  # Prediction (should work with warning or handle NA)
  pred <- model$predict(new_var_na)
  
  # Check that assignment was made
  expect_length(pred, 1)
  expect_true("new1" %in% names(pred))
})

# ==============================================================================
# TESTS FOR VAR_KMEANS$predict()
# ==============================================================================

test_that("VAR_KMEANS predict() works with a single variable", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data_train)
  
  # New variable to predict
  new_var <- data.frame(new1 = rnorm(100))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Checks
  expect_type(pred, "list")
  expect_true("new1" %in% names(pred))
  expect_true("cluster" %in% names(pred$new1))
  expect_true("scores" %in% names(pred$new1))
  expect_true("best_score" %in% names(pred$new1))
  expect_length(pred$new1$cluster, 1)
  expect_length(pred$new1$scores, 2)
  expect_true(pred$new1$cluster %in% 1:2)
  expect_true(pred$new1$best_score >= 0 && pred$new1$best_score <= 1)
})

test_that("VAR_KMEANS predict() works with multiple variables", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 3, n_init = 10)
  model$fit(data_train)
  
  # New variables to predict
  new_vars <- data.frame(
    new1 = rnorm(100),
    new2 = rnorm(100, mean = 5),
    new3 = rnorm(100, mean = 10)
  )
  
  # Prediction
  pred <- model$predict(new_vars)
  
  # Checks
  expect_length(pred, 3)
  expect_true(all(c("new1", "new2", "new3") %in% names(pred)))
  
  for (var_name in names(pred)) {
    expect_true(pred[[var_name]]$cluster %in% 1:3)
    expect_true(pred[[var_name]]$best_score >= 0)
    expect_true(pred[[var_name]]$best_score <= 1)
    expect_length(pred[[var_name]]$scores, 3)
  }
})

test_that("VAR_KMEANS predict() handles dimension errors", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data_train)
  
  # New variable with wrong number of observations
  new_var_wrong <- data.frame(new1 = rnorm(50))
  
  # Should throw an error
  expect_error(
    model$predict(new_var_wrong),
    "must have"
  )
})

test_that("VAR_KMEANS predict() requires a fitted model", {
  # Unfitted model
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  new_var <- data.frame(new1 = rnorm(100))
  
  # Should throw an error
  expect_error(
    model$predict(new_var),
    "must be fitted"
  )
})

test_that("VAR_KMEANS predict() accepts a vector", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data_train)
  
  # New variable as vector
  new_var_vec <- rnorm(100)
  
  # Prediction
  pred <- model$predict(new_var_vec)
  
  # Checks
  expect_type(pred, "list")
  expect_length(pred, 1)
  expect_true(names(pred)[1] %in% c("NewVar1", "V1"))
})

test_that("VAR_KMEANS predict() handles missing values", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data_train)
  
  # New variable with NA
  new_var_na <- data.frame(new1 = c(rnorm(90), rep(NA, 10)))
  
  # Prediction (should work with use="complete.obs" in correlation)
  pred <- model$predict(new_var_na)
  
  # Check that assignment was made
  expect_length(pred, 1)
  expect_true("new1" %in% names(pred))
  expect_true(pred$new1$cluster %in% 1:2)
})

test_that("VAR_KMEANS predict() with highly correlated variables", {
  # Data with highly correlated variables
  set.seed(789)
  base_var <- rnorm(100)
  data_corr <- data.frame(
    var1 = base_var,
    var2 = base_var + rnorm(100, sd = 0.1),
    var3 = base_var + rnorm(100, sd = 0.1),
    var4 = rnorm(100)  # Independent variable
  )
  
  model <- VAR_KMEANS$new(K = 2, n_init = 20)
  model$fit(data_corr)
  
  # New variable similar to base_var
  new_var <- data.frame(new1 = base_var + rnorm(100, sd = 0.1))
  
  pred <- model$predict(new_var)
  
  # The new variable should be assigned to the same cluster as var1, var2, var3
  cluster_base_vars <- model$Groupes["var1"]
  expect_equal(pred$new1$cluster, unname(cluster_base_vars))
  expect_true(pred$new1$best_score > 0.4) 
})

test_that("VAR_KMEANS predict() with uncorrelated variables", {
  # Data with independent variables
  set.seed(321)
  data_indep <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100),
    var4 = rnorm(100)
  )
  
  model <- VAR_KMEANS$new(K = 2, n_init = 20)
  model$fit(data_indep)
  
  # New independent variable
  new_var <- data.frame(new1 = rnorm(100))
  
  pred <- model$predict(new_var)
  
  # Scores should be relatively low (no strong correlation)
  expect_true(pred$new1$best_score < 0.4)
})

test_that("VAR_KMEANS predict() works after modifying K", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 10)
  model$fit(data_train)
  
  # Change K
  model$K <- 3
  
  # New variable
  new_var <- data.frame(new1 = rnorm(100))
  
  # Prediction should work with the new K
  pred <- model$predict(new_var)
  
  expect_length(pred$new1$scores, 3)
  expect_true(pred$new1$cluster %in% 1:3)
})

test_that("VAR_KMEANS predict() is consistent with training variable assignment", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_KMEANS$new(K = 2, n_init = 20)
  model$fit(data_train)
  
  # Predict with a training variable
  var_train <- data.frame(var1 = data_train$var1)
  
  pred <- model$predict(var_train)
  
  # The predicted cluster should be the same as in Groupes
  expect_equal(pred$var1$cluster, unname(model$Groupes["var1"]))
  expect_true(pred$var1$best_score > 0.3)  # Realistic score for independent variables
})

# ==============================================================================
# TESTS FOR TandemVarClust$predict()
# ==============================================================================

test_that("TandemVarClust predict() works with a single categorical variable", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variable to analyze
  new_var <- data.frame(illus1 = sample(c("A", "B"), 100, replace = TRUE))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Checks - TandemVarClust returns different structure
  expect_type(pred, "list")
  expect_true("illus1" %in% names(pred))
  expect_true("contingency" %in% names(pred$illus1))
  expect_true("chi2_test" %in% names(pred$illus1))
  expect_true("cramers_v" %in% names(pred$illus1))
  expect_true("dice_scores" %in% names(pred$illus1))
})

test_that("TandemVarClust predict() works with multiple categorical variables", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 3, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variables to analyze
  new_vars <- data.frame(
    illus1 = sample(c("A", "B", "C"), 100, replace = TRUE),
    illus2 = sample(c("X", "Y"), 100, replace = TRUE)
  )
  
  # Prediction
  pred <- model$predict(new_vars)
  
  # Checks
  expect_length(pred, 2)
  expect_true("illus1" %in% names(pred))
  expect_true("illus2" %in% names(pred))
  
  # Check structure for each variable
  for (var_name in c("illus1", "illus2")) {
    expect_true("contingency" %in% names(pred[[var_name]]))
    expect_true("chi2_test" %in% names(pred[[var_name]]))
    expect_true("cramers_v" %in% names(pred[[var_name]]))
    expect_true("significant" %in% names(pred[[var_name]]))
    expect_true("dice_scores" %in% names(pred[[var_name]]))
  }
})

test_that("TandemVarClust predict() computes Cramer's V correctly", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variable - WITH SEED for reproducibility
  set.seed(789)
  new_var <- data.frame(illus1 = sample(c("A", "B", "C"), 100, replace = TRUE))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Cramer's V should be between 0 and 1 (if calculable)
  if (!is.na(pred$illus1$cramers_v)) {
    expect_true(pred$illus1$cramers_v >= 0)
    expect_true(pred$illus1$cramers_v <= 1)
  } else {
    # If NA, ensure chi2 is also NA (consistent behavior)
    expect_true(is.na(pred$illus1$chi2_test$p.value))
  }
})

test_that("TandemVarClust predict() computes Chi-square test", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variable - WITH SEED
  set.seed(456)
  new_var <- data.frame(illus1 = sample(c("A", "B", "C"), 100, replace = TRUE))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Chi-square test should be present
  expect_true("chi2_test" %in% names(pred$illus1))
  expect_true("statistic" %in% names(pred$illus1$chi2_test))
  expect_true("p.value" %in% names(pred$illus1$chi2_test))
  
  # If chi-square is calculable, p-value should be between 0 and 1
  if (!is.na(pred$illus1$chi2_test$p.value)) {
    expect_true(pred$illus1$chi2_test$p.value >= 0)
    expect_true(pred$illus1$chi2_test$p.value <= 1)
  }
})

test_that("TandemVarClust predict() creates contingency table", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variable with 3 modalities
  new_var <- data.frame(illus1 = sample(c("A", "B", "C"), 100, replace = TRUE))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Check contingency table
  contingency <- pred$illus1$contingency
  expect_equal(nrow(contingency), 3)  # 3 modalities
  expect_equal(ncol(contingency), 2)  # 2 clusters
  expect_equal(sum(contingency), 100) # Total observations
})

test_that("TandemVarClust predict() computes row and column percentages", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variable - WITH SEED
  set.seed(321)
  new_var <- data.frame(illus1 = sample(c("A", "B", "C"), 100, replace = TRUE))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Row percentages should sum to 100 for each modality
  pct_row <- pred$illus1$percentages_by_modality
  expect_true(all(abs(rowSums(pct_row) - 100) < 0.01))
  
  # Column percentages should sum to 100 for each cluster (if cluster has observations)
  pct_col <- pred$illus1$percentages_by_cluster
  contingency <- pred$illus1$contingency
  non_empty_cols <- colSums(contingency) > 0
  
  if (sum(non_empty_cols) > 0) {
    expect_true(all(abs(colSums(pct_col[, non_empty_cols, drop = FALSE]) - 100) < 0.01))
  }
})

test_that("TandemVarClust predict() returns Dice scores", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New illustrative variable
  new_var <- data.frame(illus1 = sample(c("A", "B"), 100, replace = TRUE))
  
  # Prediction
  pred <- model$predict(new_var)
  
  # Check Dice scores matrix
  dice_scores <- pred$illus1$dice_scores
  expect_equal(nrow(dice_scores), 100)  # One row per observation
  expect_equal(ncol(dice_scores), 2)    # One column per cluster
  expect_true(all(dice_scores >= 0 & dice_scores <= 1, na.rm = TRUE))  # Scores in [0,1]
})

test_that("TandemVarClust predict() handles dimension errors", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New variable with wrong number of observations
  new_var_wrong <- data.frame(illus1 = sample(c("A", "B"), 50, replace = TRUE))
  
  # Should throw an error
  expect_error(
    model$predict(new_var_wrong),
    "must have the same number of observations"
  )
})

test_that("TandemVarClust predict() requires a fitted model", {
  # Unfitted model
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  new_var <- data.frame(illus1 = sample(c("A", "B"), 100, replace = TRUE))
  
  # Should throw an error
  expect_error(
    model$predict(new_var),
    "model is not fitted"
  )
})

test_that("TandemVarClust predict() converts numeric to categorical with warning", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New variable that is numeric (should be categorical)
  new_var_numeric <- data.frame(illus1 = rnorm(100))
  
  # Should issue a warning and convert to factor
  expect_warning(
    pred <- model$predict(new_var_numeric),
    "not categorical"
  )
  
  # Prediction should still work
  expect_true("illus1" %in% names(pred))
})

# ==============================================================================
# CONSISTENCY TESTS BETWEEN ALGORITHMS
# ==============================================================================

test_that("All algorithms return appropriate result structure", {
  # Preparation of data
  data_numeric <- create_numeric_data(100)
  data_mixed <- create_mixed_data(100)
  
  # Models
  model_cah <- VAR_CAH$new(K = 2)
  model_cah$fit(data_numeric)
  
  model_kmeans <- VAR_KMEANS$new(K = 2, n_init = 10)
  model_kmeans$fit(data_numeric)
  
  model_tandem <- TandemVarClust$new(K = 2, n_bins = 3)
  model_tandem$fit(data_mixed)
  
  # New variables
  new_var_numeric <- data.frame(new1 = rnorm(100))
  new_var_categorical <- data.frame(new1 = sample(c("A", "B"), 100, replace = TRUE))
  
  # Predictions
  pred_cah <- model_cah$predict(new_var_numeric)
  pred_kmeans <- model_kmeans$predict(new_var_numeric)
  pred_tandem <- model_tandem$predict(new_var_categorical)
  
  # VAR_CAH and VAR_KMEANS should have similar structure
  for (pred in list(pred_cah, pred_kmeans)) {
    expect_true("new1" %in% names(pred))
    expect_true("cluster" %in% names(pred$new1))
    expect_true("scores" %in% names(pred$new1))
    expect_true("best_score" %in% names(pred$new1))
  }
  
  # TandemVarClust has a different structure (per-variable analysis)
  expect_true("new1" %in% names(pred_tandem))
  expect_true("contingency" %in% names(pred_tandem$new1))
  expect_true("chi2_test" %in% names(pred_tandem$new1))
  expect_true("cramers_v" %in% names(pred_tandem$new1))
  expect_true("dice_scores" %in% names(pred_tandem$new1))
})

# ==============================================================================
# EDGE CASE TESTS
# ==============================================================================

test_that("predict() with K=1 is not supported", {
  skip("K=1 is not supported by the package (K >= 2 required)")
})

test_that("predict() with highly correlated variables for VAR_CAH", {
  # Data with highly correlated variables
  set.seed(789)
  base_var <- rnorm(100)
  data_corr <- data.frame(
    var1 = base_var,
    var2 = base_var + rnorm(100, sd = 0.1),
    var3 = base_var + rnorm(100, sd = 0.1),
    var4 = rnorm(100)  # Independent variable
  )
  
  model <- VAR_CAH$new(K = 2)
  model$fit(data_corr)
  
  # New variable similar to base_var
  new_var <- data.frame(new1 = base_var + rnorm(100, sd = 0.1))
  
  pred <- model$predict(new_var)
  
  # The new variable should be assigned to the same cluster as var1, var2, var3
  cluster_base_vars <- model$Groupes["var1"]
  expect_equal(pred$new1$cluster, unname(cluster_base_vars))
  expect_true(pred$new1$best_score > 0.7)  # High score due to high correlation
})

test_that("predict() with uncorrelated variables for VAR_CAH", {
  # Data with independent variables
  set.seed(321)
  data_indep <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100),
    var4 = rnorm(100)
  )
  
  model <- VAR_CAH$new(K = 2)
  model$fit(data_indep)
  
  # New independent variable
  new_var <- data.frame(new1 = rnorm(100))
  
  pred <- model$predict(new_var)
  
  # Scores should be relatively low (no strong correlation)
  expect_true(pred$new1$best_score < 0.5)
})

test_that("TandemVarClust predict() with variable having rare modalities", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # New variable with one rare modality
  new_var <- data.frame(
    illus1 = c(rep("A", 90), rep("B", 9), rep("C", 1))
  )
  
  # Prediction should handle rare modalities
  pred <- model$predict(new_var)
  
  # Check that all modalities are in the contingency table
  expect_equal(nrow(pred$illus1$contingency), 3)
  expect_true(all(c("A", "B", "C") %in% rownames(pred$illus1$contingency)))
})

# ==============================================================================
# INTEGRATION TESTS
# ==============================================================================

test_that("predict() works after modifying K for VAR_CAH", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 2)
  model$fit(data_train)
  
  # Change K
  model$K <- 3
  
  # New variable
  new_var <- data.frame(new1 = rnorm(100))
  
  # Prediction should work with the new K
  pred <- model$predict(new_var)
  
  expect_length(pred$new1$scores, 3)
  expect_true(pred$new1$cluster %in% 1:3)
})

test_that("predict() is consistent with training variable assignment for VAR_CAH", {
  # Preparation
  data_train <- create_numeric_data(100)
  model <- VAR_CAH$new(K = 2)
  model$fit(data_train)
  
  # Predict with a training variable
  var_train <- data.frame(var1 = data_train$var1)
  
  pred <- model$predict(var_train)
  
  # The predicted cluster should be the same as in Groupes
  expect_equal(pred$var1$cluster, unname(model$Groupes["var1"]))
  expect_true(pred$var1$best_score > 0.7)  # Very high score
})

test_that("TandemVarClust predict() works after refit_with_k", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  model$fit(data_train)
  
  # Refit with different K
  model$refit_with_k(3)
  
  # New illustrative variable
  new_var <- data.frame(illus1 = sample(c("A", "B"), 100, replace = TRUE))
  
  # Prediction should work with the new K
  pred <- model$predict(new_var)
  
  # Contingency table should have 3 columns (3 clusters)
  expect_equal(ncol(pred$illus1$contingency), 3)
  expect_equal(ncol(pred$illus1$dice_scores), 3)
})

test_that("TandemVarClust predict() with multiple variables produces comprehensive output", {
  # Preparation
  data_train <- create_mixed_data(100)
  model <- TandemVarClust$new(K = 3, n_bins = 3)
  model$fit(data_train)
  
  # Multiple illustrative variables - WITH SEED
  set.seed(999)
  new_vars <- data.frame(
    illus1 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
    illus2 = sample(c("P", "Q"), 100, replace = TRUE),
    illus3 = sample(c("M", "N", "O"), 100, replace = TRUE)
  )
  
  # Prediction
  pred <- model$predict(new_vars)
  
  # Check each variable
  expect_equal(length(pred), 3)
  
  for (var_name in names(new_vars)) {
    var_result <- pred[[var_name]]
    
    # Check all expected fields are present
    expect_true("contingency" %in% names(var_result))
    expect_true("cramers_v" %in% names(var_result))
    expect_true("chi2_test" %in% names(var_result))
    expect_true("significant" %in% names(var_result))
    expect_true(is.logical(var_result$significant))
    
    # If statistical tests are calculable, verify bounds
    if (!is.na(var_result$cramers_v)) {
      expect_true(var_result$cramers_v >= 0)
      expect_true(var_result$cramers_v <= 1)
    }
    
    if (!is.na(var_result$chi2_test$p.value)) {
      expect_true(var_result$chi2_test$p.value >= 0)
      expect_true(var_result$chi2_test$p.value <= 1)
    }
  }
})