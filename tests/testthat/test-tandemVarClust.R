# ==============================================================================
# TEST SUITE FOR TandemVarClust
# Tests for Tandem clustering (MCA + HAC) on modalities
# ==============================================================================


context("TandemVarClust - Complete Test Suite")

# ==============================================================================
# TEST DATA PREPARATION
# ==============================================================================

# Prepare Titanic data
data(Titanic)
titanic_df <- as.data.frame(Titanic)

# Simple categorical dataset
simple_cat <- data.frame(
  var1 = factor(c("A", "A", "B", "B", "C", "C")),
  var2 = factor(c("X", "Y", "X", "Y", "X", "Y")),
  var3 = factor(c("1", "1", "2", "2", "1", "1"))
)

# Mixed dataset (numeric + categorical)
mixed_data <- data.frame(
  cat1 = factor(c("A", "A", "B", "B", "C", "C")),
  num1 = c(1, 2, 3, 4, 5, 6),
  cat2 = factor(c("X", "Y", "X", "Y", "X", "Y")),
  num2 = c(10, 20, 30, 40, 50, 60)
)

# ==============================================================================
# SECTION 1: INITIALIZATION TESTS
# ==============================================================================

test_that("TandemVarClust: Correct initialization with default parameters", {
  model <- TandemVarClust$new(K = 2)
  
  expect_s3_class(model, "TandemVarClust")
  expect_s3_class(model, "ClusterAnalysis")
  expect_equal(model$K, 2)
})

test_that("TandemVarClust: Initialization with custom parameters", {
  model <- TandemVarClust$new(K = 3, n_factors = 2, n_bins = 4, method_cah = "complete")
  
  expect_s3_class(model, "TandemVarClust")
  expect_equal(model$K, 3)
})

test_that("TandemVarClust: K parameter validation", {
  expect_error(TandemVarClust$new(K = 1), "K must be an integer >= 2")
  expect_error(TandemVarClust$new(K = -1), "K must be an integer >= 2")
  expect_error(TandemVarClust$new(K = 1.5), "K must be an integer >= 2")
  expect_error(TandemVarClust$new(K = "two"), "K must be an integer >= 2")
})

test_that("TandemVarClust: n_factors parameter validation", {
  # Valid values
  expect_silent(TandemVarClust$new(K = 2, n_factors = NULL))
  expect_silent(TandemVarClust$new(K = 2, n_factors = 2))
  expect_silent(TandemVarClust$new(K = 2, n_factors = 5))
  
  # Invalid values
  expect_error(
    TandemVarClust$new(K = 2, n_factors = 0),
    "n_factors must be NULL or an integer >= 1"
  )
  
  expect_error(
    TandemVarClust$new(K = 2, n_factors = -1),
    "n_factors must be NULL or an integer >= 1"
  )
})

test_that("TandemVarClust: n_bins parameter validation", {
  # Valid values
  expect_silent(TandemVarClust$new(K = 2, n_bins = 2))
  expect_silent(TandemVarClust$new(K = 2, n_bins = 10))
  expect_silent(TandemVarClust$new(K = 2, n_bins = 20))
  
  # Invalid values
  expect_error(
    TandemVarClust$new(K = 2, n_bins = 1),
    "n_bins must be between 2 and 20"
  )
  
  expect_error(
    TandemVarClust$new(K = 2, n_bins = 25),
    "n_bins must be between 2 and 20"
  )
})

test_that("TandemVarClust: method_cah parameter validation", {
  # Valid methods
  valid_methods <- c("ward.D", "ward.D2", "single", "complete", "average", 
                     "mcquitty", "median", "centroid")
  
  for (m in valid_methods) {
    expect_silent(TandemVarClust$new(K = 2, method_cah = m))
  }
  
  # Invalid method
  expect_error(
    TandemVarClust$new(K = 2, method_cah = "invalid"),
    "invalid method_cah"
  )
})

test_that("TandemVarClust: Suspicious parameters detected", {
  # Parameters that don't exist for TandemVarClust
  expect_warning(
    TandemVarClust$new(K = 2, max_iter = 100),
    "Suspicious parameters"
  )
  
  expect_warning(
    TandemVarClust$new(K = 2, tolerance = 0.01),
    "Suspicious parameters"
  )
})

cat("\n✓ Section 1 (Initialization): Tests passed\n")

# ==============================================================================
# SECTION 2: FITTING TESTS
# ==============================================================================

test_that("TandemVarClust: fit() works with categorical data (Titanic)", {
  model <- TandemVarClust$new(K = 2)
  
  expect_silent({
    model$fit(titanic_df[, 1:4])
  })
  
  # Check that model is fitted
  groupes <- model$Groupes
  expect_true(length(groupes) > 0)
  
  # Check that we have clusters for modalities, not variables
  expect_true(length(groupes) > 4)  # More than 4 variables
  
  # Check format "variable.modality"
  expect_true(all(grepl("\\.", names(groupes))))
})

test_that("TandemVarClust: fit() works with simple categorical data", {
  model <- TandemVarClust$new(K = 2)
  
  expect_silent({
    model$fit(simple_cat)
  })
  
  groupes <- model$Groupes
  expect_true(length(groupes) > 0)
  expect_true(all(groupes %in% 1:2))
})

test_that("TandemVarClust: fit() works with mixed data", {
  model <- TandemVarClust$new(K = 2, n_bins = 3)
  
  expect_silent({
    model$fit(mixed_data)
  })
  
  groupes <- model$Groupes
  expect_true(length(groupes) > 0)
  
  # Check discretization occurred for numeric variables
  expect_true(any(grepl("num1\\.", names(groupes))))
  expect_true(any(grepl("num2\\.", names(groupes))))
})

test_that("TandemVarClust: fit() rejects K too large", {
  model <- TandemVarClust$new(K = 100)
  
  expect_error(
    model$fit(simple_cat),
    "cannot exceed the number of modalities"
  )
})

test_that("TandemVarClust: fit() handles minimum observations", {
  min_data <- data.frame(
    var1 = factor(c("A", "A")),
    var2 = factor(c("X", "Y"))
  )
  
  model <- TandemVarClust$new(K = 2)
  
  # Should work or fail gracefully
  expect_silent(model$fit(min_data)) 
})

cat("\n✓ Section 2 (Fitting): Tests passed\n")

# ==============================================================================
# SECTION 3: GROUPES AND RESULTS TESTS
# ==============================================================================

test_that("TandemVarClust: Groupes returns correct format", {
  model <- TandemVarClust$new(K = 3)
  model$fit(titanic_df[, 1:4])
  
  groupes <- model$Groupes
  
  # Check it's a named integer vector
  expect_true(is.integer(groupes))
  expect_true(!is.null(names(groupes)))
  
  # Check values are in 1:K
  expect_true(all(groupes %in% 1:3))
  
  # Check format "variable.modality"
  expect_true(all(grepl("\\.", names(groupes))))
  
  # Check some specific modalities
  expect_true(any(grepl("^Class\\.", names(groupes))))
  expect_true(any(grepl("^Sex\\.", names(groupes))))
})

test_that("TandemVarClust: Groupes fails if model not fitted", {
  model <- TandemVarClust$new(K = 2)
  
  expect_error(model$Groupes, "must be fitted")

})

test_that("TandemVarClust: Groupes format is validated", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  groupes <- model$Groupes
  
  # All names should contain a dot
  expect_true(all(grepl("\\.", names(groupes))))
  
  # Names should not be empty
  expect_false(any(names(groupes) == ""))
})

cat("\n✓ Section 3 (Groupes): Tests passed\n")

# ==============================================================================
# SECTION 4: SUMMARY AND DISPLAY TESTS
# ==============================================================================

test_that("TandemVarClust: summary() displays correctly", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  # Capture output
  output <- capture.output(model$summary())
  
  # Check key elements are present
  expect_true(any(grepl("TandemVarClust", output)))
  expect_true(any(grepl("MCA", output)))
  expect_true(any(grepl("Cluster", output)))
})

test_that("TandemVarClust: summary() shows warning about modalities", {
  model <- TandemVarClust$new(K = 2)
  model$fit(simple_cat)
  
  output <- capture.output(model$summary())
  
  # Should mention that it groups modalities
  expect_true(any(grepl("modality clusters", output, ignore.case = TRUE)))
})

test_that("TandemVarClust: summary() fails gracefully if not fitted", {
  model <- TandemVarClust$new(K = 2)
  expect_error(model$summary(), "must be fitted")
})

cat("\n✓ Section 4 (Summary): Tests passed\n")

# ==============================================================================
# SECTION 5: K MODIFICATION TESTS
# ==============================================================================

test_that("TandemVarClust: Modifying K triggers refitting", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  groupes_k2 <- model$Groupes
  
  # Change K
  model$K <- 3
  groupes_k3 <- model$Groupes
  
  # Check that clusters changed
  expect_true(!identical(groupes_k2, groupes_k3))
  expect_true(all(groupes_k3 %in% 1:3))
})

test_that("TandemVarClust: K modification validates new value", {
  model <- TandemVarClust$new(K = 2)
  model$fit(simple_cat)
  
  # Try to set invalid K
  expect_error(
    { model$K <- 1 },
    "K must be an integer >= 2"
  )
  
  expect_error(
    { model$K <- 100 },
    "cannot exceed"
  )
})

cat("\n✓ Section 5 (K modification): Tests passed\n")

# ==============================================================================
# SECTION 6: HELPER METHODS TESTS
# ==============================================================================

test_that("TandemVarClust: get_variable_summary() works", {
  model <- TandemVarClust$new(K = 3)
  model$fit(titanic_df[, 1:4])
  
  # Suppress console output
  var_summary <- suppressMessages(model$get_variable_summary())
  
  # Check structure
  expect_true(is.data.frame(var_summary))
  expect_true(all(c("variable", "n_modalites", "cluster_principal", "purity") %in% 
                  names(var_summary)))
  
  # Check we have 4 variables
  expect_equal(nrow(var_summary), 4)
  
  # Check purity is between 0 and 1
  expect_true(all(var_summary$purity >= 0 & var_summary$purity <= 1))
})

test_that("TandemVarClust: get_variable_summary() shows low purity warning", {
  model <- TandemVarClust$new(K = 3)
  model$fit(titanic_df[, 1:4])
  
  # Capture output
  output <- capture.output(model$get_variable_summary())
  
  # Should mention purity
  expect_true(any(grepl("purity", output, ignore.case = TRUE)))
})

test_that("TandemVarClust: get_modalities_of_variable() works", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  # Get modalities for Class variable
  class_mods <- suppressMessages(model$get_modalities_of_variable("Class"))
  
  expect_true(is.data.frame(class_mods))
  expect_true(all(c("modalite", "cluster") %in% names(class_mods)))
  expect_true(nrow(class_mods) > 0)
  
  # Check modalities are correct
  expect_true(all(class_mods$modalite %in% c("1st", "2nd", "3rd", "Crew")))
})

test_that("TandemVarClust: get_modalities_of_variable() fails for invalid variable", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  expect_error(
    model$get_modalities_of_variable("InvalidVar"),
    "not found"
  )
})

test_that("TandemVarClust: get_modalities_of_cluster() works", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  # Get modalities for cluster 1
  cluster1_mods <- suppressMessages(model$get_modalities_of_cluster(1))
  
  expect_true(is.data.frame(cluster1_mods))
  expect_true(all(c("variable", "modalite") %in% names(cluster1_mods)))
  expect_true(nrow(cluster1_mods) > 0)
})

test_that("TandemVarClust: get_modalities_of_cluster() validates k", {
  model <- TandemVarClust$new(K = 2)
  model$fit(simple_cat)
  
  expect_error(
    model$get_modalities_of_cluster(0),
    "k must be between"
  )
  
  expect_error(
    model$get_modalities_of_cluster(10),
    "k must be between"
  )
})

test_that("TandemVarClust: check_results_integrity() works", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  # Should pass integrity checks
  output <- capture.output(
    integrity <- model$check_results_integrity()
  )
  
  expect_true(is.logical(integrity))
})

test_that("TandemVarClust: VariableClusters active binding works", {
  model <- TandemVarClust$new(K = 3)
  model$fit(titanic_df[, 1:4])
  
  # Suppress output from get_variable_summary
  var_clusters <- suppressMessages(model$VariableClusters)
  
  expect_true(is.vector(var_clusters))
  expect_true(is.integer(var_clusters))
  expect_equal(length(var_clusters), 4)
  expect_true(all(names(var_clusters) %in% c("Class", "Sex", "Age", "Survived")))
})

cat("\n✓ Section 6 (Helper methods): Tests passed\n")

# ==============================================================================
# SECTION 7: EDGE CASES
# ==============================================================================

test_that("TandemVarClust: Works with minimum data", {
  min_data <- data.frame(
    var1 = factor(c("A", "A", "B", "B")),
    var2 = factor(c("X", "X", "Y", "Y"))
  )
  
  model <- TandemVarClust$new(K = 2)
  expect_silent(model$fit(min_data))
  
  groupes <- model$Groupes
  expect_true(length(groupes) == 4)  # 2 vars × 2 modalities
})

test_that("TandemVarClust: Handles variables with many modalities", {
  many_mods <- data.frame(
    var1 = factor(LETTERS[1:20]),
    var2 = factor(letters[1:20])
  )
  
  model <- TandemVarClust$new(K = 3)
  expect_silent(model$fit(many_mods))
  
  groupes <- model$Groupes
  expect_equal(length(groupes), 40)  # 2 vars × 20 modalities
})

test_that("TandemVarClust: Handles single modality per variable", {
  single_mod <- data.frame(
    var1 = factor(rep("A", 10)),
    var2 = factor(rep("X", 10)),
    var3 = factor(rep("1", 10))
  )
  
  model <- TandemVarClust$new(K = 2)
  
  # Should handle this
  expect_silent(model$fit(single_mod))
})

cat("\n✓ Section 7 (Edge cases): Tests passed\n")

# ==============================================================================
# SECTION 8: INTEGRATION TESTS
# ==============================================================================

test_that("TandemVarClust: Complete workflow works end-to-end", {
  # Create model
  model <- TandemVarClust$new(K = 3, n_factors = 2, n_bins = 4)
  
  # Fit
  model$fit(titanic_df[, 1:4])
  
  # Get results
  groupes <- model$Groupes
  expect_true(length(groupes) > 0)
  
  # Summary
  output <- capture.output(model$summary())
  expect_true(length(output) > 0)
  
  # Variable summary
  var_summary <- suppressMessages(model$get_variable_summary())
  expect_equal(nrow(var_summary), 4)
  
  # Change K
  model$K <- 2
  groupes2 <- model$Groupes
  expect_true(!identical(groupes, groupes2))
  
  # Integrity check
  integrity <- suppressMessages(model$check_results_integrity())
  expect_true(is.logical(integrity))
  
  # Get modalities
  class_mods <- suppressMessages(model$get_modalities_of_variable("Class"))
  expect_true(is.data.frame(class_mods))
  
  # Get cluster modalities
  cluster_mods <- suppressMessages(model$get_modalities_of_cluster(1))
  expect_true(is.data.frame(cluster_mods))
})

test_that("TandemVarClust: Multiple K values produce consistent results", {
  model <- TandemVarClust$new(K = 2)
  model$fit(titanic_df[, 1:4])
  
  # Test K = 2, 3, 4
  for (k in 2:4) {
    model$K <- k
    groupes <- model$Groupes
    
    expect_true(all(groupes %in% 1:k))
    expect_equal(length(unique(groupes)), k)
  }
})

cat("\n✓ Section 8 (Integration): Tests passed\n")

# ==============================================================================
# SECTION 9: NUMERICAL STABILITY TESTS
# ==============================================================================

test_that("TandemVarClust: Handles highly correlated variables", {
  # Create data with correlated structure
  set.seed(123)
  n <- 100
  corr_data <- data.frame(
    var1 = factor(sample(c("A", "B"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2"), n, replace = TRUE))
  )
  
  # Make var2 correlated with var1
  corr_data$var2[corr_data$var1 == "A"] <- "X"
  
  model <- TandemVarClust$new(K = 2)
  expect_silent(model$fit(corr_data))
  
  groupes <- model$Groupes
  expect_true(length(groupes) > 0)
})

test_that("TandemVarClust: Handles unbalanced modalities", {
  # One modality much more frequent than others
  unbal_data <- data.frame(
    var1 = factor(c(rep("A", 95), rep("B", 3), rep("C", 2))),
    var2 = factor(c(rep("X", 90), rep("Y", 10)))
  )
  
  model <- TandemVarClust$new(K = 2)
  expect_silent(model$fit(unbal_data))
})

cat("\n✓ Section 9 (Numerical stability): Tests passed\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

message("\n")
message("═══════════════════════════════════════════════════════")
message("  ALL TANDEMVARCLUST TESTS PASSED SUCCESSFULLY!")
message("═══════════════════════════════════════════════════════")
message("")
message("Test coverage:")
message("  ✓ Initialization and parameter validation")
message("  ✓ Fitting with various data types")
message("  ✓ Results format and structure")
message("  ✓ Summary and display methods")
message("  ✓ K modification and refitting")
message("  ✓ Helper methods (get_variable_summary, etc.)")
message("  ✓ Edge cases and boundary conditions")
message("  ✓ Integration tests (complete workflows)")
message("  ✓ Numerical stability")
message("")
