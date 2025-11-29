#' VAR_KMEANS: K-Means Variable Clustering with Principal Components
#'
#' @description
#' Implementation of the Vigneau & Qannari algorithm for variable clustering.
#' Uses iterative reallocation with cluster centers represented by the first
#' principal component. Maximizes the sum of squared correlations (r²) between
#' variables and their cluster centers.
#'
#' @details
#' The algorithm follows the method described by Vigneau & Qannari:
#' 
#' 1. **Initialization**: K variables randomly selected as initial centers
#' 2. **Allocation**: Each variable assigned to cluster with maximum r²
#' 3. **Representation**: Cluster center = 1st principal component via PCA
#' 4. **Convergence**: Iterate until convergence or max iterations
#' 
#' The optimization criterion is: maximize Σ_k λ_k where λ_k = Σ r²(X_j, U_k)
#' and U_k is the first principal component of cluster k.
#'
#' @references
#' Chavent, M., Kuentz-Simonet, V., Liquet, B., & Saracco, J. (2012).
#' ClustOfVar: An R Package for the Clustering of Variables.
#' Journal of Statistical Software, 50(13), 1-16.
#'
#' @examples
#' # Basic usage
#' model <- VAR_KMEANS$new(K = 3, n_init = 20)
#' model$fit(iris[, 1:4])
#' print(model$Groupes)
#' model$summary()
#'
#' # Predict new variables
#' new_var <- data.frame(NewVar = iris$Sepal.Length + iris$Petal.Length)
#' prediction <- model$predict(new_var)
#'
#' @seealso [VAR_CAH], [TandemVarClust], [roller_clust()]
#'
#' @export
VAR_KMEANS <- R6::R6Class(
  "VAR_KMEANS",
  inherit = ClusterAnalysis,
  
  # ===========================================================
  # PUBLIC INTERFACE
  # ===========================================================
  
  public = list(
    
    #' @description
    #' Initialize a new VAR_KMEANS object
    #'
    #' @param K Number of clusters (integer >= 2)
    #' @param n_init Number of random initializations (default: 10)
    #' @param max_iter Maximum number of iterations (default: 100)
    #' @param tolerance Convergence tolerance (default: 1e-6)
    #' @param scale Logical, standardize variables (default: TRUE)
    #' @param na_action How to handle missing values: "warn", "omit", or "fail"
    #'
    #' @return A new VAR_KMEANS object
    initialize = function(K = 2,
                          n_init = 10,
                          max_iter = 100,
                          tolerance = 1e-6,
                          scale = TRUE,
                          na_action = "warn") {
      
      # === CORRECTION 1: Validation K avec is.numeric() ===
      if (!is.numeric(K)) {
        stop("K must be a numeric value")
      }
      K <- as.integer(K)
      if (is.na(K) || K < 2) {
        stop("K must be an integer >= 2")
      }
      
      # Validation n_init
      if (!is.numeric(n_init) || n_init < 1) {
        stop("n_init must be a positive integer")
      }
      n_init <- as.integer(n_init)
      
      # Validation max_iter
      if (!is.numeric(max_iter) || max_iter < 1) {
        stop("max_iter must be a positive integer")
      }
      max_iter <- as.integer(max_iter)
      
      # Validation tolerance
      if (!is.numeric(tolerance) || tolerance <= 0) {
        stop("tolerance must be a positive number")
      }
      
      # Validation scale
      if (!is.logical(scale)) {
        stop("scale must be TRUE or FALSE")
      }
      
      # Validation na_action
      if (!na_action %in% c("warn", "omit", "fail")) {
        stop("na_action must be 'warn', 'omit', or 'fail'")
      }
      
      # Initialisation des champs prives
      private$FNbGroupes <- K
      private$Fn_init <- n_init
      private$Fmax_iter <- max_iter
      private$Ftolerance <- tolerance
      private$FScale <- scale
      private$FNAAction <- na_action
      
      # === CORRECTION 2: Initialiser FGroupes pour eviter NULL ===
      private$FGroupes <- integer(0)
      
      # Initialisation des metriques
      private$FWithinClusterInertia <- Inf
      private$FHomogeneite <- 0
      private$FConverged <- FALSE
      private$FNIterations <- 0
      private$FClusterCenters <- NULL
      
      # Initialisation des donnees
      private$FFitted <- FALSE
      private$FX <- NULL
      private$FX_scaled <- NULL
      private$FVarNames <- NULL
      private$FHasMissing <- FALSE
      private$FNAIndices <- NULL
      
      invisible(self)
    },
    
    #' @description
    #' Get variables in a specific cluster
    #'
    #' @param k Cluster number
    #' @return Character vector of variable names
    get_cluster_variables = function(k) {
      if (!private$FFitted) {
        stop("Model must be fitted first")
      }
      if (k < 1 || k > private$FNbGroupes) {
        stop("k must be between 1 and ", private$FNbGroupes)
      }
      names(private$FGroupes)[private$FGroupes == k]
    },
    
    #' @description
    #' Get cluster centers
    #'
    #' @return Matrix of cluster centers (observations x clusters)
    get_cluster_centers = function() {
      if (!private$FFitted) {
        stop("Model must be fitted first")
      }
      if (is.null(private$FClusterCenters)) {
        stop("Cluster centers not available")
      }
      # Transpose: FClusterCenters is (clusters, observations)
      # Return (observations, clusters)
      centers <- t(private$FClusterCenters)
      if (!is.matrix(centers)) {
        centers <- as.matrix(centers)
      }
      return(centers)
    }
  ),
  
  # ===========================================================
  # ACTIVE BINDINGS
  # ===========================================================
  
  active = list(
    
    #' @field K Number of clusters (modifiable)
    K = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        if (!is.numeric(value) || value < 2) {
          stop("K must be an integer >= 2")
        }
        value <- as.integer(value)
        
        if (value != private$FNbGroupes) {
          private$FNbGroupes <- value
          
          if (private$FFitted) {
            message("K has been modified. Refitting the model...")
            private$do_refit_with_k(value)
          }
        }
        invisible(self)
      }
    },
    
    #' @field Groupes Cluster assignments (read-only)
    Groupes = function() {
      if (is.null(private$FGroupes) || length(private$FGroupes) == 0) {
        stop("The model must be fitted with $fit() before accessing Groupes.")
      }
      return(private$FGroupes)
    },
    
    #' @field WithinClusterInertia Total within-cluster inertia (read-only)
    WithinClusterInertia = function() {
      if (!private$FFitted) {
        stop("The model must be fitted before accessing WithinClusterInertia")
      }
      return(private$FWithinClusterInertia)
    },
    
    #' @field Homogeneite Overall homogeneity (read-only)
    Homogeneite = function() {
      if (!private$FFitted) {
        stop("The model must be fitted before accessing Homogeneite")
      }
      return(private$FHomogeneite)
    },
    
    #' @field Converged Convergence status (read-only)
    Converged = function() {
      if (!private$FFitted) {
        stop("The model must be fitted before accessing Converged")
      }
      return(private$FConverged)
    },
    
    #' @field NIterations Number of iterations (read-only)
    NIterations = function() {
      if (!private$FFitted) {
        stop("The model must be fitted before accessing NIterations")
      }
      return(private$FNIterations)
    }
  ),
  
  # ===========================================================
  # PRIVATE IMPLEMENTATION
  # ===========================================================
  
  private = list(
    
    # Fields
    FNbGroupes = NULL,
    Fn_init = NULL,
    Fmax_iter = NULL,
    Ftolerance = NULL,
    FScale = NULL,
    FNAAction = NULL,
    FGroupes = NULL,
    FWithinClusterInertia = NULL,
    FHomogeneite = NULL,
    FConverged = NULL,
    FNIterations = NULL,
    FClusterCenters = NULL,
    FFitted = NULL,
    FX = NULL,
    FX_scaled = NULL,
    FVarNames = NULL,
    FHasMissing = NULL,
    FNAIndices = NULL,
    
    # ===========================================================
    # MAIN ALGORITHM
    # ===========================================================
    
    #'
    #' Fit the K-means model
    do_fit = function(X) {
      
      # Convert to data.frame
      if (!is.data.frame(X)) {
        X <- as.data.frame(X)
      }
      
      # Store original data
      private$FX <- X
      private$FVarNames <- colnames(X)
      
      if (is.null(private$FVarNames)) {
        private$FVarNames <- paste0("Var", 1:ncol(X))
        colnames(private$FX) <- private$FVarNames
      }
      
      # Check dimensions
      n_vars <- ncol(X)
      if (n_vars < 2) {
        stop("At least 2 variables are required for clustering")
      }
      
      if (private$FNbGroupes > n_vars) {
        stop("K (", private$FNbGroupes, ") cannot exceed the number of variables (", n_vars, ")")
      }
      
      # Handle missing values
      private$FHasMissing <- any(is.na(X))
      
      if (private$FHasMissing) {
        if (private$FNAAction == "fail") {
          stop("Missing values detected and na_action='fail'")
        } else if (private$FNAAction == "warn") {
          warning("Missing values detected. Using pairwise complete observations.")
        }
        
        private$FNAIndices <- which(is.na(X), arr.ind = TRUE)
      }
      
      # Standardize data
      if (private$FScale) {
        X_scaled <- scale(X, center = TRUE, scale = TRUE)
        
        # Handle zero variance
        zero_var <- apply(X, 2, function(x) sd(x, na.rm = TRUE)) == 0
        if (any(zero_var)) {
          warning("Variables with zero variance will not be scaled: ",
                  paste(colnames(X)[zero_var], collapse = ", "))
          X_scaled[, zero_var] <- X[, zero_var]
        }
      } else {
        X_scaled <- as.matrix(X)
      }
      
      # Transpose: rows = variables, cols = observations
      private$FX_scaled <- t(X_scaled)
      
      # Run K-means with multiple initializations
      best_result <- private$kmeans_multiple_runs()
      
      # Store results
      private$FGroupes <- best_result$clusters
      private$FClusterCenters <- best_result$centers
      private$FWithinClusterInertia <- best_result$inertia
      private$FConverged <- best_result$converged
      private$FNIterations <- best_result$iterations
      
      # Calculate homogeneity
      private$FHomogeneite <- private$compute_homogeneity()
      
      # ===  Assignation noms securisee ===
      if (!is.null(private$FGroupes) && 
          !is.null(private$FVarNames) &&
          length(private$FGroupes) == length(private$FVarNames)) {
        names(private$FGroupes) <- private$FVarNames
      } else {
        if (is.null(private$FGroupes)) {
          warning("FGroupes is NULL after fitting - this should not happen")
        } else if (length(private$FGroupes) != length(private$FVarNames)) {
          warning("Length mismatch: FGroupes (", length(private$FGroupes), 
                  ") vs FVarNames (", length(private$FVarNames), ")")
        }
      }
      
      private$FFitted <- TRUE
      
      invisible(self)
    },
    
    #'
    #' Run K-means with multiple random initializations
    kmeans_multiple_runs = function() {
      
      best_criterion <- -Inf  # We MAXIMIZE the criterion (sum of r²)
      best_result <- NULL
      
      for (i in 1:private$Fn_init) {
        result <- private$kmeans_single_run()
        
        if (result$inertia > best_criterion) {  # Changed from < to >
          best_criterion <- result$inertia
          best_result <- result
        }
      }
      
      return(best_result)
    },
    
    #'
    #' Single run of K-means algorithm
    kmeans_single_run = function() {
      
      # Initialize cluster centers
      centers <- private$initialize_centers()
      
      n_vars <- nrow(private$FX_scaled)  # Number of rows = variables
      clusters <- integer(n_vars)
      converged <- FALSE
      iteration <- 0
      
      # Iterative optimization
      while (!converged && iteration < private$Fmax_iter) {
        iteration <- iteration + 1
        
        # Assignment step
        old_clusters <- clusters
        clusters <- private$assign_to_clusters(centers)
        
        # Update step
        centers <- private$update_centers(clusters)
        
        # Check convergence
        if (all(clusters == old_clusters)) {
          converged <- TRUE
        }
      }
      
      # Compute final inertia
      inertia <- private$compute_inertia(clusters, centers)
      
      return(list(
        clusters = clusters,
        centers = centers,
        inertia = inertia,
        converged = converged,
        iterations = iteration
      ))
    },
    
    # ===========================================================
    # INITIALIZATION
    # ===========================================================
    
    #'
    #' Initialize centers with empty cluster protection ===
    initialize_centers = function() {
      
      n_vars <- nrow(private$FX_scaled)  # Number of rows = variables
      
      # Verify K <= n_vars
      if (private$FNbGroupes > n_vars) {
        stop("K (", private$FNbGroupes, ") cannot be greater than ",
             "the number of variables (", n_vars, ")")
      }
      
      # Special case: K = n_vars (each variable in its own cluster)
      if (private$FNbGroupes == n_vars) {
        initial_assignment <- 1:n_vars
      } else {
        # Random initialization with protection against empty clusters
        max_attempts <- 100
        initial_assignment <- NULL
        
        for (attempt in 1:max_attempts) {
          # Random assignment
          temp_assignment <- sample(1:private$FNbGroupes, n_vars, replace = TRUE)
          
          # Check that no cluster is empty
          cluster_counts <- table(factor(temp_assignment, 
                                         levels = 1:private$FNbGroupes))
          
          if (all(cluster_counts > 0)) {
            # Success - all clusters have at least one variable
            initial_assignment <- temp_assignment
            break
          }
        }
        
        # Fallback strategy if failed after max_attempts
        if (is.null(initial_assignment)) {
          # Equal distribution then random permutation
          initial_assignment <- rep(1:private$FNbGroupes, 
                                    length.out = n_vars)
          initial_assignment <- sample(initial_assignment)
        }
      }
      
      # Compute initial centers
      centers <- matrix(NA, nrow = private$FNbGroupes, 
                        ncol = ncol(private$FX_scaled))  # ncol = observations
      
      for (k in 1:private$FNbGroupes) {
        vars_in_cluster <- which(initial_assignment == k)
        
        # Double check (should never happen)
        if (length(vars_in_cluster) == 0) {
          stop("Internal error: Empty cluster ", k, " after initialization")
        }
        
        centers[k, ] <- private$compute_cluster_center(vars_in_cluster)
      }
      
      return(centers)
    },
    
    # ===========================================================
    # ASSIGNMENT AND UPDATE
    # ===========================================================
    
    #'
    #' Assign variables to nearest cluster (maximum r²)
    assign_to_clusters = function(centers) {
      
      n_vars <- nrow(private$FX_scaled)  # Number of rows = variables
      clusters <- integer(n_vars)
      
      for (j in 1:n_vars) {
        var_j <- private$FX_scaled[j, ]  # Select j-th row (variable)
        
        # Compute r² to each center
        r_squareds <- numeric(private$FNbGroupes)
        for (k in 1:private$FNbGroupes) {
          # r² (squared correlation) - always positive
          cor_val <- cor(var_j, centers[k, ], use = "complete.obs")
          r_squareds[k] <- cor_val^2
        }
        
        # Assign to cluster with maximum r²
        clusters[j] <- which.max(r_squareds)
      }
      
      return(clusters)
    },
    
    #'
    #' Update cluster centers
    update_centers = function(clusters) {
      
      centers <- matrix(NA, nrow = private$FNbGroupes, 
                        ncol = ncol(private$FX_scaled))  # ncol = observations
      
      for (k in 1:private$FNbGroupes) {
        vars_in_cluster <- which(clusters == k)
        
        if (length(vars_in_cluster) > 0) {
          centers[k, ] <- private$compute_cluster_center(vars_in_cluster)
        } else {
          # Handle empty cluster (should be rare)
          warning("Empty cluster ", k, " detected during update")
          # Reinitialize randomly
          random_var <- sample(nrow(private$FX_scaled), 1)  # nrow = variables
          centers[k, ] <- private$FX_scaled[random_var, ]
        }
      }
      
      return(centers)
    },
    
    #'
    #' Compute cluster center (first principal component)
    compute_cluster_center = function(var_indices) {
      
      # ===  Verify cluster is not empty ===
      if (length(var_indices) == 0) {
        stop("Cannot compute center for empty cluster")
      }
      
      if (length(var_indices) == 1) {
        # Single variable: return it as is (normalized)
        center <- private$FX_scaled[var_indices, ]
        center_norm <- sqrt(sum(center^2, na.rm = TRUE))
        if (center_norm > 0) {
          center <- center / center_norm
        }
        return(center)
      } else {
        # Multiple variables: compute 1st principal component via PCA
        # FX_scaled is (variables, observations), so select rows
        cluster_data <- private$FX_scaled[var_indices, , drop = FALSE]
        
        # Center the data (by observation)
        cluster_centered <- t(scale(t(cluster_data), center = TRUE, scale = FALSE))
        
        # Correlation matrix between variables
        R <- cor(t(cluster_centered), use = "pairwise.complete.obs")
        
        # Eigendecomposition
        eigen_result <- eigen(R, symmetric = TRUE)
        
        # 1st eigenvalue and eigenvector
        lambda1 <- eigen_result$values[1]
        v1 <- eigen_result$vectors[, 1, drop = TRUE]
        
        # 1st principal component: U_k = cluster_centered^T %*% v1
        # cluster_centered is (n_vars, n_obs), v1 is (n_vars,)
        # Result should be (n_obs,) vector
        pc1 <- as.vector(t(cluster_centered) %*% v1)
        
        # Normalize to unit variance (standard normalization for PC)
        pc1_sd <- sd(pc1, na.rm = TRUE)
        if (pc1_sd > 0) {
          pc1 <- pc1 / pc1_sd
        }
        
        # Final normalization to unit length
        pc1_norm <- sqrt(sum(pc1^2, na.rm = TRUE))
        if (pc1_norm > 0) {
          pc1 <- pc1 / pc1_norm
        }
        
        return(pc1)
      }
    },
    
    # ===========================================================
    # METRICS
    # ===========================================================
    
    #'
    #' Compute within-cluster criterion (sum of r²)
    #' Higher values indicate better clustering (we MAXIMIZE this)
    compute_inertia = function(clusters, centers) {
      
      total_criterion <- 0
      
      for (k in 1:private$FNbGroupes) {
        vars_in_cluster <- which(clusters == k)
        
        if (length(vars_in_cluster) > 0) {
          for (j in vars_in_cluster) {
            var_j <- private$FX_scaled[j, ]  # Select j-th row (variable)
            cor_val <- cor(var_j, centers[k, ], use = "complete.obs")
            r_squared <- cor_val^2  # r² (always positive)
            total_criterion <- total_criterion + r_squared
          }
        }
      }
      
      return(total_criterion)
    },
    
    #'
    #' Compute overall homogeneity (mean r²)
    compute_homogeneity = function() {
      
      homogeneities <- numeric(private$FNbGroupes)
      cluster_sizes <- integer(private$FNbGroupes)
      
      for (k in 1:private$FNbGroupes) {
        vars_in_cluster <- which(private$FGroupes == k)
        n_vars_k <- length(vars_in_cluster)
        cluster_sizes[k] <- n_vars_k
        
        if (n_vars_k == 0) {
          homogeneities[k] <- 0
        } else if (n_vars_k == 1) {
          homogeneities[k] <- 1
        } else {
          # r² with cluster center (1st PC)
          cluster_data <- private$FX_scaled[vars_in_cluster, , drop = FALSE]
          center_k <- private$FClusterCenters[k, ]
          
          # Compute r² for each variable with center
          r_squareds <- numeric(n_vars_k)
          for (i in 1:n_vars_k) {
            cor_val <- cor(cluster_data[i, ], center_k, use = "complete.obs")
            r_squareds[i] <- cor_val^2
          }
          homogeneities[k] <- mean(r_squareds, na.rm = TRUE)
        }
      }
      
      # Weighted average
      overall_homogeneity <- sum(homogeneities * cluster_sizes) / sum(cluster_sizes)
      
      return(overall_homogeneity)
    },
    
    # ===========================================================
    # REFIT WITH NEW K
    # ===========================================================
    
    #'
    #' Refit model with new number of clusters
    do_refit_with_k = function(new_k) {
      
      if (is.null(private$FX)) {
        stop("Cannot refit: original data not available")
      }
      
      old_k <- private$FNbGroupes
      private$FNbGroupes <- as.integer(new_k)
      
      # Refit
      private$do_fit(private$FX)
      
      message("Model refitted with K = ", new_k, " (was K = ", old_k, ")")
      
      invisible(self)
    },
    
    # ===========================================================
    # PREDICTION
    # ===========================================================
    
    #'
    #' Predict cluster for new variables
    do_predict = function(newdata) {
      
      if (!private$FFitted) {
        stop("Model must be fitted before prediction")
      }
      
      # Handle vector input explicitly
      if (is.vector(newdata)) {
        newdata <- data.frame(V1 = newdata)
      }
      
      if (!is.data.frame(newdata)) {
        newdata <- as.data.frame(newdata)
      }
      
      # Ensure same number of observations
      if (nrow(newdata) != ncol(private$FX_scaled)) {
        stop("newdata must have ", ncol(private$FX_scaled), " observations")
      }
      
      # Standardize if needed
      if (private$FScale) {
        newdata_scaled <- scale(newdata, center = TRUE, scale = TRUE)
      } else {
        newdata_scaled <- as.matrix(newdata)
      }
      
      # Transpose: rows = variables, cols = observations
      newdata_scaled <- t(newdata_scaled)
      
      # Predict for each new variable
      results <- list()
      
      for (j in 1:nrow(newdata_scaled)) {  # nrow = variables
        var_name <- colnames(newdata)[j]
        if (is.null(var_name) || var_name == "") {
          var_name <- paste0("V", j)
        }
        
        var_j <- newdata_scaled[j, ]  # Select j-th row (variable)
        
        # Compute r² with each center (squared correlation)
        scores <- numeric(private$FNbGroupes)
        for (k in 1:private$FNbGroupes) {
          cor_val <- cor(var_j, private$FClusterCenters[k, ], use = "complete.obs")
          scores[k] <- cor_val^2  # r² (always positive)
        }
        
        # Assign to cluster with highest r²
        best_cluster <- which.max(scores)
        
        results[[var_name]] <- list(
          cluster = best_cluster,
          scores = scores,
          best_score = scores[best_cluster]
        )
      }
      
      return(results)
    },
    
    # ===========================================================
    # SUMMARY
    # ===========================================================
    
    #'
    #' Display summary with fitted verification ===
    do_summary = function() {
      
      # Verify model is fitted
      if (!private$FFitted) {
        stop("The model must be fitted with $fit() before calling summary()")
      }
      
      # Display banner
      cat("\n")
      cat("===========================================================\n")
      cat("   VAR_KMEANS - K-Means with Principal Components\n")
      cat("            (Vigneau & Qannari Algorithm)\n")
      cat("===========================================================\n\n")
      
      # Algorithm parameters
      cat("Algorithm: K-Means with 1st Principal Components (PCA)\n")
      cat("Optimization criterion: Maximize sum of squared correlations (r²)\n")
      cat("Cluster centers: 1st principal component of each cluster\n")
      cat("Number of initializations (n_init):", private$Fn_init, "\n")
      cat("Maximum iterations:", private$Fmax_iter, "\n")
      cat("Convergence tolerance:", private$Ftolerance, "\n")
      cat("Data standardization:", private$FScale, "\n")
      cat("Number of clusters (K):", private$FNbGroupes, "\n")
      cat("Number of variables:", length(private$FGroupes), "\n\n")
      
      # Quality metrics
      cat("=== Clustering Quality Metrics ===\n\n")
      cat("Sum of r² (criterion):", round(private$FWithinClusterInertia, 4), "\n")
      cat("Mean homogeneity (r²):", round(private$FHomogeneite, 4), "\n")
      cat(sprintf("Proportion of variance explained: %.2f%%\n", 
                  100 * private$FHomogeneite))
      cat("Convergence status:", private$FConverged, "\n")
      cat("Number of iterations:", private$FNIterations, "\n\n")
      
      # Cluster details
      cat("=== Cluster Details ===\n\n")
      
      for (k in 1:private$FNbGroupes) {
        vars_in_k <- which(private$FGroupes == k)
        n_vars_k <- length(vars_in_k)
        
        cat("Cluster", k, ":", n_vars_k, "variable(s)\n")
        
        if (n_vars_k > 0) {
          var_names_k <- names(private$FGroupes)[vars_in_k]
          cat("  Variables:", paste(var_names_k, collapse = ", "), "\n")
          
          # Cluster homogeneity (mean r²)
          if (n_vars_k > 1) {
            cluster_data <- private$FX_scaled[vars_in_k, , drop = FALSE]
            center_k <- private$FClusterCenters[k, ]
            
            # Compute r² for each variable with center
            r_squareds <- numeric(n_vars_k)
            for (i in 1:n_vars_k) {
              cor_val <- cor(cluster_data[i, ], center_k, use = "complete.obs")
              r_squareds[i] <- cor_val^2
            }
            mean_r2 <- mean(r_squareds, na.rm = TRUE)
            cat(sprintf("  Homogeneity (mean r²): %.4f\n", mean_r2))
          } else {
            cat("  Homogeneity (r²): 1.0000 (single variable)\n")
          }
        }
        cat("\n")
      }
      
      cat("===========================================================\n")
      
      invisible(self)
    }
  )
)