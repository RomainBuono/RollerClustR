#' TandemVarClust: Tandem Clustering (MCA + HAC) for Mixed Variables
#'
#' @description
#' This class implements a Tandem approach combining Multiple Correspondence Analysis
#' (MCA) with Hierarchical Agglomerative Clustering (HAC) to perform clustering of
#' modalities (rather than directly clustering variables).
#'
#' The clustering operates at the modality level:
#' - Categorical variables: each category becomes a modality
#' - Quantitative variables: discretization into bins, each bin becomes a modality
#'
#' The predict() method predicts the cluster assignment for new variables by projecting
#' them into the AFDM space and calculating distances to cluster centers.
#'
#' @export
TandemVarClust <- R6::R6Class(
  "TandemVarClust",
  inherit = ClusterAnalysis,
  
  # ============================================================================
  # PRIVATE ATTRIBUTES
  # ============================================================================
  private = list(
    # Configuration parameters
    FNbGroupes = NULL,
    FNBins = NULL,
    FMethodCAH = NULL,
    FNFactors = NULL,
    FScale = NULL,
    
    # Data and metadata
    FX = NULL,
    FCatVarNames = NULL,
    FNumVarNames = NULL,
    FHasCategoricalVars = FALSE,
    FHasNumericVars = FALSE,
    FVarTypes = NULL,
    
    # Intermediate results
    FDisjunctiveTable = NULL,
    FFactorialCoords = NULL,
    FACMVariance = NULL,
    FInertie = NULL,
    FNFactorsTotal = NULL,
    
    # Clustering results
    FHclustTree = NULL,
    FGroupes = NULL,
    
    # NOUVEAUX CHAMPS pour predict_variable
    FColMargins = NULL,
    FColWeights = NULL,
    FRowWeights = NULL,
    FSVDv = NULL,
    FDColSqrt = NULL,
    FClusterCenters = NULL,
    
    # ═══════════════════════════════════════════════════════════════
    # MÉTHODES PRIVÉES
    # ═══════════════════════════════════════════════════════════════
    
    prepare_mixed_data = function(X) {
      var_types <- sapply(X, function(col) {
        if (is.factor(col) || is.character(col)) "categorical"
        else if (is.numeric(col)) "numeric"
        else "unknown"
      })
      
      if (any(var_types == "unknown")) {
        unknown_vars <- names(var_types)[var_types == "unknown"]
        stop("Unknown variable types detected: ", paste(unknown_vars, collapse = ", "))
      }
      
      private$FVarTypes <- var_types
      private$FCatVarNames <- names(var_types)[var_types == "categorical"]
      private$FNumVarNames <- names(var_types)[var_types == "numeric"]
      private$FHasCategoricalVars <- length(private$FCatVarNames) > 0
      private$FHasNumericVars <- length(private$FNumVarNames) > 0
      
      if (!private$FHasCategoricalVars && !private$FHasNumericVars) {
        stop("No valid variables detected in the data.")
      }
    },
    
    build_disjunctive_table = function(X) {
      disjunctive_list <- list()
      
      if (private$FHasCategoricalVars) {
        for (var_name in private$FCatVarNames) {
          col <- X[[var_name]]
          if (!is.factor(col)) col <- as.factor(col)
          
          levels_var <- levels(col)
          for (level in levels_var) {
            indicator <- as.integer(col == level)
            col_name <- paste0(var_name, ".", level)
            disjunctive_list[[col_name]] <- indicator
          }
        }
      }
      
      if (private$FHasNumericVars) {
        for (var_name in private$FNumVarNames) {
          col <- X[[var_name]]
          
          if (private$FScale) {
            col <- scale(col)[, 1]
          }
          
          breaks <- quantile(col, probs = seq(0, 1, length.out = private$FNBins + 1),
                             na.rm = TRUE)
          breaks <- unique(breaks)
          
          if (length(breaks) <= 1) {
            warning("Variable '", var_name, "' has zero or very low variance. Ignored.")
            next
          }
          
          discretized <- cut(col, breaks = breaks, include.lowest = TRUE, labels = FALSE)
          
          n_bins_actual <- max(discretized, na.rm = TRUE)
          for (bin in 1:n_bins_actual) {
            indicator <- as.integer(discretized == bin)
            col_name <- paste0(var_name, ".bin", bin)
            disjunctive_list[[col_name]] <- indicator
          }
        }
      }
      
      Z <- as.data.frame(disjunctive_list)
      
      if (ncol(Z) == 0) {
        stop("The disjunctive table is empty. Check your data.")
      }
      
      return(Z)
    },
    
    perform_factorial_analysis = function() {
      Z <- private$FDisjunctiveTable
      n <- nrow(Z)
      p <- ncol(Z)
      
      col_margins <- colSums(Z) / n
      col_weights <- 1 / col_margins
      col_weights[!is.finite(col_weights)] <- 0
      
      private$FColMargins <- col_margins
      private$FColWeights <- col_weights
      
      row_weights <- rep(1/n, n)
      private$FRowWeights <- row_weights
      
      Z_centered <- sweep(Z, 2, col_margins, "-")
      
      D_col_sqrt <- diag(sqrt(col_weights))
      D_row_sqrt <- diag(sqrt(row_weights))
      
      private$FDColSqrt <- D_col_sqrt
      
      Z_weighted <- D_row_sqrt %*% as.matrix(Z_centered) %*% D_col_sqrt
      
      svd_res <- svd(Z_weighted)
      
      private$FSVDv <- svd_res$v
      
      private$FFactorialCoords <- D_col_sqrt %*% svd_res$v
      rownames(private$FFactorialCoords) <- colnames(Z)
      
      eigenvalues <- svd_res$d^2
      private$FACMVariance <- 100 * eigenvalues / sum(eigenvalues)
      private$FInertie <- sum(eigenvalues)
      private$FNFactorsTotal <- length(eigenvalues)
    },
    
    perform_clustering = function() {
      n_factors_to_use <- if (is.null(private$FNFactors)) {
        private$FNFactorsTotal
      } else {
        min(private$FNFactors, private$FNFactorsTotal)
      }
      
      coords <- private$FFactorialCoords[, 1:n_factors_to_use, drop = FALSE]
      
      dist_matrix <- dist(coords, method = "euclidean")
      
      private$FHclustTree <- hclust(dist_matrix, method = private$FMethodCAH)
      
      n_modalities <- nrow(private$FFactorialCoords)
      if (private$FNbGroupes > n_modalities) {
        stop("K (", private$FNbGroupes, 
             ") cannot exceed the number of modalities (", n_modalities, ")")
      }
      
      private$FGroupes <- cutree(private$FHclustTree, k = private$FNbGroupes)
      names(private$FGroupes) <- rownames(coords)
      
      # Calculer les centres de clusters
      cluster_centers <- matrix(NA, nrow = private$FNbGroupes, ncol = n_factors_to_use)
      
      for (k in 1:private$FNbGroupes) {
        modalities_in_cluster <- which(private$FGroupes == k)
        
        if (length(modalities_in_cluster) > 0) {
          cluster_centers[k, ] <- colMeans(coords[modalities_in_cluster, , drop = FALSE])
        }
      }
      
      rownames(cluster_centers) <- paste0("Cluster_", 1:private$FNbGroupes)
      colnames(cluster_centers) <- paste0("Dim", 1:n_factors_to_use)
      
      private$FClusterCenters <- cluster_centers
    },
    
    assign_observations_to_clusters = function() {
      Z <- private$FDisjunctiveTable
      n <- nrow(Z)
      groupes <- private$FGroupes
      K <- private$FNbGroupes
      
      obs_clusters <- integer(n)
      dice_scores <- matrix(0, nrow = n, ncol = K)
      colnames(dice_scores) <- paste0("Cluster_", 1:K)
      
      cluster_modalities <- lapply(1:K, function(k) {
        which(groupes == k)
      })
      
      for (i in 1:n) {
        modalities_i <- which(Z[i, ] == 1)
        n_modalities_i <- length(modalities_i)
        
        if (n_modalities_i == 0) {
          obs_clusters[i] <- 1
          dice_scores[i, ] <- NA
          next
        }
        
        for (k in 1:K) {
          modalities_k <- cluster_modalities[[k]]
          n_modalities_k <- length(modalities_k)
          
          intersection <- length(intersect(modalities_i, modalities_k))
          
          if (n_modalities_i + n_modalities_k > 0) {
            dice_scores[i, k] <- (2 * intersection) / (n_modalities_i + n_modalities_k)
          } else {
            dice_scores[i, k] <- 0
          }
        }
        
        obs_clusters[i] <- which.max(dice_scores[i, ])
      }
      
      return(list(
        clusters = obs_clusters,
        scores = dice_scores
      ))
    },
    
    compute_cramers_v = function(contingency) {
      chi2_test <- suppressWarnings(chisq.test(contingency))
      chi2 <- chi2_test$statistic
      n <- sum(contingency)
      min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
      
      if (min_dim == 0) return(NA)
      
      v <- sqrt(chi2 / (n * min_dim))
      return(as.numeric(v))
    },
    
    # ========================================================================
    # CORRECTED predict_variable() method with complete statistical output
    # ========================================================================
    predict_variable = function(new_var, var_name = "new_var") {
      
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted. Call $fit() first.")
      }
      
      if (is.null(private$FClusterCenters)) {
        stop("Cluster centers not computed. This shouldn't happen.")
      }
      
      n <- nrow(private$FX)
      
      if (length(new_var) != n) {
        stop(paste0("new_var must have the same number of observations as training data. ",
                    "Expected: ", n, ", Received: ", length(new_var)))
      }
      
      # Convert to factor if needed
      if (!(is.factor(new_var) || is.character(new_var))) {
        warning("Variable '", var_name, "' is not categorical. Converting to factor.")
        new_var <- as.factor(new_var)
      }
      
      if (!is.factor(new_var)) {
        new_var <- as.factor(new_var)
      }
      
      levels_new <- levels(new_var)
      n_modalities <- length(levels_new)
      
      # ========================================================================
      # STEP 1: Create disjunctive table for new variable
      # ========================================================================
      Z_new <- matrix(0, nrow = n, ncol = n_modalities)
      colnames(Z_new) <- paste0(var_name, ".", levels_new)
      
      for (j in 1:n_modalities) {
        Z_new[, j] <- as.integer(new_var == levels_new[j])
      }
      
      # ========================================================================
      # STEP 2: AFDM projection of new modalities
      # ========================================================================
      # Number of factors used for clustering
      n_factors_to_use <- if (is.null(private$FNFactors)) {
        private$FNFactorsTotal
      } else {
        min(private$FNFactors, private$FNFactorsTotal)
      }
      
      # Validation
      n_factors_available <- ncol(private$FFactorialCoords)
      if (n_factors_to_use > n_factors_available) {
        n_factors_to_use <- n_factors_available
      }
      
      # Calculate observation coordinates in factorial space
      Z_train <- private$FDisjunctiveTable
      col_margins_train <- private$FColMargins
      Z_train_centered <- sweep(as.matrix(Z_train), 2, col_margins_train, "-")
      
      row_coords <- (1/sqrt(n)) * Z_train_centered %*% 
                    private$FFactorialCoords[, 1:n_factors_to_use, drop = FALSE]
      
      # Project new modalities using observation barycenters
      factorial_coords_new <- matrix(0, nrow = n_modalities, ncol = n_factors_to_use)
      rownames(factorial_coords_new) <- colnames(Z_new)
      colnames(factorial_coords_new) <- paste0("Dim", 1:n_factors_to_use)
      
      for (j in 1:n_modalities) {
        obs_with_modality <- which(Z_new[, j] == 1)
        
        if (length(obs_with_modality) > 0) {
          factorial_coords_new[j, ] <- colMeans(row_coords[obs_with_modality, , drop = FALSE])
        }
      }
      
      # ========================================================================
      # STEP 3: Calculate distances to cluster centers
      # ========================================================================
      distances_matrix <- matrix(NA, nrow = n_modalities, ncol = private$FNbGroupes)
      rownames(distances_matrix) <- rownames(factorial_coords_new)
      colnames(distances_matrix) <- paste0("Cluster_", 1:private$FNbGroupes)
      
      for (i in 1:n_modalities) {
        for (k in 1:private$FNbGroupes) {
          distances_matrix[i, k] <- sqrt(sum((factorial_coords_new[i, ] - 
                                         private$FClusterCenters[k, ])^2))
        }
      }
      
      # Average distance to each cluster
      distances <- colMeans(distances_matrix)
      names(distances) <- paste0("Cluster", 1:private$FNbGroupes)
      
      # ========================================================================
      # STEP 4: Assignment to cluster with minimum distance
      # ========================================================================
      predicted_cluster <- which.min(distances)
      
      # Individual assignment of each modality
      modality_clusters <- apply(distances_matrix, 1, which.min)
      names(modality_clusters) <- levels_new
      
      # ========================================================================
      # STEP 5: Get observation clusters and compute contingency
      # ========================================================================
      obs_results <- private$assign_observations_to_clusters()
      obs_clusters <- obs_results$clusters
      dice_scores <- obs_results$scores
      
      # Contingency table
      obs_clusters_factor <- factor(obs_clusters, levels = 1:private$FNbGroupes)
      contingency <- table(new_var, obs_clusters_factor)
      
      # ========================================================================
      # STEP 6: Statistical tests and metrics
      # ========================================================================
      chi2_test <- tryCatch({
        suppressWarnings(chisq.test(contingency))
      }, error = function(e) {
        list(statistic = NA, p.value = NA, parameter = NA)
      })
      
      # Cramér's V
      cramers_v <- tryCatch({
        n_total <- sum(contingency)
        chi2 <- as.numeric(chi2_test$statistic)
        min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
        
        if (is.na(chi2) || min_dim == 0) {
          NA
        } else {
          v <- sqrt(chi2 / (n_total * min_dim))
          min(v, 1.0)
        }
      }, error = function(e) {
        NA
      })
      
      significant <- !is.na(chi2_test$p.value) && chi2_test$p.value < 0.05
      
      # ========================================================================
      # STEP 7: Build complete result
      # ========================================================================
      result <- list(
        cluster = predicted_cluster,
        n_modalities = n_modalities,
        modality_clusters = modality_clusters,
        distances = distances,
        contingency = contingency,
        chi2_test = chi2_test,
        cramers_v = cramers_v,
        significant = significant,
        dice_scores = dice_scores
      )
      
      class(result) <- c("TandemVarClust_predict_single", class(result))
      return(result)
    },
    
    do_fit = function(X) {
      private$FX <- X
      private$prepare_mixed_data(X)
      private$FDisjunctiveTable <- private$build_disjunctive_table(X)
      private$perform_factorial_analysis()
      private$perform_clustering()
      
      invisible(self)
    },
    
    do_predict = function(newdata) {
      
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted. Call $fit() first.")
      }
      
      if (is.vector(newdata)) {
        newdata <- data.frame(new_var = newdata)
      } else if (!is.data.frame(newdata)) {
        stop("newdata must be a data frame or vector")
      }
      
      results <- list()
      
      for (var_name in names(newdata)) {
        
        message("\n=== Predicting for variable: ", var_name, " ===")
        
        tryCatch({
          
          pred <- private$predict_variable(newdata[[var_name]], var_name)
          results[[var_name]] <- pred
          
        }, error = function(e) {
          warning("Error predicting variable '", var_name, "': ", e$message)
          results[[var_name]] <- list(
            cluster = NA,
            error = e$message
          )
        })
      }
      
      class(results) <- c("TandemVarClust_predict", "list")
      return(results)
    },
    
    do_summary = function() {
      if (is.null(private$FGroupes)) {
        stop("The model must be fitted with $fit() before calling summary().")
      }
      
      cat("\n")
      cat("================================================================\n")
      cat("   TandemVarClust - Tandem Clustering (MCA + HAC)              \n")
      cat("================================================================\n")
      cat("Number of modality clusters : ", private$FNbGroupes, "\n", sep = "")
      cat("Total number of modalities  : ", length(private$FGroupes), "\n", sep = "")
      cat("Number of variables         : ", 
          length(unique(sub("\\..*", "", names(private$FGroupes)))), "\n", sep = "")
      cat("----------------------------------------------------------------\n")
      
      cluster_sizes <- table(private$FGroupes)
      cat("\nModalities per cluster:\n")
      for (k in 1:private$FNbGroupes) {
        size <- if (as.character(k) %in% names(cluster_sizes)) {
          cluster_sizes[as.character(k)]
        } else {
          0
        }
        cat("  Cluster ", k, ": ", size, " modalities\n", sep = "")
      }
      
      if (!is.null(private$FCatVarNames) || !is.null(private$FNumVarNames)) {
        cat("\nVariable types:\n")
        if (!is.null(private$FCatVarNames)) {
          cat("  Categorical: ", length(private$FCatVarNames), 
              " (", paste(head(private$FCatVarNames, 5), collapse = ", "), 
              if (length(private$FCatVarNames) > 5) "..." else "", ")\n", sep = "")
        }
        if (!is.null(private$FNumVarNames)) {
          cat("  Numeric    : ", length(private$FNumVarNames), 
              " (discretized into ", private$FNBins, " bins)\n", sep = "")
        }
      }
      
      cat("================================================================\n")
      cat("\n")
      
      invisible(self)
    }
  ),
  
  # ============================================================================
  # PUBLIC METHODS
  # ============================================================================
  public = list(
    
    initialize = function(K, n_bins = 5, method_cah = "ward.D2",
                          n_factors = NULL, scale = TRUE, na_action = "omit", ...) {
      super$initialize(na_action = na_action)
      
      extra_args <- list(...)
      if (length(extra_args) > 0) {
        warning("Suspicious parameters detected and ignored: ",
                paste(names(extra_args), collapse = ", "))
      }
      
      if (!is.numeric(K) || K < 2) {
        stop("K must be an integer >= 2")
      }
      
      if (!is.numeric(n_bins) || n_bins < 2 || n_bins > 20) {
        stop("n_bins must be between 2 and 20")
      }
      
      valid_methods <- c("ward.D", "ward.D2", "single", "complete", "average",
                         "mcquitty", "median", "centroid")
      if (!method_cah %in% valid_methods) {
        stop("invalid method_cah: ", method_cah)
      }
      
      if (!is.null(n_factors) && (!is.numeric(n_factors) || n_factors < 1)) {
        stop("n_factors must be NULL or an integer >= 1")
      }
      
      private$FNbGroupes <- as.integer(K)
      private$FNBins <- as.integer(n_bins)
      private$FMethodCAH <- method_cah
      private$FNFactors <- if (!is.null(n_factors)) as.integer(n_factors) else NULL
      private$FScale <- as.logical(scale)
    },
    
    refit_with_k = function(new_k) {
      if (is.null(private$FHclustTree)) {
        stop("The model is not yet fitted. Call $fit() first.")
      }
      
      if (!is.numeric(new_k) || new_k < 2) {
        stop("new_k must be an integer >= 2")
      }
      
      private$FNbGroupes <- as.integer(new_k)
      private$FGroupes <- cutree(private$FHclustTree, k = new_k)
      names(private$FGroupes) <- names(private$FGroupes)
      
      # CRITIQUE : Recalculer les centres de clusters après refit
      n_factors_to_use <- if (is.null(private$FNFactors)) {
        private$FNFactorsTotal
      } else {
        min(private$FNFactors, private$FNFactorsTotal)
      }
      
      coords <- private$FFactorialCoords[, 1:n_factors_to_use, drop = FALSE]
      
      cluster_centers <- matrix(NA, nrow = new_k, ncol = n_factors_to_use)
      
      for (k in 1:new_k) {
        modalities_in_cluster <- which(private$FGroupes == k)
        
        if (length(modalities_in_cluster) > 0) {
          cluster_centers[k, ] <- colMeans(coords[modalities_in_cluster, , drop = FALSE])
        }
      }
      
      rownames(cluster_centers) <- paste0("Cluster_", 1:new_k)
      colnames(cluster_centers) <- paste0("Dim", 1:n_factors_to_use)
      
      private$FClusterCenters <- cluster_centers
      
      invisible(self)
    },
    
    get_variable_summary = function() {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      groupes_modalites <- private$FGroupes
      noms_modalites <- names(groupes_modalites)
      
      noms_variables <- sub("\\..*", "", noms_modalites)
      
      df <- data.frame(
        modalite = noms_modalites,
        variable = noms_variables,
        cluster = groupes_modalites,
        stringsAsFactors = FALSE
      )
      
      result <- do.call(rbind, lapply(unique(noms_variables), function(v) {
        subset_v <- df[df$variable == v, ]
        tbl <- table(subset_v$cluster)
        cluster_principal <- as.integer(names(tbl)[which.max(tbl)])
        purity <- max(tbl) / sum(tbl)
        
        data.frame(
          variable = v,
          n_modalites = nrow(subset_v),
          cluster_principal = cluster_principal,
          purity = purity,
          stringsAsFactors = FALSE
        )
      }))
      
      result <- result[order(-result$purity), ]
      rownames(result) <- NULL
      
      return(result)
    },
    
    get_modalities_of_variable = function(variable_name) {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      groupes <- private$FGroupes
      noms <- names(groupes)
      
      pattern <- paste0("^", variable_name, "\\.")
      indices <- grep(pattern, noms)
      
      if (length(indices) == 0) {
        stop("Variable '", variable_name, "' not found")
      }
      
      result <- data.frame(
        modalite = sub("^[^\\.]+\\.", "", noms[indices]),
        cluster = groupes[indices],
        stringsAsFactors = FALSE
      )
      
      return(result)
    },
    
    get_modalities_of_cluster = function(cluster_id) {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      if (cluster_id < 1 || cluster_id > private$FNbGroupes) {
        stop("k must be between 1 and ", private$FNbGroupes)
      }
      
      groupes <- private$FGroupes
      noms <- names(groupes)[groupes == cluster_id]
      
      result <- data.frame(
        variable = sub("\\..*", "", noms),
        modalite = sub("^[^\\.]+\\.", "", noms),
        stringsAsFactors = FALSE
      )
      
      return(result)
    },
    
    get_tree = function() {
      if (is.null(private$FHclustTree)) {
        stop("The model is not fitted.")
      }
      return(private$FHclustTree)
    },
    
    check_results_integrity = function() {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      groupes <- private$FGroupes
      issues <- list()
      
      cluster_counts <- table(groupes)
      empty_clusters <- setdiff(1:private$FNbGroupes,
                                as.integer(names(cluster_counts)))
      
      if (length(empty_clusters) > 0) {
        issues$empty_clusters <- empty_clusters
      }
      
      if (!all(grepl("\\.", names(groupes)))) {
        issues$invalid_names <- names(groupes)[!grepl("\\.", names(groupes))]
      }
      
      if (length(issues) == 0) {
        return(TRUE)
      } else {
        warning("Integrity issues detected: ", 
                paste(names(issues), collapse = ", "))
        return(FALSE)
      }
    }
  ),
  
  # ============================================================================
  # ACTIVE BINDINGS
  # ============================================================================
  active = list(
    K = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        if (!is.numeric(value) || value < 2) {
          stop("K must be an integer >= 2")
        }
        value <- as.integer(value)
        
        if (!is.null(private$FGroupes)) {
          n_modalities_actual <- length(private$FGroupes)
          if (value > n_modalities_actual) {
            stop("K (", value, ") cannot exceed the number of modalities (", 
                 n_modalities_actual, ")")
          }
        }
        
        if (!is.null(private$FHclustTree)) {
          private$FNbGroupes <- as.integer(value)
          private$FGroupes <- cutree(private$FHclustTree, k = value)
          
          # CRITIQUE : Recalculer les centres de clusters après changement de K
          n_factors_to_use <- if (is.null(private$FNFactors)) {
            private$FNFactorsTotal
          } else {
            min(private$FNFactors, private$FNFactorsTotal)
          }
          
          coords <- private$FFactorialCoords[, 1:n_factors_to_use, drop = FALSE]
          
          cluster_centers <- matrix(NA, nrow = value, ncol = n_factors_to_use)
          
          for (k in 1:value) {
            modalities_in_cluster <- which(private$FGroupes == k)
            
            if (length(modalities_in_cluster) > 0) {
              cluster_centers[k, ] <- colMeans(coords[modalities_in_cluster, , drop = FALSE])
            }
          }
          
          rownames(cluster_centers) <- paste0("Cluster_", 1:value)
          colnames(cluster_centers) <- paste0("Dim", 1:n_factors_to_use)
          
          private$FClusterCenters <- cluster_centers
        } else {
          private$FNbGroupes <- as.integer(value)
        }
        
        invisible(self)
      }
    },
    
    Groupes = function() {
      if (is.null(private$FGroupes)) {
        stop("The model must be fitted with $fit() before accessing Groupes.")
      }
      return(private$FGroupes)
    }, 
    
    NbModalites = function() {
      if (is.null(private$FGroupes)) return(0)
      return(length(private$FGroupes))
    },
    
    Tree = function() private$FHclustTree,
    VarianceExplained = function() private$FACMVariance,
    Inertie = function() private$FInertie,
    DisjunctiveTable = function() private$FDisjunctiveTable,
    FactorialCoords = function() private$FFactorialCoords,
    CategoricalVars = function() private$FCatVarNames,
    NumericVars = function() private$FNumVarNames,
    
    VariableClusters = function() {
      if (is.null(private$FGroupes)) {
        return(NULL)
      }
      
      modality_names <- names(private$FGroupes)
      var_names <- sub("\\..*", "", modality_names)
      
      unique_vars <- unique(var_names)
      var_clusters <- integer(length(unique_vars))
      names(var_clusters) <- unique_vars
      
      for (var in unique_vars) {
        var_modalities <- modality_names[var_names == var]
        clusters <- unique(private$FGroupes[var_modalities])
        
        if (length(clusters) == 1) {
          var_clusters[var] <- clusters[1]
        } else {
          cluster_counts <- table(private$FGroupes[var_modalities])
          var_clusters[var] <- as.integer(names(cluster_counts)[which.max(cluster_counts)])
        }
      }
      
      return(var_clusters)
    }
  )
)

#' Print method for TandemVarClust predict results
#' @export
print.TandemVarClust_predict <- function(x, ...) {
  cat("\n")
  cat("================================================================\n")
  cat("   TandemVarClust - Variable Cluster Prediction                \n")
  cat("================================================================\n\n")
  
  for (var_name in names(x)) {
    result <- x[[var_name]]
    
    cat("Variable: ", var_name, "\n", sep = "")
    
    if (!is.null(result$error)) {
      cat("  Error: ", result$error, "\n\n", sep = "")
      next
    }
    
    # Display cluster (may be NULL)
    if (!is.null(result$cluster)) {
      cat("  Predicted Cluster: ", result$cluster, "\n", sep = "")
    } else {
      cat("  Predicted Cluster: N/A\n")
    }
    
    # Display number of modalities (may be NULL)
    if (!is.null(result$n_modalities)) {
      cat("  Number of modalities: ", result$n_modalities, "\n", sep = "")
    }
    
    # Display modality assignments (may be NULL)
    if (!is.null(result$modality_clusters)) {
      cat("  Modality assignments: ", 
          paste(names(result$modality_clusters), "=", result$modality_clusters, collapse = ", "), 
          "\n", sep = "")
    }
    
    # Display distances ONLY if they exist AND are numeric
    if (!is.null(result$distances) && is.numeric(result$distances)) {
      cat("\n  Distances to cluster centers:\n")
      print(round(result$distances, 3))
    } else {
      cat("\n  Distances to cluster centers: Not calculated\n")
    }
    
    # Display contingency table if it exists
    if (!is.null(result$contingency)) {
      cat("\n  Contingency table:\n")
      print(result$contingency)
    }
    
    # Display Cramér's V if it exists
    if (!is.null(result$cramers_v) && !is.na(result$cramers_v)) {
      cat("\n  Cramér's V: ", round(result$cramers_v, 4), "\n", sep = "")
    }
    
    # Display significance
    if (!is.null(result$significant)) {
      cat("  Significant association: ", result$significant, "\n", sep = "")
    }
    
    cat("\n")
  }
  
  cat("================================================================\n")
  invisible(x)
}
