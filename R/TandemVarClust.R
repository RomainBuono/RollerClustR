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
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(K, n_bins, method_cah, n_factors, scale, na_action, ...)}}{
#'     Initialize a new TandemVarClust object.
#'   }
#'   \item{\code{fit(X, ...)}}{
#'     Fit the model to the data (inherited from ClusterAnalysis).
#'   }
#'   \item{\code{predict(new_data)}}{
#'     Predict cluster assignment for new illustrative variables (inherited from ClusterAnalysis).
#'   }
#'   \item{\code{refit_with_k(new_k)}}{
#'     Refit the clustering with a different number of clusters without re-running MCA.
#'   }
#'   \item{\code{get_variable_summary()}}{
#'     Get a summary of how variables are distributed across clusters.
#'   }
#'   \item{\code{get_modalities_of_variable(variable_name)}}{
#'     Get the cluster assignments for all modalities of a specific variable.
#'   }
#'   \item{\code{get_modalities_of_cluster(cluster_id)}}{
#'     Get all modalities belonging to a specific cluster.
#'   }
#'   \item{\code{get_tree()}}{
#'     Get the hierarchical clustering tree (hclust object).
#'   }
#'   \item{\code{check_results_integrity()}}{
#'     Verify the integrity of clustering results.
#'   }
#' }
#'
#' @section Active Bindings:
#' \describe{
#'   \item{\code{K}}{Get or set the number of clusters. Setting K will re-cut the tree.}
#'   \item{\code{Groupes}}{Get the cluster assignments for all modalities (read-only).}
#'   \item{\code{NbModalites}}{Get the total number of modalities (read-only).}
#'   \item{\code{Tree}}{Get the hierarchical clustering tree (read-only).}
#'   \item{\code{VarianceExplained}}{Get the percentage of variance explained by each factorial axis (read-only).}
#'   \item{\code{Inertie}}{Get the total inertia from MCA (read-only).}
#'   \item{\code{DisjunctiveTable}}{Get the disjunctive (indicator) table (read-only).}
#'   \item{\code{FactorialCoords}}{Get the factorial coordinates of modalities (read-only).}
#'   \item{\code{CategoricalVars}}{Get the names of categorical variables (read-only).}
#'   \item{\code{NumericVars}}{Get the names of numeric variables (read-only).}
#'   \item{\code{VariableClusters}}{Get the cluster assignment for each original variable (read-only).}
#' }
#'
#' @param K Integer, number of clusters (must be >= 2).
#' @param n_bins Integer, number of bins for discretizing numeric variables (default: 3).
#' @param method_cah Character, linkage method for hierarchical clustering.
#'   One of: "ward.D2" (default), "single", "complete", "average", "mcquitty", "median", "centroid".
#' @param n_factors Integer or NULL, number of factorial axes to use for clustering.
#'   If NULL (default), all axes are used.
#' @param scale Logical, whether to scale numeric variables before discretization (default: TRUE).
#' @param na_action Character, how to handle missing values. Currently only "fail" is supported.
#' @param ... Additional arguments passed to parent class.
#' @param new_k Integer, new number of clusters for refit_with_k().
#' @param variable_name Character, name of the variable for get_modalities_of_variable().
#' @param cluster_id Integer, cluster number for get_modalities_of_cluster().
#'
#' @examples
#' \dontrun{
#' # Create mixed data
#' data <- data.frame(
#'   cat1 = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   cat2 = sample(c("X", "Y"), 100, replace = TRUE),
#'   num1 = rnorm(100),
#'   num2 = runif(100, 0, 10)
#' )
#'
#' # Fit model
#' model <- TandemVarClust$new(K = 3, n_bins = 4)
#' model$fit(data)
#'
#' # Access results
#' print(model$Groupes)
#' print(model$VariableClusters)
#'
#' # Get variable summary
#' summary <- model$get_variable_summary()
#'
#' # Refit with different K
#' model$refit_with_k(4)
#'
#' # Predict new variables
#' new_data <- data.frame(
#'   new_cat = sample(c("A", "B"), 100, replace = TRUE)
#' )
#' predictions <- model$predict(new_data)
#' }
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
    
    # Model state
    FIsFitted = FALSE,
    
    # NOUVEAUX CHAMPS pour predict_variable
    FColMargins = NULL,
    FColWeights = NULL,
    FRowWeights = NULL,
    FSVDv = NULL,
    FDColSqrt = NULL,
    FClusterCenters = NULL,
    FObsClusters = NULL,
    FDiceScores = NULL,
    
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
          
          dice <- 2 * intersection / (n_modalities_i + n_modalities_k)
          dice_scores[i, k] <- dice
        }
        
        obs_clusters[i] <- which.max(dice_scores[i, ])
      }
      
      private$FObsClusters <- obs_clusters
      private$FDiceScores <- dice_scores
      
      return(list(obs_clusters = obs_clusters, dice_scores = dice_scores))
    },
    
    # Méthode privée pour predict_variable (utilisée dans ClusterAnalysis)
    predict_variable = function(var_vector, var_name = "new_var") {
      n_train <- nrow(private$FX)
      n_new <- length(var_vector)
      
      if (n_new != n_train) {
        stop("New data must have the same number of observations as training data (", 
             n_train, " observations)")
      }
      
      if (!(is.factor(var_vector) || is.character(var_vector))) {
        warning("Variable '", var_name, "' is not categorical. Converting to factor.")
        var_vector <- as.factor(var_vector)
      }
      
      if (!is.factor(var_vector)) {
        var_vector <- as.factor(var_vector)
      }
      
      levels_new <- levels(var_vector)
      n_levels <- length(levels_new)
      
      # Obtenir les clusters d'observations
      obs_results <- private$assign_observations_to_clusters()
      obs_clusters <- obs_results$obs_clusters
      dice_scores <- obs_results$dice_scores
      
      # Construire la table de contingence
      # S'assurer que tous les clusters sont représentés
      obs_clusters_factor <- factor(obs_clusters, levels = 1:private$FNbGroupes)
      contingency <- table(var_vector, obs_clusters_factor)
      
      # Test du chi-carré
      chi2_test <- tryCatch({
        suppressWarnings(chisq.test(contingency))
      }, error = function(e) {
        list(statistic = NA, p.value = NA, parameter = NA)
      })
      
      # V de Cramer
      cramers_v <- tryCatch({
        n <- sum(contingency)
        chi2 <- as.numeric(chi2_test$statistic)
        min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
        
        if (is.na(chi2) || min_dim == 0) {
          NA  # Ne pas utiliser return() ici !
        } else {
          v <- sqrt(chi2 / (n * min_dim))
          
          # Le V de Cramer doit être dans [0, 1]
          # Si > 1, c'est dû à une approximation numérique
          min(v, 1.0)
        }
      }, error = function(e) {
        NA
      })
      
      significant <- !is.na(chi2_test$p.value) && chi2_test$p.value < 0.05
      
      result <- list(
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
      
      # Validation des dimensions
      n_train <- nrow(private$FX)
      n_new <- nrow(newdata)
      
      if (n_new != n_train) {
        stop("New data must have the same number of observations as training data (", 
             n_train, " observations)")
      }
      
      results <- list()
      
      for (var_name in names(newdata)) {
        tryCatch({
          pred <- private$predict_variable(newdata[[var_name]], var_name)
          results[[var_name]] <- pred
          
        }, error = function(e) {
          warning("Error predicting variable '", var_name, "': ", e$message)
          results[[var_name]] <- list(
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
              " (", paste(head(private$FNumVarNames, 5), collapse = ", "), 
              if (length(private$FNumVarNames) > 5) "..." else "", ")\n", sep = "")
        }
      }
      
      cat("================================================================\n")
      invisible(NULL)
    }
  ),
  
  # ============================================================================
  # PUBLIC METHODS
  # ============================================================================
  public = list(
    #' @description
    #' Initialize a new TandemVarClust object
    #' 
    #' @param K Integer, number of clusters (must be >= 2)
    #' @param n_bins Integer, number of bins for discretizing numeric variables (default: 3)
    #' @param method_cah Character, linkage method for hierarchical clustering (default: "ward.D2")
    #' @param n_factors Integer or NULL, number of factorial axes to use (default: NULL = all axes)
    #' @param scale Logical, whether to scale numeric variables before discretization (default: TRUE)
    #' @param na_action Character, how to handle missing values (default: "fail")
    #' @param ... Additional arguments passed to parent class
    #' 
    #' @return A new TandemVarClust object
    initialize = function(K = 2, 
                          n_bins = 3, 
                          method_cah = "ward.D2", 
                          n_factors = NULL,
                          scale = TRUE,
                          na_action = "fail",
                          ...) {
      
      if (!is.numeric(K) || K < 2) {
        stop("K must be an integer >= 2")
      }
      private$FNbGroupes <- as.integer(K)
      
      if (!is.numeric(n_bins) || n_bins < 2 || n_bins > 20) {
        stop("n_bins must be between 2 and 20")
      }
      private$FNBins <- as.integer(n_bins)
      
      valid_methods <- c("ward.D", "ward.D2", "single", "complete", 
                         "average", "mcquitty", "median", "centroid")
      if (!method_cah %in% valid_methods) {
        stop("invalid method_cah: ", method_cah, 
             ". Must be one of: ", paste(valid_methods, collapse = ", "))
      }
      private$FMethodCAH <- method_cah
      
      if (!is.null(n_factors)) {
        if (!is.numeric(n_factors) || n_factors < 1) {
          stop("n_factors must be NULL or an integer >= 1")
        }
        private$FNFactors <- as.integer(n_factors)
      } else {
        private$FNFactors <- NULL
      }
      
      if (!is.logical(scale)) {
        stop("scale must be TRUE or FALSE")
      }
      private$FScale <- scale
      
      # Vérifier les paramètres suspects (pas pour TandemVarClust)
      dots <- list(...)
      suspicious_params <- c("max_iter", "tolerance", "n_init", "method")
      found_suspicious <- intersect(names(dots), suspicious_params)
      if (length(found_suspicious) > 0) {
        warning("Suspicious parameters detected: ", paste(found_suspicious, collapse = ", "), 
                " not used by TandemVarClust. Did you mean to use a different algorithm?")
      }
      
      super$initialize(
        algorithm = "TandemVarClust",
        K = K,
        na_action = na_action,
        ...
      )
    },
    
    #' @description
    #' Fit the TandemVarClust model to the data
    #' 
    #' This performs the following steps:
    #' 1. Prepare mixed data (identify categorical and numeric variables)
    #' 2. Build disjunctive table (indicator matrix for all modalities)
    #' 3. Perform Multiple Correspondence Analysis (MCA)
    #' 4. Cluster modalities using Hierarchical Agglomerative Clustering (HAC)
    #' 
    #' @param X A data.frame containing the variables to cluster
    #' @param ... Additional arguments (not used)
    #' 
    #' @return Invisible self (for method chaining)
    fit = function(X, ...) {
      if (!is.data.frame(X)) {
        stop("X must be a data.frame")
      }
      
      if (nrow(X) == 0 || ncol(X) == 0) {
        stop("X must have at least one row and one column")
      }
      
      private$FX <- X
      
      private$prepare_mixed_data(X)
      private$FDisjunctiveTable <- private$build_disjunctive_table(X)
      private$perform_factorial_analysis()
      private$perform_clustering()
      
      private$FIsFitted <- TRUE
      
      invisible(self)
    },
    
    #' @description
    #' Refit the clustering with a different number of clusters
    #' 
    #' This method re-cuts the hierarchical tree without re-running the MCA,
    #' which is much faster than fitting a new model from scratch.
    #' 
    #' @param new_k Integer, new number of clusters
    #' 
    #' @return Invisible self (for method chaining)
    refit_with_k = function(new_k) {
      if (!private$FIsFitted) {
        stop("The model must be fitted before refitting with a new K")
      }
      
      if (!is.numeric(new_k) || new_k < 2) {
        stop("new_k must be an integer >= 2")
      }
      
      new_k <- as.integer(new_k)
      
      n_modalities <- length(private$FGroupes)
      if (new_k > n_modalities) {
        stop("new_k (", new_k, ") cannot exceed the number of modalities (", 
             n_modalities, ")")
      }
      
      private$FNbGroupes <- new_k
      private$FGroupes <- cutree(private$FHclustTree, k = new_k)
      
      # Recalculer les centres de clusters
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
    
    #' @description
    #' Get a summary of how variables are distributed across clusters
    #' 
    #' For each original variable, this method calculates:
    #' - The number of modalities
    #' - The principal cluster (most frequent cluster among modalities)
    #' - The purity (proportion of modalities in the principal cluster)
    #' 
    #' @return A data.frame with columns: variable, n_modalites, cluster_principal, purity
    get_variable_summary = function() {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      groupes <- private$FGroupes
      noms_modalites <- names(groupes)
      groupes_modalites <- as.vector(groupes)
      
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
    
    #' @description
    #' Get the cluster assignments for all modalities of a specific variable
    #' 
    #' @param variable_name Character, name of the variable
    #' 
    #' @return A data.frame with columns: modalite, cluster
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
    
    #' @description
    #' Get all modalities belonging to a specific cluster
    #' 
    #' @param cluster_id Integer, cluster number (between 1 and K)
    #' 
    #' @return A data.frame with columns: variable, modalite
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
    
    #' @description
    #' Get the hierarchical clustering tree
    #' 
    #' @return An hclust object representing the hierarchical tree
    get_tree = function() {
      if (is.null(private$FHclustTree)) {
        stop("The model is not fitted.")
      }
      return(private$FHclustTree)
    },
    
    #' @description
    #' Verify the integrity of clustering results
    #' 
    #' Checks for:
    #' - Empty clusters
    #' - Invalid modality names
    #' 
    #' @return TRUE if no issues are found, FALSE otherwise (with warnings)
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
    #' @field K Get or set the number of clusters. Setting K will re-cut the hierarchical tree.
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
    
    #' @field Groupes Get the cluster assignments for all modalities (named integer vector).
    Groupes = function() {
      if (is.null(private$FGroupes)) {
        stop("The model must be fitted with $fit() before accessing Groupes.")
      }
      return(private$FGroupes)
    }, 
    
    #' @field NbModalites Get the total number of modalities.
    NbModalites = function() {
      if (is.null(private$FGroupes)) return(0)
      return(length(private$FGroupes))
    },
    
    #' @field Tree Get the hierarchical clustering tree (hclust object).
    Tree = function() private$FHclustTree,
    
    #' @field VarianceExplained Get the percentage of variance explained by each factorial axis.
    VarianceExplained = function() private$FACMVariance,
    
    #' @field Inertie Get the total inertia from MCA.
    Inertie = function() private$FInertie,
    
    #' @field DisjunctiveTable Get the disjunctive (indicator) table.
    DisjunctiveTable = function() private$FDisjunctiveTable,
    
    #' @field FactorialCoords Get the factorial coordinates of modalities.
    FactorialCoords = function() private$FFactorialCoords,
    
    #' @field CategoricalVars Get the names of categorical variables.
    CategoricalVars = function() private$FCatVarNames,
    
    #' @field NumericVars Get the names of numeric variables.
    NumericVars = function() private$FNumVarNames,
    
    #' @field VariableClusters Get the cluster assignment for each original variable (based on majority rule).
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
#' @param x A TandemVarClust_predict object
#' @param ... Additional arguments (not used)
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
    
    cat("  Predicted Cluster: ", result$cluster, "\n", sep = "")
    cat("  Number of modalities: ", result$n_modalities, "\n", sep = "")
    cat("  Modality assignments: ", paste(result$modality_clusters, collapse = ", "), "\n", sep = "")
    
    cat("\n  Distances to cluster centers:\n")
    print(round(result$distances, 3))
    cat("\n")
  }
  
  cat("================================================================\n")
  invisible(x)
}