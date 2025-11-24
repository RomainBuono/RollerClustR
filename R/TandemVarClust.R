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
#' The predict() method allows analysis of illustrative variables by computing
#' their association with clusters via the Dice index, Chi-square test, and
#' Cramer's V coefficient.
#' 
#' Observations are assigned to clusters using the Dice similarity index, which
#' measures the overlap between an observation's active modalities and each cluster's
#' modalities. This provides a mathematically sound and interpretable assignment method.
#'
#' @details
#' ## Workflow
#' 
#' 1. **Data Preparation**: Mixed data (quantitative and categorical) are transformed
#'    into a complete disjunctive table
#' 2. **MCA**: Multiple Correspondence Analysis for dimensionality reduction
#' 3. **HAC**: Hierarchical Agglomerative Clustering on factorial coordinates
#' 4. **Assignment**: Observations assigned to clusters using Dice similarity index
#' 
#' ## Methods
#' 
#' - `initialize(K, n_bins, method_cah, n_factors, scale, na_action)`: Create a new TandemVarClust object
#' - `fit(X)`: Fit the model to data
#' - `refit_with_k(new_k)`: Refit with a different number of clusters
#' - `predict(newdata)`: Predict association of illustrative variables
#' - `get_variable_summary()`: Get summary by original variable
#' - `get_modalities_of_variable(variable_name)`: Get modalities of a specific variable
#' - `get_modalities_of_cluster(cluster_id)`: Get modalities of a specific cluster
#' - `get_tree()`: Get the dendrogram (hclust object)
#' - `check_results_integrity()`: Verify results integrity
#' 
#' ## Active Bindings
#' 
#' - `K`: Number of clusters
#' - `Groupes`: Assignment of modalities to clusters
#' - `NbModalites`: Total number of modalities
#' - `Tree`: Hierarchical tree (hclust object)
#' - `VarianceExplained`: Variance explained by factorial axes (percent)
#' - `Inertie`: Total inertia
#' - `DisjunctiveTable`: Complete disjunctive table
#' - `FactorialCoords`: Factorial coordinates of modalities
#' - `CategoricalVars`: Names of categorical variables
#' - `NumericVars`: Names of quantitative variables
#'
#' @param K Number of desired clusters (integer >= 2)
#' @param n_bins Number of bins for discretizing quantitative variables (default: 5)
#' @param method_cah HAC method: "ward.D2" (default), "ward.D", "single", "complete", "average", "mcquitty", "median", "centroid"
#' @param n_factors Number of factorial axes to retain for HAC (NULL = all axes)
#' @param scale Standardize quantitative variables before discretization (default: TRUE)
#' @param na_action Action for missing values: "omit" (default) or "fail"
#' @param X Data frame containing variables (quantitative and/or categorical)
#' @param new_k New number of clusters for refitting
#' @param newdata Data frame containing illustrative categorical variables
#' @param variable_name Name of the variable to query
#' @param cluster_id Cluster identifier (integer between 1 and K)
#'
#' @return A TandemVarClust R6 object
#'
#' @examples
#' \dontrun{
#' # Example with iris data (mixed: quantitative + categorical)
#' iris_mixed <- iris
#' iris_mixed$Species_group <- as.character(iris$Species)
#' 
#' # Create and fit model
#' model <- TandemVarClust$new(K = 3, n_bins = 5, n_factors = 3)
#' model$fit(iris_mixed)
#' 
#' # Examine results
#' model$K  # Number of clusters
#' model$NbModalites  # Number of modalities
#' 
#' # Variable summary
#' summary_table <- model$get_variable_summary()
#' print(summary_table)
#' 
#' # Modalities of a specific variable
#' model$get_modalities_of_variable("Sepal.Length")
#' 
#' # Modalities in a specific cluster
#' model$get_modalities_of_cluster(1)
#' 
#' # Refit with different K
#' model$refit_with_k(4)
#' 
#' # Predict with illustrative variable
#' results <- model$predict(newdata = data.frame(Species = iris$Species))
#' print(results)
#' }
#'
#' @references
#' Escofier, B. (1979). Traitement simultané de variables quantitatives et qualitatives.
#'   Les Cahiers de l'Analyse des Données, IV(2), 137-146.
#'
#' Pagès, J. (2004). Analyse factorielle de données mixtes. Revue de Statistique
#'   Appliquée, 52(4), 93-111.
#'
#' Ward, J. H. (1963). Hierarchical grouping to optimize an objective function.
#'   Journal of the American Statistical Association, 58(301), 236-244.
#'
#' Dice, L. R. (1945). Measures of the amount of ecologic association between species.
#'   Ecology, 26(3), 297-302.
#'
#' Husson, F., Lê, S., & Pagès, J. (2016). Analyse de données avec R (2e éd.).
#'   Presses Universitaires de Rennes.
#'
#' @seealso [VAR_CAH], [VARCLUS], [roller_clust()]
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
    FNbGroupes = NULL,           # Number of desired clusters
    FNBins = NULL,               # Number of bins for discretization
    FMethodCAH = NULL,           # HAC method (ward.D2, average, etc.)
    FNFactors = NULL,            # Number of factorial axes to retain
    FScale = NULL,               # Standardize quantitative variables
    
    # Data and metadata
    FX = NULL,                   # Original data
    FCatVarNames = NULL,         # Names of categorical variables
    FNumVarNames = NULL,         # Names of quantitative variables
    FHasCategoricalVars = FALSE, # Presence of categorical variables
    FHasNumericVars = FALSE,     # Presence of quantitative variables
    FVarTypes = NULL,            # Type of each variable
    
    # Intermediate results
    FDisjunctiveTable = NULL,    # Complete disjunctive table
    FFactorialCoords = NULL,     # Factorial coordinates of modalities
    FACMVariance = NULL,         # Variance explained by factorial axis
    FInertie = NULL,             # Total inertia
    FNFactorsTotal = NULL,       # Total number of factorial axes
    
    # Clustering results
    FHclustTree = NULL,          # Hierarchical tree (hclust object)
    FGroupes = NULL,             # Assignment of modalities to clusters
    
    # Private methods
    
    # Prepare mixed data
    prepare_mixed_data = function(X) {
      # Identify variable types
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
    
    # Build complete disjunctive table
    build_disjunctive_table = function(X) {
      disjunctive_list <- list()
      
      # Categorical variables: disjunctive table
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
      
      # Quantitative variables: discretization then disjunctive table
      if (private$FHasNumericVars) {
        for (var_name in private$FNumVarNames) {
          col <- X[[var_name]]
          
          # Optional standardization
          if (private$FScale) {
            col <- scale(col)[, 1]
          }
          
          # Discretization by quantiles
          breaks <- quantile(col, probs = seq(0, 1, length.out = private$FNBins + 1),
                            na.rm = TRUE)
          breaks <- unique(breaks)
          
          if (length(breaks) <= 1) {
            warning("Variable '", var_name, "' has zero or very low variance. Ignored.")
            next
          }
          
          discretized <- cut(col, breaks = breaks, include.lowest = TRUE, labels = FALSE)
          
          # Create indicator variables
          n_bins_actual <- max(discretized, na.rm = TRUE)
          for (bin in 1:n_bins_actual) {
            indicator <- as.integer(discretized == bin)
            col_name <- paste0(var_name, ".bin", bin)
            disjunctive_list[[col_name]] <- indicator
          }
        }
      }
      
      # Convert to data frame
      Z <- as.data.frame(disjunctive_list)
      
      if (ncol(Z) == 0) {
        stop("The disjunctive table is empty. Check your data.")
      }
      
      
      return(Z)
    },
    
    # Perform MCA (factorial analysis)
    perform_factorial_analysis = function() {
      Z <- private$FDisjunctiveTable
      n <- nrow(Z)
      p <- ncol(Z)
      
      # Column weights (inverse of marginal frequencies)
      col_margins <- colSums(Z) / n
      col_weights <- 1 / col_margins
      col_weights[!is.finite(col_weights)] <- 0
      
      # Row weights (uniform)
      row_weights <- rep(1/n, n)
      
      # Centering
      Z_centered <- sweep(Z, 2, col_margins, "-")
      
      # Weight matrices
      D_col_sqrt <- diag(sqrt(col_weights))
      D_row_sqrt <- diag(sqrt(row_weights))
      
      # Weighted matrix for SVD
      Z_weighted <- D_row_sqrt %*% as.matrix(Z_centered) %*% D_col_sqrt
      
      # SVD
      svd_res <- svd(Z_weighted)
      
      # Factorial coordinates of modalities
      private$FFactorialCoords <- D_col_sqrt %*% svd_res$v
      rownames(private$FFactorialCoords) <- colnames(Z)
      
      # Eigenvalues and explained variance
      eigenvalues <- svd_res$d^2
      private$FACMVariance <- 100 * eigenvalues / sum(eigenvalues)
      private$FInertie <- sum(eigenvalues)
      private$FNFactorsTotal <- length(eigenvalues)
      
    },
    
    # Perform HAC on factorial coordinates
    perform_clustering = function() {
      # Select axes to use
      n_factors_to_use <- if (is.null(private$FNFactors)) {
        private$FNFactorsTotal
      } else {
        min(private$FNFactors, private$FNFactorsTotal)
      }
      
      coords <- private$FFactorialCoords[, 1:n_factors_to_use, drop = FALSE]
            
      # Euclidean distance
      dist_matrix <- dist(coords, method = "euclidean")
      
      # Hierarchical clustering
      private$FHclustTree <- hclust(dist_matrix, method = private$FMethodCAH)
      
      # K validation
      n_modalities <- nrow(private$FFactorialCoords)
      if (private$FNbGroupes > n_modalities) {
        stop("K (", private$FNbGroupes, 
             ") cannot exceed the number of modalities (", n_modalities, ")")
      }
      # Puis cutree normal :
      private$FGroupes <- cutree(private$FHclustTree, k = private$FNbGroupes)
      # Tree cutting
      private$FGroupes <- cutree(private$FHclustTree, k = private$FNbGroupes)
      names(private$FGroupes) <- rownames(coords)
            
    },
    
    # Assign observations to clusters using Dice index
    # 
    # For each observation, compute the Dice similarity index with each cluster
    # and assign the observation to the cluster with the highest score.
    # 
    # The Dice index between observation i and cluster k is:
    # Dice(M_i, C_k) = 2 * |M_i ∩ C_k| / (|M_i| + |C_k|)
    # 
    # where M_i is the set of active modalities for observation i,
    # and C_k is the set of modalities in cluster k.
    assign_observations_to_clusters = function() {
      Z <- private$FDisjunctiveTable
      n <- nrow(Z)
      groupes <- private$FGroupes
      K <- private$FNbGroupes
      
      obs_clusters <- integer(n)
      dice_scores <- matrix(0, nrow = n, ncol = K)
      colnames(dice_scores) <- paste0("Cluster_", 1:K)
      
      # For each cluster, identify its modalities
      cluster_modalities <- lapply(1:K, function(k) {
        which(groupes == k)
      })
      
      for (i in 1:n) {
        # Active modalities of observation i
        modalities_i <- which(Z[i, ] == 1)
        n_modalities_i <- length(modalities_i)
        
        if (n_modalities_i == 0) {
          obs_clusters[i] <- 1
          dice_scores[i, ] <- NA
          next
        }
        
        # Calculate Dice with each cluster
        for (k in 1:K) {
          modalities_k <- cluster_modalities[[k]]
          n_modalities_k <- length(modalities_k)
          
          # Intersection
          intersection <- length(intersect(modalities_i, modalities_k))
          
          # Dice index
          if (n_modalities_i + n_modalities_k > 0) {
            dice_scores[i, k] <- (2 * intersection) / (n_modalities_i + n_modalities_k)
          } else {
            dice_scores[i, k] <- 0
          }
        }
        
        # Assign to cluster with maximum Dice
        obs_clusters[i] <- which.max(dice_scores[i, ])
      }
      
      return(list(
        clusters = obs_clusters,
        scores = dice_scores
      ))
    },
    
    # Calculate Cramer's V
    compute_cramers_v = function(contingency) {
      chi2_test <- suppressWarnings(chisq.test(contingency))
      chi2 <- chi2_test$statistic
      n <- sum(contingency)
      min_dim <- min(nrow(contingency) - 1, ncol(contingency) - 1)
      
      if (min_dim == 0) return(NA)
      
      v <- sqrt(chi2 / (n * min_dim))
      return(as.numeric(v))
    },
    
    # ============================================================================
    # MÉTHODES DO_FIT ET DO_PREDICT (ARCHITECTURE CLUSTLERANALYSIS)
    # ============================================================================
    
    #' @description do_fit method: fits TandemVarClust algorithm
    do_fit = function(X) {
      # Store original data
      private$FX <- X
      
      # Prepare mixed data
      private$prepare_mixed_data(X)
      
      # Build disjunctive table
      private$FDisjunctiveTable <- private$build_disjunctive_table(X)
      
      # Factorial analysis (MCA)
      private$perform_factorial_analysis()
      
      # Hierarchical clustering
      private$perform_clustering()
    
      
      invisible(self)
    },
    
    #' @description do_predict method: predict association of illustrative variables
    do_predict = function(newdata) {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted. Call $fit() first.")
      }
      
      if (!is.data.frame(newdata)) {
        if (is.vector(newdata)) {
          newdata <- data.frame(new_var = newdata)
        } else {
          stop("newdata must be a data frame or vector")
        }
      }
      
      if (nrow(newdata) != nrow(private$FX)) {
        stop(paste0("newdata must have the same number of observations (rows) as training data. ",
                    "Expected: ", nrow(private$FX), ", Received: ", nrow(newdata)))
      }
      
      message("\nAssigning observations to clusters using Dice index...")
      
      # Assign observations to clusters using Dice index
      result <- private$assign_observations_to_clusters()
      obs_clusters <- result$clusters
      dice_scores <- result$scores
      
      # Display observation distribution
      obs_table <- table(obs_clusters)
      message("Observation distribution across clusters: ", paste(obs_table, collapse = ", "))
      
      # Analyze each illustrative variable
      results <- list()
      
      for (var_name in names(newdata)) {
        var_col <- newdata[[var_name]]
        
        # Check that variable is categorical
        if (!is.factor(var_col) && !is.character(var_col)) {
          warning("Variable '", var_name, "' is not categorical. Converting to factor.")
          var_col <- as.factor(var_col)
        }
        
        if (!is.factor(var_col)) {
          var_col <- as.factor(var_col)
        }
        
        # Contingency table
        # Force obs_clusters to be a factor with all levels 1:K to ensure K columns
        obs_clusters_factor <- factor(obs_clusters, levels = 1:private$FNbGroupes)
        contingency <- table(var_col, obs_clusters_factor)
        colnames(contingency) <- paste0("Cluster_", 1:private$FNbGroupes)
        
        # Row profiles (% by modality)
        pct_row <- prop.table(contingency, margin = 1) * 100
        
        # Column profiles (% by cluster)
        pct_col <- prop.table(contingency, margin = 2) * 100
        
        # Chi-square test
        chi2_test <- suppressWarnings(chisq.test(contingency))
        
        # Cramer's V
        cramers_v <- private$compute_cramers_v(contingency)
        
        # Store results
        results[[var_name]] <- list(
          contingency = contingency,
          percentages_by_modality = pct_row,
          percentages_by_cluster = pct_col,
          chi2_test = chi2_test,
          cramers_v = cramers_v,
          significant = chi2_test$p.value < 0.05,
          dice_scores = dice_scores
        )
        
        # Display summary
        message("\nVariable: ", var_name)
        message("  Chi²: ", round(chi2_test$statistic, 2),
                " (p = ", format.pval(chi2_test$p.value, digits = 3), ")")
        message("  Cramer's V: ", round(cramers_v, 3))
        message("  Association: ", ifelse(chi2_test$p.value < 0.05, "Significant ✓", "Not significant"))
      }
      
      class(results) <- c("TandemVarClust_predict", "list")
      return(results)
    },
    
    #' @description do_summary method: display TandemVarClust summary
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
      
      # Count modalities per cluster
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
      
      # Variable types
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
    
    #' @description Constructor
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
    
    #' @description Refit the model with a different number of clusters
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
            
      invisible(self)
    },
    
    #' @description Get summary by original variable
    get_variable_summary = function() {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      groupes_modalites <- private$FGroupes
      noms_modalites <- names(groupes_modalites)
      
      # Extract original variable name
      noms_variables <- sub("\\..*", "", noms_modalites)
      
      df <- data.frame(
        modalite = noms_modalites,
        variable = noms_variables,
        cluster = groupes_modalites,
        stringsAsFactors = FALSE
      )
      
      # Aggregate by variable
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
      
      # Sort by descending purity
      result <- result[order(-result$purity), ]
      rownames(result) <- NULL
      
      return(result)
    },
    
    #' @description Get modalities of a specific variable
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
    
    #' @description Get modalities of a specific cluster
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
    
    #' @description Get the dendrogram (hclust object)
    get_tree = function() {
      if (is.null(private$FHclustTree)) {
        stop("The model is not fitted.")
      }
      return(private$FHclustTree)
    },
    
    #' @description Verify results integrity
    check_results_integrity = function() {
      if (is.null(private$FGroupes)) {
        stop("The model is not fitted.")
      }
      
      groupes <- private$FGroupes
      issues <- list()
      
      # Check that all clusters have modalities
      cluster_counts <- table(groupes)
      empty_clusters <- setdiff(1:private$FNbGroupes,
                               as.integer(names(cluster_counts)))
      
      if (length(empty_clusters) > 0) {
        issues$empty_clusters <- empty_clusters
      }
      
      # Check name format
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
  # ACTIVE BINDINGS (Read-only accessors)
  # ============================================================================
  active = list(
        K = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        if (!is.numeric(value) || value < 2) {
          stop("K must be an integer >= 2")
        }
        
        if (!is.null(private$FGroupes)) {
          n_modalities <- length(private$FGroupes)  # ← CORRIGER ICI
          if (value > n_modalities) {
            stop("K (", value, ") cannot exceed the number of modalities (", 
                n_modalities, ")")
          }
        }
        
        if (!is.null(private$FHclustTree)) {
          private$FNbGroupes <- as.integer(value)
          private$FGroupes <- cutree(private$FHclustTree, k = value)
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
    NbModalites = function() length(private$FGroupes),
    Tree = function() private$FHclustTree,
    VarianceExplained = function() private$FACMVariance,
    Inertie = function() private$FInertie,
    DisjunctiveTable = function() private$FDisjunctiveTable,
    FactorialCoords = function() private$FFactorialCoords,
    CategoricalVars = function() private$FCatVarNames,
       VariableClusters = function() {
      if (is.null(private$FGroupes)) {
        return(NULL)
      }
      
      # Extract variable names from modality names
      modality_names <- names(private$FGroupes)
      var_names <- sub("\\..*", "", modality_names)
      
      # For each variable, find which cluster(s) its modalities belong to
      unique_vars <- unique(var_names)
      var_clusters <- integer(length(unique_vars))
      names(var_clusters) <- unique_vars
      
      for (var in unique_vars) {
        # Get clusters of all modalities of this variable
        var_modalities <- modality_names[var_names == var]
        clusters <- unique(private$FGroupes[var_modalities])
        
        # Use majority cluster (most modalities)
        if (length(clusters) == 1) {
          var_clusters[var] <- clusters[1]
        } else {
          # Count modalities per cluster for this variable
          cluster_counts <- table(private$FGroupes[var_modalities])
          var_clusters[var] <- as.integer(names(cluster_counts)[which.max(cluster_counts)])
        }
      }
      
      return(var_clusters)
    }
  )
)

#' Print method for TandemVarClust prediction results
#'
#' @param x A TandemVarClust_predict object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns x
#' @export
print.TandemVarClust_predict <- function(x, ...) {
  cat("TandemVarClust - Prediction Results\n")
  cat("====================================\n")
  cat("Assignment method: Dice similarity index\n\n")
  
  for (var_name in names(x)) {
    result <- x[[var_name]]
    
    cat("Variable:", var_name, "\n")
    cat("\n")
    
    cat("Contingency table:\n")
    print(result$contingency)
    cat("\n")
    
    cat("Chi-square test:\n")
    cat("  Statistic:", round(result$chi2_test$statistic, 2), "\n")
    cat("  p-value:", format.pval(result$chi2_test$p.value, digits = 3), "\n")
    cat("  Association:", ifelse(result$significant, "Significant ✓", "Not significant"), "\n")
    cat("\n")
    
    cat("Cramer's V:", round(result$cramers_v, 3), "\n")
    
    interpretation <- if (result$cramers_v < 0.1) {
      "negligible"
    } else if (result$cramers_v < 0.3) {
      "weak"
    } else if (result$cramers_v < 0.5) {
      "moderate"
    } else if (result$cramers_v < 0.7) {
      "strong"
    } else {
      "very strong"
    }
    
    cat("Interpretation: Association is", interpretation, "\n")
    cat("\n")
    cat(strrep("=", 70), "\n\n")
  }
  
  invisible(x)
}