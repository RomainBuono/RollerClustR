#' Hierarchical Agglomerative Clustering of Variables (VAR_CAH)
#'
#' @description
#' `VAR_CAH` implements a hierarchical agglomerative clustering (HAC) algorithm
#' for grouping numeric variables. The algorithm uses the correlation matrix
#' to compute distances between variables and builds a dendrogram to identify
#' groups of similar variables.
#'
#' @details
#' ## Algorithm Principle
#'
#' 1. Compute correlation matrix between all variables
#' 2. Transform into distance matrix: `d = 1 - |cor|`
#' 3. Build hierarchical tree (complete linkage method)
#' 4. Cut tree to obtain K clusters
#' 5. Compute synthetic variables by PCA for each cluster
#'
#' ## Synthetic Variables
#'
#' For each variable cluster, a synthetic variable is computed as the first
#' principal component (PC1) of the cluster variables. This synthesis maximizes
#' explained variance and provides an optimal representation of the cluster.
#'
#' ## Quality Metrics
#'
#' - **Homogeneity**: Average intra-cluster correlation
#' - **Cluster Quality**: Variance explained by PC1
#' - **Correlations**: Complete correlation matrix
#'
#' @param scale Boolean indicating if variables should be centered and scaled (default: TRUE)
#' @param K Desired number of clusters (default: 2)
#' @param ... Additional arguments (for compatibility with parent class)
#'
#' @return An R6 object of class `VAR_CAH`
#'
#' @examples
#' # Simple example with iris dataset
#' library(RollerClustR)
#' data(iris)
#'
#' # Create and fit the model
#' model <- VAR_CAH$new(K = 2, scale = TRUE)
#' model$fit(iris[, 1:4])
#'
#' # Display summary
#' model$summary()
#'
#' # Access variable groups
#' groups <- model$Groupes
#' print(groups)
#'
#' # Modify number of clusters
#' model$K <- 3
#' model$summary()
#'
#' @section Public methods:
#' \describe{
#'   \item{`$new(...)`}{Class constructor}
#'   \item{`$fit(X)`}{Fits the model on data X}
#'   \item{`$summary()`}{Displays a detailed clustering summary}
#'   \item{`$predict(newdata)`}{Not implemented for VAR_CAH (variable clustering)}
#' }
#'
#' @section Active bindings:
#' \describe{
#'   \item{`$K`}{Number of clusters (read/write)}
#'   \item{`$Groupes`}{Named vector of variable assignments to clusters}
#' }
#'
#' @seealso [ClusterAnalysis], [VAR_KMEANS], [TandemVarClust], [roller_clust()]
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom stats cor hclust cutree prcomp dist
#' @importFrom utils head

VAR_CAH <- R6Class("VAR_CAH",
  inherit = ClusterAnalysis,
  
  private = list(
    # =================================================================
    # VAR_CAH SPECIFIC FIELDS (parent fields are inherited)
    # =================================================================
    FMaxIter = 100,
    FTolerance = 1e-6,
    FConverged = FALSE,
    FArbre = NULL,                # Hierarchical tree
    FVariablesSynthetiques = NULL, # Synthetic variables (PC1 per cluster)
    FCorrelations = NULL,         # Correlations with synthetic variables
    FQualiteClusters = NULL,      # Cluster quality metrics
    FHomogeneite = NULL,          # Overall homogeneity
    FVarNames = NULL,
    FNumVarNames = NULL,          # Numeric variable names
    FCatVarNames = NULL,          # Categorical variable names
    FVarTypes = NULL,
    FPCAParams = NULL,            # PCA parameters
    
    # =================================================================
    # Utility Functions
    # =================================================================
    
    #' @description Computes synthetic variable (PC1) for a numeric cluster
    pca_synthetique = function(X_cluster) {
      
      row_names_clean <- rownames(na.omit(X_cluster))
      X_clean <- na.omit(X_cluster)
      full_synthetique <- rep(NA_real_, nrow(X_cluster))
      indices_clean <- match(row_names_clean, rownames(X_cluster))

      # Trivial case: single variable
      if (ncol(X_clean) < 2) {
        full_synthetique[indices_clean] <- X_clean[, 1]
        pca_res_simule <- list(
          synthetic = full_synthetique,
          loading = 1.0, center = 0, scale = 1, variance_explained = 1.0 
        )
        return(pca_res_simule)
      } else {
        # Execute PCA
        pca_res <- prcomp(X_clean, center = TRUE, scale. = TRUE)
        pc1_scores <- pca_res$x[, 1, drop = TRUE]
        full_synthetique[indices_clean] <- pc1_scores
        var_expl <- pca_res$sdev^2 / sum(pca_res$sdev^2)
        
        pca_result_list <- list(
          synthetic = full_synthetique,
          loading = pca_res$rotation[, 1, drop = TRUE],
          center = pca_res$center,
          scale = pca_res$scale,
          variance_explained = var_expl[1]
        )
        return(pca_result_list)
      }
    },
    
    # =================================================================
    # Main Methods (PRIVATE, called by public methods)
    # =================================================================
    
    #' @description do_fit method: fits HAC algorithm on variables
    do_fit = function(X) {
      
      val_res <- validate_data_type(X) 
      if (val_res$type != "numeric") {
        stop("VAR_CAH only accepts data of type 'numeric'.")
      }
      
      # ADDED: Validate k <= number of variables
      if (private$FNbGroupes > ncol(X)) {
        stop(paste0(
          "Number of clusters (k=", private$FNbGroupes, 
          ") cannot exceed number of variables (", 
          ncol(X), ")"
        ))
      }
      
      private$FX <- X
      private$FVarNames <- colnames(X)
      private$FNumVarNames <- private$FVarNames
      private$FX_clean <- X 
      
      # 3. Compute Correlation and Distance Matrix
      # (dissimilarity without square root, assumed strategy)
      cor_matrix <- cor(X, use = "pairwise.complete.obs") 
      dist_matrix <- as.dist(1 - abs(cor_matrix)) 
      
      # 4. Execute HAC
      arbre_cah <- hclust(dist_matrix, method = "complete") 
      private$FArbre <- arbre_cah
      
      # 5. Cut Tree to obtain K clusters
      groupes_variables <- cutree(arbre_cah, k = private$FNbGroupes)
      private$FGroupes <- groupes_variables
      
      # 6. Evaluate Clusters
      private$evaluate_clusters()
      
      # 7. Finalization (MUST BE TRUE!)
      private$FFitted <- TRUE
      
      invisible(self)
    },
    
    #' @description Internal method to refit if K is modified
    do_refit_with_k = function(new_k) {
      if (!private$FFitted) {
        private$FNbGroupes <- new_k
        return(invisible(self))
      }
      
      # Validate k <= number of variables
      if (new_k > ncol(private$FX)) {
        stop(paste0(
          "Number of clusters (k=", new_k, 
          ") cannot exceed number of variables (", 
          ncol(private$FX), ")"
        ))
      }
      
      private$FNbGroupes <- new_k
      groupes_variables <- cutree(private$FArbre, k = private$FNbGroupes)
      private$FGroupes <- groupes_variables
      private$evaluate_clusters()
      
      message(paste0("VAR_CAH model has been recalculated for k = ", new_k, "."))
      
      invisible(self)
    },
    
    #' @description Predict cluster membership for new variables
    #' @param newdata Data frame or vector containing new variables
    #' @return List containing assignments and similarity scores
    do_predict = function(newdata) {
      if (!private$FFitted) {
        stop("The model must be fitted with $fit() before predicting.")
      }
      
      # Validation and data preparation
      if (is.vector(newdata)) {
        newdata <- data.frame(new_var = newdata)
      }
      
      if (!is.data.frame(newdata) && !is.matrix(newdata)) {
        stop("newdata must be a data frame, matrix or vector.")
      }
      
      # Check that newdata has the correct number of observations
      if (nrow(newdata) != nrow(private$FX)) {
        stop(paste0("newdata must have the same number of observations (rows) as training data. ",
                    "Expected: ", nrow(private$FX), ", Received: ", nrow(newdata)))
      }
      
      # Convert to data.frame if necessary
      newdata <- as.data.frame(newdata)
      
      # Process each new variable - STRUCTURE PAR VARIABLE
      results <- list()
      
      for (i in 1:ncol(newdata)) {
        var_name <- colnames(newdata)[i]
        if (is.null(var_name) || var_name == "") {
          var_name <- paste0("V", i)
        }
        
        new_var <- newdata[, i, drop = TRUE]
        
        # Handle missing values
        if (any(is.na(new_var))) {
          warning(paste0("Variable '", var_name, 
                        "' contains missing values. Assignment to cluster 1 by default."))
          
          results[[var_name]] <- list(
            cluster = 1L,
            scores = rep(NA_real_, private$FNbGroupes),
            best_score = NA_real_,
            second_best_score = NA_real_
          )
          next
        }
        
        # Standardize new variable if necessary
        if (private$FScale) {
          new_var <- as.vector(scale(new_var))
        }
        
        # Compute similarity score with each cluster
        cluster_scores <- numeric(private$FNbGroupes)
        
        for (k in 1:private$FNbGroupes) {
          synth_var <- private$FVariablesSynthetiques[, k]
          cor_val <- cor(new_var, synth_var, use = "pairwise.complete.obs")
          cluster_scores[k] <- abs(cor_val)
        }
        
        # Assign to cluster with highest score
        best_cluster <- which.max(cluster_scores)
        best_score <- cluster_scores[best_cluster]
        
        # Second best score
        sorted_scores <- sort(cluster_scores, decreasing = TRUE)
        second_best <- if (length(sorted_scores) >= 2) sorted_scores[2] else NA_real_
        
        # Store result for this variable
        results[[var_name]] <- list(
          cluster = best_cluster,
          scores = cluster_scores,
          best_score = best_score,
          second_best_score = second_best
        )
      }
      
      return(results)
    },
    
    #' @description Cluster evaluation
    evaluate_clusters = function() {
      if (is.null(private$FGroupes)) stop("Groups have not been defined.")
      
      n_vars <- ncol(private$FX_clean)
      private$FVariablesSynthetiques <- matrix(NA_real_, 
                                               nrow = nrow(private$FX_clean), 
                                               ncol = private$FNbGroupes)
      private$FCorrelations <- rep(NA_real_, n_vars)
      private$FQualiteClusters <- list()
      
      for (k in 1:private$FNbGroupes) {
        vars_in_cluster <- private$FNumVarNames[private$FGroupes == k]
        X_cluster <- private$FX_clean[, vars_in_cluster, drop = FALSE]
        
        pca_result <- private$pca_synthetique(X_cluster)
        private$FVariablesSynthetiques[, k] <- pca_result$synthetic
        var_indices <- match(vars_in_cluster, private$FNumVarNames)
        
        cors_cluster <- sapply(vars_in_cluster, function(var_name) {
          cor(
            private$FX_clean[, var_name], 
            pca_result$synthetic, 
            use = "pairwise.complete.obs"
          )
        })
        
        private$FCorrelations[var_indices] <- abs(cors_cluster)
        
        private$FQualiteClusters[[paste0("C", k)]] <- list(
          homogeneite = mean(abs(cors_cluster), na.rm = TRUE),
          n_vars = length(vars_in_cluster),
          variance_pc1 = pca_result$variance_explained,
          vars = vars_in_cluster
        )
      }
      private$FHomogeneite <- mean(private$FCorrelations, na.rm = TRUE)
    },
    
    #' @description Private summary method
    do_summary = function() {
      if (!private$FFitted) {
         stop("The model must be fitted with $fit() first.")
      }
      
      cat("\n===========================================================\n")
      cat("   VAR_CAH - Hierarchical Variable Clustering Summary\n")
      cat("===========================================================\n\n")
      
      cat("Algorithm: Hierarchical Agglomerative Clustering (HAC) on variables\n")
      cat("Linkage Method: Complete (on 1 - |Correlation|)\n")
      cat("Data standardization:", private$FScale, "\n")
      cat("Number of clusters (k):", private$FNbGroupes, "\n")
      cat("Number of variables:", ncol(private$FX), "\n\n")
      
      cat("Overall Mean Homogeneity (Mean(|Correlation Var/PC1|)):", 
          round(private$FHomogeneite, 4), "\n\n")
      
      cat("Details by Cluster:\n")
      
      summary_df <- data.frame(
        Cluster = integer(),
        Nb_Vars = integer(),
        Mean_Homogeneity = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (k in 1:private$FNbGroupes) {
        qc <- private$FQualiteClusters[[paste0("C", k)]]
        summary_df <- rbind(summary_df, data.frame(
          Cluster = k,
          Nb_Vars = qc$n_vars,
          Mean_Homogeneity = round(qc$homogeneite, 4)
        ))
      }
      print(summary_df)
    }
  ),
  
  public = list(
    
    #' @description VAR_CAH class constructor
    initialize = function(scale = TRUE, K = 2, ...) {
      if (!is.numeric(K) || K < 2) stop("K must be an integer >= 2.")
      if (!is.logical(scale)) stop("scale must be a boolean (TRUE/FALSE).")
      
      private$FScale <- scale
      private$FNbGroupes <- as.integer(K)
      private$FDataType <- "numeric"
    },
    
    # =========================================================================
    # Public accessors
    # =========================================================================
    
    #' @description Get variables belonging to a given cluster
    #' @param cluster_id Cluster identifier (integer between 1 and K)
    get_cluster_variables = function(cluster_id) {
      if (!private$FFitted) stop("The model must be fitted with $fit() first.")
      
      if (cluster_id < 1 || cluster_id > private$FNbGroupes) {
        stop(paste0("cluster_id must be between 1 and ", private$FNbGroupes))
      }
      
      vars <- names(private$FGroupes)[private$FGroupes == cluster_id]
      return(vars)
    },
    
    #' @description Get the most representative variable of a cluster
    #' @param cluster_id Cluster identifier (integer between 1 and K)
    get_representative_variable = function(cluster_id) {
      if (!private$FFitted) stop("The model must be fitted with $fit() first.")
      
      vars <- self$get_cluster_variables(cluster_id)
      cors <- private$FCorrelations[match(vars, private$FVarNames)]
      return(vars[which.max(cors)])
    },
    
    #' @description Get the hierarchical tree of variables
    get_tree = function() {
      if (!private$FFitted) stop("The model must be fitted with $fit() first.")
      return(private$FArbre)
    },
    
    # ==============================
    # METHOD: inertie() 
    # ==============================
    
    #' @description Compute clustering inertias
    #' @details This method computes total, within-cluster and between-cluster inertia
    #' based on correlations of variables with their principal component.
    #' @return List with following elements:
    #' \itemize{
    #'   \item totale: Total variance of correlations
    #'   \item intra: Within-cluster inertia (average variance within clusters)
    #'   \item inter: Between-cluster inertia (total - within)
    #'   \item pct_expliquee: Percentage of variance explained by clustering
    #' }
    inertie = function() {
      if (!private$FFitted) {
        stop("The model must be fitted with $fit() first.")
      }
      
      # Mean homogeneity is already computed in private$FCorrelations
      # We use variance of correlations as inertia measure
      
      # Total inertia = total variance of correlations
      total_var <- var(private$FCorrelations, na.rm = TRUE)
      
      # Within inertia = average variance within clusters
      intra_vars <- sapply(1:private$FNbGroupes, function(k) {
        vars_in_cluster <- names(private$FGroupes)[private$FGroupes == k]
        cors <- private$FCorrelations[match(vars_in_cluster, private$FVarNames)]
        var(cors, na.rm = TRUE)
      })
      inertie_intra <- mean(intra_vars, na.rm = TRUE)
      
      # Between inertia = total - within
      inertie_inter <- total_var - inertie_intra
      
      # Percentage explained = between / total
      pct_expliquee <- if (total_var > 0) {
        (inertie_inter / total_var) * 100
      } else {
        0
      }
      
      return(list(
        totale = total_var,
        intra = inertie_intra,
        inter = inertie_inter,
        pct_expliquee = pct_expliquee
      ))
    }
  ),
  
  # =========================================================================
  # Specific active bindings (override Groupes for variables)
  # =========================================================================
  active = list(
    #' @field Groupes Variable groups (override with variable names)
    Groupes = function() {
      if (!private$FFitted) {
        stop("The model must be fitted with $fit() first")
      }
      
      # For VAR_CAH, groups concern VARIABLES (columns)
      # so we use colnames, not rownames
      if (!is.null(private$FVarNames)) {
        names(private$FGroupes) <- private$FVarNames
      }
      return(private$FGroupes)
    }
  )
)