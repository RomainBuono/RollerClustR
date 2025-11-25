#' Parent Class for Cluster Analysis
#'
#' @description
#' `ClusterAnalysis` is an R6 parent class that defines the base architecture
#' for all clustering algorithms in the RollerClustR package. It implements
#' the Template Method pattern to ensure a consistent interface across
#' different clustering methods.
#'
#' @details
#' This class should not be instantiated directly. It serves as a base for
#' child classes: [VAR_CAH], [VAR_KMEANS] and [TandemVarClust].
#'
#' ## Public Methods
#'
#' - `fit(X)` : Fits the model to the data
#' - `summary()` : Displays a summary of the fitted model
#' - `predict(newdata)` : Predicts groups for new data
#'
#' ## Active Bindings
#'
#' - `K` : Number of clusters (read/write)
#' - `Groupes` : Vector of group assignments
#'
#' ## Inheritance Contracts
#'
#' Child classes must implement the following private methods:
#'
#' - `do_fit(X)` : Specific fitting logic
#' - `do_refit_with_k(new_k)` : Refitting with a new K
#' - `do_predict(newdata)` : Prediction for new data
#' - `do_summary()` : Display of specific summary
#'
#' @examples
#' # This class should not be used directly
#' # Use child classes instead:
#' \dontrun{
#' model <- VAR_CAH$new(K = 3)
#' model$fit(iris[, 1:4])
#' model$summary()
#' }
#'
#' @seealso [VAR_CAH], [VAR_KMEANS], [TandemVarClust], [roller_clust()]
#'
#' @importFrom R6 R6Class
#' @export
ClusterAnalysis <- R6Class("ClusterAnalysis",
  
  # =================================================================
  # 1. Private Fields
  # =================================================================
  private = list(
    FX = NULL,           # Original training data
    FScale = TRUE,       
    FNbGroupes = 2,      
    FGroupes = c(),      
    FDataType = "numeric", 
    FFitted = FALSE,     # Is the model fitted?
    FNAAction = "warn",  
    FHasMissing = FALSE, 
    FNAIndices = NULL,   
    FX_clean = NULL,     
    
    # =================================================================
    # 2. Inheritance Contracts (Must be implemented by child class)
    # These methods are PRIVATE and called by public methods
    # =================================================================
    
    #' @description Abstract method for fitting
    #' @param X Data frame or data matrix
    do_fit = function(X) {
      stop("Method 'do_fit(X)' must be implemented by child class.")
    },
    
    #' @description Abstract method for refitting with new K
    #' @param new_k New number of clusters
    do_refit_with_k = function(new_k) {
      stop("Method 'do_refit_with_k(new_k)' must be implemented by child class.")
    },
    
    #' @description Abstract method for prediction
    #' @param newdata New data
    do_predict = function(newdata) {
      stop("Method 'do_predict(newdata)' must be implemented by child class.")
    },
    
    #' @description Abstract method for summary
    do_summary = function() {
      stop("Method 'do_summary()' must be implemented by child class.")
    }
  ),
  
  # =================================================================
  # 3. Public Methods
  # =================================================================
  public = list(
    
    #' @description Constructor of parent class
    #' @param ... Arguments passed to child classes
    initialize = function(...) {
      NULL
    },
    
    #' @description Fits the model to the data
    #' @param X Data frame or matrix of training data
    #' @return The object itself (invisibly) to allow chaining
    #' @examples
    #' \dontrun{
    #' model <- VAR_CAH$new(K = 3)
    #' model$fit(iris[, 1:4])
    #' }
    fit = function(X) {
      private$do_fit(X)
      invisible(self)
    },
    
    #' @description Displays a summary of the fitted model
    #' @return The object itself (invisibly)
    #' @examples
    #' \dontrun{
    #' model <- VAR_CAH$new(K = 3)
    #' model$fit(iris[, 1:4])
    #' model$summary()
    #' }
    summary = function() {
      private$do_summary()
      invisible(self)
    },
    
    #' @description Predicts groups for new data
    #' @param newdata New data (same structure as training X)
    #' @return Vector of group predictions
    #' @examples
    #' \dontrun{
    #' model <- VAR_CAH$new(K = 3)
    #' model$fit(iris[1:100, 1:4])
    #' predictions <- model$predict(iris[101:150, 1:4])
    #' }
    predict = function(newdata) {
      private$do_predict(newdata)
    }
  ),
  
  # =================================================================
  # 4. Active Bindings (Active Fields)
  # =================================================================
  active = list(
    
    #' @field K Number of clusters (read/write). 
    #'   Writing triggers a model refitting.
    K = function(value) {
      if (missing(value)) {
        # READ
        return(private$FNbGroupes)
      } else {
        # WRITE
        if (!is.numeric(value) || value < 2) {
          stop("K must be an integer >= 2.")
        }
        value <- as.integer(value)
        
        private$do_refit_with_k(value) # Call inheritance contract
        
        return(private$FNbGroupes)
      }
    },
    
    #' @field Groupes Vector of group assignments for each
    #'   observation or variable (depending on clustering method).
    Groupes = function() {
      if (!private$FFitted) {
        stop("The model must be fitted with $fit() first")
      }
      
      # Reassign groups to initial observations if NAs were omitted
      if (private$FNAAction == "omit" && private$FHasMissing) {
        result <- rep(NA_integer_, nrow(private$FX))
        
        # Index of observations WITHOUT NA
        idx_clean <- setdiff(1:nrow(private$FX), private$FNAIndices)
        
        if (length(idx_clean) != length(private$FGroupes)) {
          stop("Critical internal error: Group size inconsistency. Check NA/fit() logic.") 
        }
        
        result[idx_clean] <- private$FGroupes
        names(result) <- rownames(private$FX)
        return(result)
      } else {
        # Simply return groups
        # Names are assigned by child class if needed
        return(private$FGroupes)
      }
    }
  )
)