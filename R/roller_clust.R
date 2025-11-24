#' Unified Interface for Variable Clustering
#'
#' @description
#' `roller_clust()` is the main function of the RollerClustR package. It
#' provides a simple and unified interface to access the three variable
#' clustering algorithms: VAR_CAH, VARCLUS, and TandemVarClust.
#'
#' @param X Data frame or matrix containing the data. Columns represent
#'   the variables to be clustered.
#' @param method Clustering method to use:
#'   - `"var_cah"`: Hierarchical Agglomerative Clustering (default)
#'   - `"varclus"`: Divisive clustering with PCA
#'   - `"tandem"`: Tandem clustering (MCA + HAC) for mixed data
#' @param K Number of desired clusters (default: 2). Must be >= 2.
#' @param scale Boolean indicating whether numeric variables should be
#'   centered and scaled (default: TRUE).
#' @param na.action Action for missing values:
#'   - `"warn"`: Issue a warning (default)
#'   - `"omit"`: Remove observations with NA
#'   - `"fail"`: Stop execution
#' @param ... Additional method-specific arguments:
#'   - For VAR_CAH: `max.iter`, `tolerance`
#'   - For VARCLUS: `stop_eigenvalue`
#'   - For TandemVarClust: `n_bins`, `method_cah`, `n_factors`
#'
#' @return An R6 object of the corresponding class (VAR_CAH, VARCLUS, or
#'   TandemVarClust) already fitted to the data.
#'
#' @details
#' ## Choosing the Method
#'
#' - **VAR_CAH**: Agglomerative approach, progressive merging. Recommended for
#'   initial exploration or when variables are continuous numeric.
#'
#' - **VARCLUS**: Divisive approach, recursive splitting. Better for
#'   identifying complex hierarchical structures.
#'
#' - **TandemVarClust**: Specialized for mixed data (quantitative and categorical).
#'   Uses Multiple Correspondence Analysis (MCA) followed by Hierarchical
#'   Agglomerative Clustering (HAC) on modalities.
#'
#' ## Return Value
#'
#' The returned object has the following methods and properties:
#'
#' - `$summary()`: Displays a detailed clustering summary
#' - `$Groupes`: Accesses the assignment vector
#' - `$K`: Reads or modifies the number of clusters
#'
#' @examples
#' # Example 1: Hierarchical agglomerative clustering
#' library(RollerClustR)
#' data(iris)
#'
#' model_cah <- roller_clust(
#'   X = iris[, 1:4],
#'   method = "var_cah",
#'   K = 2,
#'   scale = TRUE
#' )
#' model_cah$summary()
#'
#' # Example 2: Divisive VARCLUS clustering
#' model_vc <- roller_clust(
#'   X = iris[, 1:4],
#'   method = "varclus",
#'   K = 3
#' )
#' print(model_vc$Groupes)
#'
#' # Example 3: Tandem clustering for mixed data
#' # Create mixed data (quantitative + categorical)
#' iris_mixed <- iris
#' iris_mixed$Size <- cut(iris$Sepal.Length, 
#'                        breaks = 3, 
#'                        labels = c("Small", "Medium", "Large"))
#'
#' model_tandem <- roller_clust(
#'   X = iris_mixed[, c(1:4, 6)],
#'   method = "tandem",
#'   K = 3,
#'   n_bins = 5
#' )
#' model_tandem$summary()
#'
#' # Example 4: Modify K after fitting
#' model_cah$K <- 3  # Automatically refits with K=3
#'
#' @seealso [VAR_CAH], [VARCLUS], \code{TandemVarClust}
#'
#' @export
roller_clust <- function(X,
                         method = c("var_cah", "varclus", "tandem"),
                         K = 2,
                         scale = TRUE,
                         na.action = c("warn", "omit", "fail"),
                         ...) {
  
  # Validate arguments
  method <- match.arg(method)
  na.action <- match.arg(na.action)
  
  if (!is.numeric(K) || K < 2) {
    stop("K must be an integer >= 2")
  }
  
  if (!is.data.frame(X) && !is.matrix(X)) {
    stop("X must be a data.frame or a matrix")
  }
  
  # Create and fit the model according to the chosen method
  model <- switch(method,
    "var_cah" = {
      obj <- VAR_CAH$new(K = K, scale = scale, na_action = na.action, ...)
      obj$fit(X)
      obj
    },
    "varclus" = {
      obj <- VARCLUS$new(K = K, scale = scale, na_action = na.action, ...)
      obj$fit(X)
      obj
    },
    "tandem" = {
      obj <- TandemVarClust$new(K = K, scale = scale, na_action = na.action, ...)
      obj$fit(X)
      obj
    }
  )
  
  return(model)
}