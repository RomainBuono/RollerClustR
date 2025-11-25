#' Unified Interface for Variable Clustering
#'
#' @description
#' `roller_clust()` is the main function of the RollerClustR package. It
#' provides a simple and unified interface to access the three variable
#' clustering algorithms: VAR_CAH, VAR_KMEANS, and TandemVarClust.
#'
#' @param X Data frame or matrix containing the data. Columns represent
#'   the variables to be clustered.
#' @param method Clustering method to use:
#'   - `"var_cah"`: Hierarchical Agglomerative Clustering (default)
#'   - `"var_kmeans"`: K-means clustering with iterative reallocation
#'   - `"tandem"`: Tandem clustering (MCA + HAC) for mixed data
#' @param K Number of desired clusters (default: 2). Must be >= 2.
#' @param scale Boolean indicating whether numeric variables should be
#'   centered and scaled (default: TRUE).
#' @param na.action Action for missing values:
#'   - `"warn"`: Issue a warning (default)
#'   - `"omit"`: Remove observations with NA
#'   - `"fail"`: Stop execution
#' @param ... Additional method-specific arguments:
#'   - For VAR_CAH: No additional arguments required
#'   - For VAR_KMEANS: `n_init` (number of initializations, default: 10),
#'     `max_iter` (maximum iterations, default: 100), 
#'     `tolerance` (convergence threshold, default: 1e-6)
#'   - For TandemVarClust: `n_bins` (number of bins for discretization, default: 3),
#'     `assignment_method` (assignment method: "dice", "voting", or "barycenter")
#'
#' @return An R6 object of the corresponding class (VAR_CAH, VAR_KMEANS, or
#'   TandemVarClust) already fitted to the data.
#'
#' @details
#' ## Choosing the Method
#'
#' - **VAR_CAH**: Agglomerative hierarchical approach with progressive merging.
#'   Recommended for initial exploration and visualization (dendrograms).
#'   Provides a complete hierarchy that can be cut at any level.
#'
#' - **VAR_KMEANS**: Iterative reallocation approach that explicitly minimizes
#'   within-cluster inertia. Recommended when K is known in advance and when
#'   optimality of the partition is important. Uses multiple initializations
#'   for robustness. Faster convergence than hierarchical methods for large datasets.
#'
#' - **TandemVarClust**: Specialized for mixed data (quantitative and categorical).
#'   Uses Multiple Correspondence Analysis (MCA) followed by Hierarchical
#'   Agglomerative Clustering (HAC) on modalities. Automatically handles
#'   discretization of quantitative variables.
#'
#' ## Return Value
#'
#' The returned object has the following methods and properties:
#'
#' - `$summary()`: Displays a detailed clustering summary
#' - `$Groupes`: Accesses the cluster assignment vector
#' - `$K`: Reads or modifies the number of clusters (triggers refitting)
#' - `$predict(new_data)`: Assigns new variables to existing clusters
#'
#' ## Algorithm-Specific Properties
#'
#' **VAR_CAH:**
#' - `$Tree`: Access to the hierarchical tree (hclust object)
#' - `$get_cluster_variables(k)`: Get variables in cluster k
#'
#' **VAR_KMEANS:**
#' - `$WithinClusterInertia`: Within-cluster inertia (lower is better)
#' - `$Converged`: Boolean indicating if algorithm converged
#' - `$NIterations`: Number of iterations until convergence
#' - `$get_cluster_centers()`: Get cluster centers (PC1 of each cluster)
#'
#' **TandemVarClust:**
#' - `$VarianceExplained`: Variance explained by MCA dimensions
#' - `$CategoricalVars`: Names of categorical variables
#' - `$DisjunctiveTable`: Complete disjunctive table used for MCA
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
#' print(model_cah$Groupes)
#'
#' # Example 2: K-means clustering with multiple initializations
#' model_kmeans <- roller_clust(
#'   X = iris[, 1:4],
#'   method = "var_kmeans",
#'   K = 3,
#'   n_init = 20,        # 20 random initializations
#'   max_iter = 100,
#'   scale = TRUE
#' )
#' model_kmeans$summary()
#' print(model_kmeans$WithinClusterInertia)
#' print(model_kmeans$Converged)
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
#'   n_bins = 3,
#'   assignment_method = "dice"
#' )
#' model_tandem$summary()
#'
#' # Example 4: Modify K after fitting (automatic refitting)
#' model_cah$K <- 3  # Automatically refits with K=3
#' model_cah$summary()
#'
#' # Example 5: Compare methods on same data
#' \dontrun{
#' # Fit all three methods
#' cah <- roller_clust(iris[, 1:4], method = "var_cah", K = 3)
#' kmeans <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 3, n_init = 10)
#' 
#' # Compare cluster assignments
#' table(cah$Groupes, kmeans$Groupes)
#' 
#' # VAR_KMEANS typically has lower within-cluster inertia
#' print(kmeans$WithinClusterInertia)
#' }
#'
#' # Example 6: Predict cluster for new variables
#' \dontrun{
#' model <- roller_clust(iris[, 1:4], method = "var_kmeans", K = 2)
#' 
#' # Create a new variable (linear combination)
#' new_var <- data.frame(
#'   NewVar = iris$Sepal.Length + iris$Sepal.Width
#' )
#' 
#' # Predict cluster assignment
#' prediction <- model$predict(new_var)
#' print(prediction)
#' }
#'
#' @seealso [VAR_CAH], [VAR_KMEANS], [TandemVarClust]
#'
#' @export
roller_clust <- function(X,
                         method = c("var_cah", "var_kmeans", "tandem"),
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
    "var_kmeans" = {
      obj <- VAR_KMEANS$new(K = K, scale = scale, na_action = na.action, ...)
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