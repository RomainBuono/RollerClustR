#' @keywords internal
"_PACKAGE"

#' RollerClustR: Advanced Clustering Algorithms for Variables
#'
#' @description
#' RollerClustR provides a comprehensive suite of clustering algorithms
#' specifically designed for grouping variables (not observations). The package
#' implements three complementary approaches with a unified R6 architecture.
#'
#' @section Main Features:
#'
#' **Three Clustering Methods:**
#' - **VAR_CAH**: Hierarchical Ascendant Clustering using correlation-based distances
#' - **VARCLUS**: Divisive clustering with PCA-based recursive splitting
#' - **KmodesVarClust**: K-modes algorithm adapted for categorical variables
#'
#' **Unified Interface:**
#' - Single wrapper function `roller_clust()` for all methods
#' - Consistent R6 object-oriented design
#' - Common methods: `$fit()`, `$summary()`, `$predict()`
#'
#' **Flexible Configuration:**
#' - User-configurable discretization parameters
#' - Robust missing data handling
#' - Automatic data type detection and validation
#'
#' @section Getting Started:
#'
#' The easiest way to use RollerClustR is through the `roller_clust()` function:
#'
#' ```r
#' # Quick start
#' library(RollerClustR)
#' result <- roller_clust(iris[, 1:4], method = "var_cah", K = 2)
#' result$summary()
#' ```
#'
#' For more control, use the R6 classes directly:
#'
#' ```r
#' # Advanced usage
#' model <- VAR_CAH$new(K = 3, scale = TRUE)
#' model$fit(iris[, 1:4])
#' model$K <- 4  # Re-fit with different K
#' ```
#'
#' @section Package Architecture:
#'
#' The package uses an object-oriented architecture with:
#' - **ClusterAnalysis**: Parent class defining the common interface
#' - **VAR_CAH, VARCLUS, KmodesVarClust**: Specialized implementations
#' - Template Method pattern for consistent behavior
#' - Active bindings for clean API (e.g., `$K`, `$Groupes`)
#'
#' @section Key Functions:
#' - [roller_clust()]: Main wrapper function
#' - [VAR_CAH]: Hierarchical clustering class
#' - [VARCLUS]: Divisive clustering class
#' - [KmodesVarClust]: K-modes clustering class
#' - [validate_data_type()]: Data validation utilities
#'
#' @docType package
#' @name RollerClustR-package
#' @aliases RollerClustR
#'
#' @author Romain BUONO <r.buono@univ-lyon2.fr>
#'
#' @seealso
#' Useful links:
#' - Report bugs at <https://github.com/RomainBuono/RollerClustR/issues>
#'
#' @importFrom R6 R6Class
#' @importFrom stats cor dist hclust cutree prcomp varimax var
#' @importFrom utils head tail
NULL
