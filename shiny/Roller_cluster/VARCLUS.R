# =================================================================
# VARCLUS - VERSION CORRIGÉE POUR HÉRITAGE
# =================================================================

VARCLUS <- R6Class("VARCLUS",
                   inherit = ClusterAnalysis,
                   
                   private = list(
                     # Attributs privés
                     FStopEigenvalue = 1.0,
                     FTree = NULL,
                     FIterations = NULL,
                     FDistanceMetric = "correlation",
                     
                     # =========================================================================
                     # CONTRAT D'HÉRITAGE : do_fit (OBLIGATOIRE)
                     # =========================================================================
                     do_fit = function(X) {
                       cat("╔════════════════════════════════════════════════════════════╗\n")
                       cat("║   AJUSTEMENT VARCLUS                                       ║\n")
                       cat("╚════════════════════════════════════════════════════════════╝\n\n")
                       
                       # Validation des données
                       if (!is.data.frame(X) && !is.matrix(X)) {
                         stop("X doit être un data frame ou une matrice")
                       }
                       
                       # Vérifier que toutes les colonnes sont numériques
                       if (!all(sapply(X, is.numeric))) {
                         stop("VARCLUS nécessite uniquement des variables numériques")
                       }
                       
                       private$FX <- X
                       
                       if (ncol(X) < 3) {
                         stop("VARCLUS nécessite au moins 3 variables")
                       }
                       
                       # Lancer la division récursive
                       message("→ Division récursive en cours...")
                       private$FTree <- private$recursive_split(1:ncol(X), depth = 0)
                       
                       # Extraire les clusters
                       private$FGroupes <- integer(ncol(X))
                       names(private$FGroupes) <- colnames(X)
                       private$extract_clusters(private$FTree)
                       
                       # Compter le nombre de clusters
                       private$FNbGroupes <- max(private$FGroupes)
                       
                       private$FFitted <- TRUE
                       
                       cat("\n✓ Partitionnement terminé\n")
                       cat("  Nombre de clusters détectés :", private$FNbGroupes, "\n\n")
                       
                       invisible(self)
                     },
                     
                     # =========================================================================
                     # CONTRAT D'HÉRITAGE : do_refit_with_k (NON APPLICABLE)
                     # =========================================================================
                     do_refit_with_k = function(new_k) {
                       stop("VARCLUS est un algorithme descendant hiérarchique. ",
                            "Le nombre de clusters est déterminé automatiquement par le critère λ₂ ≥ 1. ",
                            "Utilisez $cut_tree(k) pour obtenir k clusters à partir de l'arbre construit.")
                     },
                     
                     # =========================================================================
                     # CONTRAT D'HÉRITAGE : do_predict (NON APPLICABLE)
                     # =========================================================================
                     do_predict = function(newdata) {
                       stop("predict() n'est pas implémenté pour VARCLUS (clustering de variables).")
                     },
                     
                     # =========================================================================
                     # CONTRAT D'HÉRITAGE : do_summary
                     # =========================================================================
                     do_summary = function() {
                       cat("\n═══════════════════════════════════════════════════════════\n")
                       cat("   VARCLUS - Résumé du Clustering\n")
                       cat("═══════════════════════════════════════════════════════════\n\n")
                       
                       cat("Nombre de clusters :", private$FNbGroupes, "\n")
                       cat("Nombre de variables:", ncol(private$FX), "\n")
                       cat("Critère d'arrêt    : λ₂ ≥", private$FStopEigenvalue, "\n\n")
                       
                       # Afficher composition des clusters
                       for (k in 1:private$FNbGroupes) {
                         vars_in_cluster <- names(private$FGroupes)[private$FGroupes == k]
                         cat("Cluster", k, ":", length(vars_in_cluster), "variables\n")
                         cat("  ", paste(vars_in_cluster, collapse = ", "), "\n\n")
                       }
                       
                       invisible(self)
                     },
                     
                     # =========================================================================
                     # MÉTHODE PRINCIPALE : Division récursive
                     # =========================================================================
                     recursive_split = function(vars, depth = 0) {
                       n_vars <- length(vars)
                       
                       # Critère d'arrêt : moins de 3 variables
                       if (n_vars < 3) {
                         return(list(
                           type = "leaf",
                           vars = vars,
                           depth = depth
                         ))
                       }
                       
                       # ACP sur le sous-groupe de variables
                       X_sub <- private$FX[, vars, drop = FALSE]
                       pca_result <- prcomp(X_sub, scale. = TRUE, center = TRUE)
                       
                       # Récupérer les 2 premières valeurs propres
                       eigenvalues <- pca_result$sdev^2
                       lambda1 <- eigenvalues[1]
                       lambda2 <- if(length(eigenvalues) >= 2) eigenvalues[2] else 0
                       
                       # Critère d'arrêt : λ₂ < seuil
                       if (lambda2 < private$FStopEigenvalue) {
                         return(list(
                           type = "leaf",
                           vars = vars,
                           depth = depth,
                           lambda1 = lambda1,
                           lambda2 = lambda2
                         ))
                       }
                       
                       # Rotation Varimax sur les 2 premiers axes
                       if (ncol(pca_result$rotation) >= 2) {
                         rotated <- varimax(pca_result$rotation[, 1:2])
                         loadings <- rotated$loadings
                       } else {
                         loadings <- pca_result$rotation[, 1, drop = FALSE]
                       }
                       
                       # Partitionnement : affecter chaque variable à l'axe le plus proche
                       axis1_corr <- abs(loadings[, 1])
                       axis2_corr <- if(ncol(loadings) >= 2) abs(loadings[, 2]) else rep(0, nrow(loadings))
                       
                       group1_idx <- which(axis1_corr >= axis2_corr)
                       group2_idx <- which(axis1_corr < axis2_corr)
                       
                       # Récursion sur les deux groupes
                       left_branch <- private$recursive_split(vars[group1_idx], depth + 1)
                       right_branch <- private$recursive_split(vars[group2_idx], depth + 1)
                       
                       return(list(
                         type = "node",
                         depth = depth,
                         lambda1 = lambda1,
                         lambda2 = lambda2,
                         left = left_branch,
                         right = right_branch
                       ))
                     },
                     
                     # =========================================================================
                     # Extraire les clusters terminaux
                     # =========================================================================
                     extract_clusters = function(tree, cluster_id = 1) {
                       if (tree$type == "leaf") {
                         for (var in tree$vars) {
                           private$FGroupes[var] <- cluster_id
                         }
                         return(cluster_id + 1)
                       } else {
                         cluster_id <- private$extract_clusters(tree$left, cluster_id)
                         cluster_id <- private$extract_clusters(tree$right, cluster_id)
                         return(cluster_id)
                       }
                     }
                   ),
                   
                   # ===========================================================================
                   # MÉTHODES PUBLIQUES
                   # ===========================================================================
                   public = list(
                     
                     # =========================================================================
                     # Constructeur
                     # =========================================================================
                     initialize = function(stop_eigenvalue = 1.0, 
                                           distance_metric = "correlation") {
                       # Appel du constructeur parent (important!)
                       super$initialize()
                       
                       private$FStopEigenvalue <- stop_eigenvalue
                       private$FDistanceMetric <- distance_metric
                       
                       cat("╔════════════════════════════════════════════════════════════╗\n")
                       cat("║   VARCLUS - Clustering Descendant de Variables            ║\n")
                       cat("╚════════════════════════════════════════════════════════════╝\n")
                       cat("Critère d'arrêt   : λ₂ ≥", stop_eigenvalue, "\n")
                       cat("Distance          :", distance_metric, "\n")
                       cat("────────────────────────────────────────────────────────────\n\n")
                     },
                     
                     # =========================================================================
                     # COUPER L'ARBRE à un niveau K donné
                     # =========================================================================
                     cut_tree = function(k) {
                       if (!private$FFitted) {
                         stop("Le modèle n'a pas été ajusté. Utilisez $fit() d'abord.")
                       }
                       
                       message("Fonctionnalité cut_tree() à implémenter selon les besoins")
                       invisible(self)
                     }
                   )
)