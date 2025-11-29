# =============================================================================
# VARCLUS : Clustering Descendant Hiérarchique de Variables
# Algorithme basé sur la division récursive par ACP et rotation Varimax
# =============================================================================

library(R6)

VARCLUS <- R6Class("VARCLUS",
  inherit = ClusterAnalysis,
  
  private = list(
    # =================================================================
    # ATTRIBUTS PRIVÉS
    # =================================================================
    FStopEigenvalue = 1.0,      # Seuil d'arrêt (λ₂ ≥ 1)
    FTree = NULL,               # Arbre de partitionnement
    FIterations = NULL,         # Historique des divisions
    FDistanceMetric = "correlation",
    
    # =================================================================
    # MÉTHODE PRINCIPALE : Division récursive
    # =================================================================
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
      
      # Rotation Varimax (ou Quartimax) sur les 2 premiers axes
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
    
    # =================================================================
    # EXTRAIRE LES CLUSTERS TERMINAUX
    # =================================================================
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
    },
    
    # =================================================================
    # MÉTHODES DO_* (CONTRATS D'HÉRITAGE)
    # =================================================================
    
    #' @description Méthode do_fit (appelée par fit())
    do_fit = function(X) {
      # CORRECTIF BUG #5: Validation des données sans appel à validateDataset()
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("X doit être un data frame ou une matrice")
      }
      
      # Vérifier que toutes les colonnes sont numériques
      if (!all(sapply(X, is.numeric))) {
        stop("VARCLUS n'accepte que des variables numériques")
      }
      
      # Convertir en data frame si nécessaire
      X <- as.data.frame(X)
      
      # Vérifier le nombre minimum de variables
      if (ncol(X) < 3) {
        stop("VARCLUS nécessite au moins 3 variables")
      }
      
      # Stocker les données
      private$FX <- X
      
      # Lancer la division récursive
      message("→ Division récursive en cours...")
      private$FTree <- private$recursive_split(1:ncol(X), depth = 0)
      
      # Extraire les clusters
      private$FGroupes <- integer(ncol(X))
      names(private$FGroupes) <- colnames(X)
      private$extract_clusters(private$FTree)
      
      # Compter le nombre de clusters
      private$FNbGroupes <- max(private$FGroupes)
      
      # Marquer comme ajusté
      private$FFitted <- TRUE
      
      cat("\n✓ Partitionnement terminé\n")
      cat("  Nombre de clusters détectés :", private$FNbGroupes, "\n\n")
      
      invisible(self)
    },
    
    #' @description Méthode do_refit_with_k (non applicable pour VARCLUS)
    do_refit_with_k = function(new_k) {
      stop("VARCLUS est un algorithme descendant hiérarchique. ",
           "Le nombre de clusters est déterminé automatiquement par le critère λ₂ ≥ 1. ",
           "Utilisez $cut_tree(k) pour obtenir k clusters à partir de l'arbre construit.")
    },
    
    #' @description Méthode do_predict (non applicable pour clustering de variables)
    do_predict = function(newdata) {
      stop("predict() n'est pas implémenté pour VARCLUS. ",
           "Utilisez $Groupes pour obtenir l'affectation des variables.")
    },
    
    #' @description Méthode do_summary (appelée par summary())
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
    }
  ),
  
  # ===========================================================================
  # MÉTHODES PUBLIQUES
  # ===========================================================================
  public = list(
    
    # =========================================================================
    # CONSTRUCTEUR
    # =========================================================================
    
    #' @description Constructeur de la classe VARCLUS
    #' @param stop_eigenvalue Seuil d'arrêt pour la deuxième valeur propre (défaut: 1.0)
    #' @param distance_metric Métrique de distance (défaut: "correlation")
    initialize = function(stop_eigenvalue = 1.0, 
                          distance_metric = "correlation") {
      
      # CORRECTIF BUG #1: Appel correct à super$initialize() sans paramètres
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
    # COUPER L'ARBRE À UN NIVEAU K DONNÉ
    # =========================================================================
    
    #' @description Couper l'arbre pour obtenir k clusters
    #' @param k Nombre de clusters souhaité
    #' @return Vecteur des groupes de variables
    cut_tree = function(k) {
      if (!private$FFitted) {
        stop("Le modèle n'a pas été ajusté. Utilisez $fit() d'abord.")
      }
      
      if (k < 2 || k > ncol(private$FX)) {
        stop(paste0("k doit être entre 2 et ", ncol(private$FX)))
      }
      
      message("Fonctionnalité cut_tree() à implémenter selon les besoins")
      invisible(self)
    },
    
    # =========================================================================
    # NOUVELLE MÉTHODE: inertie() - CORRECTIF BUG #3
    # =========================================================================
    
    #' @description Calculer les inerties du clustering
    #' @details Cette méthode calcule l'inertie basée sur les valeurs propres
    #' des ACP réalisées sur chaque cluster de variables.
    #' @return Liste avec les éléments suivants:
    #' \itemize{
    #'   \item totale: Inertie totale (nombre de variables, maximum théorique)
    #'   \item intra: Inertie intra-cluster (variance non expliquée)
    #'   \item inter: Inertie inter-cluster (variance expliquée par le clustering)
    #'   \item pct_expliquee: Pourcentage de variance expliquée
    #' }
    inertie = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      
      # Calculer l'inertie basée sur les valeurs propres des clusters
      cluster_variances <- sapply(1:private$FNbGroupes, function(k) {
        vars_in_cluster <- names(private$FGroupes)[private$FGroupes == k]
        
        # Si un seul variable dans le cluster, variance expliquée = 0
        if (length(vars_in_cluster) < 2) {
          return(0)
        }
        
        # ACP sur le cluster
        X_sub <- private$FX[, vars_in_cluster, drop = FALSE]
        pca_result <- prcomp(X_sub, scale. = TRUE, center = TRUE)
        
        # Première valeur propre = variance expliquée par PC1
        eigenvalues <- pca_result$sdev^2
        return(eigenvalues[1])
      })
      
      # Inertie inter = somme des premières valeurs propres
      # (variance capturée par la première composante de chaque cluster)
      inertie_inter <- sum(cluster_variances)
      
      # Inertie totale = nombre de variables (maximum théorique après standardisation)
      inertie_totale <- ncol(private$FX)
      
      # Inertie intra = variance non expliquée
      inertie_intra <- inertie_totale - inertie_inter
      
      # Pourcentage expliqué
      pct_expliquee <- (inertie_inter / inertie_totale) * 100
      
      return(list(
        totale = inertie_totale,
        intra = inertie_intra,
        inter = inertie_inter,
        pct_expliquee = pct_expliquee
      ))
    }
  ),
  
  # ===========================================================================
  # ACTIVE BINDINGS
  # ===========================================================================
  active = list(
    
    #' @field K Nombre de clusters (lecture seule pour VARCLUS)
    K = function(value) {
      if (missing(value)) {
        # LECTURE
        return(private$FNbGroupes)
      } else {
        # ÉCRITURE - Bloquer la modification
        stop("Le nombre de clusters (K) ne peut pas être modifié dans VARCLUS. ",
             "VARCLUS détermine automatiquement le nombre optimal de clusters ",
             "basé sur le critère λ₂ ≥ ", private$FStopEigenvalue, ". ",
             "Si vous souhaitez un nombre spécifique de clusters, ",
             "utilisez plutôt VAR_CAH ou KmodesVarClust.")
      }
    },
    
    #' @field Groupes Groupes des variables (lecture seule)
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      return(private$FGroupes)
    }
  )
)
