#' Clustering Descendant Hierarchique de Variables (VARCLUS)
#'
#' @description
#' `VARCLUS` implemente un algorithme de clustering descendant (divisif) pour le
#' regroupement de variables. L'algorithme procede par division recursive des
#' groupes de variables en utilisant l'analyse en composantes principales (ACP)
#' et la rotation Varimax.
#'
#' @details
#' ## Principe de l'algorithme
#'
#' VARCLUS utilise une approche descendante (top-down) :
#' 1. Initialisation : toutes les variables dans un cluster
#' 2. Division recursive basee sur ACP et rotation Varimax
#' 3. Critere d'arret : λ₂ < seuil ou < 3 variables
#'
#' @param K Nombre de clusters (optionnel, ignore - determine automatiquement)
#' @param stop_eigenvalue Seuil pour λ₂ (defaut : 1.0)
#'
#' @export
#' @importFrom R6 R6Class
#' @importFrom stats prcomp varimax

VARCLUS <- R6Class("VARCLUS",
  inherit = ClusterAnalysis,
  
  private = list(
    # =================================================================
    # ATTRIBUTS PRIVES
    # =================================================================
    FStopEigenvalue = 1.0,      # Seuil d'arret (λ₂ ≥ 1)
    FTree = NULL,               # Arbre de partitionnement
    FIterations = NULL,         # Historique des divisions
    FDistanceMetric = "correlation",
    
    # =================================================================
    # METHODE PRINCIPALE : Division recursive
    # =================================================================
    recursive_split = function(vars, depth = 0) {
      n_vars <- length(vars)
      
      # Critere d'arret : moins de 3 variables
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
      
      # Recuperer les 2 premieres valeurs propres
      eigenvalues <- pca_result$sdev^2
      lambda1 <- eigenvalues[1]
      lambda2 <- if(length(eigenvalues) >= 2) eigenvalues[2] else 0
      
      # Critere d'arret : λ₂ < seuil
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
      
      # Partitionnement : affecter chaque variable a l'axe le plus proche
      axis1_corr <- abs(loadings[, 1])
      axis2_corr <- if(ncol(loadings) >= 2) abs(loadings[, 2]) else rep(0, nrow(loadings))
      
      group1_idx <- which(axis1_corr >= axis2_corr)
      group2_idx <- which(axis1_corr < axis2_corr)
      
      # Recursion sur les deux groupes
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
    # METHODES DO_* (CONTRATS D'HERITAGE)
    # =================================================================
    
    #' @description Methode do_fit (appelee par fit())
    do_fit = function(X) {
      # CORRECTIF BUG #5: Validation des donnees sans appel a validateDataset()
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("X doit etre un data frame ou une matrice")
      }
      
      # Verifier que toutes les colonnes sont numeriques
      if (!all(sapply(X, is.numeric))) {
        stop("VARCLUS n'accepte que des variables numeriques")
      }
      
      # Convertir en data frame si necessaire
      X <- as.data.frame(X)
      
      # Verifier le nombre minimum de variables
      if (ncol(X) < 3) {
        stop("VARCLUS necessite au moins 3 variables")
      }
      
      # Stocker les donnees
      private$FX <- X
      
      # Lancer la division recursive
      message("-> Division recursive en cours...")
      private$FTree <- private$recursive_split(1:ncol(X), depth = 0)
      
      # Extraire les clusters
      private$FGroupes <- integer(ncol(X))
      names(private$FGroupes) <- colnames(X)
      private$extract_clusters(private$FTree)
      
      # Compter le nombre de clusters
      private$FNbGroupes <- max(private$FGroupes)
      
      # Marquer comme ajuste
      private$FFitted <- TRUE
      
      cat("\n✓ Partitionnement termine\n")
      cat("  Nombre de clusters detectes :", private$FNbGroupes, "\n\n")
      
      invisible(self)
    },
    
    #' @description Methode do_refit_with_k (non applicable pour VARCLUS)
    do_refit_with_k = function(new_k) {
      stop("VARCLUS est un algorithme descendant hierarchique. ",
           "Le nombre de clusters est determine automatiquement par le critere λ₂ >= 1. ",
           "Utilisez $cut_tree(k) pour obtenir k clusters a partir de l'arbre construit.")
    },
    
    #' @description Prédit le cluster d'appartenance pour de nouvelles variables
    #' @param newdata Data frame ou vecteur contenant les nouvelles variables
    #' @return Liste contenant les assignations et les scores de similarité
    do_predict = function(newdata) {
      if (!private$FFitted) {
        stop("The model must be fitted with $fit() before predicting.")
      }
      
      # Validation et préparation des données
      if (is.vector(newdata)) {
        newdata <- data.frame(new_var = newdata)
      }
      
      if (!is.data.frame(newdata) && !is.matrix(newdata)) {
        stop("newdata doit être un data frame, une matrice ou un vecteur.")
      }
      
      # Vérifier que newdata a le bon nombre d'observations
      if (nrow(newdata) != nrow(private$FX)) {
        stop(paste0("newdata must have the same number of observations (rows) as training data. ",
            "Expected: ", nrow(private$FX), ", Received: ", nrow(newdata)))
      }
      
      # Convertir en data.frame si nécessaire
      newdata <- as.data.frame(newdata)
      
      # Traiter chaque nouvelle variable
      n_new_vars <- ncol(newdata)
      predictions <- integer(n_new_vars)
      scores_matrix <- matrix(NA, nrow = n_new_vars, ncol = private$FNbGroupes)
      colnames(scores_matrix) <- paste0("Cluster_", 1:private$FNbGroupes)
      rownames(scores_matrix) <- colnames(newdata)
      
      for (i in 1:n_new_vars) {
        new_var <- newdata[, i]
        
        # Gérer les valeurs manquantes
        if (any(is.na(new_var))) {
          warning(paste0("La variable '", colnames(newdata)[i], 
                        "' contient des valeurs manquantes. Assignation au cluster 1 par défaut."))
          predictions[i] <- 1
          scores_matrix[i, ] <- NA
          next
        }
        
        # Standardiser la nouvelle variable
        new_var_scaled <- scale(new_var)[, 1]
        
        # Calculer le score de similarité avec chaque cluster
        # Pour VARCLUS, on utilise la corrélation avec la PC1 de chaque cluster
        cluster_scores <- numeric(private$FNbGroupes)
        
        for (k in 1:private$FNbGroupes) {
          # Identifier les variables dans le cluster k
          vars_in_cluster <- names(private$FGroupes)[private$FGroupes == k]
          
          if (length(vars_in_cluster) == 0) {
            cluster_scores[k] <- 0
            next
          }
          
          # Extraire les données du cluster
          X_cluster <- private$FX[, vars_in_cluster, drop = FALSE]
          
          # Calculer la composante principale du cluster
          pca_cluster <- prcomp(X_cluster, scale. = TRUE, center = TRUE)
          pc1_cluster <- pca_cluster$x[, 1]
          
          # Calculer la corrélation avec la nouvelle variable
          cor_val <- cor(new_var_scaled, pc1_cluster, use = "pairwise.complete.obs")
          cluster_scores[k] <- abs(cor_val)
        }
        
        # Assigner au cluster avec le score le plus élevé
        predictions[i] <- which.max(cluster_scores)
        scores_matrix[i, ] <- cluster_scores
      }
      
      # Préparer le résultat
      names(predictions) <- colnames(newdata)
      
      result <- list(
        cluster = predictions,
        scores = scores_matrix,
        best_score = apply(scores_matrix, 1, max, na.rm = TRUE),
        second_best_score = apply(scores_matrix, 1, function(x) {
          sorted <- sort(x, decreasing = TRUE)
          if (length(sorted) >= 2) sorted[2] else NA
        })
      )
      
      return(result)
    },
    
    #' @description Methode do_summary (appelee par summary())
    do_summary = function() {
      cat("\n===============================================================\n")
      cat("   VARCLUS - Resume du Clustering\n")
      cat("===============================================================\n\n")
      
      cat("Nombre de clusters :", private$FNbGroupes, "\n")
      cat("Nombre de variables:", ncol(private$FX), "\n")
      cat("Critere d'arret    : λ₂ >=", private$FStopEigenvalue, "\n\n")
      
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
  # METHODES PUBLIQUES
  # ===========================================================================
  public = list(
    
    # =========================================================================
    # CONSTRUCTEUR
    # =========================================================================
    
    #' @description Constructeur de la classe VARCLUS
    #' @param K Nombre de clusters (ignore - determine automatiquement)
    #' @param stop_eigenvalue Seuil d'arret pour λ₂ (defaut: 1.0)
    #' @param distance_metric Metrique de distance (defaut: "correlation")
    #' @param ... Autres parametres (pour compatibilite)
    initialize = function(K = NULL, 
                          stop_eigenvalue = 1.0, 
                          distance_metric = "correlation",
                          ...) {
      
      # Validation de stop_eigenvalue
      if (!is.numeric(stop_eigenvalue)) {
        stop("stop_eigenvalue doit etre numerique")
      }
      if (stop_eigenvalue < -1e-8) {
        stop("stop_eigenvalue ne peut pas etre negatif")
      }
      
      # Accepter K pour compatibilite mais l'ignorer
      if (!is.null(K)) {
        message("Note: VARCLUS determine automatiquement le nombre de clusters.")
      }
      
      super$initialize()
      
      private$FStopEigenvalue <- stop_eigenvalue
      private$FDistanceMetric <- distance_metric
      
      cat("================================================================\n")
      cat("   VARCLUS - Clustering Descendant de Variables            \n")
      cat("================================================================\n")
      cat("Critere d'arret   : lambda2 >=", stop_eigenvalue, "\n")
      cat("Distance          :", distance_metric, "\n")
      cat("----------------------------------------------------------------\n\n")
    },
    
    # =========================================================================
    # COUPER L'ARBRE A UN NIVEAU K DONNE
    # =========================================================================
    
    #' @description Couper l'arbre pour obtenir k clusters
    #' @param k Nombre de clusters souhaite
    #' @return Vecteur des groupes de variables
    cut_tree = function(k) {
      if (!private$FFitted) {
        stop("Le modele n'a pas ete ajuste. Utilisez $fit() d'abord.")
      }
      
      if (k < 2 || k > ncol(private$FX)) {
        stop(paste0("k doit etre entre 2 et ", ncol(private$FX)))
      }
      
      message("Fonctionnalite cut_tree() a implementer selon les besoins")
      invisible(self)
    },
    
    # =========================================================================
    # NOUVELLE METHODE: inertie() - CORRECTIF BUG #3
    # =========================================================================
    
    #' @description Calculer les inerties du clustering
    #' @details Cette methode calcule l'inertie basee sur les valeurs propres
    #' des ACP realisees sur chaque cluster de variables.
    #' @return Liste avec les elements suivants:
    #' \itemize{
    #'   \item totale: Inertie totale (nombre de variables, maximum theorique)
    #'   \item intra: Inertie intra-cluster (variance non expliquee)
    #'   \item inter: Inertie inter-cluster (variance expliquee par le clustering)
    #'   \item pct_expliquee: Pourcentage de variance expliquee
    #' }
    inertie = function() {
      if (!private$FFitted) {
        stop("Le modele doit etre ajuste avec $fit() d'abord.")
      }
      
      # Calculer l'inertie basee sur les valeurs propres des clusters
      cluster_variances <- sapply(1:private$FNbGroupes, function(k) {
        vars_in_cluster <- names(private$FGroupes)[private$FGroupes == k]
        
        # Si un seul variable dans le cluster, variance expliquee = 0
        if (length(vars_in_cluster) < 2) {
          return(0)
        }
        
        # ACP sur le cluster
        X_sub <- private$FX[, vars_in_cluster, drop = FALSE]
        pca_result <- prcomp(X_sub, scale. = TRUE, center = TRUE)
        
        # Premiere valeur propre = variance expliquee par PC1
        eigenvalues <- pca_result$sdev^2
        return(eigenvalues[1])
      })
      
      # Inertie inter = somme des premieres valeurs propres
      # (variance capturee par la premiere composante de chaque cluster)
      inertie_inter <- sum(cluster_variances)
      
      # Inertie totale = nombre de variables (maximum theorique apres standardisation)
      inertie_totale <- ncol(private$FX)
      
      # Inertie intra = variance non expliquee
      inertie_intra <- inertie_totale - inertie_inter
      
      # Pourcentage explique
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
        # ECRITURE - Bloquer la modification
        stop("Le nombre de clusters (K) ne peut pas etre modifie dans VARCLUS. ",
             "VARCLUS determine automatiquement le nombre optimal de clusters ",
             "base sur le critere lambda2 >= ", private$FStopEigenvalue, ". ",
             "Si vous souhaitez un nombre specifique de clusters, ",
             "utilisez plutot VAR_CAH ou KmodesVarClust.")
      }
    },
    
    #' @field Groupes Groupes des variables (lecture seule)
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modele doit etre ajuste avec $fit() d'abord")
      }
      return(private$FGroupes)
    }
  )
)