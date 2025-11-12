# ===========================================
# CAH + K-means : Classification Hybride
# Variables quantitatives uniquement
# ===========================================
library(R6)

CAH_Kmeans <- R6Class("CAH_Kmeans",
 inherit = ClusterAnalysis,
 
 private = list(
  FArbre = NULL,
  FMethod = "ward.D2",
  FKmeansResult = NULL,
  FNstart = 10,
  FMaxIter = 100,
  FCentroides = NULL,
  FInertie = NULL,
  FUseKmeans = TRUE, 

  # ------------------------------------------------
  # SURCHARGE : Logique de re-fit pour K<-value
  # ------------------------------------------------
  refit_with_k = function(new_k) {
    if (is.null(private$FArbre)) {
      stop("L'arbre CAH n'est pas encore calculé. Le fit initial doit être lancé.")
    }
    
    private$FNbGroupes <- new_k
    groupes_cah <- cutree(private$FArbre, k = new_k)

    X_clean <- private$cleanDataset(private$FX)
    Z <- private$standardize(X_clean)
    
    # 1. Calculer les centroïdes d'initialisation à partir des groupes CAH
    centres_init <- t(sapply(1:new_k, function(k_idx) {
      idx_g <- which(groupes_cah == k_idx)
      if (length(idx_g) == 0) {
         # Cas de groupe vide, utiliser une ligne aléatoire (ou un autre centre)
         idx_g <- sample(1:nrow(Z), 1) 
      }
      colMeans(Z[idx_g, , drop = FALSE], na.rm = TRUE)
    }))
    
    # 2. Lancer K-means avec cette initialisation
    if (private$FUseKmeans) {
      private$FKmeansResult <- kmeans(Z, centers = centres_init, 
                                      iter.max = private$FMaxIter, 
                                      nstart = 1) # nstart=1 car l'initialisation est fixe
      
      private$FGroupes <- private$FKmeansResult$cluster
      private$FCentroides <- private$FKmeansResult$centers
      
    } else {
      # Alternative: simple coupure de l'arbre
      private$FGroupes <- groupes_cah
      # Calculer les centroïdes
      private$FCentroides <- t(sapply(1:new_k, function(k_idx) {
         colMeans(Z[private$FGroupes == k_idx, , drop = FALSE], na.rm = TRUE)
      }))
    }
    
    # Calculer l'inertie (s'il s'agit du résultat K-means)
    if (private$FUseKmeans) {
      private$FInertie <- list(
          totale = private$FKmeansResult$totss,
          intra = private$FKmeansResult$tot.withinss,
          inter = private$FKmeansResult$betweenss,
          pct_expliquee = (private$FKmeansResult$betweenss / private$FKmeansResult$totss) * 100
      )
    }
    
    message(paste0("CAH_Kmeans re-ajusté avec succès pour K=", private$FNbGroupes))
  }
 ),
 
 public = list(
  
  #' @description Initialiser
  initialize = function(k = 2, cr = TRUE, method = "ward.D2", na_action = "warn", use_kmeans = TRUE, ...) {
   super$initialize(k = k, cr = cr, na_action = na_action, ...)
   private$FMethod <- method
   private$FDataType <- "numeric"
   private$FUseKmeans <- use_kmeans
  },
  
  fit = function(X) {
    X_clean <- super$fit(X) # Nettoyage/Standardisation géré par le parent

    # MODIFICATION : Avertissement de résilience pour les grands N
    N <- nrow(X_clean)
    if (N > 10000) {
      warning("Le dataset est grand (N > 10,000). La matrice de distance de la CAH (O(N²)) peut être très lente ou saturer la mémoire. Envisagez d'utiliser Kmeans simple si le temps de calcul est critique.")
    }

    Z <- private$standardize(X_clean)
    
    # 1. CAH
    dist_matrix <- dist(Z)
    private$FArbre <- hclust(dist_matrix, method = private$FMethod)
    
    # 2. Raffinement K-means (ou simple coupure) via refit_with_k
    private$refit_with_k(private$FNbGroupes)
    
    private$FFitted <- TRUE
    invisible(self)
  },
  
  #' @description Prédire les groupes de nouvelles observations
  predict = function(newdata) {
    if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
    if (is.null(private$FCentroides)) stop("Les centroïdes ne sont pas disponibles pour la prédiction.")
    
    # Standardisation des nouvelles données
    Z_new <- private$standardize(newdata, is_new_data = TRUE)
    
    # La logique de prédiction est la même que K-means (plus proche centroïde)
    distances <- apply(Z_new, 1, function(row) {
      if (any(is.na(row))) return(rep(NA_real_, private$FNbGroupes)) 
      colSums((t(private$FCentroides) - row)^2)
    })
    
    # distances est une matrice (NbGroupes x N), on la transpose
    if (!is.matrix(distances)) {
      distances <- matrix(distances, nrow = private$FNbGroupes)
    } else {
      distances <- t(distances)
    }

    # Trouver le groupe le plus proche (indice de la distance minimale)
    predictions <- apply(distances, 1, which.min)

    # Remplacer les prédictions des lignes contenant des NA par NA
    predictions[is.na(predictions) | sapply(predictions, length) == 0] <- NA_integer_
    
    names(predictions) <- rownames(newdata)
    return(predictions)
  },
  
  #' @description Obtenir l'arbre hiérarchique
  get_arbre = function() {
    if (!private$FFitted) stop("Modèle non ajusté")
    return(private$FArbre)
  },
  
  #' @description Obtenir les centroïdes
  get_centroides = function() {
    if (!private$FFitted) stop("Modèle non ajusté")
    return(private$FCentroides)
  },
  
  #' @description Résumé du modèle
  summary = function() {
    cat("----------------------------------------------\n")
    cat("R6 CAH_Kmeans Summary\n")
    cat("----------------------------------------------\n")
    cat("Algorithme : Classification Ascendante Hiérarchique + K-means\n")
    cat("Méthode de lien :", private$FMethod, "\n")
    cat("Raffinement K-means utilisé :", ifelse(private$FUseKmeans, "Oui", "Non"), "\n")
    cat("Groupes (k) :", private$FNbGroupes, "\n")
    if (!is.null(private$FInertie)) {
      cat("Inertie expliquée :", round(private$FInertie$pct_expliquee, 2), "%\n")
      cat("Inertie totale :", round(private$FInertie$totale, 2), "\n")
      cat("Inertie intra-cluster (résiduelle) :", round(private$FInertie$intra, 2), "\n")
    } else {
      cat("Mesures d'inertie non disponibles.\n")
    }
  },
  
  #' @description Obtenir les mesures d'inertie
  inertie = function() {
    if (!private$FFitted) stop("Modèle non ajusté")
    return(private$FInertie)
  }
 )
)