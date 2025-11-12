# ==========================================
# K-means : Clustering par centres mobiles
# Variables quantitatives uniquement
# ==========================================
library(R6)

Kmeans <- R6Class("Kmeans",
  inherit = ClusterAnalysis,
  
  private = list(
    FModel = NULL,
    FNstart = 10,
    FCentroides = NULL,
    FInertie = NULL,

    # ------------------------------------------------
    # SURCHARGE : Logique de re-fit pour K<-value
    # ------------------------------------------------
    refit_with_k = function(new_k) {
      private$FNbGroupes <- new_k
      
      X_clean <- private$cleanDataset(private$FX)
      Z <- private$standardize(X_clean)
      
      tryCatch({
        # Lancer le K-means
        private$FModel <- kmeans(Z, centers = new_k, nstart = private$FNstart)
        private$FGroupes <- private$FModel$cluster
        private$FCentroides <- private$FModel$centers
        private$FInertie <- list(
          totale = private$FModel$totss,
          intra = private$FModel$tot.withinss,
          inter = private$FModel$betweenss,
          pct_expliquee = (private$FModel$betweenss / private$FModel$totss) * 100
        )
        message(paste0("Kmeans ré-ajusté avec succès avec k = ", new_k))
      }, error = function(e) {
        stop(paste("Erreur lors du ré-ajustement K-means:", e$message))
      })
    }
  ),
  
  public = list(
    initialize = function(k = 3, cr = TRUE, nstart = 10, na_action = "warn") {
      super$initialize(k = k, cr = cr, na_action = na_action)
      private$FNstart <- as.integer(nstart)
      private$FDataType <- "numeric"
    },

    fit = function(X) {
      X_clean <- super$fit(X) # Nettoyage/Standardisation géré par le parent
      Z <- private$standardize(X_clean)
      
      if (nrow(Z) < private$FNbGroupes) {
        stop("Pas assez d'observations pour le nombre de groupes demandé (k=", private$FNbGroupes, ").")
      }
      
      # Appeler la logique de fit de la méthode refit_with_k
      private$refit_with_k(private$FNbGroupes)
      private$FFitted <- TRUE
      
      invisible(self)
    },
    
    #' @description Prédire les groupes de nouvelles observations
    predict = function(newdata) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      if (is.null(private$FCentroides)) stop("Les centroïdes sont manquants.")
      
      # Standardisation des nouvelles données
      Z_new <- private$standardize(newdata, is_new_data = TRUE)
      
      # Calcul de la distance euclidienne (distance au carré pour l'efficacité)
      distances <- apply(Z_new, 1, function(row) {
        if (any(is.na(row))) return(rep(NA_real_, private$FNbGroupes)) 
        colSums((t(private$FCentroides) - row)^2)
      })
      
      distances <- t(distances) 
      
      # Trouver le groupe le plus proche (indice de la distance minimale)
      predictions <- apply(distances, 1, which.min)
      
      # Remplacer les prédictions des lignes contenant des NA par NA
      predictions[is.na(predictions) | sapply(predictions, length) == 0] <- NA
      
      return(as.integer(unlist(predictions)))
    },
    
    inertie = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      return(private$FInertie)
    }
  )
)