# ==========================================
# K-means : Clustering par centres mobiles
# Variables quantitatives uniquement
# ==========================================

Kmeans <- R6Class("Kmeans",
  inherit = ClusterAnalysis,
  
  private = list(
    FModel = NULL,
    FNstart = 10
  ),
  
  public = list(
    #' @description Initialiser un objet K-means
    #' @param k Nombre de groupes (défaut: 3)
    #' @param cr Standardiser les données ? (défaut: TRUE)
    #' @param nstart Nombre d'initialisations aléatoires (défaut: 10)
    initialize = function(k = 3, cr = TRUE, nstart = 10) {
      super$initialize(k = k, cr = cr)
      private$FNstart <- nstart
    },
    
    #' @description Ajuster le modèle K-means sur les données
    #' @param X Data frame avec variables numériques uniquement
    fit = function(X) {
      # Validation
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      
      # Vérifier que toutes les variables sont numériques
      if (!all(sapply(X, is.numeric))) {
        stop("K-means accepte uniquement des variables numériques. Utilisez Kprototypes pour des données mixtes.")
      }
      
      # Stocker les données
      private$FX <- X
      private$FDataType <- private$detectDataType(X)
      
      # Standardisation
      Z <- if (private$FScale) scale(X) else as.matrix(X)
      
      # Clustering K-means
      private$FModel <- kmeans(Z, centers = private$FNbGroupes, nstart = private$FNstart)
      private$FGroupes <- private$FModel$cluster
      
      # Marquer comme ajusté
      private$FFitted <- TRUE
      
      message("✓ Modèle K-means ajusté avec succès (", private$FNbGroupes, " groupes, ", 
              round(private$FModel$betweenss / private$FModel$totss * 100, 1), "% inertie expliquée)")
      invisible(self)
    },
    
    #' @description Affichage succinct spécifique à K-means
    print = function() {
      super$print()
      if (private$FFitted) {
        pct_inertie <- round(private$FModel$betweenss / private$FModel$totss * 100, 2)
        cat("Inertie expliquée :", pct_inertie, "%\n")
      }
      invisible(self)
    },
    
    #' @description Affichage détaillé spécifique à K-means
    summary = function() {
      super$summary()
      
      if (private$FFitted) {
        cat("\n--- Spécifique K-means ---\n")
        cat("Nombre d'initialisations (nstart) :", private$FNstart, "\n")
        cat("Nombre d'itérations :", private$FModel$iter, "\n\n")
        
        cat("--- Décomposition de l'inertie ---\n")
        cat("Inertie totale      :", round(private$FModel$totss, 2), "\n")
        cat("Inertie intra-classe:", round(private$FModel$tot.withinss, 2), "\n")
        cat("Inertie inter-classe:", round(private$FModel$betweenss, 2), "\n")
        pct <- round(private$FModel$betweenss / private$FModel$totss * 100, 2)
        cat("% Inertie expliquée :", pct, "%\n")
      }
      
      invisible(self)
    },
    
    #' @description Visualiser le clustering
    plot = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      # Projection sur les 2 premières dimensions
      if (ncol(private$FX) >= 2) {
        plot(private$FX[, 1:2], col = private$FGroupes, pch = 19,
             main = "K-means clustering",
             xlab = names(private$FX)[1],
             ylab = names(private$FX)[2])
        
        # Afficher les centres si possible (données standardisées)
        if (!is.null(private$FModel$centers) && ncol(private$FModel$centers) >= 2) {
          points(private$FModel$centers[, 1:2], 
                 col = 1:private$FNbGroupes, pch = 8, cex = 2, lwd = 2)
        }
        legend("topright", legend = paste("Groupe", 1:private$FNbGroupes),
               col = 1:private$FNbGroupes, pch = 19, cex = 0.8)
      } else {
        cat("Visualisation nécessite au moins 2 variables\n")
      }
      invisible(self)
    },
    
    #' @description Obtenir les informations d'inertie
    #' @return Liste avec composantes d'inertie
    inertie = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      return(list(
        totale = private$FModel$totss,
        intra = private$FModel$tot.withinss,
        inter = private$FModel$betweenss,
        pct_expliquee = 100 * private$FModel$betweenss / private$FModel$totss
      ))
    }
  ),
  
  active = list(
    NbGroupes = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        if (!private$FFitted) {
          stop("Le modèle doit être ajusté avec $fit() d'abord")
        }
        # Re-run k-means avec nouveau k
        Z <- if (private$FScale) scale(private$FX) else as.matrix(private$FX)
        private$FNbGroupes <- value
        private$FModel <- kmeans(Z, centers = value, nstart = private$FNstart)
        private$FGroupes <- private$FModel$cluster
        message("Nombre de groupes modifié : k = ", value, 
                " (", round(private$FModel$betweenss / private$FModel$totss * 100, 1), "% inertie)")
      }
    }
  )
)