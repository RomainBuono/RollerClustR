# ==========================================
# CAH : Classification Ascendante Hiérarchique
# Variables quantitatives uniquement
# ==========================================

CAH <- R6Class("CAH",
  inherit = ClusterAnalysis,
  
  private = list(
    FArbre = NULL,
    FMethod = "ward.D2"
  ),
  
  public = list(
    #' @description Initialiser un objet CAH
    #' @param k Nombre de groupes (défaut: 2)
    #' @param cr Standardiser les données ? (défaut: TRUE)
    #' @param method Méthode de linkage (défaut: "ward.D2")
    initialize = function(k = 2, cr = TRUE, method = "ward.D2") {
      super$initialize(k = k, cr = cr)
      private$FMethod <- method
    },
    
    #' @description Ajuster le modèle CAH sur les données
    #' @param X Data frame avec variables numériques uniquement
    fit = function(X) {
      # Validation
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      
      # Vérifier que toutes les variables sont numériques
      if (!all(sapply(X, is.numeric))) {
        stop("CAH accepte uniquement des variables numériques. Utilisez Kprototypes pour des données mixtes.")
      }
      
      # Stocker les données
      private$FX <- X
      private$FDataType <- private$detectDataType(X)
      
      # Standardisation
      Z <- if (private$FScale) scale(X) else as.matrix(X)
      
      # Calcul de la distance et clustering
      d <- dist(Z)
      private$FArbre <- hclust(d, method = private$FMethod)
      private$FGroupes <- cutree(private$FArbre, k = private$FNbGroupes)
      
      # Marquer comme ajusté
      private$FFitted <- TRUE
      
      message("✓ Modèle CAH ajusté avec succès (", private$FNbGroupes, " groupes)")
      invisible(self)
    },
    
    #' @description Affichage succinct spécifique à CAH
    print = function() {
      super$print()
      if (private$FFitted) {
        cat("Méthode de linkage :", private$FMethod, "\n")
      }
      invisible(self)
    },
    
    #' @description Affichage détaillé spécifique à CAH
    summary = function() {
      super$summary()
      
      if (private$FFitted) {
        cat("\n--- Spécifique CAH ---\n")
        cat("Méthode de linkage :", private$FMethod, "\n")
        cat("Hauteur de fusion :", round(tail(private$FArbre$height, 1), 3), "\n")
      }
      
      invisible(self)
    },
    
    #' @description Visualiser le dendrogramme
    #' @param showGroups Afficher les groupes ? (défaut: FALSE)
    plot = function(showGroups = FALSE) {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      plot(private$FArbre, main = "Dendrogramme CAH", xlab = "", sub = "")
      if (showGroups) {
        rect.hclust(private$FArbre, k = private$FNbGroupes, border = "red")
      }
      invisible(self)
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
        private$FNbGroupes <- value
        private$FGroupes <- cutree(private$FArbre, k = value)
        message("Nombre de groupes modifié : k = ", value)
      }
    }
  )
)