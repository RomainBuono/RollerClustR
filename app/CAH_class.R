# ==========================================
# CAH + K-means : Classification Hybride
# Variables quantitatives uniquement
# ==========================================

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
    FUseKmeans = TRUE  # Activer/désactiver le raffinement K-means
  ),
  
  public = list(
    #' @description Initialiser un objet CAH + K-means hybride
    #' @param k Nombre de groupes (défaut: 2)
    #' @param cr Standardiser les données ? (défaut: TRUE)
    #' @param method Méthode de linkage CAH (défaut: "ward.D2")
    #' @param use_kmeans Utiliser le raffinement K-means ? (défaut: TRUE)
    #' @param nstart Nombre d'initialisations K-means (défaut: 10)
    #' @param max_iter Nombre max d'itérations K-means (défaut: 100)
    initialize = function(k = 2, cr = TRUE, method = "ward.D2", 
                         use_kmeans = TRUE, nstart = 10, max_iter = 100) {
      super$initialize(k = k, cr = cr)
      private$FMethod <- method
      private$FUseKmeans <- use_kmeans
      private$FNstart <- nstart
      private$FMaxIter <- max_iter
    },
    
    #' @description Ajuster le modèle CAH + K-means sur les données
    #' @param X Data frame avec variables numériques uniquement
    fit = function(X) {
      # Validation
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      
      # Vérifier que toutes les variables sont numériques
      if (!all(sapply(X, is.numeric))) {
        stop("CAH_Kmeans accepte uniquement des variables numériques.")
      }
      
      # Stocker les données
      private$FX <- X
      private$FDataType <- private$detectDataType(X)
      
      # Standardisation
      Z <- if (private$FScale) scale(X) else as.matrix(X)
      
      # ÉTAPE 1 : CAH pour initialisation
      message("Étape 1/2 : Classification Ascendante Hiérarchique...")
      d <- dist(Z)
      private$FArbre <- hclust(d, method = private$FMethod)
      groupes_cah <- cutree(private$FArbre, k = private$FNbGroupes)
      
      if (!private$FUseKmeans) {
        # Si pas de raffinement K-means, utiliser directement CAH
        private$FGroupes <- groupes_cah
        private$FFitted <- TRUE
        message("✓ Modèle CAH ajusté (", private$FNbGroupes, " groupes)")
        return(invisible(self))
      }
      
      # ÉTAPE 2 : K-means avec centres initialisés par CAH
      message("Étape 2/2 : Raffinement par K-means...")
      
      # Calculer les centres initiaux à partir des groupes CAH
      centres_init <- matrix(0, nrow = private$FNbGroupes, ncol = ncol(Z))
      for (k in 1:private$FNbGroupes) {
        if (sum(groupes_cah == k) > 0) {
          centres_init[k, ] <- colMeans(Z[groupes_cah == k, , drop = FALSE])
        }
      }
      
      # K-means avec centres initialisés
      tryCatch({
        private$FKmeansResult <- kmeans(Z, 
                                       centers = centres_init,
                                       iter.max = private$FMaxIter,
                                       nstart = 1,  # 1 seul car on a déjà de bons centres
                                       algorithm = "Hartigan-Wong")
        
        private$FGroupes <- private$FKmeansResult$cluster
        private$FCentroides <- private$FKmeansResult$centers
        
        # Calculer l'inertie
        private$FInertie <- list(
          totale = private$FKmeansResult$totss,
          intra = private$FKmeansResult$tot.withinss,
          inter = private$FKmeansResult$betweenss,
          pct_expliquee = (private$FKmeansResult$betweenss / private$FKmeansResult$totss) * 100
        )
        
      }, error = function(e) {
        warning("Erreur K-means, utilisation des groupes CAH: ", e$message)
        private$FGroupes <- groupes_cah
      })
      
      # Marquer comme ajusté
      private$FFitted <- TRUE
      
      message("✓ Modèle CAH+K-means ajusté avec succès (", private$FNbGroupes, " groupes)")
      if (!is.null(private$FInertie)) {
        message("  Inertie expliquée : ", round(private$FInertie$pct_expliquee, 2), "%")
      }
      
      invisible(self)
    },
    
    #' @description Affichage succinct spécifique à CAH+K-means
    print = function() {
      super$print()
      if (private$FFitted) {
        cat("Méthode de linkage CAH :", private$FMethod, "\n")
        cat("Raffinement K-means :", ifelse(private$FUseKmeans, "Oui", "Non"), "\n")
        if (!is.null(private$FInertie)) {
          cat("Inertie expliquée :", round(private$FInertie$pct_expliquee, 2), "%\n")
        }
      }
      invisible(self)
    },
    
    #' @description Affichage détaillé spécifique à CAH+K-means
    summary = function() {
      super$summary()
      
      if (private$FFitted) {
        cat("\n--- Spécifique CAH + K-means ---\n")
        cat("Méthode de linkage CAH :", private$FMethod, "\n")
        cat("Hauteur de fusion CAH :", round(tail(private$FArbre$height, 1), 3), "\n")
        cat("Raffinement K-means :", ifelse(private$FUseKmeans, "Oui", "Non"), "\n")
        
        if (!is.null(private$FInertie)) {
          cat("\n--- Inertie (K-means) ---\n")
          cat("Inertie totale :", round(private$FInertie$totale, 3), "\n")
          cat("Inertie inter-classe :", round(private$FInertie$inter, 3), "\n")
          cat("Inertie intra-classe :", round(private$FInertie$intra, 3), "\n")
          cat("% expliqué :", round(private$FInertie$pct_expliquee, 2), "%\n")
        }
      }
      
      invisible(self)
    },
    
    #' @description Visualiser le dendrogramme CAH
    #' @param showGroups Afficher les groupes ? (défaut: FALSE)
    plot = function(showGroups = FALSE) {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      plot(private$FArbre, 
           main = "Dendrogramme CAH (avant raffinement K-means)", 
           xlab = "", sub = "")
      if (showGroups) {
        rect.hclust(private$FArbre, k = private$FNbGroupes, border = "red")
      }
      invisible(self)
    },
    
    #' @description Obtenir les informations d'inertie
    #' @return Liste avec les inerties
    inertie = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      if (is.null(private$FInertie)) {
        stop("Inertie non disponible (K-means n'a pas été appliqué)")
      }
      
      return(private$FInertie)
    },
    
    #' @description Obtenir les centroïdes des groupes
    #' @return Matrice des centroïdes
    centroides = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      if (is.null(private$FCentroides)) {
        # Calculer les centroïdes si pas disponibles
        Z <- if (private$FScale) scale(private$FX) else as.matrix(private$FX)
        centres <- matrix(0, nrow = private$FNbGroupes, ncol = ncol(Z))
        for (k in 1:private$FNbGroupes) {
          centres[k, ] <- colMeans(Z[private$FGroupes == k, , drop = FALSE])
        }
        colnames(centres) <- colnames(private$FX)
        return(centres)
      }
      
      centres <- private$FCentroides
      colnames(centres) <- colnames(private$FX)
      return(centres)
    },
    
    #' @description Visualiser les groupes dans un espace 2D
    #' @param var_x Index ou nom de la variable X
    #' @param var_y Index ou nom de la variable Y
    plot_groups = function(var_x = 1, var_y = 2) {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      if (is.character(var_x)) var_x <- which(names(private$FX) == var_x)
      if (is.character(var_y)) var_y <- which(names(private$FX) == var_y)
      
      plot(private$FX[, var_x], private$FX[, var_y],
           col = private$FGroupes, pch = 19,
           xlab = names(private$FX)[var_x],
           ylab = names(private$FX)[var_y],
           main = "Groupes CAH+K-means")
      
      # Ajouter les centroïdes si disponibles
      if (!is.null(private$FCentroides)) {
        centres_originaux <- private$FCentroides
        if (private$FScale) {
          # Dé-standardiser les centres
          centres_originaux <- sweep(private$FCentroides, 2, 
                                    attr(scale(private$FX), "scaled:scale"), "*")
          centres_originaux <- sweep(centres_originaux, 2, 
                                    attr(scale(private$FX), "scaled:center"), "+")
        }
        points(centres_originaux[, var_x], centres_originaux[, var_y],
               col = 1:private$FNbGroupes, pch = 8, cex = 2, lwd = 2)
      }
      
      legend("topright", legend = paste("Groupe", 1:private$FNbGroupes),
             col = 1:private$FNbGroupes, pch = 19)
      
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
        
        # Recalculer avec CAH
        groupes_cah <- cutree(private$FArbre, k = value)
        
        if (private$FUseKmeans && !is.null(private$FX)) {
          # Refaire K-means avec nouveau k
          Z <- if (private$FScale) scale(private$FX) else as.matrix(private$FX)
          centres_init <- matrix(0, nrow = value, ncol = ncol(Z))
          for (k in 1:value) {
            if (sum(groupes_cah == k) > 0) {
              centres_init[k, ] <- colMeans(Z[groupes_cah == k, , drop = FALSE])
            }
          }
          
          private$FKmeansResult <- kmeans(Z, centers = centres_init,
                                          iter.max = private$FMaxIter,
                                          nstart = 1)
          private$FGroupes <- private$FKmeansResult$cluster
          private$FCentroides <- private$FKmeansResult$centers
          
          private$FInertie <- list(
            totale = private$FKmeansResult$totss,
            intra = private$FKmeansResult$tot.withinss,
            inter = private$FKmeansResult$betweenss,
            pct_expliquee = (private$FKmeansResult$betweenss / private$FKmeansResult$totss) * 100
          )
        } else {
          private$FGroupes <- groupes_cah
        }
        
        message("Nombre de groupes modifié : k = ", value)
      }
    }
  )
)