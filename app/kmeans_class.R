# ==========================================
# K-means : Clustering par centres mobiles
# Variables quantitatives uniquement
# VERSION MODIFIÉE : Résiliente aux données manquantes
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
    #' @param na_action Action pour les NA : "warn" (défaut), "fail", "omit"
    initialize = function(k = 3, cr = TRUE, nstart = 10, na_action = "warn") {
      super$initialize(k = k, cr = cr, na_action = na_action)
      
      # Validation de nstart
      if (!is.numeric(nstart) || length(nstart) != 1 || nstart < 1) {
        stop("Le paramètre 'nstart' doit être un entier positif. Valeur reçue : ", nstart)
      }
      nstart <- as.integer(nstart)
      
      if (nstart > 1000) {
        warning("nstart très élevé (", nstart, "). L'ajustement pourrait être long.")
      }
      private$FNstart <- nstart
    },
    
    #' @description Ajuster le modèle K-means
    #' @param X Data frame de données numériques
    #' @return L'objet Kmeans
    fit = function(X) {
      private$FX <- private$validateDataset(X)
      
      if (private$FDataType != "numeric") {
        stop("Kmeans nécessite des données purement numériques.")
      }
      
      # Nettoyage des données
      X_clean <- private$cleanDataset(private$FX)
      
      # Standardisation (si demandée)
      Z <- if (private$FScale) {
        tryCatch({ 
          scale(X_clean) 
        }, error = function(e) { 
          warning("Échec de la standardisation (toutes les variables constantes ?). Utilisation des données brutes.")
          as.matrix(X_clean) 
        })
      } else {
        as.matrix(X_clean)
      }
      
      # Exécution de K-means
      tryCatch({
        private$FModel <- kmeans(Z, centers = private$FNbGroupes, nstart = private$FNstart)
        private$FGroupes <- private$FModel$cluster
        private$FFitted <- TRUE
        message("Modèle Kmeans ajusté avec succès sur ", length(private$FGroupes), " observations.")
      }, error = function(e) {
        stop(paste0("Erreur lors de l'exécution de kmeans : ", e$message))
      })
      
      return(invisible(self))
    },
    
    #' @description Retourner les informations d'inertie
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
    },
    
    #' @description Retourner les centroïdes des groupes
    #' @return Data frame des centroïdes
    centroides = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      return(as.data.frame(private$FModel$centers))
    }
  ),
  
  active = list(
    #' @field NbGroupes Nombre de groupes, peut être modifié pour re-run K-means.
    NbGroupes = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        if (!private$FFitted) {
          stop("Le modèle doit être ajusté avec $fit() d'abord")
        }
        
        # Validation de la nouvelle valeur de k
        if (!is.numeric(value) || length(value) != 1 || value < 1) {
          stop("Le nombre de groupes (k) doit être un entier positif.")
        }
        value <- as.integer(value)
        
        # Récupération des données originales et nettoyage/standardisation
        X_clean <- private$cleanDataset(private$FX) # Réapplique le nettoyage pour s'assurer de l'état
        
        Z <- if (private$FScale) {
          tryCatch({ 
            scale(X_clean) 
          }, error = function(e) { 
            as.matrix(X_clean) 
          })
        } else {
          as.matrix(X_clean)
        }
        
        # Re-run k-means avec nouveau k
        private$FNbGroupes <- value
        
        tryCatch({
          private$FModel <- kmeans(Z, centers = value, nstart = private$FNstart)
          private$FGroupes <- private$FModel$cluster
          message(paste0("Kmeans ré-ajusté avec succès avec k = ", value))
        }, error = function(e) {
          warning("Erreur K-means lors du re-ajustement: ", e$message)
          # Revert k if re-fitting fails might be better, but we let the user see the current state
        })
        
        return(invisible(value))
      }
    }
  )
)