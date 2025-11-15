# =================================================================
# Fichier : ClusterAnalysis_parentclass.R (VERSION FINALE CORRIGÉE)
# Classe PARENTE : ClusterAnalysis
# =================================================================

# ATTENTION : La dépendance R6 doit être chargée via library(R6) dans le script principal.

ClusterAnalysis <- R6Class("ClusterAnalysis",
  
  # =================================================================
  # 1. Champs Privés
  # =================================================================
  private = list(
    FX = NULL,           # Données originales d'apprentissage
    FScale = TRUE,       
    FNbGroupes = 2,      
    FGroupes = c(),      
    FDataType = "numeric", 
    FFitted = FALSE,     # Le modèle est-il ajusté ?
    FNAAction = "warn",  
    FHasMissing = FALSE, 
    FNAIndices = NULL,   
    FX_clean = NULL,     
    
    # =================================================================
    # 2. Contrats d'Héritage (Doivent être implémentés par l'enfant)
    # Ces méthodes sont PRIVÉES et appelées par les méthodes publiques
    # =================================================================
    
    do_fit = function(X) {
      stop("La méthode 'do_fit(X)' doit être implémentée par la classe enfant.")
    },
    
    do_refit_with_k = function(new_k) {
      stop("La méthode 'do_refit_with_k(new_k)' doit être implémentée par la classe enfant.")
    },
    
    do_predict = function(newdata) {
      stop("La méthode 'do_predict(newdata)' doit être implémentée par la classe enfant.")
    },
    
    do_summary = function() {
      stop("La méthode 'do_summary()' doit être implémentée par la classe enfant.")
    }
  ),
  
  # =================================================================
  # 3. Méthodes Publiques
  # =================================================================
  public = list(
    
    #' @description Constructeur de la classe parente
    initialize = function(...) {
      NULL
    },
    
    #' @description Ajuste le modèle sur les données
    #' @param X Data frame ou matrice de données
    fit = function(X) {
      private$do_fit(X)
      invisible(self)
    },
    
    #' @description Affiche un résumé du modèle
    summary = function() {
      private$do_summary()
      invisible(self)
    },
    
    #' @description Prédit les groupes pour de nouvelles données
    #' @param newdata Nouvelles données
    predict = function(newdata) {
      private$do_predict(newdata)
    }
  ),
  
  # =================================================================
  # 4. Active Bindings (Champs Actifs)
  # =================================================================
  active = list(
    
    #' @field K Nombre de clusters (lecture/écriture). L'écriture appelle do_refit_with_k.
    K = function(value) {
      if (missing(value)) {
        # LECTURE
        return(private$FNbGroupes)
      } else {
        # ÉCRITURE
        if (!is.numeric(value) || value < 2) {
          stop("K doit être un nombre entier >= 2.")
        }
        value <- as.integer(value)
        
        private$do_refit_with_k(value) # Appel du contrat d'héritage
        
        return(private$FNbGroupes)
      }
    },
    
    #' @field Groupes Groupes des observations/variables.
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      # Réaffectation des groupes aux observations initiales si NA ont été omis
      if (private$FNAAction == "omit" && private$FHasMissing) {
        result <- rep(NA_integer_, nrow(private$FX))
        
        # Index des observations SANS NA
        idx_clean <- setdiff(1:nrow(private$FX), private$FNAIndices)
        
        if (length(idx_clean) != length(private$FGroupes)) {
          stop("Erreur interne critique: Incohérence de taille des groupes. Vérifiez la logique NA/fit().") 
        }
        
        result[idx_clean] <- private$FGroupes
        names(result) <- rownames(private$FX)
        return(result)
      } else {
        # Retourne simplement les groupes
        # Les noms sont assignés par la classe enfant si nécessaire
        return(private$FGroupes)
      }
    }
  )
)