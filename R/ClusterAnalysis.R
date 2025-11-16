#' Classe Parente pour l'Analyse de Clustering
#'
#' @description
#' `ClusterAnalysis` est une classe parente R6 qui définit l'architecture de base
#' pour tous les algorithmes de clustering du package RollerClustR. Elle implémente
#' le pattern Template Method pour assurer une interface cohérente entre les
#' différentes méthodes de clustering.
#'
#' @details
#' Cette classe ne doit pas être instanciée directement. Elle sert de base pour
#' les classes filles : [VAR_CAH], [KmodesVarClust], et [VARCLUS].
#'
#' ## Méthodes Publiques
#'
#' - `fit(X)` : Ajuste le modèle sur les données
#' - `summary()` : Affiche un résumé du modèle ajusté
#' - `predict(newdata)` : Prédit les groupes pour de nouvelles données
#'
#' ## Active Bindings
#'
#' - `K` : Nombre de clusters (lecture/écriture)
#' - `Groupes` : Vecteur des affectations aux groupes
#'
#' ## Contrats d'Héritage
#'
#' Les classes filles doivent implémenter les méthodes privées suivantes :
#'
#' - `do_fit(X)` : Logique d'ajustement spécifique
#' - `do_refit_with_k(new_k)` : Ré-ajustement avec un nouveau K
#' - `do_predict(newdata)` : Prédiction pour nouvelles données
#' - `do_summary()` : Affichage du résumé spécifique
#'
#' @examples
#' # Cette classe ne doit pas être utilisée directement
#' # Utilisez plutôt les classes filles :
#' \dontrun{
#' model <- VAR_CAH$new(K = 3)
#' model$fit(iris[, 1:4])
#' model$summary()
#' }
#'
#' @seealso [VAR_CAH], [KmodesVarClust], [VARCLUS], [roller_clust()]
#'
#' @importFrom R6 R6Class
#' @export
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
    
    #' @description Méthode abstraite pour l'ajustement
    #' @param X Data frame ou matrice de données
    do_fit = function(X) {
      stop("La méthode 'do_fit(X)' doit être implémentée par la classe enfant.")
    },
    
    #' @description Méthode abstraite pour le ré-ajustement avec nouveau K
    #' @param new_k Nouveau nombre de clusters
    do_refit_with_k = function(new_k) {
      stop("La méthode 'do_refit_with_k(new_k)' doit être implémentée par la classe enfant.")
    },
    
    #' @description Méthode abstraite pour la prédiction
    #' @param newdata Nouvelles données
    do_predict = function(newdata) {
      stop("La méthode 'do_predict(newdata)' doit être implémentée par la classe enfant.")
    },
    
    #' @description Méthode abstraite pour le résumé
    do_summary = function() {
      stop("La méthode 'do_summary()' doit être implémentée par la classe enfant.")
    }
  ),
  
  # =================================================================
  # 3. Méthodes Publiques
  # =================================================================
  public = list(
    
    #' @description Constructeur de la classe parente
    #' @param ... Arguments passés aux classes filles
    initialize = function(...) {
      NULL
    },
    
    #' @description Ajuste le modèle sur les données
    #' @param X Data frame ou matrice de données d'apprentissage
    #' @return L'objet lui-même (invisiblement) pour permettre le chaînage
    #' @examples
    #' \dontrun{
    #' model <- VAR_CAH$new(K = 3)
    #' model$fit(iris[, 1:4])
    #' }
    fit = function(X) {
      private$do_fit(X)
      invisible(self)
    },
    
    #' @description Affiche un résumé du modèle ajusté
    #' @return L'objet lui-même (invisiblement)
    #' @examples
    #' \dontrun{
    #' model <- VAR_CAH$new(K = 3)
    #' model$fit(iris[, 1:4])
    #' model$summary()
    #' }
    summary = function() {
      private$do_summary()
      invisible(self)
    },
    
    #' @description Prédit les groupes pour de nouvelles données
    #' @param newdata Nouvelles données (même structure que X d'apprentissage)
    #' @return Vecteur des prédictions de groupes
    #' @examples
    #' \dontrun{
    #' model <- VAR_CAH$new(K = 3)
    #' model$fit(iris[1:100, 1:4])
    #' predictions <- model$predict(iris[101:150, 1:4])
    #' }
    predict = function(newdata) {
      private$do_predict(newdata)
    }
  ),
  
  # =================================================================
  # 4. Active Bindings (Champs Actifs)
  # =================================================================
  active = list(
    
    #' @field K Nombre de clusters (lecture/écriture). 
    #'   L'écriture déclenche un ré-ajustement du modèle.
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
    
    #' @field Groupes Vecteur des affectations aux groupes pour chaque
    #'   observation ou variable (selon la méthode de clustering).
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
