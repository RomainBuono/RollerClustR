# =======================================================
# KmodesVarClust : Clustering de Variables par K-Modes 
# Algorithme de clustering de variables catégorielles/mixtes
# =======================================================

# Utilité pour trouver le mode d'un vecteur (si plusieurs, prend le premier)
find_mode <- function(x) {
  # Fonction pour trouver la modalité la plus fréquente (le mode)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

#' Clustering de Variables par K-Modes
#'
#' @description
#' Implémentation de l'algorithme K-Modes pour le clustering de variables
#' catégorielles et mixtes (catégorielles + numériques). Cet algorithme 
#' regroupe les variables similaires en utilisant une mesure de désaccord
#' pour les données catégorielles.
#' 
#' Pour les variables numériques, elles sont automatiquement discrétisées
#' en intervalles avant l'application de l'algorithme.
#'
#' @details
#' L'algorithme K-Modes pour variables suit ces étapes :
#' \enumerate{
#'   \item Discrétisation des variables numériques (si présentes)
#'   \item Initialisation aléatoire de K prototypes (modes)
#'   \item Affectation de chaque variable au cluster dont le mode est le plus proche
#'   \item Mise à jour des modes (modalités les plus fréquentes)
#'   \item Répétition des étapes 3-4 jusqu'à convergence
#' }
#'
#' La mesure de dissimilarité utilisée est le désaccord simple : 
#' le nombre de positions où deux vecteurs diffèrent.
#'
#' @section Utilisation:
#' \preformatted{
#' # Créer un modèle
#' model <- KmodesVarClust$new(K = 3, max.iter = 100, n_bins = 5)
#' 
#' # Ajuster sur des données
#' data(Titanic)
#' df <- as.data.frame(Titanic)
#' model$fit(df[, 1:4])
#' 
#' # Obtenir les groupes
#' groupes <- model$Groupes
#' 
#' # Résumé
#' model$summary()
#' }
#'
#' @examples
#' \dontrun{
#' # Exemple avec données catégorielles
#' data(Titanic)
#' df <- as.data.frame(Titanic)
#' 
#' model <- KmodesVarClust$new(K = 2)
#' model$fit(df[, 1:4])
#' print(model$Groupes)
#' 
#' # Exemple avec données mixtes
#' data_mixed <- data.frame(
#'   cat1 = sample(c("A", "B", "C"), 100, replace = TRUE),
#'   cat2 = sample(c("X", "Y"), 100, replace = TRUE),
#'   num1 = rnorm(100),
#'   num2 = runif(100)
#' )
#' 
#' model <- KmodesVarClust$new(K = 2, n_bins = 5)
#' model$fit(data_mixed)
#' model$summary()
#' }
#'
#' @seealso 
#' \code{\link{VAR_CAH}} pour clustering de variables numériques par CAH,
#' \code{\link{VARCLUS}} pour clustering de variables avec critère de variance,
#' \code{\link{roller_clust}} pour une interface simplifiée
#'
#' @export
#' @importFrom R6 R6Class
KmodesVarClust <- R6Class("KmodesVarClust",
  inherit = ClusterAnalysis,
  
  public = list(
    #' @description
    #' Créer une nouvelle instance de KmodesVarClust
    #' 
    #' @param K Nombre de clusters de variables (entier >= 2)
    #' @param max.iter Nombre maximum d'itérations (défaut: 100)
    #' @param n_bins Nombre d'intervalles pour discrétisation des variables numériques (défaut: 5)
    #' @param ... Arguments supplémentaires (pour compatibilité)
    #' 
    #' @return Une nouvelle instance de KmodesVarClust
    initialize = function(K = 2, max.iter = 100, n_bins = 5, ...) {
      # Validation des paramètres
      if (!is.numeric(K) || K < 2 || K != as.integer(K)) {
        stop("K doit être un entier >= 2")
      }
      if (!is.numeric(max.iter) || max.iter < 1) {
        stop("max.iter doit être un entier >= 1")
      }
      if (!is.numeric(n_bins) || n_bins < 2) {
        stop("n_bins doit être un entier >= 2")
      }
      
      super$initialize(K = K, ...)
      private$FMaxIter <- max.iter
      private$FNBins <- n_bins
    },
    
    #' @description
    #' Afficher un résumé du modèle ajusté
    #' 
    #' @return NULL (affiche le résumé dans la console)
    summary = function() {
      if (is.null(private$FX)) {
        cat("Modèle KmodesVarClust non ajusté.\n")
        cat("Nombre de clusters: K =", self$K, "\n")
        return(invisible(NULL))
      }
      
      cat("═══════════════════════════════════════════════════════\n")
      cat("  KmodesVarClust - Clustering de Variables par K-Modes\n")
      cat("═══════════════════════════════════════════════════════\n\n")
      
      cat("Nombre de variables:", ncol(private$FX), "\n")
      cat("Nombre d'observations:", nrow(private$FX), "\n")
      cat("Nombre de clusters: K =", self$K, "\n")
      
      if (private$FHasNumericVars) {
        cat("Discrétisation:", private$FNBins, "intervalles\n")
      }
      
      cat("Convergence:", ifelse(private$FConverged, "Oui ✓", "Non ✗"), "\n")
      
      if (!is.null(private$FInertie)) {
        cat("Inertie totale:", round(private$FInertie, 3), "\n")
      }
      
      cat("\nRépartition des variables par cluster:\n")
      groupes <- self$Groupes
      for (k in 1:self$K) {
        vars_k <- names(groupes)[groupes == k]
        cat("  Cluster", k, ":", length(vars_k), "variable(s)\n")
        if (length(vars_k) > 0) {
          cat("    →", paste(vars_k, collapse = ", "), "\n")
        }
      }
      
      cat("\n═══════════════════════════════════════════════════════\n")
      invisible(NULL)
    }
  ),
  
  private = list(
    # =================================================================
    # CHAMPS SPÉCIFIQUES À KmodesVarClust
    # =================================================================
    FModes = NULL,             # Prototypes (modes) de variables : Df (k x N)
    FVarNames = NULL,          # Noms des variables
    FMaxIter = 100,
    FConverged = FALSE,
    FInertie = NULL,
    FNBins = 5,                # Nombre d'intervalles pour la discrétisation
    FHasNumericVars = FALSE,   # Indicateur de présence de variables numériques
    
    # ------------------------------------------------
    # Méthode pour nettoyer les données
    # ------------------------------------------------
    cleanDataset = function(X) {
      # Simple nettoyage : supprimer les lignes avec NA si nécessaire
      # Pour l'instant, on retourne X tel quel
      # (le traitement des NA est fait dans les calculs via na.rm)
      return(X)
    },
    
    # ------------------------------------------------
    # Algorithme K-Modes pour Variables (Logique de Base)
    # ------------------------------------------------
    calc_dissimilarity = function(vec, mode) {
      # La distance de désaccord simple est le nombre de fois où les valeurs ne correspondent pas
      return(sum(vec != mode, na.rm = TRUE))
    },
    
    # ------------------------------------------------
    # IMPLÉMENTATION : do_fit (appelée par la méthode publique fit())
    # ------------------------------------------------
    do_fit = function(X) {
      private$FX <- X
      private$FVarNames <- colnames(X)
      
      # Vérifier la présence de variables numériques
      private$FHasNumericVars <- any(sapply(X, is.numeric))
      
      # SI variables numériques → Discrétisation
      if (private$FHasNumericVars) {
        X <- private$discretize_numeric(X)
      }
      
      # Transposer : lignes = variables
      X_t <- as.data.frame(t(X))
      n_vars <- nrow(X_t)
      n_obs <- ncol(X_t)
      
      # Validation
      if (self$K > n_vars) {
        stop("K ne peut pas être supérieur au nombre de variables")
      }
      
      # Initialisation aléatoire des clusters
      set.seed(123)
      clusters <- sample(1:self$K, n_vars, replace = TRUE)
      
      # Itérations K-Modes
      converged <- FALSE
      iter <- 0
      
      while (!converged && iter < private$FMaxIter) {
        iter <- iter + 1
        old_clusters <- clusters
        
        # Mise à jour des modes
        modes <- matrix(NA, nrow = self$K, ncol = n_obs)
        for (k in 1:self$K) {
          vars_in_k <- which(clusters == k)
          if (length(vars_in_k) > 0) {
            for (j in 1:n_obs) {
              # Mode = modalité la plus fréquente
              vals <- unlist(X_t[vars_in_k, j])
              modes[k, j] <- find_mode(vals)
            }
          }
        }
        
        # Réaffectation
        for (i in 1:n_vars) {
          var_vec <- unlist(X_t[i, ])
          dists <- numeric(self$K)
          for (k in 1:self$K) {
            dists[k] <- private$calc_dissimilarity(var_vec, modes[k, ])
          }
          clusters[i] <- which.min(dists)
        }
        
        # Vérifier convergence
        if (identical(clusters, old_clusters)) {
          converged <- TRUE
        }
      }
      
      # Stocker résultats
      private$FNbGroupes <- self$K
      names(clusters) <- private$FVarNames
      private$FGroupes <- clusters
      private$FModes <- modes
      private$FConverged <- converged
      
      # Calculer inertie
      inertie <- 0
      for (i in 1:n_vars) {
        k <- clusters[i]
        inertie <- inertie + private$calc_dissimilarity(unlist(X_t[i, ]), modes[k, ])
      }
      private$FInertie <- inertie
      
      # ✅ CORRECTION : Marquer le modèle comme ajusté
      private$FFitted <- TRUE
      
      return(invisible(self))
    },
    
    # Discrétise les variables numériques
    discretize_numeric = function(X) {
      X_disc <- X
      for (col in names(X)) {
        if (is.numeric(X[[col]])) {
          # Discrétisation en n_bins intervalles
          X_disc[[col]] <- cut(X[[col]], 
                              breaks = private$FNBins, 
                              labels = paste0("Bin", 1:private$FNBins),
                              include.lowest = TRUE)
        }
      }
      return(X_disc)
    }
  ),
  
  # =================================================================
  # ACTIVE BINDINGS
  # =================================================================
  active = list(
    #' @field converged Indicateur de convergence de l'algorithme
    converged = function() {
      private$FConverged
    },
    
    #' @field inertie Inertie totale (somme des distances intra-cluster)
    inertie = function() {
      private$FInertie
    },
    
    #' @field max.iter Nombre maximum d'itérations
    max.iter = function(value) {
      if (missing(value)) {
        return(private$FMaxIter)
      } else {
        private$FMaxIter <- value
      }
    },
    
    #' @field n_bins Nombre d'intervalles pour la discrétisation
    n_bins = function(value) {
      if (missing(value)) {
        return(private$FNBins)
      } else {
        private$FNBins <- value
      }
    }
  )
)