# =======================================================
# KmodesVarClust : Clustering de Variables par K-Modes
# Troisième algorithme de clustering de variables (Réallocation, Qualitatives)
# =======================================================
library(R6)

# Utilité pour trouver le mode d'un vecteur (si plusieurs, prend le premier)
find_mode <- function(x) {
  # Fonction pour trouver la modalité la plus fréquente (le mode)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

KmodesVarClust <- R6Class("KmodesVarClust",
  inherit = ClusterAnalysis,
  
  private = list(
    FModes = NULL,             # Prototypes (modes) de variables : Df (k x N)
    FVarNames = NULL,          # Noms des variables (lignes de X^T)
    FMaxIter = 100,
    
    # ------------------------------------------------
    # Algorithme K-Modes pour Variables (Logique de Base)
    # ------------------------------------------------
    
    #' @description Calcule le désaccord entre un vecteur (variable) et un mode
    #' @param vec Un vecteur d'observations (une ligne de X^T : une variable)
    #' @param mode Le prototype (mode de cluster, une ligne de FModes)
    #' @return Distance de désaccord simple
    calc_dissimilarity = function(vec, mode) {
      # Assurez-vous que les types sont comparables (les deux sont des facteurs/caractères)
      # La distance de désaccord simple est le nombre de fois où les valeurs ne correspondent pas
      return(sum(vec != mode, na.rm = TRUE))
    },
    
    # ------------------------------------------------
    # SURCHARGE : Logique de re-fit pour K<-value
    # ------------------------------------------------
    refit_with_k = function(new_k) {
      private$FNbGroupes <- new_k
      
      X_clean <- private$cleanDataset(private$FX)
      
      # === Étape Cruciale : Transposition pour clustering de variables ===
      # Convertir toutes les colonnes en facteur (K-Modes)
      X_factor <- data.frame(lapply(X_clean, function(v) if (is.numeric(v)) cut(v, breaks = 5, include.lowest = TRUE) else as.factor(v)))
      
      # X_vars : Variables deviennent les observations (lignes)
      # X_vars : Observations deviennent les caractéristiques (colonnes)
      X_vars <- t(X_factor)
      X_vars <- as.data.frame(X_vars) # Convertir en DF pour les opérations par colonne
      
      # 1. Initialisation
      D <- nrow(X_vars) # Nombre de variables à clusteriser
      # Sélectionner K variables comme modes initiaux
      initial_modes_indices <- sample(1:D, new_k)
      current_modes <- X_vars[initial_modes_indices, , drop = FALSE]
      rownames(current_modes) <- paste0("Mode_", 1:new_k)
      
      current_groups <- rep(NA_integer_, D)
      
      for (iter in 1:private$FMaxIter) {
        
        # 2. Étape d'affectation (Assignment Step)
        old_groups <- current_groups
        
        for (i in 1:D) {
          variable_i <- X_vars[i, ]
          distances <- sapply(1:new_k, function(k_idx) {
            # Calculer la distance de la variable i au mode k
            private$calc_dissimilarity(variable_i, current_modes[k_idx, ])
          })
          current_groups[i] <- which.min(distances)
        }
        
        # 3. Étape de mise à jour (Update Step) - NOUVEAU MODE
        new_modes <- current_modes
        for (k_idx in 1:new_k) {
          cluster_vars_indices <- which(current_groups == k_idx)
          
          if (length(cluster_vars_indices) > 0) {
            # Le nouveau mode est la variable qui est la plus représentative du cluster
            # Dans le K-Modes standard, c'est le mode majoritaire pour chaque colonne (Observation)
            X_cluster <- X_vars[cluster_vars_indices, , drop = FALSE]
            
            # Appliquer 'find_mode' à chaque colonne (Observation) du sous-ensemble
            new_mode_vec <- sapply(X_cluster, find_mode)
            new_modes[k_idx, ] <- new_mode_vec
          } else {
            # Si le cluster est vide, réinitialiser le mode (ex: prendre une variable au hasard)
            new_modes[k_idx, ] <- X_vars[sample(1:D, 1), ]
          }
        }
        current_modes <- new_modes
        
        # 4. Vérification de la convergence
        if (all(current_groups == old_groups)) {
          private$FConverged <- TRUE
          break
        }
      }
      
      # Stockage des résultats
      private$FGroupes <- current_groups
      names(private$FGroupes) <- private$FVarNames
      private$FModes <- current_modes
      
      # Calcul d'une inertie de simulation (basée sur le désaccord total intra-cluster)
      total_diss <- sum(sapply(1:D, function(i) {
        k_idx <- private$FGroupes[i]
        private$calc_dissimilarity(X_vars[i, ], private$FModes[k_idx, ])
      }))
      
      private$FInertie <- list(
        totale = NA,
        intra = total_diss,
        inter = NA,
        pct_expliquee = NA,
        converged = private$FConverged,
        iterations = iter
      )
    }
  ),
  
  public = list(
    #' @description Initialiser l'objet KmodesVarClust
    initialize = function(k = 2, max_iter = 100, ...) {
      # na_action="omit" est souvent nécessaire pour la transposition
      super$initialize(k = k, cr = FALSE, na_action = "omit", data_type = "mixed")
      private$FMaxIter <- max_iter
      
      message("KmodesVarClust (Clustering de Variables Qualitatives/Mixtes) initialisé.")
    },
    
    #' @description Ajuster le modèle aux données
    fit = function(X) {
      private$FX <- X
      private$FVarNames <- colnames(X)
      
      # Le K-modes fonctionne mieux sur des données catégorielles, 
      # mais gère les variables mixtes en les convertissant (discrétisation implicite ici).
      
      private$refit_with_k(private$FNbGroupes)
      private$FFitted <- TRUE
      
      message("KmodesVarClust ajusté avec succès.")
      invisible(self)
    },
    
    #' @description Prédire le groupe de nouvelles variables
    predict = function(newdata) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      if (is.null(private$FModes)) stop("Les modes sont manquants pour la prédiction.")
      
      # 1. Préparation des nouvelles variables (transposition et factorisation)
      # Les nouvelles colonnes de 'newdata' sont les variables à assigner.
      X_new_vars <- t(sapply(newdata, as.factor))
      X_new_vars <- as.data.frame(X_new_vars)
      
      # 2. Vérification de la structure (les colonnes doivent correspondre aux observations originales)
      if (ncol(X_new_vars) != ncol(private$FModes)) {
        stop(paste0("Le nombre d'observations dans newdata (", ncol(X_new_vars), 
                    ") ne correspond pas au nombre d'observations du fit original (", 
                    ncol(private$FModes), ")."))
      }
      
      predictions <- rep(NA_integer_, nrow(X_new_vars))
      
      # 3. Calcul de la distance à chaque mode de variable
      for (i in 1:nrow(X_new_vars)) {
        variable_i <- X_new_vars[i, ]
        
        # Calculer la distance de la nouvelle variable 'i' à chaque mode existant
        distances <- sapply(1:private$FNbGroupes, function(k_idx) {
          mode_k <- private$FModes[k_idx, ]
          private$calc_dissimilarity(variable_i, mode_k)
        })
        
        # Le groupe le plus proche (indice de la distance minimale)
        if (all(is.na(distances))) {
             predictions[i] <- NA_integer_
        } else {
             predictions[i] <- which.min(distances)
        }
      }
      
      names(predictions) <- colnames(newdata)
      return(predictions)
    },
    
    #' @description Afficher le résumé du modèle (surcharge)
    summary = function() {
      cat("----------------------------------------------\n")
      cat("R6 KmodesVarClust Summary\n")
      cat("----------------------------------------------\n")
      cat("Algorithme : K-Modes (Réallocation de Variables)\n")
      cat("Groupes (k) :", private$FNbGroupes, "\n")
      cat("Iterations :", private$FInertie$iterations, "\n")
      cat("Convergence :", ifelse(private$FConverged, "Oui", "Non"), "\n")
      cat("Désaccord Intra-cluster Total :", round(private$FInertie$intra, 2), "\n")
      cat("----------------------------------------------\n")
      cat("Distribution des Groupes de Variables :\n")
      print(table(self$Groupes))
      cat("----------------------------------------------\n")
      invisible(self)
    }
  )
)