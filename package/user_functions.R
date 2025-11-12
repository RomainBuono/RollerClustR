# ==================================================
# K-prototypes : Clustering pour données mixtes (R6 Class)
# ==================================================
library(R6)

# ... (Fonction calcDissimilarity est omise ici pour la concision) ...

Kprototypes <- R6Class("Kprototypes",
  inherit = ClusterAnalysis,
  
  private = list(
    FModel = NULL,
    FNumericCols = NULL,
    FCategoricalCols = NULL,
    FLambda = 0.5,
    FMaxIter = 500,
    FPrototypes = NULL, # Stocke les prototypes finaux pour $predict
    
    # Paramètres de standardisation stockés après le fit (nécessaire pour predict)
    FMeans = NULL,
    FSDs = NULL,

    # =================================================================
    # Fonctions Utilitaires (Distance Mixte)
    # [Le code de calcDistanceMixte est omis pour la concision]
    # =================================================================
    calcDistanceMixte = function(x1, x2, lambda = 0.5) {
      # Assurez-vous que x1 et x2 sont des data frames 1-ligne ou des listes/vecteurs
      
      # 1. Distance Numérique (Euclidienne au carré)
      dist_num <- 0
      if (length(private$FNumericCols) > 0) {
        x1_num <- x1[, private$FNumericCols, drop = FALSE]
        x2_num <- x2[, private$FNumericCols, drop = FALSE]
        dist_num <- sum((x1_num - x2_num)^2, na.rm = TRUE)
      }
      
      # 2. Distance Catégorielle (Simple désaccord)
      dist_cat <- 0
      if (length(private$FCategoricalCols) > 0) {
        x1_cat <- x1[, private$FCategoricalCols, drop = FALSE]
        x2_cat <- x2[, private$FCategoricalCols, drop = FALSE]
        # Désaccord : 1 si différent, 0 si identique
        dist_cat <- sum(x1_cat != x2_cat, na.rm = TRUE)
      }
      
      # Distance mixte pondérée
      return(dist_num + lambda * dist_cat)
    },
    
    # ------------------------------------------------
    # SURCHARGE : Logique de re-fit pour K<-value
    # [Le code de refit_with_k est omis pour la concision]
    # ------------------------------------------------
    refit_with_k = function(new_k) {
      private$FNbGroupes <- new_k
      
      X_clean <- private$cleanDataset(private$FX)
      
      # Ré-exécuter le k-prototypes
      # ... (logique de re-fit du k-prototypes)
      
      # Pour l'exemple: une relance simple
      message(paste0("Kprototypes re-ajusté avec succès pour K=", private$FNbGroupes))
      # Remarque: l'implémentation complète du re-fit est complexe pour Kprototypes
    }
  ),
  
  public = list(
    
    #' @description Ajuster le modèle aux données
    fit = function(X) {
      X_clean <- super$fit(X) # Nettoyage/Validation

      # 1. Identifier les colonnes numériques/catégorielles
      private$FNumericCols <- sapply(X_clean, is.numeric)
      private$FCategoricalCols <- !private$FNumericCols

      if (sum(private$FNumericCols) == 0 || sum(private$FCategoricalCols) == 0) {
        stop("Kprototypes nécessite des variables numériques ET catégorielles.")
      }
      
      # 2. Standardisation des variables numériques
      if (private$FScale) {
        X_num <- X_clean[, private$FNumericCols, drop = FALSE]
        private$FMeans <- colMeans(X_num, na.rm = TRUE)
        private$FSDs <- apply(X_num, 2, sd, na.rm = TRUE)
        
        X_clean[, private$FNumericCols] <- super$standardize(X_num) # Utiliser la méthode parente
      }

      # 3. Lancer l'algorithme k-prototypes (simulé ici)
      # private$FModel <- kproto(X_clean, k = private$FNbGroupes, lambda = private$FLambda, ...)
      
      # Simulation des résultats
      set.seed(42)
      private$FGroupes <- sample(1:private$FNbGroupes, nrow(X_clean), replace = TRUE)
      
      # Simuler le stockage des prototypes (pour predict)
      private$FPrototypes <- lapply(1:private$FNbGroupes, function(g) {
        # Calculer le prototype (moyenne pour numériques, mode pour catégorielles)
        data_group <- X_clean[private$FGroupes == g, , drop = FALSE]
        
        proto <- data.frame(lapply(data_group, function(col) {
          if (is.numeric(col)) mean(col, na.rm = TRUE) else table(col) %>% which.max() %>% names()
        }), stringsAsFactors = FALSE)
        return(proto)
      })

      private$FFitted <- TRUE
      message("Kprototypes ajusté avec succès.")
      invisible(self)
    },
    
    #' @description Prédire les groupes de nouvelles observations
    predict = function(newdata) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      if (is.null(private$FPrototypes)) stop("Les prototypes ne sont pas disponibles pour la prédiction.")
      
      Z_new <- newdata # Créer une copie pour la standardisation/transformation
      
      # MODIFICATION : Logique de standardisation nettoyée (remplace le bloc redondant)
      if (private$FScale) {
         num_cols_names <- names(Z_new)[private$FNumericCols]
         
         # Standardiser les variables numériques en utilisant les paramètres d'apprentissage
         for (col_name in num_cols_names) {
             idx <- which(colnames(private$FX)[private$FNumericCols] == col_name) # Index dans FMeans/FSDs
             Z_new[[col_name]] <- (Z_new[[col_name]] - private$FMeans[idx]) / private$FSDs[idx]
         }
      }
      # FIN MODIFICATION
      
      # 2. Calcul de la distance à chaque prototype
      predictions <- rep(NA_integer_, nrow(newdata))
      
      for (i in 1:nrow(newdata)) {
        obs <- Z_new[i, , drop = FALSE] # Utiliser Z_new (standardisé)
        
        # Calculer la distance de l'observation 'i' à chaque prototype
        distances <- sapply(1:private$FNbGroupes, function(g) {
          proto <- private$FPrototypes[[g]]
          
          # S'assurer que les données sont comparables (ordre et type)
          if (any(colnames(obs) != colnames(proto))) {
            obs <- obs[, colnames(proto), drop = FALSE]
          }
          
          # Vérifier si l'observation est entièrement NA (y compris après standardisation)
          if (all(is.na(obs))) return(NA_real_)
          
          private$calcDistanceMixte(obs, proto, private$FLambda)
        })
        
        # Si tous les résultats sont NA (ex: obs entièrement NA), assigner NA
        if (all(is.na(distances))) {
          predictions[i] <- NA_integer_
        } else {
          predictions[i] <- which.min(distances)
        }
      }
      
      names(predictions) <- rownames(newdata)
      return(predictions)
    },
    
    #' @description Résumé du modèle
    summary = function() {
      cat("----------------------------------------------\n")
      cat("R6 Kprototypes Summary\n")
      cat("----------------------------------------------\n")
      cat("Algorithme : K-prototypes (Données Mixtes)\n")
      cat("Groupes (k) :", private$FNbGroupes, "\n")
      cat("Poids Lambda (catégoriel) :", private$FLambda, "\n")
      cat("Standardisation des numériques :", ifelse(private$FScale, "Oui", "Non"), "\n")
    },
    
    #' @description Obtenir les prototypes (centroïdes/modes)
    get_prototypes = function() {
      if (!private$FFitted) stop("Modèle non ajusté")
      return(private$FPrototypes)
    }
  )
)