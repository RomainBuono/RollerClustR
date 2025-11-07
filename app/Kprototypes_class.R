# ==========================================
# K-prototypes : Clustering pour données mixtes
# Gère : numériques seules, catégorielles seules, ou mixtes
# ==========================================

Kprototypes <- R6Class("Kprototypes",
  inherit = ClusterAnalysis,
  
  private = list(
    FModel = NULL,
    FNumericCols = NULL,
    FCategoricalCols = NULL,
    FLambda = 0.5,
    FMaxIter = 100,
    
    # Distance mixte
    calcDistanceMixte = function(x1, x2, lambda = 0.5) {
      n_num <- length(private$FNumericCols)
      n_cat <- length(private$FCategoricalCols)
      
      # CAS 1 : Que des variables numériques → K-means classique
      if (n_num > 0 && n_cat == 0) {
        num_diff <- x1[private$FNumericCols] - x2[private$FNumericCols]
        return(sqrt(sum(num_diff^2)))
      }
      
      # CAS 2 : Que des variables catégorielles → K-modes
      if (n_num == 0 && n_cat > 0) {
        cat_diff <- x1[private$FCategoricalCols] != x2[private$FCategoricalCols]
        return(sum(cat_diff))
      }
      
      # CAS 3 : Variables mixtes → K-prototypes véritable
      if (n_num > 0 && n_cat > 0) {
        # Distance euclidienne (normalisée)
        num_diff <- x1[private$FNumericCols] - x2[private$FNumericCols]
        dist_num <- sqrt(sum(num_diff^2)) / n_num
        
        # Distance de Hamming
        cat_diff <- x1[private$FCategoricalCols] != x2[private$FCategoricalCols]
        dist_cat <- sum(cat_diff) / n_cat
        
        # Pondération par lambda
        return((1 - lambda) * dist_num + lambda * dist_cat)
      }
      
      stop("Aucune variable détectée")
    },
    
    # Calculer le prototype (centre) d'un groupe
    calcPrototype = function(data_groupe) {
      prototype <- rep(NA, ncol(data_groupe))
      
      # Moyenne pour variables numériques
      if (length(private$FNumericCols) > 0) {
        for (j in private$FNumericCols) {
          prototype[j] <- mean(data_groupe[, j], na.rm = TRUE)
        }
      }
      
      # Mode pour variables catégorielles
      if (length(private$FCategoricalCols) > 0) {
        for (j in private$FCategoricalCols) {
          tab <- table(data_groupe[, j])
          prototype[j] <- names(tab)[which.max(tab)]
        }
      }
      
      return(prototype)
    }
  ),
  
  public = list(
    #' @description Initialiser un objet K-prototypes
    #' @param k Nombre de groupes (défaut: 3)
    #' @param cr Standardiser les variables numériques ? (défaut: TRUE)
    #' @param lambda Poids des variables catégorielles (0-1, défaut: 0.5)
    #' @param max_iter Nombre maximum d'itérations (défaut: 100)
    initialize = function(k = 3, cr = TRUE, lambda = 0.5, max_iter = 100) {
      super$initialize(k = k, cr = cr)
      private$FLambda <- lambda
      private$FMaxIter <- max_iter
    },
    
    #' @description Ajuster le modèle K-prototypes sur les données
    #' @param X Data frame avec variables numériques et/ou catégorielles
    fit = function(X) {
      # Validation
      if (!is.data.frame(X)) {
        stop("X doit être un data frame")
      }
      # Gestion des variables en caractère
      char_cols <- which(sapply(X, is.character))
      if (length(char_cols) > 0) {
        stop("Les variables dans X doivent être des facteurs")
      }
      # Identifier les colonnes
      private$FNumericCols <- which(sapply(X, is.numeric))
      private$FCategoricalCols <- which(sapply(X, is.factor))
      
      n_num <- length(private$FNumericCols)
      n_cat <- length(private$FCategoricalCols)
      
      if (n_num == 0 && n_cat == 0) {
        stop("Aucune variable numérique ou catégorielle détectée")
      }
      
      # Stocker les données
      private$FX <- X
      private$FDataType <- private$detectDataType(X)
      
      # Message selon le type
      if (n_num > 0 && n_cat == 0) {
        message("Mode K-means : données uniquement numériques")
      } else if (n_num == 0 && n_cat > 0) {
        message("Mode K-modes : données uniquement catégorielles")
      } else {
        message("Mode K-prototypes : données mixtes (", n_num, " numériques, ", n_cat, " catégorielles)")
      }
      
      # Standardisation des variables numériques
      data_work <- X
      if (private$FScale && n_num > 0) {
        data_work[, private$FNumericCols] <- scale(X[, private$FNumericCols])
      }
      
      # Algorithme K-prototypes
      n <- nrow(data_work)
      
      # Initialisation : k observations aléatoires comme prototypes
      init_indices <- sample(1:n, private$FNbGroupes)
      private$FGroupes <- rep(NA, n)
      
      # Assigner chaque obs au prototype initial le plus proche
      for (i in 1:n) {
        distances <- sapply(init_indices, function(idx) {
          private$calcDistanceMixte(data_work[i, ], data_work[idx, ], private$FLambda)
        })
        private$FGroupes[i] <- which.min(distances)
      }
      
      # Itérations
      converged <- FALSE
      for (iter in 1:private$FMaxIter) {
        groupes_old <- private$FGroupes
        
        # Étape 1 : Calculer les prototypes
        prototypes <- lapply(1:private$FNbGroupes, function(g) {
          indices_g <- which(private$FGroupes == g)
          if (length(indices_g) > 0) {
            private$calcPrototype(data_work[indices_g, , drop = FALSE])
          } else {
            data_work[sample(1:n, 1), ]  # Groupe vide : observation aléatoire
          }
        })
        
        # Étape 2 : Réassigner
        for (i in 1:n) {
          distances <- sapply(1:private$FNbGroupes, function(g) {
            private$calcDistanceMixte(data_work[i, ], prototypes[[g]], private$FLambda)
          })
          private$FGroupes[i] <- which.min(distances)
        }
        
        # Convergence ?
        if (all(private$FGroupes == groupes_old)) {
          message("✓ Convergence atteinte après ", iter, " itérations")
          converged <- TRUE
          break
        }
      }
      
      if (!converged) {
        warning("⚠ Nombre maximum d'itérations atteint sans convergence")
      }
      
      # Stocker les résultats
      private$FModel <- list(
        prototypes = prototypes,
        lambda = private$FLambda,
        iter = if(converged) iter else private$FMaxIter,
        converged = converged
      )
      
      # Marquer comme ajusté
      private$FFitted <- TRUE
      
      message("✓ Modèle K-prototypes ajusté avec succès (", private$FNbGroupes, " groupes)")
      invisible(self)
    },
    
    #' @description Affichage succinct spécifique à K-prototypes
    print = function() {
      super$print()
      if (private$FFitted) {
        n_num <- length(private$FNumericCols)
        n_cat <- length(private$FCategoricalCols)
        cat("Variables numériques :", n_num, "\n")
        cat("Variables catégorielles :", n_cat, "\n")
        if (n_num > 0 && n_cat > 0) {
          cat("Lambda (poids catég.) :", private$FLambda, "\n")
        }
      }
      invisible(self)
    },
    
    #' @description Affichage détaillé spécifique à K-prototypes
    summary = function() {
      super$summary()
      
      if (private$FFitted) {
        cat("\n--- Spécifique K-prototypes ---\n")
        cat("Variables numériques :", length(private$FNumericCols), "\n")
        if (length(private$FNumericCols) > 0) {
          cat("  →", paste(names(private$FX)[private$FNumericCols], collapse = ", "), "\n")
        }
        cat("Variables catégorielles :", length(private$FCategoricalCols), "\n")
        if (length(private$FCategoricalCols) > 0) {
          cat("  →", paste(names(private$FX)[private$FCategoricalCols], collapse = ", "), "\n")
        }
        
        if (length(private$FNumericCols) > 0 && length(private$FCategoricalCols) > 0) {
          cat("Lambda (poids variables catégorielles) :", private$FLambda, "\n")
        }
        
        cat("Nombre d'itérations :", private$FModel$iter, "\n")
        cat("Convergence :", ifelse(private$FModel$converged, "Oui", "Non"), "\n")
      }
      
      invisible(self)
    },
    
    #' @description Visualiser le clustering
    plot = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      n_num <- length(private$FNumericCols)
      n_cat <- length(private$FCategoricalCols)
      
      # CAS 1 : Que des variables numériques
      if (n_num >= 2 && n_cat == 0) {
        plot(private$FX[, private$FNumericCols[1:2]], 
             col = private$FGroupes, pch = 19,
             main = "K-prototypes (mode K-means)",
             xlab = names(private$FX)[private$FNumericCols[1]],
             ylab = names(private$FX)[private$FNumericCols[2]])
        legend("topright", legend = paste("Groupe", 1:private$FNbGroupes),
               col = 1:private$FNbGroupes, pch = 19)
      }
      
      # CAS 2 : Que des variables catégorielles
      else if (n_num == 0 && n_cat >= 2) {
        par(mfrow = c(1, 2))
        
        tab1 <- table(private$FGroupes, private$FX[, private$FCategoricalCols[1]])
        barplot(tab1, beside = TRUE, legend = TRUE,
                main = "Répartition par groupe",
                xlab = names(private$FX)[private$FCategoricalCols[1]],
                ylab = "Effectif",
                col = 1:private$FNbGroupes)
        
        tab2 <- table(private$FGroupes, private$FX[, private$FCategoricalCols[2]])
        barplot(tab2, beside = TRUE, legend = TRUE,
                main = "Répartition par groupe",
                xlab = names(private$FX)[private$FCategoricalCols[2]],
                ylab = "Effectif",
                col = 1:private$FNbGroupes)
        
        par(mfrow = c(1, 1))
      }
      
      # CAS 3 : Variables mixtes
      else if (n_num > 0 && n_cat > 0) {
        par(mfrow = c(1, 2))
        
        num_col <- private$FNumericCols[1]
        boxplot(private$FX[, num_col] ~ private$FGroupes,
                main = "Distribution numérique",
                xlab = "Groupe", ylab = names(private$FX)[num_col],
                col = 1:private$FNbGroupes)
        
        cat_col <- private$FCategoricalCols[1]
        tab <- table(private$FGroupes, private$FX[, cat_col])
        barplot(tab, beside = TRUE, legend = TRUE,
                main = "Répartition catégorielle",
                xlab = names(private$FX)[cat_col],
                ylab = "Effectif",
                col = 1:private$FNbGroupes)
        
        par(mfrow = c(1, 1))
      }
      
      else {
        cat("Visualisation non disponible pour ce type de données\n")
      }
      
      invisible(self)
    },
    
    #' @description Afficher les prototypes des groupes
    prototypes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      cat("=== Prototypes des", private$FNbGroupes, "groupes ===\n\n")
      
      for (g in 1:private$FNbGroupes) {
        cat("Groupe", g, "(n =", sum(private$FGroupes == g), ") :\n")
        proto <- private$FModel$prototypes[[g]]
        
        # Variables numériques
        if (length(private$FNumericCols) > 0) {
          cat("  Variables numériques :\n")
          for (j in private$FNumericCols) {
            cat("   ", names(private$FX)[j], ":", round(as.numeric(proto[j]), 3), "\n")
          }
        }
        
        # Variables catégorielles
        if (length(private$FCategoricalCols) > 0) {
          cat("  Variables catégorielles :\n")
          for (j in private$FCategoricalCols) {
            cat("   ", names(private$FX)[j], ":", proto[j], "\n")
          }
        }
        cat("\n")
      }
      
      invisible(self)
    }
  ),
  
  active = list(
    NbGroupes = function(value) {
      if (missing(value)) {
        return(private$FNbGroupes)
      } else {
        message("Pour changer le nombre de groupes avec K-prototypes, recréez un nouvel objet avec k différent et réajustez avec $fit()")
        return(private$FNbGroupes)
      }
    }
  )
)