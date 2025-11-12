# ==================================================
# K-prototypes : Clustering pour données mixtes (R6 Class)
# Implémentation de l'algorithme de Huang (1998)
# ==================================================
library(R6)

#' @title Kprototypes
#' @description Classe R6 pour le clustering K-prototypes sur données mixtes
#' @details Implémente l'algorithme de Huang (1998) qui combine K-means pour les 
#'   variables numériques et K-modes pour les variables catégorielles.
#'   La distance mixte est : d(x,y) = d_num(x,y) + lambda * d_cat(x,y)
#' @references Huang, Z. (1998). Extensions to the k-means algorithm for clustering 
#'   large data sets with categorical values. Data Mining and Knowledge Discovery, 2(3), 283-304.

Kprototypes <- R6Class("Kprototypes",
  inherit = ClusterAnalysis,
  
  private = list(
    FModel = NULL,
    FNumericCols = NULL,           # Vecteur booléen des colonnes numériques
    FCategoricalCols = NULL,       # Vecteur booléen des colonnes catégorielles
    FNumericColNames = NULL,       # Noms des colonnes numériques
    FCategoricalColNames = NULL,   # Noms des colonnes catégorielles
    FLambda = 0.5,                 # Poids de la distance catégorielle
    FMaxIter = 100,
    FNstart = 10,                  # Nombre d'initialisations aléatoires
    FPrototypes = NULL,            # Stocke les prototypes finaux pour $predict
    FInertie = NULL,               # Mesures de qualité
    FConverged = FALSE,            # Indicateur de convergence
    FIterations = 0,               # Nombre d'itérations effectuées
    
    # Paramètres de standardisation stockés après le fit
    FMeans = NULL,
    FSDs = NULL,
    
    # =================================================================
    # Fonction Mode pour Variables Catégorielles
    # =================================================================
    calculate_mode = function(x) {
      # Retourne la modalité la plus fréquente (mode)
      # En cas d'égalité, retourne la première
      if (length(x) == 0) return(NA)
      tab <- table(x, useNA = "no")
      if (length(tab) == 0) return(NA)
      names(tab)[which.max(tab)]
    },
    
    # =================================================================
    # Distance Mixte (Numérique + Catégorielle)
    # =================================================================
    calcDistanceMixte = function(x1, x2, lambda = 0.5) {
      # x1 et x2 : data frames avec 1 ligne
      # Retourne la distance mixte pondérée
      
      # 1. Distance Numérique (Euclidienne au carré)
      dist_num <- 0
      if (length(private$FNumericColNames) > 0) {
        x1_num <- as.numeric(x1[, private$FNumericColNames, drop = FALSE])
        x2_num <- as.numeric(x2[, private$FNumericColNames, drop = FALSE])
        
        # Gérer les NA individuellement
        valid_idx <- !is.na(x1_num) & !is.na(x2_num)
        if (sum(valid_idx) > 0) {
          dist_num <- sum((x1_num[valid_idx] - x2_num[valid_idx])^2)
        }
      }
      
      # 2. Distance Catégorielle (Désaccord simple)
      dist_cat <- 0
      if (length(private$FCategoricalColNames) > 0) {
        x1_cat <- x1[, private$FCategoricalColNames, drop = FALSE]
        x2_cat <- x2[, private$FCategoricalColNames, drop = FALSE]
        
        # Compter les désaccords (1 si différent, 0 si identique)
        for (col in private$FCategoricalColNames) {
          val1 <- as.character(x1_cat[[col]])
          val2 <- as.character(x2_cat[[col]])
          
          # Ignorer les comparaisons avec NA
          if (!is.na(val1) && !is.na(val2)) {
            dist_cat <- dist_cat + as.numeric(val1 != val2)
          }
        }
      }
      
      # Distance mixte pondérée
      return(dist_num + lambda * dist_cat)
    },
    
    # =================================================================
    # Algorithme K-prototypes (Huang 1998)
    # =================================================================
    kprototypes_algorithm = function(X, k, lambda, max_iter = 100) {
      # X : data frame avec variables numériques et catégorielles (déjà nettoyé)
      # Retourne : liste(clusters, prototypes, inertia, converged, iterations)
      
      n <- nrow(X)
      
      # 1. INITIALISATION : Sélection aléatoire de k observations comme prototypes initiaux
      initial_indices <- sample(1:n, k)
      prototypes <- X[initial_indices, , drop = FALSE]
      rownames(prototypes) <- paste0("Proto_", 1:k)
      
      clusters <- rep(NA_integer_, n)
      old_clusters <- rep(NA_integer_, n)
      
      # 2. ITÉRATIONS
      for (iter in 1:max_iter) {
        old_clusters <- clusters
        
        # ÉTAPE A : Affectation des observations au prototype le plus proche
        for (i in 1:n) {
          obs_i <- X[i, , drop = FALSE]
          
          # Calculer la distance à chaque prototype
          distances <- sapply(1:k, function(g) {
            private$calcDistanceMixte(obs_i, prototypes[g, , drop = FALSE], lambda)
          })
          
          # Affecter au cluster du prototype le plus proche
          clusters[i] <- which.min(distances)
        }
        
        # ÉTAPE B : Mise à jour des prototypes
        for (g in 1:k) {
          cluster_obs <- which(clusters == g)
          
          if (length(cluster_obs) == 0) {
            # Cluster vide : réinitialiser avec une observation aléatoire
            warning(paste("Cluster", g, "vide à l'itération", iter, "- réinitialisation"))
            prototypes[g, ] <- X[sample(1:n, 1), ]
          } else {
            # Données du cluster
            X_cluster <- X[cluster_obs, , drop = FALSE]
            
            # Mise à jour du prototype : 
            # - Moyenne pour les variables numériques
            # - Mode pour les variables catégorielles
            new_proto <- data.frame(lapply(X_cluster, function(col) {
              if (is.numeric(col)) {
                mean(col, na.rm = TRUE)
              } else {
                private$calculate_mode(col)
              }
            }), stringsAsFactors = FALSE)
            
            prototypes[g, ] <- new_proto
          }
        }
        
        # ÉTAPE C : Vérification de la convergence
        if (all(clusters == old_clusters)) {
          converged <- TRUE
          break
        } else {
          converged <- FALSE
        }
      }
      
      # 3. CALCUL DE L'INERTIE INTRA-CLUSTER
      inertia_intra <- sum(sapply(1:n, function(i) {
        cluster_i <- clusters[i]
        private$calcDistanceMixte(X[i, , drop = FALSE], 
                                   prototypes[cluster_i, , drop = FALSE], 
                                   lambda)
      }))
      
      # Calcul de l'inertie totale (distance de chaque point au centre global)
      global_proto <- data.frame(lapply(X, function(col) {
        if (is.numeric(col)) mean(col, na.rm = TRUE) else private$calculate_mode(col)
      }), stringsAsFactors = FALSE)
      
      inertia_totale <- sum(sapply(1:n, function(i) {
        private$calcDistanceMixte(X[i, , drop = FALSE], global_proto, lambda)
      }))
      
      inertia_inter <- inertia_totale - inertia_intra
      pct_expliquee <- (inertia_inter / inertia_totale) * 100
      
      return(list(
        clusters = clusters,
        prototypes = prototypes,
        inertia = list(
          totale = inertia_totale,
          intra = inertia_intra,
          inter = inertia_inter,
          pct_expliquee = pct_expliquee
        ),
        converged = converged,
        iterations = iter
      ))
    },
    
    # ------------------------------------------------
    # SURCHARGE : Logique de re-fit pour K<-value
    # ------------------------------------------------
    refit_with_k = function(new_k) {
      if (new_k < 2) {
        stop("K doit être >= 2")
      }
      
      private$FNbGroupes <- new_k
      
      X_clean <- private$cleanDataset(private$FX)
      
      # Ré-identifier les colonnes (au cas où)
      private$FNumericCols <- sapply(X_clean, is.numeric)
      private$FCategoricalCols <- !private$FNumericCols
      private$FNumericColNames <- names(X_clean)[private$FNumericCols]
      private$FCategoricalColNames <- names(X_clean)[private$FCategoricalCols]
      
      # Standardisation si nécessaire
      if (private$FScale) {
        X_num <- X_clean[, private$FNumericCols, drop = FALSE]
        X_clean[, private$FNumericCols] <- sweep(X_num, 2, private$FMeans, "-")
        X_clean[, private$FNumericCols] <- sweep(X_clean[, private$FNumericCols], 2, private$FSDs, "/")
        
        # Gérer les colonnes à variance nulle
        zero_sd_cols <- which(private$FSDs == 0)
        if (length(zero_sd_cols) > 0) {
          X_clean[, private$FNumericColNames[zero_sd_cols]] <- 0
        }
      }
      
      # Lancer l'algorithme K-prototypes avec plusieurs initialisations
      best_result <- NULL
      best_inertia <- Inf
      
      for (attempt in 1:private$FNstart) {
        result <- private$kprototypes_algorithm(X_clean, new_k, private$FLambda, private$FMaxIter)
        
        if (result$inertia$intra < best_inertia) {
          best_inertia <- result$inertia$intra
          best_result <- result
        }
      }
      
      # Stocker les résultats
      private$FGroupes <- best_result$clusters
      private$FPrototypes <- best_result$prototypes
      private$FInertie <- best_result$inertia
      private$FConverged <- best_result$converged
      private$FIterations <- best_result$iterations
      
      message(paste0("K-prototypes ré-ajusté avec succès pour K=", new_k,
                     " (", private$FIterations, " itérations, convergé: ", 
                     private$FConverged, ")"))
    }
  ),
  
  # =================================================================
  # MÉTHODES PUBLIQUES
  # =================================================================
  public = list(
    
    #' @description Initialiser un objet K-prototypes
    #' @param k Nombre de clusters
    #' @param cr Standardiser les variables numériques (défaut: TRUE)
    #' @param lambda Poids de la distance catégorielle (défaut: 0.5)
    #' @param max_iter Nombre maximum d'itérations (défaut: 100)
    #' @param nstart Nombre d'initialisations aléatoires (défaut: 10)
    #' @param na_action Action pour les NA : "warn", "fail", "omit"
    initialize = function(k = 2, cr = TRUE, lambda = 0.5, max_iter = 100, 
                          nstart = 10, na_action = "warn") {
      super$initialize(k = k, cr = cr, na_action = na_action)
      private$FLambda <- lambda
      private$FMaxIter <- as.integer(max_iter)
      private$FNstart <- as.integer(nstart)
      private$FDataType <- "mixed"
      
      # Validation des paramètres
      if (lambda < 0) {
        stop("Lambda doit être >= 0")
      }
      if (max_iter < 1) {
        stop("max_iter doit être >= 1")
      }
      if (nstart < 1) {
        stop("nstart doit être >= 1")
      }
    },
    
    #' @description Ajuster le modèle aux données
    #' @param X Data frame avec variables numériques ET catégorielles
    fit = function(X) {
      X_clean <- super$fit(X) # Nettoyage/Validation du parent
      
      # 1. Identifier les colonnes numériques/catégorielles
      private$FNumericCols <- sapply(X_clean, is.numeric)
      private$FCategoricalCols <- !private$FNumericCols
      private$FNumericColNames <- names(X_clean)[private$FNumericCols]
      private$FCategoricalColNames <- names(X_clean)[private$FCategoricalCols]
      
      # Vérification : doit avoir des variables numériques ET catégorielles
      if (sum(private$FNumericCols) == 0 || sum(private$FCategoricalCols) == 0) {
        stop("K-prototypes nécessite des variables numériques ET catégorielles. ",
             "Variables numériques: ", sum(private$FNumericCols), 
             ", Variables catégorielles: ", sum(private$FCategoricalCols))
      }
      
      # 2. Standardisation des variables numériques
      if (private$FScale) {
        X_num <- X_clean[, private$FNumericCols, drop = FALSE]
        private$FMeans <- colMeans(X_num, na.rm = TRUE)
        private$FSDs <- apply(X_num, 2, sd, na.rm = TRUE)
        
        # Appliquer la standardisation
        X_clean[, private$FNumericCols] <- sweep(X_num, 2, private$FMeans, "-")
        X_clean[, private$FNumericCols] <- sweep(X_clean[, private$FNumericCols], 2, private$FSDs, "/")
        
        # Gérer les colonnes à variance nulle
        zero_sd_cols <- which(private$FSDs == 0)
        if (length(zero_sd_cols) > 0) {
          warning(paste("Colonnes à variance nulle détectées:", 
                        paste(private$FNumericColNames[zero_sd_cols], collapse = ", ")))
          X_clean[, private$FNumericColNames[zero_sd_cols]] <- 0
        }
      }
      
      # 3. Lancer l'algorithme K-prototypes avec plusieurs initialisations
      message(paste("Lancement de K-prototypes avec", private$FNstart, "initialisations..."))
      
      best_result <- NULL
      best_inertia <- Inf
      
      for (attempt in 1:private$FNstart) {
        result <- private$kprototypes_algorithm(X_clean, private$FNbGroupes, 
                                                private$FLambda, private$FMaxIter)
        
        if (result$inertia$intra < best_inertia) {
          best_inertia <- result$inertia$intra
          best_result <- result
        }
      }
      
      # 4. Stocker les résultats de la meilleure exécution
      private$FGroupes <- best_result$clusters
      private$FPrototypes <- best_result$prototypes
      private$FInertie <- best_result$inertia
      private$FConverged <- best_result$converged
      private$FIterations <- best_result$iterations
      
      private$FFitted <- TRUE
      
      message(paste0("K-prototypes ajusté avec succès (", private$FIterations, 
                     " itérations, convergé: ", private$FConverged, 
                     ", inertie expliquée: ", round(private$FInertie$pct_expliquee, 2), "%)"))
      
      invisible(self)
    },
    
    #' @description Prédire les groupes de nouvelles observations
    #' @param newdata Data frame avec les mêmes colonnes que les données d'apprentissage
    #' @param return_distances Retourner aussi les distances aux prototypes (défaut: FALSE)
    #' @return Vecteur d'entiers (clusters) ou liste si return_distances = TRUE
    predict = function(newdata, return_distances = FALSE) {
      # === VALIDATION DES ENTRÉES ===
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      if (is.null(private$FPrototypes)) {
        stop("Les prototypes ne sont pas disponibles pour la prédiction.")
      }
      if (missing(newdata)) {
        stop("L'argument 'newdata' est obligatoire.")
      }
      if (!is.data.frame(newdata)) {
        if (is.matrix(newdata)) {
          newdata <- as.data.frame(newdata)
        } else {
          stop("'newdata' doit être un data frame ou une matrice.")
        }
      }
      if (nrow(newdata) == 0) {
        stop("'newdata' est vide (0 lignes).")
      }
      
      # Vérifier que toutes les colonnes nécessaires sont présentes
      required_cols <- c(private$FNumericColNames, private$FCategoricalColNames)
      missing_cols <- setdiff(required_cols, colnames(newdata))
      if (length(missing_cols) > 0) {
        stop("Colonnes manquantes dans newdata: ", 
             paste(missing_cols, collapse = ", "))
      }
      
      # Vérifier les colonnes supplémentaires (warning seulement)
      extra_cols <- setdiff(colnames(newdata), required_cols)
      if (length(extra_cols) > 0) {
        warning("Colonnes supplémentaires dans newdata (ignorées): ", 
                paste(extra_cols, collapse = ", "))
      }
      
      # Sélectionner uniquement les colonnes nécessaires dans le bon ordre
      newdata <- newdata[, required_cols, drop = FALSE]
      
      # === VÉRIFICATION DES NIVEAUX DE FACTEURS ===
      for (col in private$FCategoricalColNames) {
        if (is.factor(newdata[[col]])) {
          new_levels <- levels(newdata[[col]])
          
          # Extraire les niveaux originaux du prototype
          proto_values <- unique(unlist(lapply(1:private$FNbGroupes, function(g) {
            as.character(private$FPrototypes[g, col])
          })))
          
          unknown_levels <- setdiff(new_levels, proto_values)
          if (length(unknown_levels) > 0) {
            warning(paste0("Modalités inconnues dans '", col, "': ", 
                           paste(unknown_levels, collapse = ", "), 
                           " - Seront traitées comme différentes de tous les prototypes"))
          }
        }
      }
      
      # === STANDARDISATION ===
      Z_new <- newdata
      
      if (private$FScale) {
        for (col_name in private$FNumericColNames) {
          idx <- which(private$FNumericColNames == col_name)
          Z_new[[col_name]] <- (Z_new[[col_name]] - private$FMeans[idx]) / private$FSDs[idx]
          
          # Gérer les colonnes à variance nulle
          if (private$FSDs[idx] == 0) {
            Z_new[[col_name]] <- 0
          }
        }
      }
      
      # === PRÉDICTION ===
      predictions <- rep(NA_integer_, nrow(newdata))
      all_distances <- matrix(NA_real_, nrow = nrow(newdata), ncol = private$FNbGroupes)
      colnames(all_distances) <- paste0("Proto_", 1:private$FNbGroupes)
      
      for (i in 1:nrow(newdata)) {
        obs <- Z_new[i, , drop = FALSE]
        
        # Vérifier si l'observation est complète
        if (all(is.na(obs[, private$FNumericColNames])) && 
            all(is.na(obs[, private$FCategoricalColNames]))) {
          # Observation entièrement manquante
          predictions[i] <- NA_integer_
          next
        }
        
        # Calculer la distance à chaque prototype
        distances <- sapply(1:private$FNbGroupes, function(g) {
          proto <- private$FPrototypes[g, , drop = FALSE]
          private$calcDistanceMixte(obs, proto, private$FLambda)
        })
        
        # Stocker les distances
        all_distances[i, ] <- distances
        
        # Assigner au cluster le plus proche
        if (all(is.na(distances))) {
          predictions[i] <- NA_integer_
        } else {
          predictions[i] <- which.min(distances)
        }
      }
      
      # Noms des prédictions
      names(predictions) <- rownames(newdata)
      
      # === RETOUR ===
      if (return_distances) {
        return(list(
          cluster = predictions,
          distances = all_distances,
          prototype_names = colnames(all_distances)
        ))
      } else {
        return(predictions)
      }
    },
    
    #' @description Résumé du modèle
    summary = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      
      cat("================================================================\n")
      cat("                   K-PROTOTYPES SUMMARY\n")
      cat("================================================================\n")
      cat("Algorithme          : K-prototypes (Huang, 1998)\n")
      cat("Nombre de clusters  :", private$FNbGroupes, "\n")
      cat("Poids lambda        :", private$FLambda, "\n")
      cat("Convergence         :", ifelse(private$FConverged, "Oui", "Non"), "\n")
      cat("Itérations          :", private$FIterations, "/", private$FMaxIter, "\n")
      cat("----------------------------------------------------------------\n")
      cat("Variables numériques      :", length(private$FNumericColNames), "\n")
      cat("  ", paste(private$FNumericColNames, collapse = ", "), "\n")
      cat("Variables catégorielles   :", length(private$FCategoricalColNames), "\n")
      cat("  ", paste(private$FCategoricalColNames, collapse = ", "), "\n")
      cat("Standardisation           :", ifelse(private$FScale, "Oui", "Non"), "\n")
      cat("----------------------------------------------------------------\n")
      cat("Inertie totale            :", round(private$FInertie$totale, 2), "\n")
      cat("Inertie intra-cluster     :", round(private$FInertie$intra, 2), "\n")
      cat("Inertie inter-cluster     :", round(private$FInertie$inter, 2), "\n")
      cat("Variance expliquée        :", round(private$FInertie$pct_expliquee, 2), "%\n")
      cat("----------------------------------------------------------------\n")
      cat("Distribution des clusters :\n")
      print(table(self$Groupes))
      cat("================================================================\n")
      
      invisible(self)
    },
    
    #' @description Obtenir les prototypes (centroïdes numériques + modes catégoriels)
    get_prototypes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      return(private$FPrototypes)
    },
    
    #' @description Obtenir les mesures d'inertie
    inertie = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      return(private$FInertie)
    }
  )
)