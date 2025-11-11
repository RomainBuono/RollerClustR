# ==========================================
# ClustOfVar : Clustering de Variables
# Approche factorielle pour variables mixtes
# VERSION MODIFIÉE : Résiliente aux données manquantes
# ==========================================

# Nécessite le package R6 pour l'héritage
# Nécessite FactoMineR pour PCA/MCA pour une implémentation complète, 
# ici nous utilisons prcomp pour le numérique.

# Dépend de la classe parente ClusterAnalysis

ClustOfVar <- R6Class("ClustOfVar",
  inherit = ClusterAnalysis,
  
  private = list(
    FMaxIter = 100,
    FTolerance = 1e-6,
    FConverged = FALSE,
    FIterations = 0,
    FVariablesSynthetiques = NULL, # Dataframe des variables synthétiques
    FCorrelations = NULL,         # Corrélation Var/Synthétique de son cluster
    FQualiteClusters = NULL,      # Moyenne des corrélations par cluster (Homogénéité)
    FHomogeneite = NULL,          # Homogénéité totale (somme des corrélations / nb vars)
    FVarNames = NULL,
    FVarTypes = NULL,
    
    # =================================================================
    # Fonctions Utilitaires (Simulées ou Simplifiées)
    # =================================================================
    
    # Variable synthétique pour un cluster purement numérique (basé sur PC1 de PCA)
    pca_synthetique = function(X_cluster) {
      # Gérer les NA en retirant les lignes complètes temporairement
      X_clean <- na.omit(X_cluster)
      if (nrow(X_clean) < 2 || ncol(X_clean) == 0) return(rep(NA, nrow(X_cluster)))
      
      # Centrer/réduire pour PCA. prcomp le fait par défaut.
      # L'utilisation de prcomp est simple, mais pour une vraie robustesse, il faudrait 
      # gérer les lignes complètes pour la projection.
      pca_res <- tryCatch({
        prcomp(X_clean, scale. = TRUE, center = TRUE)
      }, error = function(e) {
        warning("PCA a échoué pour un cluster. Utilisation de la moyenne simple.")
        # Fallback simple
        return(rep(NA, nrow(X_cluster)))
      })
      
      if (inherits(pca_res, "warning") || inherits(pca_res, "error")) {
        return(rep(NA, nrow(X_cluster)))
      }
      
      # Récupérer la première composante principale
      synth_var_clean <- pca_res$x[, 1]
      
      # Reconstruire la variable synthétique avec les NA originaux
      synth_var <- rep(NA, nrow(X_cluster))
      synth_var[as.numeric(rownames(X_clean))] <- synth_var_clean
      
      return(synth_var)
    },
    
    # Variable synthétique pour un cluster purement catégoriel (Placeholder pour MCA)
    mca_synthetique = function(X_cluster) {
      warning("L'approche MCA pour les variables synthétiques catégorielles n'est pas complètement implémentée.")
      # Dans une implémentation réelle, on utiliserait la première composante 
      # principale d'une ACM, comme avec FactoMineR::MCA
      
      # Placeholder: Retourne une variable vide
      return(rep(NA, nrow(X_cluster)))
    },
    
    # Calculer la variable synthétique d'un cluster avec gestion NA
    calculerVariableSynthetique = function(X_cluster) {
      if (ncol(X_cluster) == 0) return(NULL)
      
      # Détecter les types de variables dans le cluster
      types <- sapply(X_cluster, function(v) {
        if (is.numeric(v)) "numeric" else "factor"
      })
      
      n_numeric <- sum(types == "numeric")
      n_factor <- sum(types == "factor")
      
      if (n_numeric == ncol(X_cluster)) {
        # Cluster purement numérique : PCA
        return(private$pca_synthetique(X_cluster))
      } else if (n_factor == ncol(X_cluster)) {
        # Cluster purement catégoriel : MCA (utilisation du placeholder)
        return(private$mca_synthetique(X_cluster))
      } else {
        # Cluster mixte : Factorielle des Données Mixtes (FAMD)
        warning("L'approche FAMD pour les variables synthétiques mixtes n'est pas complètement implémentée. Traitement des variables numériques seulement.")
        # Simplement utiliser les variables numériques pour la PCA pour l'instant
        X_num <- X_cluster[, types == "numeric", drop = FALSE]
        return(private$pca_synthetique(X_num))
      }
    },
    
    # Met à jour les corrélations et l'homogénéité globale
    calculerQualiteClusters = function() {
      if (is.null(private$FVariablesSynthetiques) || is.null(private$FGroupes)) {
        return(NULL)
      }
      
      X_vars <- private$FX
      X_synth <- private$FVariablesSynthetiques
      groupes <- private$FGroupes
      
      vars_corr <- numeric(ncol(X_vars))
      qualite_clusters <- numeric(private$FNbGroupes)
      
      for (k in 1:private$FNbGroupes) {
        idx_vars <- which(groupes == k)
        if (length(idx_vars) > 0) {
          X_cluster <- X_vars[, idx_vars, drop = FALSE]
          V_synth <- X_synth[, k]
          
          # Calculer la corrélation (au carré, comme ClustOfVar) entre chaque var et V_synth
          # On utilise la corrélation entre les variables centrées/réduites (ou factorielles)
          # et V_synth. Ici, on utilise la corrélation standard pour simplifier.
          cor_sq <- sapply(X_cluster, function(v) {
            # Utiliser use="pairwise.complete.obs" pour la robustesse aux NA
            corr_val <- cor(v, V_synth, use = "pairwise.complete.obs")
            # La qualité est mesurée par la corrélation au carré (R²)
            return(corr_val^2)
          })
          
          vars_corr[idx_vars] <- cor_sq
          qualite_clusters[k] <- mean(cor_sq, na.rm = TRUE)
        }
      }
      
      private$FCorrelations <- vars_corr
      private$FQualiteClusters <- qualite_clusters
      private$FHomogeneite <- mean(vars_corr, na.rm = TRUE)
      
      invisible(NULL)
    }
    
  ),
  
  public = list(
    #' @description Initialiser un objet ClustOfVar
    #' @param k Nombre de groupes (défaut: 2)
    #' @param max_iter Nombre max d'itérations (défaut: 100)
    #' @param tolerance Tolérance de convergence (défaut: 1e-6)
    #' @param na_action Action pour les NA : "warn" (défaut), "fail", "omit"
    initialize = function(k = 2, max_iter = 100, tolerance = 1e-6, na_action = "warn") {
      # ClustOfVar n'utilise pas la standardisation 'cr=TRUE' sur les données brutes 
      # (elle est faite en interne pour PCA/MCA), mais on la laisse 'FALSE' par défaut ici.
      super$initialize(k = k, cr = FALSE, na_action = na_action) 
      
      if (!is.numeric(max_iter) || max_iter < 1) stop("max_iter doit être > 0")
      if (!is.numeric(tolerance) || tolerance <= 0) stop("tolerance doit être > 0")
      
      private$FMaxIter <- as.integer(max_iter)
      private$FTolerance <- tolerance
      
      # ClustOfVar est conçu pour des données mixtes, donc on définit le type
      private$FDataType <- "mixed"
    },
    
    #' @description Ajuster le modèle ClustOfVar par algorithme itératif
    #' @param X Data frame de données (colonnes = variables)
    #' @return L'objet ClustOfVar ajusté (invisiblement)
    fit = function(X) {
      super$validateDataset(X)
      private$FVarNames <- names(private$FX)
      
      N <- ncol(private$FX)
      if (N < private$FNbGroupes) {
        stop("Le nombre de variables (", N, ") est inférieur au nombre de clusters (", private$FNbGroupes, ")")
      }
      
      # 1. Initialisation aléatoire des groupes
      # Utiliser une initialisation un peu plus intelligente (e.g. CAH sur les corrélations)
      # Mais pour l'implémentation de base, on utilise une répartition aléatoire
      groupes_actuel <- sample(1:private$FNbGroupes, size = N, replace = TRUE)
      
      # Variables pour l'itération
      homogeneite_actuelle <- -Inf
      
      for (iter in 1:private$FMaxIter) {
        
        # Stocker l'état précédent
        groupes_precedent <- groupes_actuel
        homogeneite_precedente <- homogeneite_actuelle
        
        # =========================================================
        # Étape 1 : Calculer les variables synthétiques (une par cluster)
        # =========================================================
        V_synth_list <- list()
        
        for (k in 1:private$FNbGroupes) {
          idx_vars <- which(groupes_actuel == k)
          if (length(idx_vars) > 0) {
            X_cluster <- private$FX[, idx_vars, drop = FALSE]
            V_synth_list[[k]] <- private$calculerVariableSynthetique(X_cluster)
          } else {
            # Cluster vide: créer un vecteur NA de la bonne taille
            V_synth_list[[k]] <- rep(NA, nrow(private$FX))
          }
        }
        
        V_synth <- do.call(cbind, V_synth_list)
        colnames(V_synth) <- paste0("SynthVar", 1:private$FNbGroupes)
        private$FVariablesSynthetiques <- as.data.frame(V_synth)
        
        # =========================================================
        # Étape 2 : Réallouer les variables au cluster le plus proche
        # =========================================================
        
        groupes_nouvel <- numeric(N)
        X_vars <- private$FX
        
        for (j in 1:N) {
          var_j <- X_vars[, j]
          
          # Calculer la corrélation (au carré) avec chaque variable synthétique
          cor_sq_to_synth <- numeric(private$FNbGroupes)
          
          for (k in 1:private$FNbGroupes) {
            V_synth_k <- V_synth[, k]
            
            # Corrélation au carré (coefficient de détermination)
            corr_val <- cor(var_j, V_synth_k, use = "pairwise.complete.obs")
            cor_sq_to_synth[k] <- corr_val^2
          }
          
          # La variable j est affectée au cluster k qui maximise R²
          groupes_nouvel[j] <- which.max(cor_sq_to_synth)
        }
        
        groupes_actuel <- groupes_nouvel
        private$FGroupes <- groupes_actuel
        private$calculerQualiteClusters()
        homogeneite_actuelle <- private$FHomogeneite
        private$FIterations <- iter
        
        # =========================================================
        # Étape 3 : Vérification de la convergence
        # =========================================================
        if (iter > 1) {
          difference <- abs(homogeneite_actuelle - homogeneite_precedente)
          if (difference < private$FTolerance) {
            private$FConverged <- TRUE
            message("ClustOfVar a convergé après ", iter, " itérations.")
            break
          }
        }
        
        if (iter == private$FMaxIter) {
          warning("ClustOfVar n'a pas convergé après ", private$FMaxIter, " itérations. Tolérance atteinte : ", difference)
        }
      }
      
      private$FFitted <- TRUE
      invisible(self)
    },
    
    #' @description Afficher un résumé des résultats
    summary = function() {
      if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      
      cat("==========================================\n")
      cat("  Résumé du Clustering de Variables (ClustOfVar)\n")
      cat("==========================================\n")
      cat("Algorithme : Factoriel Itératif\n")
      cat("Nombre de Groupes :", private$FNbGroupes, "\n")
      cat("Données traitées :", nrow(private$FX), "lignes,", ncol(private$FX), "variables\n")
      cat("Nombre d'itérations :", private$FIterations, "\n")
      cat("Convergence :", ifelse(private$FConverged, "Oui", "Non"), "\n")
      cat("\n")
      
      # Homogénéité globale
      cat("--- Homogénéité Globale ---\n")
      cat("Homogénéité totale (Moyenne des R² Var/Synth) :", round(private$FHomogeneite, 4), "\n")
      cat("\n")
      
      # Détail par cluster
      cat("--- Homogénéité par Cluster ---\n")
      qualites <- private$FQualiteClusters
      cluster_info <- data.frame(
        Cluster = 1:private$FNbGroupes,
        Taille = as.integer(table(private$FGroupes)),
        Homogeneite_Moyenne = round(qualites, 4)
      )
      print(cluster_info, row.names = FALSE)
      
      # Détail des corrélations (Top/Bottom)
      cat("\n--- Top des Variables les mieux liées (R²) ---\n")
      cor_data <- data.frame(
        Variable = private$FVarNames,
        Cluster = private$FGroupes,
        R_Carre = round(private$FCorrelations, 4)
      )
      cor_data <- cor_data[order(-cor_data$R_Carre), ]
      print(head(cor_data, 10), row.names = FALSE)
      
      invisible(self)
    },
    
    #' @description Visualiser les résultats
    #' @param type Type de graphique : "heatmap" (défaut) ou "barplot"
    #' @return L'objet ClustOfVar (invisiblement)
    plot = function(type = "heatmap") {
      if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      
      if (type == "heatmap") {
        # Créer la matrice de corrélation (R²) des variables par rapport à TOUTES les V_synth
        cor_mat <- matrix(NA, ncol = private$FNbGroupes, nrow = ncol(private$FX),
                          dimnames = list(private$FVarNames, paste0("SynthVar", 1:private$FNbGroupes)))
        
        X_vars <- private$FX
        V_synth <- private$FVariablesSynthetiques
        
        for (j in 1:ncol(X_vars)) {
          var_j <- X_vars[, j]
          for (k in 1:private$FNbGroupes) {
            V_synth_k <- V_synth[, k]
            corr_val <- cor(var_j, V_synth_k, use = "pairwise.complete.obs")
            cor_mat[j, k] <- corr_val^2
          }
        }
        
        # Trier par groupes assignés et par R² décroissant dans le groupe
        ordre <- order(private$FGroupes, -private$FCorrelations)
        cor_mat_ordered <- cor_mat[ordre, ]
        
        # Affichage (utilise l'implémentation de base R)
        image(x = 1:ncol(cor_mat_ordered), y = 1:nrow(cor_mat_ordered),
              z = t(cor_mat_ordered[nrow(cor_mat_ordered):1, ]), # Rotation et inversion de l'axe y
              col = colorRampPalette(c("white", "lightblue", "blue", "darkblue"))(100),
              xlab = "Cluster Synthétique", ylab = "Variables",
              main = "Heatmap : R² Variable/Cluster Synthétique",
              axes = FALSE)
        
        axis(1, at = 1:ncol(cor_mat_ordered), labels = colnames(cor_mat_ordered))
        axis(2, at = 1:nrow(cor_mat_ordered), labels = rownames(cor_mat_ordered)[rev(ordre)], 
             las = 2, cex.axis = 0.7)
        
        legend("topright", legend = c("0", "0.5", "1"),
               fill = c("white", "blue", "darkblue"), title = "R² (Liaison)", cex = 0.8)
        
      } else if (type == "barplot") {
        qualites <- private$FQualiteClusters
        
        barplot(qualites, names.arg = paste("Cluster", 1:private$FNbGroupes),
                col = "steelblue", main = "Qualité des clusters (Homogénéité Moyenne)",
                ylab = "Homogénéité moyenne (R²)", ylim = c(0, 1))
        abline(h = mean(qualites), col = "red", lty = 2, lwd = 2)
        legend("topright", legend = "Moyenne", col = "red", lty = 2, lwd = 2)
      }
      
      invisible(self)
    }
  ),
  
  active = list(
    Groupes = function() {
      if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      # Retourne les groupes de variables (indices des colonnes)
      return(private$FGroupes)
    },
    
    # Redéfinition de l'inertie pour renvoyer l'homogénéité globale
    inertie = function() {
      if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
      return(list(
        homogeneite_globale = private$FHomogeneite,
        qualite_clusters = private$FQualiteClusters
      ))
    }
  )
)

# Charger la classe R6 mère pour l'héritage
# Source: ClusterAnalysis_parentclass.R (snippet provided)
ClusterAnalysis <- R6Class("ClusterAnalysis",
 private = list(
  FX = NULL,       
  FScale = TRUE,     
  FNbGroupes = 2,
  FGroupes = c(),
  FDataType = "numeric", 
  FFitted = FALSE,    
  FNAAction = "warn", 
  FHasMissing = FALSE, 
  FNAIndices = NULL,  
  
  validateDataset = function(X) {
   if (!is.data.frame(X)) {
    if (is.matrix(X)) {
     warning("Conversion de la matrice en data frame")
     X <- as.data.frame(X)
    } else if (is.vector(X)) {
     stop("X ne peut pas être un simple vecteur. Utilisez un data frame avec au moins une colonne.")
    } else {
     stop("X doit être un data frame ou une matrice. Type reçu : ", class(X)[1])
    }
   }
   if (ncol(X) == 0 || nrow(X) == 0) {
    stop("Le data frame X est vide.")
   }
   
   # Détection des NA
   na_indices <- which(!complete.cases(X))
   if (length(na_indices) > 0) {
    private$FHasMissing <- TRUE
    private$FNAIndices <- na_indices
    if (private$FNAAction == "fail") {
     stop("Données manquantes trouvées et 'na_action' est réglé sur 'fail'")
    } else if (private$FNAAction == "warn") {
     warning(length(na_indices), " lignes avec des données manquantes. Les NA seront gérés au cas par cas par l'algorithme.")
    } else if (private$FNAAction == "omit") {
     X <- na.omit(X)
     message("Lignes avec NA omises (", length(na_indices), " lignes retirées).")
    }
   } else {
    private$FHasMissing <- FALSE
    private$FNAIndices <- NULL
   }
   
   private$FX <- X
   
   # Déterminer le type de données du dataset
   num_cols <- sum(sapply(X, is.numeric))
   cat_cols <- sum(sapply(X, is.factor) | sapply(X, is.character))
   
   if (num_cols == ncol(X)) {
    private$FDataType <- "numeric"
   } else if (cat_cols == ncol(X)) {
    private$FDataType <- "categorical"
   } else if (num_cols > 0 && cat_cols > 0) {
    private$FDataType <- "mixed"
   } else {
    stop("X doit contenir des variables numériques ou catégorielles (facteurs/caractères).")
   }
   
   invisible(X)
  },
  
  # Pour v-test (utilisé par Kprototypes pour les variables cat)
  v_test_cat = function(v, groupes, numModa) {
   stop("Non implémenté dans la classe parente")
  }
 ),
 
 public = list(
  #' @description Initialiser un objet ClusterAnalysis
  #' @param k Nombre de groupes
  #' @param cr Standardiser les données ?
  #' @param na_action Action pour les NA : "warn" (défaut), "fail", "omit"
  initialize = function(k = 2, cr = TRUE, na_action = "warn") {
   if (!is.numeric(k) || length(k) != 1 || k < 2) stop("k doit être un entier >= 2")
   if (!is.logical(cr) || length(cr) != 1) stop("cr doit être un booléen")
   if (!na_action %in% c("warn", "fail", "omit")) stop("Action NA invalide")
   
   private$FNbGroupes <- as.integer(k)
   private$FScale <- cr
   private$FNAAction <- na_action
  },
  
  #' @description Afficher un résumé des résultats (implémentation par défaut)
  summary = function() {
   cat("Résumé par défaut. Implémentez $summary() dans la sous-classe.\n")
   invisible(self)
  },
  
  #' @description Ajuster le modèle (implémentation par défaut)
  #' @param X Data frame de données
  fit = function(X) {
   stop("Implémentez $fit(X) dans la sous-classe.")
  },
  
  #' @description Visualiser les résultats (implémentation par défaut)
  plot = function() {
   cat("Visualisation non implémentée pour cette classe.\n")
   invisible(self)
  },
  
  #' @description Accéder aux informations d'inertie (implémentation par défaut)
  inertie = function() {
   warning("La méthode $inertie() n'est pas disponible pour cette classe.")
   return(NULL)
  },
  
  #' @description Obtenir des infos sur les données manquantes
  missing_info = function() {
   if (!private$FFitted) {
    stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
   }
   
   if (!private$FHasMissing) {
    message("Aucune donnée manquante détectée")
    return(NULL)
   }
   
   return(list(
    nb_rows_with_na = length(private$FNAIndices),
    indices_na = private$FNAIndices
   ))
  }
 ),
 
 active = list(
  Groupes = function() {
   if (!private$FFitted) stop("Le modèle n'a pas été ajusté. Utilisez $fit(X) d'abord.")
   return(private$FGroupes)
  },
  NbGroupes = function(value) {
   if (missing(value)) {
    return(private$FNbGroupes)
   } else {
    stop("La propriété NbGroupes ne peut être modifiée après ajustement.")
   }
  },
  DataType = function() { return(private$FDataType) }
 )
)