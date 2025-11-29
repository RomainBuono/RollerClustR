# Fichier : VAR_CAH.R (Version finale corrigée - FIX DU BUG)
# ==========================================
# VAR_CAH : Clustering de Variables
# Classification Ascendante Hiérarchique (CAH) sur variables
# ==========================================
library(R6)

VAR_CAH <- R6Class("VAR_CAH",
  inherit = ClusterAnalysis,
  
  private = list(
    # =================================================================
    # CHAMPS SPÉCIFIQUES À VAR_CAH (les champs parents sont hérités)
    # =================================================================
    FMaxIter = 100,
    FTolerance = 1e-6,
    FConverged = FALSE,
    FArbre = NULL,                
    FVariablesSynthetiques = NULL, 
    FCorrelations = NULL,         
    FQualiteClusters = NULL,      
    FHomogeneite = NULL,          
    FVarNames = NULL,
    FNumVarNames = NULL,          
    FCatVarNames = NULL,          
    FVarTypes = NULL,
    FPCAParams = NULL,            
    
    # =================================================================
    # Fonctions Utilitaires
    # =================================================================
    
    #' @description Calcule la variable synthétique (PC1) pour un cluster numérique
    pca_synthetique = function(X_cluster) {
      
      row_names_clean <- rownames(na.omit(X_cluster))
      X_clean <- na.omit(X_cluster)
      full_synthetique <- rep(NA_real_, nrow(X_cluster))
      indices_clean <- match(row_names_clean, rownames(X_cluster))

      # Cas trivial : une seule variable
      if (ncol(X_clean) < 2) {
        full_synthetique[indices_clean] <- X_clean[, 1]
        pca_res_simule <- list(
          synthetic = full_synthetique,
          loading = 1.0, center = 0, scale = 1, variance_explained = 1.0 
        )
        return(pca_res_simule)
      } else {
        # Exécution de l'ACP
        pca_res <- prcomp(X_clean, center = TRUE, scale. = TRUE)
        pc1_scores <- pca_res$x[, 1, drop = TRUE]
        full_synthetique[indices_clean] <- pc1_scores
        var_expl <- pca_res$sdev^2 / sum(pca_res$sdev^2)
        
        pca_result_list <- list(
          synthetic = full_synthetique,
          loading = pca_res$rotation[, 1, drop = TRUE],
          center = pca_res$center,
          scale = pca_res$scale,
          variance_explained = var_expl[1]
        )
        return(pca_result_list)
      }
    },
    
    # =================================================================
    # Méthodes principales (PRIVÉES, appelées par les méthodes publiques)
    # =================================================================
    
    #' @description Méthode do_fit : ajuste l'algorithme CAH sur les variables.
    do_fit = function(X) {
      
      val_res <- validate_data_type(X) 
      if (val_res$type != "numeric") {
        stop("VAR_CAH n'accepte que des données de type 'numeric'.")
      }
      
      # AJOUT: Validation k <= nombre de variables
      if (private$FNbGroupes > ncol(X)) {
        stop(paste0(
          "Le nombre de clusters (k=", private$FNbGroupes, 
          ") ne peut pas être supérieur au nombre de variables (", 
          ncol(X), ")"
        ))
      }
      
      private$FX <- X
      private$FVarNames <- colnames(X)
      private$FNumVarNames <- private$FVarNames
      private$FX_clean <- X 
      
      # 3. Calcul de la matrice de Corrélation et de Distance 
      # (dissimilarité sans racine carré, stratégie assumée)
      cor_matrix <- cor(X, use = "pairwise.complete.obs") 
      dist_matrix <- as.dist(1 - abs(cor_matrix)) 
      
      # 4. Exécution de la CAH
      arbre_cah <- hclust(dist_matrix, method = "complete") 
      private$FArbre <- arbre_cah
      
      # 5. Coupure de l'Arbre pour obtenir K clusters
      groupes_variables <- cutree(arbre_cah, k = private$FNbGroupes)
      private$FGroupes <- groupes_variables
      
      # 6. Évaluation des Clusters
      private$evaluate_clusters()
      
      # 7. Finalisation (DOIT ÊTRE TRUE !)
      private$FFitted <- TRUE
      
      invisible(self)
    },
    
    #' @description Méthode interne pour refaire l'ajustement si K est modifié
    do_refit_with_k = function(new_k) {
      if (!private$FFitted) {
        private$FNbGroupes <- new_k
        return(invisible(self))
      }
      
      # Validation k <= nombre de variables
      if (new_k > ncol(private$FX)) {
        stop(paste0(
          "Le nombre de clusters (k=", new_k, 
          ") ne peut pas être supérieur au nombre de variables (", 
          ncol(private$FX), ")"
        ))
      }
      
      private$FNbGroupes <- new_k
      groupes_variables <- cutree(private$FArbre, k = private$FNbGroupes)
      private$FGroupes <- groupes_variables
      private$evaluate_clusters()
      
      message(paste0("Le modèle VAR_CAH a été recalculé pour k = ", new_k, "."))
      
      invisible(self)
    },
    
    #' @description Méthode predict (non implémentée pour clustering de variables)
    do_predict = function(newdata) {
      stop("La méthode predict() n'est pas implémentée pour le clustering de variables.")
    },
    
    #' @description Évaluation des clusters 
    evaluate_clusters = function() {
      if (is.null(private$FGroupes)) stop("Les groupes n'ont pas été définis.")
      
      n_vars <- ncol(private$FX_clean)
      private$FVariablesSynthetiques <- matrix(NA_real_, 
                                               nrow = nrow(private$FX_clean), 
                                               ncol = private$FNbGroupes)
      private$FCorrelations <- rep(NA_real_, n_vars)
      private$FQualiteClusters <- list()
      
      for (k in 1:private$FNbGroupes) {
        vars_in_cluster <- private$FNumVarNames[private$FGroupes == k]
        X_cluster <- private$FX_clean[, vars_in_cluster, drop = FALSE]
        
        pca_result <- private$pca_synthetique(X_cluster)
        private$FVariablesSynthetiques[, k] <- pca_result$synthetic
        var_indices <- match(vars_in_cluster, private$FNumVarNames)
        
        cors_cluster <- sapply(vars_in_cluster, function(var_name) {
          cor(
            private$FX_clean[, var_name], 
            pca_result$synthetic, 
            use = "pairwise.complete.obs"
          )
        })
        
        private$FCorrelations[var_indices] <- abs(cors_cluster)
        
        private$FQualiteClusters[[paste0("C", k)]] <- list(
          homogeneite = mean(abs(cors_cluster), na.rm = TRUE),
          n_vars = length(vars_in_cluster),
          variance_pc1 = pca_result$variance_explained,
          vars = vars_in_cluster
        )
      }
      private$FHomogeneite <- mean(private$FCorrelations, na.rm = TRUE)
    },
    
    #' @description Méthode summary privée
    do_summary = function() {
      if (!private$FFitted) {
         stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      
      cat("\n═══════════════════════════════════════════════════════════\n")
      cat("   VAR_CAH - Résumé du Clustering Hiérarchique de Variables\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      
      cat("Algorithme : Classification Ascendante Hiérarchique (CAH) sur variables\n")
      cat("Méthode de Lien : Complete (sur 1 - |Corrélation|)\n")
      cat("Standardisation des données :", private$FScale, "\n")
      cat("Nombre de clusters (k) :", private$FNbGroupes, "\n")
      cat("Nombre de variables :", ncol(private$FX), "\n\n")
      
      cat("Homogénéité Moyenne Totale (Mean(|Corrélation Var/PC1|)) :", 
          round(private$FHomogeneite, 4), "\n\n")
      
      cat("Détail par Cluster :\n")
      
      summary_df <- data.frame(
        Cluster = integer(),
        Nb_Vars = integer(),
        Homogeneite_Moyenne = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (k in 1:private$FNbGroupes) {
        qc <- private$FQualiteClusters[[paste0("C", k)]]
        summary_df <- rbind(summary_df, data.frame(
          Cluster = k,
          Nb_Vars = qc$n_vars,
          Homogeneite_Moyenne = round(qc$homogeneite, 4)
        ))
      }
      print(summary_df)
    }
  ),
  
  public = list(
    
    #' @description Constructeur de la classe VAR_CAH
    initialize = function(scale = TRUE, k = 3, ...) {
      if (!is.numeric(k) || k < 2) stop("k doit être un nombre entier >= 2.")
      if (!is.logical(scale)) stop("scale doit être un booléen (TRUE/FALSE).")
      
      private$FScale <- scale
      private$FNbGroupes <- as.integer(k)
      private$FDataType <- "numeric"
    },
    
    # =========================================================================
    # Accesseurs publics
    # =========================================================================
    
    #' @description Obtenir les variables appartenant à un cluster donné
    get_cluster_variables = function(cluster_id) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      
      if (cluster_id < 1 || cluster_id > private$FNbGroupes) {
        stop(paste0("cluster_id doit être entre 1 et ", private$FNbGroupes))
      }
      
      vars <- names(private$FGroupes)[private$FGroupes == cluster_id]
      return(vars)
    },
    
    #' @description Obtenir la variable la plus représentative d'un cluster
    get_representative_variable = function(cluster_id) {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      
      vars <- self$get_cluster_variables(cluster_id)
      cors <- private$FCorrelations[match(vars, private$FVarNames)]
      return(vars[which.max(cors)])
    },
    
    #' @description Obtenir l'arbre hiérarchique des variables
    get_tree = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      return(private$FArbre)
    },
    
    # ==============================
    # MÉTHODE: inertie() 
    # ==============================
    
    #' @description Calculer les inerties du clustering
    #' @details Cette méthode calcule l'inertie totale, intra-cluster et inter-cluster
    #' basée sur les corrélations des variables avec leur composante principale.
    #' @return Liste avec les éléments suivants:
    #' \itemize{
    #'   \item totale: Variance totale des corrélations
    #'   \item intra: Inertie intra-cluster (variance moyenne dans les clusters)
    #'   \item inter: Inertie inter-cluster (totale - intra)
    #'   \item pct_expliquee: Pourcentage de variance expliquée par le clustering
    #' }
    inertie = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord.")
      }
      
      # L'homogénéité moyenne est déjà calculée dans private$FCorrelations
      # On utilise la variance des corrélations comme mesure d'inertie
      
      # Inertie totale = variance totale des corrélations
      total_var <- var(private$FCorrelations, na.rm = TRUE)
      
      # Inertie intra = variance moyenne dans les clusters
      intra_vars <- sapply(1:private$FNbGroupes, function(k) {
        vars_in_cluster <- names(private$FGroupes)[private$FGroupes == k]
        cors <- private$FCorrelations[match(vars_in_cluster, private$FVarNames)]
        var(cors, na.rm = TRUE)
      })
      inertie_intra <- mean(intra_vars, na.rm = TRUE)
      
      # Inertie inter = totale - intra
      inertie_inter <- total_var - inertie_intra
      
      # Pourcentage expliqué = inter / totale
      pct_expliquee <- if (total_var > 0) {
        (inertie_inter / total_var) * 100
      } else {
        0
      }
      
      return(list(
        totale = total_var,
        intra = inertie_intra,
        inter = inertie_inter,
        pct_expliquee = pct_expliquee
      ))
    }
  ),
  
  # =========================================================================
  # Active bindings spécifiques (surcharge de Groupes pour variables)
  # =========================================================================
  active = list(
    #' @field Groupes Groupes des variables (surcharge avec noms de variables)
    Groupes = function() {
      if (!private$FFitted) {
        stop("Le modèle doit être ajusté avec $fit() d'abord")
      }
      
      # Pour VAR_CAH, les groupes concernent les VARIABLES (colonnes)
      # donc on utilise colnames, pas rownames
      if (!is.null(private$FVarNames)) {
        names(private$FGroupes) <- private$FVarNames
      }
      return(private$FGroupes)
    }
  )
)