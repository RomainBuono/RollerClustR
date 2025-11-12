# ==========================================
# ClustOfVar : Clustering de Variables
# Approche basée sur FactoMineR/HCPC
# ==========================================
library(R6)

ClustOfVar <- R6Class("ClustOfVar",
  inherit = ClusterAnalysis,
  
  private = list(
    FMaxIter = 100,
    FTolerance = 1e-6,
    FConverged = FALSE,
    FArbre = NULL,                # Arbre CAH des variables
    FVariablesSynthetiques = NULL, # Variables synthétiques par cluster
    FCorrelations = NULL,         # Corrélation Var/Synthétique
    FQualiteClusters = NULL,      # Moyenne des corrélations par cluster (Homogénéité)
    FHomogeneite = NULL,          # Homogénéité totale
    FVarNames = NULL,
    FVarTypes = NULL,
    
    # =================================================================
    # Fonctions Utilitaires (Simulées)
    # =================================================================
    
    #' @description Calcule la variable synthétique (PC1) pour un cluster numérique
    pca_synthetique = function(X_cluster) {
      X_clean <- na.omit(X_cluster)
      if (ncol(X_clean) < 2) return(X_cluster[, 1]) # 1 seule var: la retourne
      
      # Utilisation simplifiée de prcomp pour l'axe principal
      pca_res <- prcomp(X_clean, center = TRUE, scale = private$FScale)
      # Simule la variable synthétique (PC1)
      synthetique <- pca_res$x[, 1, drop = TRUE]
      
      # Ré-introduire les NA s'ils ont été omis
      full_synthetique <- rep(NA_real_, nrow(X_cluster))
      full_synthetique[as.numeric(rownames(X_clean))] <- synthetique
      
      return(full_synthetique)
    },
    
    # ------------------------------------------------
    # SURCHARGE : Désactivation du re-fit via K<-value
    # ------------------------------------------------
    refit_with_k = function(new_k) {
      # ClustOfVar est basé sur une partition, mais le changement de K est une 
      # coupure d'arbre, non un re-fit K-means. 
      if (is.null(private$FArbre)) {
        stop("Le modèle n'est pas encore ajusté. Lancez $fit() d'abord.")
      }
      
      private$FNbGroupes <- new_k
      
      # Coupure de l'arbre
      groupes_vars <- cutree(private$FArbre, k = new_k)
      private$FGroupes <- groupes_vars # Stocke les groupes de variables ici
      
      # Mise à jour des résultats (nécessite de relancer les étapes 2 et 3)
      message(paste0("ClustOfVar re-coupé avec succès à k = ", new_k, 
                     ". Les métriques de qualité ont été mises à jour."))
      
      # Ré-exécution de la logique d'évaluation (étape 2 et 3 du fit)
      private$evaluate_clusters()
    },
    
    #' @description Calcule les variables synthétiques et l'homogénéité (Étape 2 & 3 du fit)
    evaluate_clusters = function() {
      if (is.null(private$FGroupes)) return()
      
      X_clean <- private$cleanDataset(private$FX)
      n_vars <- ncol(X_clean)
      groupes_vars <- private$FGroupes
      
      vars_synth <- list()
      correlations <- rep(NA_real_, n_vars)
      
      for (k_idx in 1:private$FNbGroupes) {
        vars_in_cluster <- names(groupes_vars)[groupes_vars == k_idx]
        X_cluster <- X_clean[, vars_in_cluster, drop = FALSE]
        
        # 1. Calculer la variable synthétique (simulation de FactoMineR/HCPC)
        var_synth <- private$pca_synthetique(X_cluster)
        vars_synth[[paste0("C", k_idx)]] <- var_synth
        
        # 2. Corrélation des variables du cluster avec la variable synthétique
        for (i in 1:length(vars_in_cluster)) {
          var_name <- vars_in_cluster[i]
          
          # Calcul de la corrélation (gère les NA)
          cor_val <- cor(X_cluster[, var_name], var_synth, use = "pairwise.complete.obs")
          
          # Stocke la corrélation (en valeur absolue)
          correlations[match(var_name, private$FVarNames)] <- abs(cor_val)
        }
      }
      
      # 3. Stockage final des résultats
      private$FVariablesSynthetiques <- as.data.frame(vars_synth)
      private$FCorrelations <- correlations
      
      # 4. Homogénéité (Qualité du cluster)
      qualite <- tapply(correlations, groupes_vars, mean, na.rm = TRUE)
      private$FQualiteClusters <- qualite
      private$FHomogeneite <- sum(correlations, na.rm = TRUE) / n_vars
    }
  ),
  
  public = list(
    
    #' @description Initialiser un objet ClustOfVar
    initialize = function(k = 2, cr = TRUE, max_iter = 100, na_action = "warn") {
      super$initialize(k = k, cr = cr, na_action = na_action)
      private$FDataType <- "mixed" 
      private$FMaxIter <- as.integer(max_iter)
    },
    
    #' @description Ajuster le modèle
    fit = function(X) {
      X_clean <- super$fit(X) # Nettoyage/Validation des données (parent)
      
      private$FVarNames <- names(X_clean)
      
      # Préparation des données pour l'analyse
      # Si on utilisait ClustOfVar, il faudrait un tableau FactoMineR
      
      # Étape 1 : Création de l'arbre CAH des variables
      # Simule la matrice de distance (ex: 1 - R^2 entre variables)
      cor_matrix <- cor(X_clean, use = "pairwise.complete.obs")
      dist_vars <- as.dist(1 - abs(cor_matrix)) # Distance basée sur la corrélation
      
      private$FArbre <- hclust(dist_vars, method = "ward.D2")
      
      # Étape 2 : Coupure de l'arbre pour obtenir k groupes de variables
      groupes_vars <- cutree(private$FArbre, k = private$FNbGroupes)
      private$FGroupes <- groupes_vars
      
      # Étape 3 : Calcul des variables synthétiques et de l'homogénéité
      private$evaluate_clusters()
      
      private$FFitted <- TRUE
      message("ClustOfVar ajusté avec succès. Les résultats se réfèrent aux variables.")
      invisible(self)
    },
    
    #' @description Prédire (Non applicable aux observations)
    predict = function(newdata) {
      stop("La méthode $predict() n'est pas applicable à ClustOfVar, qui clusterise les variables et non les observations. Le résultat est $Groupes (groupes de variables).")
    },
    
    #' @description Afficher la qualité des clusters de variables
    homogeneite = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      return(list(
        homogeneite_globale = private$FHomogeneite,
        qualite_par_cluster = private$FQualiteClusters,
        correlations_variables = setNames(private$FCorrelations, private$FVarNames)
      ))
    },
    
    #' @description Obtenir les variables synthétiques (pour les groupes d'observations)
    variables_synthetiques = function() {
      if (!private$FFitted) stop("Le modèle doit être ajusté avec $fit() d'abord.")
      return(private$FVariablesSynthetiques)
    }
    
    # NOTE: La section active est gérée par la classe parente. 
    # $Groupes renverra les groupes de variables.
    # $K permet de changer la coupure de l'arbre via refit_with_k.
  )
)