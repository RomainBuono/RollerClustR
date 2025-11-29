# =============================================================================
# WRAPPER R6 : FACTORY, COMPARATOR, EVALUATOR, HELPER
# Package de clustering de VARIABLES
# =============================================================================

# Définition de la classe R6 (nécessite le package R6)
library(R6) 

# =============================================================================
# 1. ClusteringFactory (Création des objets)
# =============================================================================

#' @title ClusteringFactory
#' @description Factory pour créer et initialiser les objets de clustering de variables
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    
    #' @description Initialiser la Factory
    initialize = function() {
      message("ClusteringFactory initialisée - Clustering de variables uniquement")
    },
    
    # =========================================================================
    # CORRECTIF AVERTISSEMENT #1: Séparation explicite des paramètres
    # =========================================================================
    
    #' @description Créer et ajuster un objet VAR_CAH (Clustering de Variables - CAH)
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters (défaut: 3)
    #' @param scale Booléen, standardiser les données (défaut: TRUE)
    #' @param fit_now Booléen, ajuster le modèle immédiatement (défaut: TRUE)
    #' @return Objet VAR_CAH
    #' @examples
    #' factory <- ClusteringFactory$new()
    #' model <- factory$create_var_cah(iris[, 1:4], k = 3, scale = TRUE)
    create_var_cah = function(X, k = 3, scale = TRUE, fit_now = TRUE) {
      # Validation des paramètres
      if (!is.numeric(k) || k < 2) {
        stop("k doit être un nombre entier >= 2.")
      }
      if (!is.logical(scale)) {
        stop("scale doit être un booléen (TRUE/FALSE).")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit être un booléen (TRUE/FALSE).")
      }
      
      # Créer l'objet avec les paramètres de construction
      obj <- VAR_CAH$new(scale = scale, k = k)
      
      # Ajuster si demandé
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    },
    
    #' @description Créer et ajuster un objet KmodesVarClust
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters (défaut: 3)
    #' @param max_iter Nombre maximum d'itérations (défaut: 100)
    #' @param n_bins Nombre d'intervalles pour discrétisation (défaut: 5)
    #' @param fit_now Booléen, ajuster le modèle immédiatement (défaut: TRUE)
    #' @return Objet KmodesVarClust
    #' @examples
    #' factory <- ClusteringFactory$new()
    #' model <- factory$create_kmodes_varclust(data_cat, k = 2, max_iter = 100)
    create_kmodes_varclust = function(X, k = 3, max_iter = 100, n_bins = 5, fit_now = TRUE) {
      # Validation des paramètres
      if (!is.numeric(k) || k < 2) {
        stop("k doit être un nombre entier >= 2.")
      }
      if (!is.numeric(max_iter) || max_iter < 1) {
        stop("max_iter doit être >= 1.")
      }
      if (!is.numeric(n_bins) || n_bins < 2) {
        stop("n_bins doit être un nombre entier >= 2.")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit être un booléen (TRUE/FALSE).")
      }
      
      # Créer l'objet avec les paramètres de construction
      obj <- KmodesVarClust$new(k = k, max_iter = max_iter, n_bins = n_bins)
      
      # Ajuster si demandé
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    },
    
    #' @description Créer et ajuster un objet VARCLUS (Clustering de Variables - Descendant)
    #' @param X Data frame ou matrice de données
    #' @param stop_eigenvalue Seuil d'arrêt pour λ₂ (défaut: 1.0)
    #' @param distance_metric Métrique de distance (défaut: "correlation")
    #' @param fit_now Booléen, ajuster le modèle immédiatement (défaut: TRUE)
    #' @return Objet VARCLUS
    #' @examples
    #' factory <- ClusteringFactory$new()
    #' model <- factory$create_varclus(iris[, 1:4], stop_eigenvalue = 1.0)
    create_varclus = function(X, stop_eigenvalue = 1.0, distance_metric = "correlation", fit_now = TRUE) {
      # Validation des paramètres
      if (!is.numeric(stop_eigenvalue) || stop_eigenvalue < 0) {
        stop("stop_eigenvalue doit être un nombre >= 0.")
      }
      if (!is.character(distance_metric)) {
        stop("distance_metric doit être une chaîne de caractères.")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit être un booléen (TRUE/FALSE).")
      }
      
      # Créer l'objet avec les paramètres de construction
      obj <- VARCLUS$new(stop_eigenvalue = stop_eigenvalue, 
                        distance_metric = distance_metric)
      
      # Ajuster si demandé
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    }
  )
)

# =============================================================================
# 2. ClusteringComparator (Comparaison de modèles)
# =============================================================================

#' @title ClusteringComparator
#' @description Outil pour comparer les performances de différents objets de clustering
ClusteringComparator <- R6Class("ClusteringComparator",
  public = list(
    
    #' @description Initialiser le Comparator
    initialize = function() {
      message("ClusteringComparator initialisé")
    },
    
    #' @description Comparer les inerties de plusieurs modèles
    #' @param ... Objets de clustering à comparer (ajustés avec fit())
    #' @return Un data frame de comparaison
    #' @examples
    #' comp <- ClusteringComparator$new()
    #' result <- comp$compare_inertia(model1, model2, model3)
    compare_inertia = function(...) {
      models <- list(...)
      
      if (length(models) == 0) {
        stop("Au moins un modèle doit être fourni pour la comparaison")
      }
      
      results <- list()
      
      for (i in seq_along(models)) {
        model <- models[[i]]
        model_name <- paste0("Modèle ", i, " (", class(model)[1], ")")
        
        # Tentative d'extraction de l'inertie
        inertia_data <- tryCatch({
          # Vérifier que la méthode inertie() existe
          if (!"inertie" %in% names(model)) {
            stop(paste0("Le modèle ", class(model)[1], 
                       " ne possède pas de méthode $inertie()"))
          }
          
          inertie <- model$inertie()
          
          # Vérifier que le résultat a la structure attendue
          if (!all(c("totale", "intra", "inter", "pct_expliquee") %in% names(inertie))) {
            stop("La méthode $inertie() ne retourne pas la structure attendue")
          }
          
          data.frame(
            Modèle = model_name,
            K = model$K,
            Inertie_Totale = round(inertie$totale, 4),
            Inertie_Intra = round(inertie$intra, 4),
            Inertie_Inter = round(inertie$inter, 4),
            Pct_Expliquee = round(inertie$pct_expliquee, 2),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          warning(paste0("Erreur lors de l'extraction de l'inertie pour ", 
                        model_name, ": ", e$message))
          data.frame(
            Modèle = model_name,
            K = NA,
            Inertie_Totale = NA,
            Inertie_Intra = NA,
            Inertie_Inter = NA,
            Pct_Expliquee = NA,
            stringsAsFactors = FALSE
          )
        })
        
        results[[i]] <- inertia_data
      }
      
      comparison <- do.call(rbind, results)
      
      cat("\n═══════════════════════════════════════════════════════════\n")
      cat("   COMPARAISON DES MODÈLES - Inertie\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      print(comparison, row.names = FALSE)
      cat("\n")
      
      return(comparison)
    },
    
    #' @description Identifier le meilleur modèle selon un critère
    #' @param comparison_df Data frame retourné par compare_inertia()
    #' @param criterion Critère de sélection (défaut: "Pct_Expliquee")
    #' @return Indice du meilleur modèle
    get_best_model = function(comparison_df, criterion = "Pct_Expliquee") {
      if (!criterion %in% names(comparison_df)) {
        stop(paste0("Le critère '", criterion, "' n'existe pas dans le data frame"))
      }
      
      values <- comparison_df[[criterion]]
      
      # Retirer les NA
      valid_idx <- which(!is.na(values))
      if (length(valid_idx) == 0) {
        stop("Aucune valeur valide trouvée pour le critère spécifié")
      }
      
      # Pour Pct_Expliquee et Inertie_Inter : maximiser
      # Pour Inertie_Intra : minimiser
      if (criterion %in% c("Pct_Expliquee", "Inertie_Inter")) {
        best_idx <- valid_idx[which.max(values[valid_idx])]
      } else if (criterion == "Inertie_Intra") {
        best_idx <- valid_idx[which.min(values[valid_idx])]
      } else {
        best_idx <- valid_idx[which.max(values[valid_idx])]
      }
      
      cat("\nMeilleur modèle selon", criterion, ":\n")
      cat("→", comparison_df$Modèle[best_idx], "\n")
      cat("  Valeur:", values[best_idx], "\n\n")
      
      return(best_idx)
    }
  )
)

# =============================================================================
# 3. ClusteringEvaluator (Evaluation du nombre de clusters)
# =============================================================================

#' @title ClusteringEvaluator
#' @description Outil pour évaluer le nombre optimal de clusters de variables
ClusteringEvaluator <- R6Class("ClusteringEvaluator",
  private = list(
    FData = NULL
  ),
  
  public = list(
    
    #' @description Initialiser l'Evaluator avec les données (optionnel)
    #' @param data Data frame de données (optionnel)
    initialize = function(data = NULL) {
      if (!is.null(data)) {
        private$FData <- data
      }
      message("ClusteringEvaluator initialisé - Clustering de variables")
    },
    
    #' @description Méthode du coude (Elbow Method)
    #' @param X Data frame de données
    #' @param max_k Nombre maximal de clusters à tester (défaut: 10)
    #' @param method Méthode de clustering (défaut: "var_cah")
    #'        Options: "var_cah", "kmodes_varclust"
    #' @param ... Paramètres additionnels pour la méthode
    #' @return Liste contenant le graphique (ggplot) et les données
    elbow_method = function(X, max_k = 10, method = "var_cah", ...) {
      
      if (max_k < 2) {
        stop("max_k doit être >= 2")
      }
      
      if (!method %in% c("var_cah", "kmodes_varclust")) {
        stop(paste0("Méthode non reconnue: ", method, 
                   "\nMéthodes disponibles: 'var_cah', 'kmodes_varclust'"))
      }
      
      factory <- ClusteringFactory$new()
      inerties <- numeric(max_k - 1)
      k_values <- 2:max_k
      
      cat("\n═══════════════════════════════════════════════════════════\n")
      cat("   MÉTHODE DU COUDE - Évaluation de k\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      cat("Méthode :", method, "\n")
      cat("Plage de k :", min(k_values), "à", max(k_values), "\n\n")
      
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        cat("→ Évaluation pour k =", k, "... ")
        
        # Création et ajustement selon la méthode
        model <- tryCatch({
          if (method == "var_cah") {
            suppressMessages(factory$create_var_cah(X, k = k, fit_now = TRUE, ...))
          } else if (method == "kmodes_varclust") {
            suppressMessages(suppressWarnings(
              factory$create_kmodes_varclust(X, k = k, fit_now = TRUE, ...)
            ))
          }
        }, error = function(e) { 
          cat("ERREUR\n")
          warning(paste("Erreur pour k=", k, ":", e$message))
          NULL 
        })
        
        if (!is.null(model)) {
          # Extraction de l'inertie intra-classe (à minimiser)
          inertie <- tryCatch({
            model$inertie()
          }, error = function(e) {
            warning(paste("Impossible d'extraire l'inertie pour k=", k))
            list(intra = NA)
          })
          
          inerties[i] <- inertie$intra
          cat("OK (inertie intra =", round(inertie$intra, 4), ")\n")
        } else {
          inerties[i] <- NA
          cat("ÉCHEC\n")
        }
      }
      
      # Création du data frame de résultats
      results <- data.frame(
        k = k_values,
        inertie_intra = inerties
      )
      
      cat("\n")
      
      # Visualisation
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(results, ggplot2::aes(x = k, y = inertie_intra)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = "Méthode du Coude - Clustering de Variables",
            subtitle = paste("Méthode:", method),
            x = "Nombre de Clusters (k)",
            y = "Inertie Intra-Classe"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_continuous(breaks = k_values)
        
        print(p)
        
        return(list(plot = p, data = results))
      } else {
        plot(results$k, results$inertie_intra, type = 'b',
             main = "Méthode du Coude - Clustering de Variables",
             xlab = "Nombre de Clusters (k)", 
             ylab = "Inertie Intra-Classe",
             col = "steelblue", pch = 19, lwd = 2)
        
        return(list(plot = NULL, data = results))
      }
    },
    
    #' @description Évaluer une plage de valeurs de k
    #' @param X Data frame de données
    #' @param k_range Vecteur de valeurs de k à tester (défaut: 2:10)
    #' @param method Méthode de clustering (défaut: "var_cah")
    #' @param ... Paramètres additionnels
    #' @return Data frame avec les résultats pour chaque k
    evaluate_k_range = function(X, k_range = 2:10, method = "var_cah", ...) {
      
      if (!method %in% c("var_cah", "kmodes_varclust")) {
        stop(paste0("Méthode non reconnue: ", method,
                   "\nMéthodes disponibles: 'var_cah', 'kmodes_varclust'"))
      }
      
      factory <- ClusteringFactory$new()
      
      resultats <- data.frame(
        k = integer(),
        inertie_totale = numeric(),
        inertie_intra = numeric(),
        inertie_inter = numeric(),
        inertie_expliquee = numeric(),
        stringsAsFactors = FALSE
      )
      
      cat("\n═══════════════════════════════════════════════════════════\n")
      cat("   ÉVALUATION DE K - Plage complète\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      cat("Méthode :", method, "\n")
      cat("Plage de k :", min(k_range), "à", max(k_range), "\n\n")
      
      for (k in k_range) {
        cat("→ Évaluation pour k =", k, "... ")
        
        # Création et ajustement
        model <- tryCatch({
          if (method == "var_cah") {
            suppressMessages(factory$create_var_cah(X, k = k, fit_now = TRUE, ...))
          } else if (method == "kmodes_varclust") {
            suppressMessages(suppressWarnings(
              factory$create_kmodes_varclust(X, k = k, fit_now = TRUE, ...)
            ))
          }
        }, error = function(e) { 
          cat("ERREUR\n")
          warning(paste("Erreur pour k=", k, ":", e$message))
          NULL 
        })
        
        if (!is.null(model)) {
          # Extraction de l'inertie
          inertie <- tryCatch({
            model$inertie()
          }, error = function(e) {
            warning(paste("Impossible d'extraire l'inertie pour k=", k))
            list(totale = NA, intra = NA, inter = NA, pct_expliquee = NA)
          })
          
          resultats <- rbind(resultats, data.frame(
            k = k,
            inertie_totale = inertie$totale,
            inertie_intra = inertie$intra,
            inertie_inter = inertie$inter,
            inertie_expliquee = inertie$pct_expliquee
          ))
          
          cat("OK (", round(inertie$pct_expliquee, 2), "% expliqué)\n", sep = "")
        } else {
          cat("ÉCHEC\n")
        }
      }
      
      cat("\n")
      
      if (nrow(resultats) > 0) {
        cat("Résultats:\n")
        print(resultats, row.names = FALSE)
        cat("\n")
      }
      
      return(resultats)
    },
    
    #' @description Visualiser l'évaluation
    #' @param resultats Data frame de résultats (de evaluate_k_range)
    #' @param criterion Critère à visualiser (défaut: "inertie_expliquee")
    plot_evaluation = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        stop(paste0("Critère '", criterion, "' non trouvé dans les résultats"))
      }
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(resultats, ggplot2::aes_string(x = "k", y = criterion)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = "Évaluation du nombre de clusters de variables",
            x = "Nombre de Clusters (k)",
            y = criterion
          ) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_continuous(breaks = resultats$k)
        
        print(p)
      } else {
        plot(resultats$k, resultats[[criterion]], type = 'b',
             main = "Évaluation du nombre de clusters de variables",
             xlab = "Nombre de Clusters (k)", ylab = criterion,
             col = "steelblue", pch = 19, lwd = 2)
      }
      
      invisible(self)
    },
    
    #' @description Obtenir le meilleur k selon un critère
    #' @param resultats Data frame de résultats
    #' @param criterion Critère (défaut: "inertie_expliquee")
    #' @return Valeur de k recommandée
    get_best_k = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        warning("Critère non trouvé, retourne NA")
        return(NA)
      }
      
      # Méthode du coude simple : chercher le point où la pente change le plus
      valeurs <- resultats[[criterion]]
      k_values <- resultats$k
      
      if (length(valeurs) < 3) {
        return(k_values[1])
      }
      
      # Calculer les différences de second ordre
      diff1 <- diff(valeurs)
      diff2 <- diff(diff1)
      
      # Le meilleur k est où la courbure est maximale
      if (length(diff2) > 0) {
        idx <- which.max(abs(diff2))
        best_k <- k_values[idx + 1]
        
        cat("\nk optimal recommandé (méthode du coude):", best_k, "\n")
        cat("Valeur du critère:", valeurs[idx + 1], "\n\n")
        
        return(best_k)
      } else {
        return(k_values[2])
      }
    }
  )
)

# =============================================================================
# 4. ClusteringHelper (Fonctions utilitaires)
# =============================================================================

#' @title ClusteringHelper
#' @description Fonctions utilitaires pour le package de clustering de variables
ClusteringHelper <- R6Class("ClusteringHelper",
  public = list(
    
    #' @description Initialiser le Helper
    initialize = function() {
      message("ClusteringHelper initialisé")
    },
    
    #' @description Génère un rapport textuel synthétique du clustering
    #' @param objet_clustering Objet de clustering ajusté (VAR_CAH, VARCLUS, ou KmodesVarClust)
    #' @param file Chemin du fichier où écrire le rapport (NULL pour affichage console)
    #' @return Invisiblement, l'objet lui-même. Affiche le rapport.
    generate_report = function(objet_clustering, file = NULL) {
      if (!is.null(file)) {
        sink(file = file)
      }
      
      cat("═══════════════════════════════════════════════════════════\n")
      cat("   RAPPORT D'ANALYSE DE CLUSTERING\n")
      cat("       (Clustering de Variables)\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      
      # Résumé de base (appelle la méthode $summary() de l'objet)
      cat("--- Résumé du Modèle ---\n")
      tryCatch({
        objet_clustering$summary()
      }, error = function(e) {
        cat("La méthode $summary() n'est pas disponible ou a échoué.\n")
        cat("Erreur:", e$message, "\n")
      })
      
      # Qualité
      cat("\n--- Qualité du clustering (Inertie) ---\n")
      tryCatch({
        inertie <- objet_clustering$inertie()
        cat("Inertie totale     :", round(inertie$totale, 4), "\n")
        cat("Inertie intra      :", round(inertie$intra, 4), "\n")
        cat("Inertie inter      :", round(inertie$inter, 4), "\n")
        cat("Variance expliquée :", round(inertie$pct_expliquee, 2), "%\n")
      }, error = function(e) {
        cat("Mesures d'inertie non disponibles pour cet algorithme.\n")
        cat("Erreur:", e$message, "\n")
      })
      
      # Taille des groupes
      cat("\n--- Taille des Groupes ---\n")
      tryCatch({
        groupes_table <- table(objet_clustering$Groupes)
        print(groupes_table)
      }, error = function(e) {
        cat("Propriété $Groupes non disponible.\n")
      })
      
      # Infos sur les variables (spécifique à VAR_CAH)
      if (inherits(objet_clustering, "VAR_CAH")) {
        cat("\n--- Homogénéité des Clusters (VAR_CAH) ---\n")
        tryCatch({
          # Accéder aux clusters quality via get_cluster_variables
          for (k in 1:objet_clustering$K) {
            vars <- objet_clustering$get_cluster_variables(k)
            cat("Cluster", k, ":", length(vars), "variable(s)\n")
            cat("  Variables:", paste(vars, collapse = ", "), "\n")
          }
        }, error = function(e) {
          cat("Détails non disponibles.\n")
        })
      }
      
      # Infos spécifiques à VARCLUS
      if (inherits(objet_clustering, "VARCLUS")) {
        cat("\n--- Informations VARCLUS ---\n")
        cat("Algorithme : Clustering descendant hiérarchique de variables\n")
        cat("Détection automatique du nombre de clusters basée sur λ₂\n")
      }
      
      # Infos spécifiques à KmodesVarClust
      if (inherits(objet_clustering, "KmodesVarClust")) {
        cat("\n--- Informations KmodesVarClust ---\n")
        cat("Algorithme : K-Modes pour clustering de variables catégorielles\n")
      }
      
      cat("\n═══════════════════════════════════════════════════════════\n")
      
      if (!is.null(file)) {
        sink() # Rétablit l'affichage console
        message(paste("Rapport généré dans:", file))
      }
      
      invisible(objet_clustering)
    },
    
    #' @description Affiche les méthodes disponibles dans le package
    show_methods = function() {
      cat("═══════════════════════════════════════════════════════════\n")
      cat("   MÉTHODES DE CLUSTERING DISPONIBLES\n")
      cat("       (Clustering de Variables)\n")
      cat("═══════════════════════════════════════════════════════════\n\n")
      
      cat("1. VAR_CAH\n")
      cat("   - Classification Ascendante Hiérarchique sur variables\n")
      cat("   - Type de données : Numériques\n")
      cat("   - Utilisation : factory$create_var_cah(X, k = 3)\n\n")
      
      cat("2. VARCLUS\n")
      cat("   - Clustering descendant hiérarchique de variables\n")
      cat("   - Type de données : Numériques\n")
      cat("   - Détection automatique du nombre optimal de clusters\n")
      cat("   - Utilisation : factory$create_varclus(X)\n\n")
      
      cat("3. KmodesVarClust\n")
      cat("   - K-Modes pour variables catégorielles\n")
      cat("   - Type de données : Catégorielles ou mixtes\n")
      cat("   - Utilisation : factory$create_kmodes_varclust(X, k = 3)\n\n")
      
      cat("═══════════════════════════════════════════════════════════\n")
      invisible(self)
    },
    
    #' @description Exporter les résultats du clustering
    #' @param objet_clustering Objet de clustering ajusté
    #' @param file Chemin du fichier CSV de sortie
    export_results = function(objet_clustering, file = "clustering_results.csv") {
      tryCatch({
        groupes <- objet_clustering$Groupes
        
        results <- data.frame(
          Variable = names(groupes),
          Cluster = groupes,
          stringsAsFactors = FALSE
        )
        
        results <- results[order(results$Cluster, results$Variable), ]
        
        write.csv(results, file = file, row.names = FALSE)
        message(paste("Résultats exportés dans:", file))
        
        invisible(results)
      }, error = function(e) {
        stop(paste("Erreur lors de l'export:", e$message))
      })
    }
  )
)
