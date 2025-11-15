#=========================================
# WRAPPER R6 : FACTORY, COMPARATOR, EVALUATOR, HELPER
# Package de clustering de VARIABLES
#=========================================

# Définition de la classe R6 (nécessite le package R6)
# library(R6) 

# ---------------------------------
# 1. ClusteringFactory (Création des objets)
# ---------------------------------

#' @title ClusteringFactory
#' @description Factory pour créer et initialiser les objets de clustering de variables
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    
    #' @description Initialiser
    initialize = function() {
      message("ClusteringFactory initialisée - Clustering de variables uniquement")
    },
    
    #' @description Créer et ajuster un objet VAR_CAH (Clustering de Variables - CAH)
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit()
    #' @return Objet VAR_CAH
    create_var_cah = function(X, k = 3, fit_now = TRUE, ...) {
      # VAR_CAH ne prend pas 'cr' dans $new() car la standardisation 
      # est gérée différemment. Cela évite la redondance et clarifie l'API pour cette méthode.
      # Assurez-vous que la classe VAR_CAH est disponible (définit ailleurs)
      obj <- VAR_CAH$new(k = k, ...)
      if (fit_now) {
        # VAR_CAH$fit prend X, max_iter, tolerance etc.
        obj$fit(X, ...)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet KmodesVarClust
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit()
    #' @return Objet KmodesVarClust
    create_kmodes_varclust = function(X, k = 3, fit_now = TRUE, ...) {
      # KmodesVarClust pour clustering de variables catégorielles
      obj <- KmodesVarClust$new(k = k, ...)
      if (fit_now) {
        obj$fit(X, ...)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet VARCLUS (Clustering de Variables - Descendant)
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters (optionnel, détection automatique par défaut)
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit() (stop_eigenvalue, distance_metric, etc.)
    #' @return Objet VARCLUS
    create_varclus = function(X, k = NULL, fit_now = TRUE, ...) {
      # VARCLUS prend des paramètres spécifiques dans $new() comme 'stop_eigenvalue', 'distance_metric', etc.
      # L'initialisation est faite avec '...' pour passer tous ces paramètres.
      # La classe 'VARCLUS' est définie dans 'VARCLUS.R'
      obj <- VARCLUS$new(k = k, ...) 
      if (fit_now) {
        obj$fit(X, ...)
      }
      return(obj)
    }
    
  )
)

# ---------------------------------
# 2. ClusteringComparator (Comparaison de modèles)
# ---------------------------------

#' @title ClusteringComparator
#' @description Outil pour comparer les performances de différents objets de clustering
ClusteringComparator <- R6Class("ClusteringComparator",
  public = list(
    
    #' @description Comparer les inerties
    #' @param ... Objets de clustering à comparer
    #' @return Un data frame de comparaison
    compare_inertia = function(...) {
      models <- list(...)
      results <- list()
      
      for (i in seq_along(models)) {
        model <- models[[i]]
        model_name <- paste0("Modèle ", i, " (", class(model)[1], ")")
        
        # Tentative d'extraction de l'inertie
        inertia_data <- tryCatch({
          inertie <- model$inertie()
          data.frame(
            Modèle = model_name,
            Inertie_Totale = inertie$totale,
            Inertie_Intra = inertie$intra,
            Inertie_Inter = inertie$inter,
            Pct_Expliquee = inertie$pct_expliquee
          )
        }, error = function(e) {
          data.frame(
            Modèle = model_name,
            Inertie_Totale = NA,
            Inertie_Intra = NA,
            Inertie_Inter = NA,
            Pct_Expliquee = NA
          )
        })
        
        results[[i]] <- inertia_data
      }
      
      return(do.call(rbind, results))
    }
  )
)

# ---------------------------------
# 3. ClusteringEvaluator (Evaluation du nombre de clusters)
# ---------------------------------

#' @title ClusteringEvaluator
#' @description Outil pour évaluer le nombre optimal de clusters de variables
ClusteringEvaluator <- R6Class("ClusteringEvaluator",
  private = list(
    FData = NULL
  ),
  
  public = list(
    
    #' @description Initialiser avec les données
    #' @param data Data frame de données (optionnel)
    initialize = function(data = NULL) {
      if (!is.null(data)) {
        private$FData <- data
      }
      message("ClusteringEvaluator initialisé - Clustering de variables")
    },
    
    #' @description Méthode du coude (Elbow Method)
    #' @param X Data frame de données
    #' @param max_k Nombre maximal de clusters à tester
    #' @param method Méthode de clustering à utiliser (par défaut: "var_cah")
    #' @param ... Paramètres additionnels passés à la Factory
    #' @return Liste contenant le graphique (ggplot) et les données
    elbow_method = function(X, max_k = 10, method = "var_cah", ...) {
      
      if (max_k < 2) {
        stop("max_k doit être >= 2")
      }
      
      factory <- ClusteringFactory$new()
      inerties <- numeric(max_k - 1)
      k_values <- 2:max_k
      
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        message(paste("Évaluation pour k =", k))
        
        # Création et ajustement selon la méthode
        model <- switch(method,
          var_cah = tryCatch({
            factory$create_var_cah(X, k = k, fit_now = TRUE, ...)
          }, error = function(e) { 
            warning(paste("Erreur pour k=", k, ":", e$message))
            NULL 
          }),
          kmodes_varclust = tryCatch({
            factory$create_kmodes_varclust(X, k = k, fit_now = TRUE, ...)
          }, error = function(e) { 
            warning(paste("Erreur pour k=", k, ":", e$message))
            NULL 
          }),
          stop(paste("Méthode non reconnue:", method, 
                     "\nMéthodes disponibles: 'var_cah', 'kmodes_varclust'"))
        )
        
        if (!is.null(model)) {
          # Extraction de l'inertie intra-classe (à minimiser)
          inertie <- tryCatch({
            model$inertie()
          }, error = function(e) {
            list(intra = NA)
          })
          
          inerties[i] <- inertie$intra
        } else {
          inerties[i] <- NA
        }
      }
      
      # Création du data frame de résultats
      results <- data.frame(
        k = k_values,
        inertie_intra = inerties
      )
      
      # Visualisation
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(results, ggplot2::aes(x = k, y = inertie_intra)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = "Méthode du Coude - Clustering de Variables",
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
             col = "steelblue", pch = 19)
        
        return(list(plot = NULL, data = results))
      }
    },
    
    #' @description Évaluer un intervalle de k
    #' @param k_range Vecteur des valeurs de k à tester (défaut: 2:10)
    #' @param method Méthode de clustering (défaut: "var_cah")
    #' @param ... Paramètres additionnels passés à la Factory
    #' @return Data frame avec les résultats pour chaque k
    evaluate_k = function(k_range = 2:10, method = "var_cah", ...) {
      
      X <- if (!is.null(private$FData)) private$FData else stop("Données non fournies")
      
      factory <- ClusteringFactory$new()
      resultats <- data.frame(
        k = integer(),
        inertie_totale = numeric(),
        inertie_intra = numeric(),
        inertie_inter = numeric(),
        inertie_expliquee = numeric(),
        stringsAsFactors = FALSE
      )
      
      for (k in k_range) {
        message(paste("Évaluation pour k =", k))
        
        # Création et ajustement
        model <- switch(method,
          var_cah = tryCatch({
            factory$create_var_cah(X, k = k, fit_now = TRUE, ...)
          }, error = function(e) { 
            warning(paste("Erreur pour k=", k, ":", e$message))
            NULL 
          }),
          kmodes_varclust = tryCatch({
            factory$create_kmodes_varclust(X, k = k, fit_now = TRUE, ...)
          }, error = function(e) { 
            warning(paste("Erreur pour k=", k, ":", e$message))
            NULL 
          }),
          stop(paste("Méthode non reconnue:", method,
                     "\nMéthodes disponibles: 'var_cah', 'kmodes_varclust'"))
        )
        
        if (!is.null(model)) {
          # Extraction de l'inertie
          inertie <- tryCatch({
            model$inertie()
          }, error = function(e) {
            list(totale = NA, intra = NA, inter = NA, pct_expliquee = NA)
          })
          
          resultats <- rbind(resultats, data.frame(
            k = k,
            inertie_totale = inertie$totale,
            inertie_intra = inertie$intra,
            inertie_inter = inertie$inter,
            inertie_expliquee = inertie$pct_expliquee
          ))
        }
      }
      
      return(resultats)
    },
    
    #' @description Visualiser l'évaluation
    #' @param resultats Data frame de résultats
    #' @param criterion Critère à visualiser (défaut: "inertie_expliquee")
    plot_evaluation = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        stop("Critère non trouvé dans les résultats")
      }
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(resultats, ggplot2::aes_string(x = "k", y = criterion)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = paste("Évaluation du nombre de clusters de variables"),
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
             col = "steelblue", pch = 19)
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
        return(k_values[idx + 1])
      } else {
        return(k_values[2])
      }
    }
  )
)

# ---------------------------------
# 4. ClusteringHelper (Fonctions utilitaires)
# ---------------------------------

#' @title ClusteringHelper
#' @description Fonctions utilitaires pour le package de clustering de variables
ClusteringHelper <- R6Class("ClusteringHelper",
  public = list(
    
    #' @description Génère un rapport textuel synthétique du clustering
    #' @param objet_clustering Objet de clustering ajusté (VAR_CAH, VARCLUS, ou KmodesVarClust)
    #' @param file Chemin du fichier où écrire le rapport (NULL pour affichage console)
    #' @return Invisiblement, l'objet lui-même. Affiche le rapport.
    generate_report = function(objet_clustering, file = NULL) {
      if (!is.null(file)) {
        sink(file = file)
      }
      
      cat("=========================================\n")
      cat("   RAPPORT D'ANALYSE DE CLUSTERING\n")
      cat("       (Clustering de Variables)\n")
      cat("=========================================\n\n")
      
      # Résumé de base (appelle la méthode $summary() de l'objet)
      cat("--- Résumé du Modèle ---\n")
      tryCatch({
        objet_clustering$summary()
      }, error = function(e) {
        cat("La méthode $summary() n'est pas disponible ou a échoué.\n")
      })
      
      # Qualité
      tryCatch({
        inertie <- objet_clustering$inertie()
        cat("\n--- Qualité du clustering (Inertie) ---\n")
        cat("Inertie expliquée :", round(inertie$pct_expliquee, 2), "%\n")
      }, error = function(e) {
        cat("\n--- Qualité du clustering ---\n")
        cat("Mesures d'inertie non disponibles pour cet algorithme.\n")
      })
      
      # Taille des groupes
      cat("\n--- Taille des Groupes ---\n")
      if ("Groupes" %in% names(objet_clustering)) {
        print(table(objet_clustering$Groupes))
      } else {
        cat("Propriété $Groupes non disponible.\n")
      }
      
      # Infos sur les variables (spécifique à VAR_CAH)
      if (inherits(objet_clustering, "VAR_CAH")) {
        cat("\n--- Homogénéité des Clusters (VAR_CAH) ---\n")
        # Afficher la qualité des clusters (si disponible)
        tryCatch({
          print(objet_clustering$Homogeneite)
        }, error = function(e) {
          cat("Propriété $Homogeneite non disponible.\n")
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
      
      if (!is.null(file)) {
        sink() # Rétablit l'affichage console
        message(paste("Rapport généré dans:", file))
      }
      
      invisible(objet_clustering)
    },
    
    #' @description Affiche les méthodes disponibles dans le package
    show_methods = function() {
      cat("=========================================\n")
      cat("   MÉTHODES DE CLUSTERING DISPONIBLES\n")
      cat("       (Clustering de Variables)\n")
      cat("=========================================\n\n")
      
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
      cat("   - Type de données : Catégorielles\n")
      cat("   - Utilisation : factory$create_kmodes_varclust(X, k = 3)\n\n")
      
      cat("=========================================\n")
      invisible(self)
    }
  )
)