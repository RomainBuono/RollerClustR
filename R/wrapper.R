# =============================================================================
# WRAPPER R6 : FACTORY, COMPARATOR, EVALUATOR, HELPER
# Package de clustering de VARIABLES
# =============================================================================

#' @importFrom R6 R6Class
NULL

# =============================================================================
# 1. ClusteringFactory (Création des objets)
# =============================================================================

#' Factory pour créer et initialiser les objets de clustering de variables
#' 
#' @title ClusteringFactory
#' @description Factory pour creer et initialiser les objets de clustering de variables
#' @export
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    
    #' @description Initialiser la Factory
    initialize = function() {
      message("ClusteringFactory initialisee - Clustering de variables uniquement")
    },
    
    #' @description Creer et ajuster un objet VAR_CAH (Clustering de Variables - CAH)
    #' @param X Data frame ou matrice de donnees
    #' @param k Nombre de clusters (defaut: 3)
    #' @param scale Booleen, standardiser les donnees (defaut: TRUE)
    #' @param fit_now Booleen, ajuster le modele immediatement (defaut: TRUE)
    #' @return Objet VAR_CAH
    create_var_cah = function(X, k = 3, scale = TRUE, fit_now = TRUE) {
      if (!is.numeric(k) || k < 2) {
        stop("k doit etre un nombre entier >= 2.")
      }
      if (!is.logical(scale)) {
        stop("scale doit etre un booleen (TRUE/FALSE).")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit etre un booleen (TRUE/FALSE).")
      }
      
      obj <- VAR_CAH$new(scale = scale, k = k)
      
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    },
    
    #' @description Creer et ajuster un objet VAR_KMEANS (Clustering de Variables - K-means)
    #' @param X Data frame ou matrice de donnees
    #' @param k Nombre de clusters (defaut: 3)
    #' @param n_init Nombre d'initialisations (defaut: 10)
    #' @param max_iter Nombre maximum d'iterations (defaut: 100)
    #' @param scale Booleen, standardiser les donnees (defaut: TRUE)
    #' @param tolerance Seuil de convergence (defaut: 1e-6)
    #' @param fit_now Booleen, ajuster le modele immediatement (defaut: TRUE)
    #' @return Objet VAR_KMEANS
    create_var_kmeans = function(X, k = 3, n_init = 10, max_iter = 100, 
                                  scale = TRUE, tolerance = 1e-6, fit_now = TRUE) {
      if (!is.numeric(k) || k < 2) {
        stop("k doit etre un nombre entier >= 2.")
      }
      if (!is.numeric(n_init) || n_init < 1) {
        stop("n_init doit etre >= 1.")
      }
      if (!is.numeric(max_iter) || max_iter < 1) {
        stop("max_iter doit etre >= 1.")
      }
      if (!is.logical(scale)) {
        stop("scale doit etre un booleen (TRUE/FALSE).")
      }
      if (!is.numeric(tolerance) || tolerance <= 0) {
        stop("tolerance doit etre un nombre > 0.")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit etre un booleen (TRUE/FALSE).")
      }
      
      obj <- VAR_KMEANS$new(K = k, n_init = n_init, max_iter = max_iter, 
                            scale = scale, tolerance = tolerance)
      
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    },
    
    #' @description Creer et ajuster un objet TandemVarClust (Clustering de Variables - Donnees Mixtes)
    #' @param X Data frame ou matrice de donnees (mixtes: quantitatives + qualitatives)
    #' @param k Nombre de clusters (defaut: 3)
    #' @param n_bins Nombre d'intervalles pour discretisation (defaut: 3)
    #' @param assignment_method Methode d'assignation ('dice', 'voting', 'barycenter', defaut: 'dice')
    #' @param fit_now Booleen, ajuster le modele immediatement (defaut: TRUE)
    #' @return Objet TandemVarClust
    create_tandem_varclust = function(X, k = 3, n_bins = 3, 
                                       assignment_method = "dice", fit_now = TRUE) {
      if (!is.numeric(k) || k < 2) {
        stop("k doit etre un nombre entier >= 2.")
      }
      if (!is.numeric(n_bins) || n_bins < 2) {
        stop("n_bins doit etre un nombre entier >= 2.")
      }
      if (!assignment_method %in% c("dice", "voting", "barycenter")) {
        stop("assignment_method doit etre 'dice', 'voting' ou 'barycenter'.")
      }
      if (!is.logical(fit_now)) {
        stop("fit_now doit etre un booleen (TRUE/FALSE).")
      }
      
      obj <- TandemVarClust$new(K = k, n_bins = n_bins, 
                                assignment_method = assignment_method)
      
      if (fit_now) {
        obj$fit(X)
      }
      
      return(obj)
    }
    
  )
)

# =============================================================================
# 2. ClusteringComparator (Comparaison de modeles)
# =============================================================================

#' Outil pour comparer les performances de differents objets de clustering
#' 
#' @title ClusteringComparator
#' @description Outil pour comparer les performances de differents objets de clustering
#' @export
ClusteringComparator <- R6Class("ClusteringComparator",
  public = list(
    
    #' @description Initialiser le Comparator
    initialize = function() {
      message("ClusteringComparator initialise")
    },
    
    #' @description Comparer les inerties de plusieurs modeles
    #' @param ... Objets de clustering a comparer (ajustes avec fit())
    #' @return Un data frame de comparaison
    compare_inertia = function(...) {
      models <- list(...)
      
      if (length(models) == 0) {
        stop("Au moins un modele doit etre fourni pour la comparaison")
      }
      
      results <- list()
      
      for (i in seq_along(models)) {
        model <- models[[i]]
        model_name <- paste0("Modele ", i, " (", class(model)[1], ")")
        
        inertia_data <- tryCatch({
          if (!"inertie" %in% names(model)) {
            stop(paste0("Le modele ", class(model)[1], 
                       " ne possede pas de methode $inertie()"))
          }
          
          inertie <- model$inertie()
          
          if (!all(c("totale", "intra", "inter", "pct_expliquee") %in% names(inertie))) {
            stop("La methode $inertie() ne retourne pas la structure attendue")
          }
          
          data.frame(
            Modele = model_name,
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
            Modele = model_name,
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
      
      cat("\n===============================================================\n")
      cat("   COMPARAISON DES MODELES - Inertie\n")
      cat("===============================================================\n\n")
      print(comparison, row.names = FALSE)
      cat("\n")
      
      return(comparison)
    },
    
    #' @description Identifier le meilleur modele selon un critere
    #' @param comparison_df Data frame retourne par compare_inertia()
    #' @param criterion Critere de selection (defaut: "Pct_Expliquee")
    #' @return Indice du meilleur modele
    get_best_model = function(comparison_df, criterion = "Pct_Expliquee") {
      if (!criterion %in% names(comparison_df)) {
        stop(paste0("Le critere '", criterion, "' n'existe pas dans le data frame"))
      }
      
      values <- comparison_df[[criterion]]
      
      valid_idx <- which(!is.na(values))
      if (length(valid_idx) == 0) {
        stop("Aucune valeur valide trouvee pour le critere specifie")
      }
      
      if (criterion %in% c("Pct_Expliquee", "Inertie_Inter")) {
        best_idx <- valid_idx[which.max(values[valid_idx])]
      } else if (criterion == "Inertie_Intra") {
        best_idx <- valid_idx[which.min(values[valid_idx])]
      } else {
        best_idx <- valid_idx[which.max(values[valid_idx])]
      }
      
      cat("\n===============================================================\n")
      cat("   MEILLEUR MODELE SELON LE CRITERE:", criterion, "\n")
      cat("===============================================================\n\n")
      cat("Modele selectionne:", comparison_df$Modele[best_idx], "\n")
      cat("Valeur du critere :", values[best_idx], "\n\n")
      
      return(best_idx)
    }
  )
)

# =============================================================================
# 3. ClusteringEvaluator (Evaluation pour K optimal)
# =============================================================================

#' Outil pour evaluer le nombre optimal de clusters
#' 
#' @title ClusteringEvaluator
#' @description Outil pour evaluer le nombre optimal de clusters (k)
#' @export
ClusteringEvaluator <- R6Class("ClusteringEvaluator",
  public = list(
    
    #' @description Initialiser l'Evaluator
    initialize = function() {
      message("ClusteringEvaluator initialise")
    },
    
    #' @description Evaluer plusieurs valeurs de k pour VAR_CAH
    #' @param X Data frame ou matrice de donnees
    #' @param k_min Nombre minimum de clusters (defaut: 2)
    #' @param k_max Nombre maximum de clusters (defaut: 10)
    #' @param scale Booleen, standardiser les donnees (defaut: TRUE)
    #' @return Data frame avec k et les metriques associees
    evaluate_k_var_cah = function(X, k_min = 2, k_max = 10, scale = TRUE) {
      
      if (!is.numeric(k_min) || k_min < 2) {
        stop("k_min doit etre >= 2")
      }
      if (!is.numeric(k_max) || k_max <= k_min) {
        stop("k_max doit etre > k_min")
      }
      
      k_values <- k_min:k_max
      resultats <- data.frame(
        k = k_values,
        inertie_intra = numeric(length(k_values)),
        inertie_inter = numeric(length(k_values)),
        inertie_expliquee = numeric(length(k_values))
      )
      
      cat("\nEvaluation de k pour VAR_CAH...\n")
      cat("Range de k:", k_min, "a", k_max, "\n\n")
      
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        
        model <- VAR_CAH$new(k = k, scale = scale)
        model$fit(X)
        
        inertie <- model$inertie()
        
        resultats$inertie_intra[i] <- inertie$intra
        resultats$inertie_inter[i] <- inertie$inter
        resultats$inertie_expliquee[i] <- inertie$pct_expliquee
        
        cat("k =", k, ": Inertie expliquee =", 
            round(inertie$pct_expliquee, 2), "%\n")
      }
      
      cat("\n")
      return(resultats)
    },
    
    #' @description Evaluer plusieurs valeurs de k pour VAR_KMEANS
    #' @param X Data frame ou matrice de donnees
    #' @param k_min Nombre minimum de clusters (defaut: 2)
    #' @param k_max Nombre maximum de clusters (defaut: 10)
    #' @param n_init Nombre d'initialisations (defaut: 10)
    #' @param scale Booleen, standardiser les donnees (defaut: TRUE)
    #' @return Data frame avec k et les metriques associees
    evaluate_k_var_kmeans = function(X, k_min = 2, k_max = 10, 
                                      n_init = 10, scale = TRUE) {
      
      if (!is.numeric(k_min) || k_min < 2) {
        stop("k_min doit etre >= 2")
      }
      if (!is.numeric(k_max) || k_max <= k_min) {
        stop("k_max doit etre > k_min")
      }
      
      k_values <- k_min:k_max
      resultats <- data.frame(
        k = k_values,
        inertie_intra = numeric(length(k_values)),
        homogeneite = numeric(length(k_values)),
        converged = logical(length(k_values)),
        iterations = integer(length(k_values))
      )
      
      cat("\nEvaluation de k pour VAR_KMEANS...\n")
      cat("Range de k:", k_min, "a", k_max, "\n")
      cat("Initialisations:", n_init, "\n\n")
      
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        
        model <- VAR_KMEANS$new(K = k, n_init = n_init, scale = scale)
        model$fit(X)
        
        resultats$inertie_intra[i] <- model$WithinClusterInertia
        resultats$homogeneite[i] <- model$FHomogeneite
        resultats$converged[i] <- model$Converged
        resultats$iterations[i] <- model$NIterations
        
        cat("k =", k, ": W =", round(model$WithinClusterInertia, 4),
            ", Homogeneite =", round(model$FHomogeneite, 4),
            ", Converged =", model$Converged, "\n")
      }
      
      cat("\n")
      return(resultats)
    },
    
    #' @description Visualiser les resultats d'evaluation
    #' @param resultats Data frame retourne par evaluate_k_*
    #' @param criterion Critere a visualiser (defaut: "inertie_expliquee")
    plot_evaluation = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        stop(paste0("Critere '", criterion, "' non trouve dans les resultats"))
      }
      
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        p <- ggplot2::ggplot(resultats, ggplot2::aes_string(x = "k", y = criterion)) +
          ggplot2::geom_point(color = "steelblue", size = 3) +
          ggplot2::geom_line(color = "steelblue", size = 1) +
          ggplot2::labs(
            title = "Evaluation du nombre de clusters de variables",
            x = "Nombre de Clusters (k)",
            y = criterion
          ) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_continuous(breaks = resultats$k)
        
        print(p)
      } else {
        plot(resultats$k, resultats[[criterion]], type = 'b',
             main = "Evaluation du nombre de clusters de variables",
             xlab = "Nombre de Clusters (k)", ylab = criterion,
             col = "steelblue", pch = 19, lwd = 2)
      }
      
      invisible(self)
    },
    
    #' @description Obtenir le meilleur k selon un critere
    #' @param resultats Data frame de resultats
    #' @param criterion Critere (defaut: "inertie_expliquee")
    #' @return Valeur de k recommandee
    get_best_k = function(resultats, criterion = "inertie_expliquee") {
      
      if (!criterion %in% names(resultats)) {
        warning("Critere non trouve, retourne NA")
        return(NA)
      }
      
      valeurs <- resultats[[criterion]]
      k_values <- resultats$k
      
      if (length(valeurs) < 3) {
        return(k_values[1])
      }
      
      diff1 <- diff(valeurs)
      diff2 <- diff(diff1)
      
      if (length(diff2) > 0) {
        idx <- which.max(abs(diff2))
        best_k <- k_values[idx + 1]
        
        cat("\nk optimal recommande (methode du coude):", best_k, "\n")
        cat("Valeur du critere:", valeurs[idx + 1], "\n\n")
        
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

#' Fonctions utilitaires pour le package de clustering de variables
#' 
#' @title ClusteringHelper
#' @description Fonctions utilitaires pour le package de clustering de variables
#' @importFrom utils write.csv
#' @export
ClusteringHelper <- R6Class("ClusteringHelper",
  public = list(
    
    #' @description Initialiser le Helper
    initialize = function() {
      message("ClusteringHelper initialise")
    },
    
    #' @description Genere un rapport textuel synthetique du clustering
    #' @param objet_clustering Objet de clustering ajuste (VAR_CAH, VAR_KMEANS, ou TandemVarClust)
    #' @param file Chemin du fichier ou ecrire le rapport (NULL pour affichage console)
    #' @return Invisiblement, l'objet lui-meme. Affiche le rapport.
    generate_report = function(objet_clustering, file = NULL) {
      if (!is.null(file)) {
        sink(file = file)
      }
      
      cat("===============================================================\n")
      cat("   RAPPORT D'ANALYSE DE CLUSTERING\n")
      cat("       (Clustering de Variables)\n")
      cat("===============================================================\n\n")
      
      cat("--- Resume du Modele ---\n")
      tryCatch({
        objet_clustering$summary()
      }, error = function(e) {
        cat("La methode $summary() n'est pas disponible ou a echoue.\n")
        cat("Erreur:", e$message, "\n")
      })
      
      cat("\n--- Qualite du clustering (Inertie) ---\n")
      tryCatch({
        if (inherits(objet_clustering, "VAR_KMEANS")) {
          # VAR_KMEANS utilise WithinClusterInertia directement
          cat("Inertie intra (W)  :", round(objet_clustering$WithinClusterInertia, 4), "\n")
          cat("Homogeneite        :", round(objet_clustering$FHomogeneite, 4), "\n")
          cat("Convergence        :", objet_clustering$Converged, "\n")
          cat("Iterations         :", objet_clustering$NIterations, "\n")
        } else {
          # VAR_CAH et TandemVarClust utilisent inertie()
          inertie <- objet_clustering$inertie()
          cat("Inertie totale     :", round(inertie$totale, 4), "\n")
          cat("Inertie intra      :", round(inertie$intra, 4), "\n")
          cat("Inertie inter      :", round(inertie$inter, 4), "\n")
          cat("Variance expliquee :", round(inertie$pct_expliquee, 2), "%\n")
        }
      }, error = function(e) {
        cat("Mesures d'inertie non disponibles pour cet algorithme.\n")
        cat("Erreur:", e$message, "\n")
      })
      
      cat("\n--- Taille des Groupes ---\n")
      tryCatch({
        groupes_table <- table(objet_clustering$Groupes)
        print(groupes_table)
      }, error = function(e) {
        cat("Propriete $Groupes non disponible.\n")
      })
      
      # Informations spécifiques par algorithme
      if (inherits(objet_clustering, "VAR_CAH")) {
        cat("\n--- Homogeneite des Clusters (VAR_CAH) ---\n")
        tryCatch({
          for (k in 1:objet_clustering$K) {
            vars <- objet_clustering$get_cluster_variables(k)
            cat("Cluster", k, ":", length(vars), "variable(s)\n")
            cat("  Variables:", paste(vars, collapse = ", "), "\n")
          }
        }, error = function(e) {
          cat("Details non disponibles.\n")
        })
      }
      
      if (inherits(objet_clustering, "VAR_KMEANS")) {
        cat("\n--- Informations VAR_KMEANS ---\n")
        cat("Algorithme : K-means pour clustering de variables\n")
        cat("Reallocation iterative avec optimisation de l'inertie intra-classes\n")
        tryCatch({
          for (k in 1:objet_clustering$K) {
            vars <- objet_clustering$get_cluster_variables(k)
            cat("Cluster", k, ":", length(vars), "variable(s)\n")
            cat("  Variables:", paste(vars, collapse = ", "), "\n")
          }
        }, error = function(e) {
          cat("Details non disponibles.\n")
        })
      }
      
      if (inherits(objet_clustering, "TandemVarClust")) {
        cat("\n--- Informations TandemVarClust ---\n")
        cat("Algorithme : Tandem MCA + HAC pour variables mixtes\n")
        cat("Variables quantitatives discretisees avec n_bins =", 
            objet_clustering$n_bins, "\n")
        cat("Variables categoriques :", length(objet_clustering$CategoricalVars), "\n")
      }
      
      cat("\n===============================================================\n")
      
      if (!is.null(file)) {
        sink()
        message(paste("Rapport genere dans:", file))
      }
      
      invisible(objet_clustering)
    },
    
    #' @description Affiche les methodes disponibles dans le package
    show_methods = function() {
      cat("===============================================================\n")
      cat("   METHODES DE CLUSTERING DISPONIBLES\n")
      cat("       (Clustering de Variables)\n")
      cat("===============================================================\n\n")
      
      cat("1. VAR_CAH\n")
      cat("   - Classification Ascendante Hierarchique sur variables\n")
      cat("   - Type de donnees : Numeriques\n")
      cat("   - Utilisation : factory$create_var_cah(X, k = 3)\n\n")
      
      cat("2. VAR_KMEANS\n")
      cat("   - K-Means pour clustering de variables\n")
      cat("   - Type de donnees : Numeriques\n")
      cat("   - Reallocation iterative avec optimisation\n")
      cat("   - Utilisation : factory$create_var_kmeans(X, k = 3, n_init = 10)\n\n")
      
      cat("3. TandemVarClust\n")
      cat("   - Tandem MCA + HAC pour variables mixtes\n")
      cat("   - Type de donnees : Mixtes (quantitatives + qualitatives)\n")
      cat("   - Utilisation : factory$create_tandem_varclust(X, k = 3)\n\n")
            
      cat("===============================================================\n")
      invisible(self)
    },
    
    #' @description Exporter les resultats du clustering
    #' @param objet_clustering Objet de clustering ajuste
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
        message(paste("Resultats exportes dans:", file))
        
        invisible(results)
      }, error = function(e) {
        stop(paste("Erreur lors de l'export:", e$message))
      })
    }
  )
)