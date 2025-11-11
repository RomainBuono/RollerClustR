#=========================================
# WRAPPER R6 : FACTORY, COMPARATOR, EVALUATOR, HELPER
# Package de clustering R6
#=========================================


# Définition de la classe R6 (nécessite le package R6)
# library(R6) 

# ---------------------------------
# 1. ClusteringFactory (Création des objets)
# ---------------------------------

#' @title ClusteringFactory
#' @description Factory pour créer et initialiser les objets de clustering
ClusteringFactory <- R6Class("ClusteringFactory",
  public = list(
    
    #' @description Initialiser
    initialize = function() {
      message("ClusteringFactory initialisée")
    },
    
    #' @description Créer et ajuster un objet CAH_Kmeans
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters
    #' @param cr Booléen, centrer et réduire les données
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit()
    #' @return Objet CAH_Kmeans
    create_cah_kmeans = function(X, k = 2, cr = TRUE, fit_now = TRUE, ...) {
      # Assurez-vous que la classe CAH_Kmeans est disponible (définit ailleurs)
      obj <- CAH_Kmeans$new(k = k, cr = cr, ...)
      if (fit_now) {
        # On passe X et les arguments additionnels restants à $fit()
        obj$fit(X, ...)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet Kmeans
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters
    #' @param cr Booléen, centrer et réduire les données
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit()
    #' @return Objet Kmeans
    create_kmeans = function(X, k = 3, cr = TRUE, fit_now = TRUE, ...) {
      # Assurez-vous que la classe Kmeans est disponible (définit ailleurs)
      obj <- Kmeans$new(k = k, cr = cr, ...)
      if (fit_now) {
        # On passe X et les arguments additionnels restants à $fit()
        obj$fit(X, ...)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet ClustOfVar
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit()
    #' @return Objet ClustOfVar
    create_clustofvar = function(X, k = 2, fit_now = TRUE, ...) {
      # REMARQUE : Le paramètre 'cr' (centrage/réduction) est retiré de la signature
      # car il n'est pas utilisé par ClustOfVar$new() (la standardisation est interne).
      # Cela évite la redondance et clarifie l'API pour cette méthode.
      
      # Assurez-vous que la classe ClustOfVar est disponible (définit ailleurs)
      obj <- ClustOfVar$new(k = k, ...)
      
      if (fit_now) {
        # ClustOfVar$fit prend X, max_iter, tolerance etc.
        obj$fit(X, ...)
      }
      return(obj)
    },
    
    #' @description Créer et ajuster un objet Kprototypes
    #' @param X Data frame ou matrice de données
    #' @param k Nombre de clusters
    #' @param lambda Poids relatif des variables catégorielles (défaut: 0.5)
    #' @param fit_now Booléen, ajuster le modèle immédiatement
    #' @param ... Paramètres additionnels passés à $new() ou $fit()
    #' @return Objet Kprototypes
    create_kprototypes = function(X, k = 3, lambda = 0.5, fit_now = TRUE, ...) {
      # Assurez-vous que la classe Kprototypes est disponible (définit ailleurs)
      obj <- Kprototypes$new(k = k, lambda = lambda, ...)
      
      if (fit_now) {
        # On passe X et les arguments additionnels restants à $fit()
        obj$fit(X, ...)
      }
      return(obj)
    }
    
    # ... autres méthodes de la classe (ClusteringComparator, etc.)
  ),
  
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
  ),
  
  # ---------------------------------
  # 3. ClusteringEvaluator (Evaluation du nombre de clusters)
  # ---------------------------------
  
  #' @title ClusteringEvaluator
  #' @description Outil pour évaluer le nombre optimal de clusters
  ClusteringEvaluator <- R6Class("ClusteringEvaluator",
    public = list(
      
      #' @description Initialiser
      initialize = function() {
        message("ClusteringEvaluator initialisé")
      },
      
      #' @description Méthode du coude (Elbow Method)
      #' @param X Data frame de données
      #' @param max_k Nombre maximal de clusters à tester
      #' @param method Méthode de clustering à utiliser (par défaut: "cah_kmeans")
      #' @param ... Paramètres additionnels passés à la Factory
      #' @return Liste contenant le graphique (ggplot) et les données
      elbow_method = function(X, max_k = 10, method = "cah_kmeans", ...) {
        
        if (max_k < 2) {
          stop("max_k doit être >= 2")
        }
        
        factory <- ClusteringFactory$new()
        inertias <- numeric(max_k)
        
        for (k in 2:max_k) {
          message(paste("Calcul pour k =", k))
          
          # Création et ajustement
          model <- switch(method,
            cah_kmeans = tryCatch({
              factory$create_cah_kmeans(X, k = k, fit_now = TRUE, ...)
            }, error = function(e) { warning(paste("Erreur CAH+Kmeans pour k=", k, ":", e$message)); NULL }),
            kmeans = tryCatch({
              factory$create_kmeans(X, k = k, fit_now = TRUE, ...)
            }, error = function(e) { warning(paste("Erreur Kmeans pour k=", k, ":", e$message)); NULL }),
            clustofvar = tryCatch({
              # Utilise la nouvelle signature sans 'cr'
              factory$create_clustofvar(X, k = k, fit_now = TRUE, ...) 
            }, error = function(e) { warning(paste("Erreur ClustOfVar pour k=", k, ":", e$message)); NULL }),
            kprototypes = tryCatch({
              factory$create_kprototypes(X, k = k, fit_now = TRUE, ...)
            }, error = function(e) { warning(paste("Erreur Kprototypes pour k=", k, ":", e$message)); NULL }),
            stop(paste("Méthode non reconnue:", method))
          )
          
          if (!is.null(model)) {
            # Extraction de l'inertie intra-cluster
            inertia_data <- tryCatch({
              model$inertie()$intra
            }, error = function(e) {
              NA
            })
            inertias[k] <- inertia_data
          }
        }
        
        # Préparation des données pour le graphique
        data_plot <- data.frame(k = 2:max_k, Inertie_Intra = inertias[2:max_k])
        
        # Création du graphique avec ggplot2 (nécessite le package ggplot2)
        # Utilisation de ggplot2 pour une meilleure visualisation
        if (requireNamespace("ggplot2", quietly = TRUE)) {
          p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = k, y = Inertie_Intra)) +
            ggplot2::geom_point(color = "steelblue", size = 3) +
            ggplot2::geom_line(color = "steelblue", size = 1) +
            ggplot2::labs(
              title = paste("Méthode du Coude pour", method),
              x = "Nombre de Clusters (k)",
              y = "Inertie Intra-Cluster"
            ) +
            ggplot2::theme_minimal() +
            ggplot2::scale_x_continuous(breaks = 2:max_k)
          
          return(list(plot = p, data = data_plot))
        } else {
          # Si ggplot2 n'est pas installé, retourne un graphique R de base et un avertissement
          warning("Le package 'ggplot2' n'est pas installé. Utilisation du graphique de base.")
          plot(data_plot$k, data_plot$Inertie_Intra, type = 'b', 
               main = paste("Méthode du Coude pour", method), 
               xlab = "Nombre de Clusters (k)", ylab = "Inertie Intra-Cluster",
               col = "steelblue", pch = 19)
          return(list(plot = NULL, data = data_plot))
        }
      }
    )
  ),
  
  # ---------------------------------
  # 4. ClusteringHelper (Fonctions utilitaires)
  # ---------------------------------
  
  #' @title ClusteringHelper
  #' @description Fonctions utilitaires pour le package
  ClusteringHelper <- R6Class("ClusteringHelper",
    public = list(
      
      #' @description Génère un rapport textuel synthétique du clustering
      #' @param objet_clustering Objet de clustering ajusté (e.g. CAH_Kmeans, Kmeans, ClustOfVar, Kprototypes)
      #' @param file Chemin du fichier où écrire le rapport (NULL pour affichage console)
      #' @return Invisiblement, l'objet lui-même. Affiche le rapport.
      generate_report = function(objet_clustering, file = NULL) {
        if (!is.null(file)) {
          sink(file = file)
        }
        
        cat("=========================================\n")
        cat("       RAPPORT D'ANALYSE DE CLUSTERING\n")
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
        
        # Infos sur les variables (spécifique à ClustOfVar)
        if (inherits(objet_clustering, "ClustOfVar")) {
          cat("\n--- Homogénéité des Clusters (ClustOfVar) ---\n")
          # Afficher la qualité des clusters (si disponible)
          print(objet_clustering$Homogeneite)
        }
        
        if (!is.null(file)) {
          sink() # Rétablit l'affichage console
          message(paste("Rapport généré dans:", file))
        }
        
        invisible(objet_clustering)
      }
    )
  )
)