#' =========================================
#' FONCTIONS UTILISATEUR AMÉLIORÉES
#' Package de clustering R6
#' =========================================

#' Effectuer un clustering simplifié
#' 
#' @description Fonction simplifiée pour réaliser un clustering. Choisit automatiquement
#' l'algorithme selon le type de données ou permet de le spécifier.
#' 
#' @param data Data frame contenant les données à clusteriser
#' @param k Nombre de groupes souhaités (défaut: 3)
#' @param method Méthode de clustering : "auto" (défaut), "cah_kmeans", "kmeans", "clustofvar" ou "kprototypes"
#' @param standardiser Standardiser les variables numériques ? (défaut: TRUE)
#' @param ... Arguments supplémentaires passés à l'algorithme
#' 
#' @return Un objet de clustering (CAH_Kmeans, Kmeans, ClustOfVar ou Kprototypes)
#' 
#' @export
#' @examples
#' # Clustering automatique sur iris
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' 
#' # Forcer l'utilisation de CAH+K-means
#' resultat <- faire_clustering(iris[1:4], k = 3, method = "cah_kmeans")
#' 
#' # Clustering de variables avec ClustOfVar
#' resultat <- faire_clustering(iris[1:4], k = 3, method = "clustofvar")
#' 
#' # Données mixtes
#' data_mixte <- data.frame(
#'  age = c(25, 30, 35, 40),
#'  sexe = factor(c("H", "F", "H", "F"))
#' )
#' resultat <- faire_clustering(data_mixte, k = 2)
#'
#' 
#' 
faire_clustering <- function(data, k = 3, method = "auto", standardiser = TRUE, ...) {
 # Validation
 if (!is.data.frame(data)) {
  stop("'data' doit être un data frame")
 }
 
 if (k < 2) {
  stop("'k' doit être au moins 2")
 }
 
 # Créer une factory
 factory <- ClusteringFactory$new()
 
 # Déterminer la méthode
 if (method == "auto") {
  n_numeric <- sum(sapply(data, is.numeric))
  n_total <- ncol(data)
  
  if (n_numeric == n_total) {
   method <- "cah_kmeans" # Par défaut CAH+K-means pour données numériques
   message("Méthode automatique : CAH+K-means sélectionnée (données numériques)")
  } else {
   method <- "kprototypes"
   message("Méthode automatique : K-prototypes sélectionné (données mixtes/catégorielles)")
  }
 }
 
 # Créer l'objet selon la méthode
 obj <- switch(method,
  cah_kmeans = factory$create_cah_kmeans(data, k = k, cr = standardiser, fit_now = TRUE, ...),
  kmeans = factory$create_kmeans(data, k = k, cr = standardiser, fit_now = TRUE, ...),
  clustofvar = factory$create_clustofvar(data, k = k, cr = standardiser, fit_now = TRUE, ...),
  kprototypes = factory$create_kprototypes(data, k = k, cr = standardiser, fit_now = TRUE, ...),
  stop("Méthode inconnue. Utilisez 'auto', 'cah_kmeans', 'kmeans', 'clustofvar' ou 'kprototypes'")
 )
 
 return(obj)
}


#' Analyser des variables illustratives
#' 
#' @description Utilise la méthode predict() pour analyser des variables illustratives
#' après avoir effectué un clustering
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' @param variables_illustratives Data frame avec les variables illustratives
#' @param afficher Afficher les résultats ? (défaut: TRUE)
#' 
#' @return Data frame avec les indicateurs de liaison
#' 
#' @export
#' @examples
#' # Clustering sur les dimensions d'iris
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' 
#' # Analyser Species comme variable illustrative
#' analyse <- analyser_illustratives(resultat, iris[5])
#' print(analyse)
analyser_illustratives <- function(objet_clustering, 
                 variables_illustratives, 
                 afficher = TRUE) {
 # Validation
 if (!inherits(objet_clustering, "ClusterAnalysis")) {
  stop("objet_clustering doit hériter de ClusterAnalysis")
 }
 
 if (!is.data.frame(variables_illustratives)) {
  variables_illustratives <- as.data.frame(variables_illustratives)
 }
 
 # Utiliser predict()
 resultats <- objet_clustering$predict(variables_illustratives)
 
 # Affichage optionnel
 if (afficher) {
  cat("=== ANALYSE DES VARIABLES ILLUSTRATIVES ===\n\n")
  print(resultats)
  cat("\n")
  
  # Interprétation
  cat("Interprétation :\n")
  for (i in 1:nrow(resultats)) {
   cat("- ", resultats$variable[i], " (", resultats$type[i], ") : ",
     resultats$interpretation[i], " (", 
     resultats$indicateur[i], " = ", 
     round(resultats$valeur[i], 3), ")\n", sep = "")
  }
 }
 
 return(invisible(resultats))
}


#' Caractériser les groupes avec une variable illustrative
#' 
#' @description Version améliorée utilisant predict() en plus des méthodes classiques
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' @param variable Variable illustrative (vecteur numérique ou factor)
#' @param nom_variable Nom de la variable (pour l'affichage)
#' @param mode Mode d'analyse : "complet" (défaut) ou "rapide"
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' caracteriser_groupes(resultat, iris$Species, "Species")
caracteriser_groupes <- function(objet_clustering, variable, 
                nom_variable = "Variable",
                mode = "complet") {
 cat("=== CARACTÉRISATION AVEC", nom_variable, "===\n\n")
 
 # Utiliser predict() pour l'analyse rapide
 if (mode == "rapide") {
  df_var <- data.frame(var = variable)
  names(df_var) <- nom_variable
  resultats <- objet_clustering$predict(df_var)
  print(resultats)
  return(invisible(resultats))
 }
 
 # Mode complet avec détails
 if (is.numeric(variable)) {
  # Variable quantitative
  cat("Type : Variable quantitative\n\n")
  
  # Rapport de corrélation
  rapport <- objet_clustering$rapCorrIllusQuanti(variable)
  cat("Rapport de corrélation (eta²) :", round(rapport, 4), "\n")
  cat("Interprétation : ", 
    if (rapport > 0.5) "forte liaison" 
    else if (rapport > 0.2) "liaison modérée" 
    else "liaison faible",
    "\n\n")
  
  # Statistiques par groupe
  cat("Statistiques descriptives par groupe :\n")
  groupes <- objet_clustering$Groupes
  stats <- data.frame(
   groupe = sort(unique(groupes)),
   n = as.numeric(table(groupes)),
   moyenne = tapply(variable, groupes, mean),
   ecart_type = tapply(variable, groupes, sd),
   min = tapply(variable, groupes, min),
   max = tapply(variable, groupes, max)
  )
  rownames(stats) <- NULL
  print(stats)
  cat("\n")
  
  # Valeurs-test
  cat("Valeurs-test :\n")
  objet_clustering$vTestIllusQuanti(variable)
  
 } else if (is.factor(variable)) {
  # Variable qualitative
  cat("Type : Variable qualitative\n\n")
  
  # V de Cramer
  cramer <- objet_clustering$cramerIllusQuali(variable)
  cat("V de Cramer :", round(cramer, 4), "\n")
  cat("Interprétation : ", 
    if (cramer > 0.5) "forte association" 
    else if (cramer > 0.2) "association modérée" 
    else "association faible",
    "\n\n")
  
  # Table de contingence
  cat("Table de contingence :\n")
  tab <- table(Groupe = objet_clustering$Groupes, Variable = variable)
  print(tab)
  cat("\n")
  
  # Pourcentages
  cat("Pourcentages en ligne (% par groupe) :\n")
  print(round(prop.table(tab, 1) * 100, 1))
  cat("\n")
  
  # Valeurs-test pour chaque modalité
  cat("Valeurs-test par modalité :\n")
  for (mod in 1:nlevels(variable)) {
   cat("\nModalité '", levels(variable)[mod], "' :\n", sep = "")
   objet_clustering$vTestIllusQuali(variable, numModa = mod)
  }
  
 } else {
  stop("La variable doit être numérique ou factor")
 }
 
 invisible(objet_clustering)
}


#' Trouver le nombre optimal de groupes
#' 
#' @description Teste plusieurs valeurs de k et aide à déterminer le nombre optimal de groupes
#' 
#' @param data Data frame contenant les données
#' @param k_min Nombre minimum de groupes à tester (défaut: 2)
#' @param k_max Nombre maximum de groupes à tester (défaut: 10)
#' @param method Méthode : "kmeans", "cah_kmeans", "clustofvar" ou "kprototypes" (défaut: "cah_kmeans")
#' @param afficher_graphique Afficher le graphique automatiquement ? (défaut: TRUE)
#' @param ... Arguments supplémentaires
#' 
#' @return Data frame avec les résultats pour chaque k
#' 
#' @export
#' @examples
#' # Trouver k optimal avec CAH+K-means
#' resultats <- trouver_k_optimal(iris[1:4], k_min = 2, k_max = 8)
#' 
#' # Avec K-means standard
#' resultats <- trouver_k_optimal(iris[1:4], method = "kmeans")
#' 
#' # Avec ClustOfVar
#' resultats <- trouver_k_optimal(iris[1:4], method = "clustofvar")
trouver_k_optimal <- function(data, k_min = 2, k_max = 10, 
              method = "cah_kmeans", 
              afficher_graphique = TRUE, ...) {
 evaluator <- ClusteringEvaluator$new(data)
 resultats <- evaluator$evaluate_k(k_range = k_min:k_max, method = method, ...)
 
 if (afficher_graphique && "inertie_expliquee" %in% names(resultats)) {
  evaluator$plot_evaluation(resultats, criterion = "inertie_expliquee")
 }
 
 return(resultats)
}


#' Comparer plusieurs algorithmes de clustering
#' 
#' @description Compare les résultats de différents algorithmes sur les mêmes données
#' 
#' @param data Data frame avec les données
#' @param k Nombre de groupes
#' @param methods Vecteur des méthodes à comparer (ex: c("cah_kmeans", "kmeans", "clustofvar"))
#' @param ... Arguments supplémentaires
#' 
#' @return Objet ClusteringComparator avec résultats
#' 
#' @export
#' @examples
#' # Comparer CAH+K-means et K-means standard
#' comparaison <- comparer_algorithmes(iris[1:4], k = 3, 
#'                  methods = c("cah_kmeans", "kmeans"))
#' 
#' # Comparer avec ClustOfVar
#' comparaison <- comparer_algorithmes(iris[1:4], k = 3,
#'                  methods = c("cah_kmeans", "clustofvar"))
#' resultats <- comparaison$compare()
comparer_algorithmes <- function(data, k = 3, 
                methods = c("cah_kmeans", "kmeans"), ...) {
 comparator <- ClusteringComparator$new(data, k = k)
 
 for (method in methods) {
  comparator$add_algorithm(method, ...)
 }
 
 return(comparator)
}


#' Obtenir les groupes d'un clustering
#' 
#' @description Extrait le vecteur des groupes d'un objet de clustering
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' 
#' @return Vecteur des groupes
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' groupes <- obtenir_groupes(resultat)
obtenir_groupes <- function(objet_clustering) {
 return(objet_clustering$Groupes)
}


#' Exporter les résultats du clustering
#' 
#' @description Exporte les groupes et optionnellement les données dans un data frame ou CSV
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' @param donnees Data frame original (optionnel)
#' @param inclure_donnees Inclure les données originales ? (défaut: TRUE)
#' @param fichier Nom du fichier CSV de sortie (NULL = pas d'export)
#' 
#' @return Data frame avec les résultats
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' 
#' # Exporter dans un data frame
#' resultats_df <- exporter_resultats(resultat, iris)
#' 
#' # Exporter dans un fichier CSV
#' exporter_resultats(resultat, iris, fichier = "resultats_clustering.csv")
exporter_resultats <- function(objet_clustering, donnees = NULL, 
               inclure_donnees = TRUE, fichier = NULL) {
 helper <- ClusteringHelper$new()
 resultats <- helper$export_results(objet_clustering, donnees, inclure_donnees)
 
 if (!is.null(fichier)) {
  write.csv(resultats, file = fichier, row.names = FALSE)
  message("Résultats exportés dans : ", fichier)
 }
 
 return(resultats)
}


#' Visualiser un clustering
#' 
#' @description Affiche une visualisation appropriée selon le type de clustering
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' @param ... Arguments supplémentaires passés à la méthode plot()
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3, method = "cah_kmeans")
#' visualiser_clustering(resultat)
#' 
#' # Avec ClustOfVar - visualisation heatmap
#' resultat <- faire_clustering(iris[1:4], k = 3, method = "clustofvar")
#' visualiser_clustering(resultat, type = "heatmap")
visualiser_clustering <- function(objet_clustering, ...) {
 if (inherits(objet_clustering, "CAH_Kmeans")) {
  objet_clustering$plot(showGroups = TRUE, ...)
 } else {
  objet_clustering$plot(...)
 }
 invisible(objet_clustering)
}


#' Afficher un résumé du clustering
#' 
#' @description Affiche les informations principales du clustering
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' resumer_clustering(resultat)
resumer_clustering <- function(objet_clustering) {
  # Utiliser summary()
  objet_clustering$summary()
  
  # Informations supplémentaires selon le type
  if (inherits(objet_clustering, "ClustOfVar")) {
    # Cas spécifique ClustOfVar : Afficher la distribution des variables par cluster
    # (pour corriger/doubler l'affichage de la classe mère qui peut afficher 'obs.' par erreur)
    if (!is.null(objet_clustering$Groupes)) {
      groupes <- objet_clustering$Groupes
      total_vars <- length(groupes)
      distrib_vars <- table(groupes)
      
      cat("\n--- Distribution des variables (ClustOfVar) ---\n")
      for (k in names(distrib_vars)) {
        n_vars <- distrib_vars[k]
        pourcentage <- round((n_vars / total_vars) * 100)
        cat(sprintf("  Cluster %s : %s variable(s) (%s %%)\n", k, n_vars, pourcentage))
      }
    }
    
    # Affichage de l'homogénéité (déjà dans summary, mais pour être complet)
    tryCatch({
      cat("\n--- Homogénéité (ClustOfVar) ---\n")
      cat("Homogénéité globale (somme des liaisons intra-cluster) :", 
          round(objet_clustering$Homogeneite, 4), "\n")
    }, error = function(e) {})
    
  } else if (inherits(objet_clustering, c("Kmeans", "CAH_Kmeans"))) {
    # K-means ou CAH+K-means : Afficher l'inertie expliquée
    tryCatch({
      cat("\n--- Qualité du clustering ---\n")
      inertie <- objet_clustering$inertie()
      cat("Inertie expliquée :", round(inertie$pct_expliquee, 2), "%\n")
    }, error = function(e) {})
    
  } else if (inherits(objet_clustering, "Kprototypes")) {
    # K-prototypes : Afficher les prototypes
    cat("\n--- Prototypes des groupes ---\n")
    objet_clustering$prototypes()
  }
  
  invisible(objet_clustering)
}


#' Calculer des statistiques par groupe
#' 
#' @description Calcule des statistiques descriptives pour chaque groupe
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' @param donnees Data frame avec les variables à analyser
#' @param afficher Afficher les résultats ? (défaut: TRUE)
#' 
#' @return Liste de data frames avec les statistiques par groupe
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' stats <- statistiques_par_groupe(resultat, iris[1:4])
#' 
#' # Voir les stats pour une variable
#' print(stats$Sepal.Length)
statistiques_par_groupe <- function(objet_clustering, donnees, afficher = TRUE) {
 helper <- ClusteringHelper$new()
 stats <- helper$group_statistics(objet_clustering, donnees)
 
 if (afficher) {
  cat("=== STATISTIQUES PAR GROUPE ===\n\n")
  for (var_name in names(stats)) {
   cat("📊 Variable :", var_name, "\n")
   print(stats[[var_name]])
   cat("\n")
  }
 }
 
 return(invisible(stats))
}


#' Générer un rapport de clustering
#' 
#' @description Génère un rapport textuel complet du clustering
#' 
#' @param objet_clustering Objet retourné par faire_clustering()
#' @param fichier Nom du fichier de sortie (NULL = afficher à l'écran)
#' 
#' @export
#' @examples
#' resultat <- faire_clustering(iris[1:4], k = 3)
#' 
#' # Afficher à l'écran
#' generer_rapport(resultat)
#' 
#' # Sauvegarder dans un fichier
#' generer_rapport(resultat, fichier = "rapport_clustering.txt")
generer_rapport <- function(objet_clustering, fichier = NULL) {
 helper <- ClusteringHelper$new()
 helper$generate_report(objet_clustering, file = fichier)
 invisible(objet_clustering)
}


#' Workflow complet de clustering
#' 
#' @description Fonction tout-en-un pour un workflow complet :
#' 1. Trouver k optimal
#' 2. Faire le clustering
#' 3. Générer les résultats
#' 4. Analyser les variables illustratives (optionnel)
#' 
#' @param data Data frame avec variables actives
#' @param variables_illustratives Data frame optionnel avec variables illustratives
#' @param k_min Minimum de k à tester (défaut: 2)
#' @param k_max Maximum de k à tester (défaut: 6)
#' @param k_final Nombre de groupes final (NULL = utilise k optimal)
#' @param method Méthode : "cah_kmeans" (défaut), "kmeans", "clustofvar" ou "kprototypes"
#' @param fichier_resultats Fichier CSV de sortie (NULL = pas de sauvegarde)
#' @param fichier_rapport Fichier texte de rapport (NULL = pas de sauvegarde)
#' 
#' @return Liste avec l'objet clustering et les résultats
#' 
#' @export
#' @examples
#' # Workflow complet automatique
#' workflow <- clustering_complet(iris[1:4], k_max = 5)
#' 
#' # Workflow avec variables illustratives
#' workflow <- clustering_complet(
#'  iris[1:4], 
#'  variables_illustratives = iris[5],
#'  fichier_resultats = "iris_groupes.csv"
#' )
#' 
#' # Workflow avec ClustOfVar
#' workflow <- clustering_complet(iris[1:4], method = "clustofvar")
clustering_complet <- function(data, 
               variables_illustratives = NULL,
               k_min = 2, 
               k_max = 6,
               k_final = NULL,
               method = "cah_kmeans",
               fichier_resultats = NULL,
               fichier_rapport = NULL) {
 
 cat("╔══════════════════════════════════════════╗\n")
 cat("║ WORKFLOW COMPLET DE CLUSTERING     ║\n")
 cat("╚══════════════════════════════════════════╝\n\n")
 
 # Étape 1 : Trouver k optimal
 cat("📍 Étape 1/4 : Recherche du k optimal...\n")
 resultats_k <- trouver_k_optimal(data, k_min = k_min, k_max = k_max, 
                 method = method, afficher_graphique = TRUE)
 
 # Déterminer k
 if (is.null(k_final)) {
  evaluator <- ClusteringEvaluator$new(data)
  k_final <- evaluator$get_best_k(resultats_k)
  if (is.na(k_final)) k_final <- 3
  cat("\n✓ K optimal sélectionné automatiquement : k =", k_final, "\n\n")
 }
 
 # Étape 2 : Clustering final
 cat("📍 Étape 2/4 : Clustering avec k =", k_final, "...\n")
 objet_clustering <- faire_clustering(data, k = k_final, method = method)
 resumer_clustering(objet_clustering)
 
 # Étape 3 : Variables illustratives
 resultats_illustratives <- NULL
 if (!is.null(variables_illustratives)) {
  cat("\n📍 Étape 3/4 : Analyse des variables illustratives...\n")
  resultats_illustratives <- analyser_illustratives(
   objet_clustering, 
   variables_illustratives,
   afficher = TRUE
  )
 } else {
  cat("\n⏭ Étape 3/4 : Pas de variables illustratives\n")
 }
 
 # Étape 4 : Export
 cat("\n📍 Étape 4/4 : Export des résultats...\n")
 resultats_df <- exporter_resultats(objet_clustering, data, 
                  fichier = fichier_resultats)
 
 if (!is.null(fichier_rapport)) {
  generer_rapport(objet_clustering, fichier = fichier_rapport)
 }
 
 cat("\n╔══════════════════════════════════════════╗\n")
 cat("║ ✓ WORKFLOW TERMINÉ AVEC SUCCÈS     ║\n")
 cat("╚══════════════════════════════════════════╝\n")
 
 return(list(
  objet = objet_clustering,
  groupes = obtenir_groupes(objet_clustering),
  resultats = resultats_df,
  evaluation_k = resultats_k,
  illustratives = resultats_illustratives
 ))
}


#' Créer un pipeline de clustering personnalisé
#' 
#' @description Crée un objet pipeline réutilisable pour le clustering
#' 
#' @param method Méthode : "cah_kmeans", "kmeans", "clustofvar" ou "kprototypes"
#' @param k Nombre de groupes
#' @param ... Arguments supplémentaires
#' 
#' @return Fonction qui peut être appliquée à différents datasets
#' 
#' @export
#' @examples
#' # Créer un pipeline CAH+K-means avec k=3
#' pipeline <- creer_pipeline("cah_kmeans", k = 3)
#' 
#' # Créer un pipeline ClustOfVar avec k=4
#' pipeline <- creer_pipeline("clustofvar", k = 4, max_iter = 50)
#' 
#' # Appliquer à différents datasets
#' resultat1 <- pipeline(iris[1:4])
#' resultat2 <- pipeline(mtcars[1:7])
creer_pipeline <- function(method = "cah_kmeans", k = 3, ...) {
 # Capturer les arguments
 args <- list(...)
 
 # Créer la fonction pipeline
 pipeline_func <- function(data) {
  factory <- ClusteringFactory$new()
  
  obj <- switch(method,
   cah_kmeans = do.call(factory$create_cah_kmeans, c(list(X = data, k = k), args)),
   kmeans = do.call(factory$create_kmeans, c(list(X = data, k = k), args)),
   clustofvar = do.call(factory$create_clustofvar, c(list(X = data, k = k), args)),
   kprototypes = do.call(factory$create_kprototypes, c(list(X = data, k = k), args)),
   stop("Méthode inconnue")
  )
  
  return(obj)
 }
 
 # Ajouter des attributs informatifs
 attr(pipeline_func, "method") <- method
 attr(pipeline_func, "k") <- k
 attr(pipeline_func, "args") <- args
 
 return(pipeline_func)
}