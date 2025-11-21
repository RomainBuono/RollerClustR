
# ==============================================================================
# CHARGEMENT DES PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(plotly)
  library(DT)
  library(ggplot2)
  library(R6)
  library(cluster)
  library(factoextra)
  library(readxl)
  library(writexl)
  library(gridExtra)
  library(tsne)
  library(umap)
})

source("user_functions.R")
source("ClusterAnalysis.R") 
source("VAR_CAH.R")           # Votre classe VAR_CAH
source("KmodesVarClust.R")    # Votre classe KmodesVarClust  
source("VARCLUS.R")           # Votre classe VARCLUS

# -------------------------
# Adaptateur R6 -> structure attendue par server.R
# -------------------------
# Utilitaire pour récupérer une propriété active si existe, sinon fallback
.r6_get <- function(obj, names_vec) {
  for (nm in names_vec) {
    if (!is.null(obj) && nm %in% names(obj)) {
      # si c'est une active binding ou champ public, l'accès renverra la valeur
      val <- tryCatch(obj[[nm]], error = function(e) NULL)
      if (!is.null(val)) return(val)
    }
    # si méthode publique (get_k, get_clusters, etc.)
    if (!is.null(obj) && is.function(obj[[nm]])) {
      val <- tryCatch(obj[[nm]](), error = function(e) NULL)
      if (!is.null(val)) return(val)
    }
  }
  return(NULL)
}

# Calcul rapide des métriques de clustering (fallback si ton package ne les fournit pas)
.compute_metrics <- function(clusters, data_used) {
  k <- ifelse(is.null(clusters), 0, length(unique(na.omit(clusters))))
  res <- list(
    silhouette = NA_real_,
    davies_bouldin = NA_real_,
    dunn = NA_real_,
    calinski_harabasz = NA_real_
  )
  # silhouette seulement si numeric data and at least 2 clusters
  if (!is.null(data_used) && ncol(data_used) >= 1 && !is.null(clusters)) {
    try({
      # silhouette on variables: on doit transposer pour observations = variables
      if (all(sapply(data_used, is.numeric)) && k >= 2) {
        # utiliser dist entre variables (1 - abs(cor))
        cor_mat <- cor(data_used, use = "pairwise.complete.obs")
        dmat <- as.dist(1 - abs(cor_mat))
        # create cluster labels per variable
        sil <- cluster::silhouette(clusters, dmat)
        res$silhouette <- mean(sil[, "sil_width"], na.rm = TRUE)
      }
    }, silent = TRUE)
  }
  return(res)
}

# Fonction principale: transforme un objet R6 en structure 'rv$model' attendue
model_to_shiny <- function(model_obj, algorithm = NA_character_, data_used = NULL) {
  if (is.null(model_obj)) return(NULL)
  
  # Try common names: K / k / get_k
  k_val <- .r6_get(model_obj, c("K", "k", "get_k"))
  # Try groups/clusters: Groupes / groupes / Groupes() / clusters
  clusters_val <- .r6_get(model_obj, c("Groupes", "groupes", "Groupes()", "clusters", "get_clusters"))
  # If clusters are an R6 active binding returning named integer vector, use it as is.
  
  # If clusters not present but model has FGroupes private (rare), try extraction
  if (is.null(clusters_val)) {
    # attempt to call a self$Groupes if exists
    clusters_val <- tryCatch(model_obj$Groupes, error = function(e) NULL)
  }
  
  # ensure clusters are a named integer vector
  if (!is.null(clusters_val) && is.factor(clusters_val)) {
    clusters_val <- as.integer(as.character(clusters_val))
  }
  
  # compute fallback metrics
  metrics <- .compute_metrics(clusters_val, data_used)
  
  shiny_model <- list(
    algorithm = ifelse(is.na(algorithm), class(model_obj)[1], algorithm),
    method = NA_character_,
    k = ifelse(is.null(k_val), ifelse(is.null(clusters_val), NA_integer_, max(as.integer(clusters_val), na.rm = TRUE)), as.integer(k_val)),
    clusters = clusters_val,
    silhouette_avg = metrics$silhouette,
    metrics = list(
      davies_bouldin = metrics$davies_bouldin,
      dunn = metrics$dunn,
      calinski_harabasz = metrics$calinski_harabasz,
      silhouette = metrics$silhouette
    ),
    data_used = data_used,
    timestamp = Sys.time(),
    model_object = model_obj
  )
  return(shiny_model)
}



# ==============================================================================
# FONCTION DE GÉNÉRATION DE DONNÉES
# ==============================================================================

generate_sample_data <- function(type = "economic", n = 100, noise = 0.1, seed = 42) {
  set.seed(seed)
  
  data <- switch(type,
                 
                 "economic" = {
                   df <- data.frame(
                     PIB = rnorm(n, 100, 15), Revenu = rnorm(n, 50, 10), Emploi = rnorm(n, 75, 12),
                     Consommation = rnorm(n, 80, 14), Population = rnorm(n, 1000, 200),
                     Natalite = rnorm(n, 15, 3), Mortalite = rnorm(n, 10, 2), Migration = rnorm(n, 5, 2),
                     Temperature = rnorm(n, 20, 5), Precipitation = rnorm(n, 800, 150),
                     Pollution = rnorm(n, 50, 15), Energie = rnorm(n, 100, 20)
                   )
                   df$Revenu        <- df$PIB * 0.7 + rnorm(n, 0, 5)
                   df$Emploi        <- df$PIB * 0.6 + rnorm(n, 0, 8)
                   df$Natalite      <- df$Population * 0.01 + rnorm(n, 0, 2)
                   df$Precipitation <- df$Temperature * (-20) + rnorm(n, 0, 50)
                   df
                 },
                 
                 "biological" = {
                   df <- data.frame(
                     Gene_METAB_1 = rnorm(n, 5, 1), Gene_METAB_2 = rnorm(n, 4.8, 0.9), Gene_METAB_3 = rnorm(n, 5.2, 1.1),
                     Gene_GROWTH_1 = rnorm(n, 8, 1.5), Gene_GROWTH_2 = rnorm(n, 7.5, 1.3), Gene_GROWTH_3 = rnorm(n, 8.5, 1.6),
                     Gene_STRESS_1 = rnorm(n, 3, 0.8), Gene_STRESS_2 = rnorm(n, 2.8, 0.7), Gene_STRESS_3 = rnorm(n, 3.2, 0.9),
                     Gene_IMMUNE_1 = rnorm(n, 6, 1.2), Gene_IMMUNE_2 = rnorm(n, 6.3, 1.1), Gene_IMMUNE_3 = rnorm(n, 5.7, 1.3)
                   )
                   for (i in 1:n) df[i, 1:3] <- df[i, 1:3] + rnorm(1, 0, 0.3)
                   df
                 },
                 
                 "marketing" = {
                   df <- data.frame(
                     Visites_Site = rpois(n, 20), Temps_Site_min = rnorm(n, 15, 5), Pages_Vues = rpois(n, 10),
                     Montant_Achats = rnorm(n, 200, 80), Frequence_Achats = rpois(n, 4), Panier_Moyen = rnorm(n, 50, 20),
                     Note_Satisfaction = rnorm(n, 7.5, 1.5), NPS = rnorm(n, 6, 2)
                   )
                   df$Temps_Site_min <- df$Visites_Site * 0.6 + rnorm(n, 0, 3)
                   df
                 },
                 
                 "mixed" = {
                   data.frame(
                     Age = sample(18:80, n, replace = TRUE),
                     Revenu = rnorm(n, 35000, 15000),
                     Score = rnorm(n, 70, 15),
                     Categorie = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
                     Niveau = factor(sample(c("Débutant", "Intermédiaire", "Expert"), n, replace = TRUE))
                   )
                 },
                 
                 "categorical" = {
                   data.frame(
                     Secteur = factor(sample(c("Tech", "Finance", "Santé", "Industrie"), n, replace = TRUE)),
                     Taille_Entreprise = factor(sample(c("TPE", "PME", "ETI", "GE"), n, replace = TRUE)),
                     Region = factor(sample(c("Nord", "Sud", "Est", "Ouest", "Centre"), n, replace = TRUE)),
                     Type_Client = factor(sample(c("Particulier", "Professionnel", "Entreprise"), n, replace = TRUE)),
                     Niveau_Satisfaction = factor(sample(c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé"), n, replace = TRUE)),
                     Produit_Principal = factor(sample(c("A", "B", "C", "D", "E"), n, replace = TRUE)),
                     Canal_Acquisition = factor(sample(c("Web", "Téléphone", "Magasin", "Partenaire"), n, replace = TRUE)),
                     Statut = factor(sample(c("Actif", "Inactif", "Suspendu"), n, replace = TRUE))
                   )
                 }
  )
  
  # --- Injection NA SÉCURISÉE ---
  if (noise > 0) {
    total_cells <- nrow(data) * ncol(data)
    na_positions <- sample(total_cells, size = round(total_cells * noise * 0.1))
    
    rows <- ((na_positions - 1) %% nrow(data)) + 1
    cols <- ((na_positions - 1) %/% nrow(data)) + 1
    
    for (k in seq_along(rows)) {
      data[rows[k], cols[k]] <- NA
    }
  }
  
  return(data)
}
