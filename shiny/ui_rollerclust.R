# ==============================================================================
# APPLICATION SHINY COMPLÈTE - CLUSTERING DE VARIABLES
# Version finale avec toutes les fonctionnalités
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
  library(corrplot)
  library(rmarkdown)
  library(knitr)
})

# Décommenter pour charger vos classes
# source("ClusterAnalysis.R")
# source("VAR_CAH.R")
# source("KmodesVarClust.R")
# source("VARCLUS.R")
# source("utils_clustering.R")

# ==============================================================================
# FONCTIONS UTILITAIRES
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
                   df$Revenu <- df$PIB * 0.7 + rnorm(n, 0, 5)
                   df$Emploi <- df$PIB * 0.6 + rnorm(n, 0, 8)
                   df$Natalite <- df$Population * 0.01 + rnorm(n, 0, 2)
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
                   for (i in 1:n) { df[i, 1:3] <- df[i, 1:3] + rnorm(1, 0, 0.3) }
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
                     Age = sample(18:80, n, replace = TRUE), Revenu = rnorm(n, 35000, 15000), Score = rnorm(n, 70, 15),
                     Categorie = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
                     Niveau = factor(sample(c("Débutant", "Intermédiaire", "Expert"), n, replace = TRUE))
                   )
                 }
  )
  
  if (noise > 0) {
    na_indices <- sample(1:(nrow(data) * ncol(data)), size = round(nrow(data) * ncol(data) * noise * 0.1))
    data[na_indices] <- NA
  }
  return(data)
}

save_session <- function(rv, input, filename = "clustering_session.rds") {
  session_data <- list(
    timestamp = Sys.time(), data = rv$data, model = rv$model, clustering_done = rv$clustering_done,
    config = list(algorithm = input$algorithm, n_clusters = input$n_clusters, active_vars = input$active_vars)
  )
  saveRDS(session_data, file = filename)
  return(filename)
}

load_session <- function(filename) {
  if (!file.exists(filename)) stop("Fichier introuvable")
  return(readRDS(filename))
}

# ==============================================================================
# UI
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Clustering de Variables", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("🏠 Accueil", tabName = "home", icon = icon("home")),
      menuItem("📁 Données", tabName = "data", icon = icon("database")),
      menuItem("⚙️ Configuration", tabName = "config", icon = icon("cogs")),
      menuItem("🎯 Clustering", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("📈 Résultats", tabName = "results", icon = icon("chart-line")),
      menuItem("🔍 Diagnostics", tabName = "diagnostics", icon = icon("search")),
      menuItem("📥 Export", tabName = "export", icon = icon("download")),
      hr(),
      h5(icon("save"), strong(" Session"), style = "color: #3c8dbc; padding-left: 15px;"),
      div(style = "padding: 0 15px;",
          actionButton("save_session_btn", "💾 Sauvegarder", class = "btn-primary btn-sm btn-block", style = "margin-bottom: 5px;"),
          fileInput("load_session_file", NULL, accept = ".rds", buttonLabel = "📂 Charger", placeholder = "Session.rds")
      )
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(".box { border-top: 3px solid #3c8dbc; } .content-wrapper { background-color: #f4f6f9; }"))),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(box(title = "🎯 Bienvenue", width = 12, status = "primary", solidHeader = TRUE,
                           h4("Algorithmes : VAR_CAH, KmodesVarClust, VARCLUS"),
                           tags$ol(tags$li("Importez données"), tags$li("Configurez"), tags$li("Lancez"), tags$li("Analysez"), tags$li("Exportez"))
              )),
              fluidRow(infoBoxOutput("info_algorithms", width = 4), infoBoxOutput("info_data", width = 4), infoBoxOutput("info_status", width = 4))
      ),
      
      tabItem(tabName = "data",
              fluidRow(
                box(title = "📁 Import", width = 8, status = "primary", solidHeader = TRUE,
                    fileInput("file_input", "Fichier", accept = c(".csv", ".xlsx")),
                    radioButtons("file_type", "Type", choices = c("CSV (virgule)" = "csv_comma", "CSV (;)" = "csv_semicolon", "Excel" = "excel"))
                ),
                box(title = "🎲 Exemple", width = 4, status = "success", solidHeader = TRUE,
                    selectInput("sample_type", "Type", choices = c("Économiques" = "economic", "Biologiques" = "biological", "Marketing" = "marketing", "Mixtes" = "mixed")),
                    sliderInput("sample_n", "Obs", 50, 500, 100, 50),
                    actionButton("generate_sample", "🎲 Générer", class = "btn-success btn-block")
                )
              ),
              fluidRow(box(title = "🔍 Aperçu", width = 12, status = "info", solidHeader = TRUE, DTOutput("data_preview")))
      ),
      
      tabItem(tabName = "config",
              fluidRow(
                box(title = "⚙️ Algorithme", width = 6, status = "primary", solidHeader = TRUE,
                    selectInput("algorithm", "Choisir", choices = c("VAR_CAH" = "var_cah", "KmodesVarClust" = "kmodes", "VARCLUS" = "varclus")),
                    uiOutput("algorithm_description")
                ),
                box(title = "🎛️ Paramètres", width = 6, status = "info", solidHeader = TRUE,
                    conditionalPanel(condition = "input.algorithm != 'varclus'", sliderInput("n_clusters", "k", 2, 10, 3, 1)),
                    conditionalPanel(condition = "input.algorithm == 'varclus'", numericInput("stop_eigenvalue", "λ₂", 1.0, 0.1, 5, 0.1)),
                    checkboxInput("standardize", "Standardiser", TRUE)
                )
              ),
              fluidRow(box(title = "📋 Variables", width = 12, status = "success", solidHeader = TRUE,
                           uiOutput("active_vars_ui"), hr(),
                           actionButton("select_all", "✓ Tout", class = "btn-sm btn-info"),
                           actionButton("select_numeric", "🔢 Numériques", class = "btn-sm btn-success")
              ))
      ),
      
      tabItem(tabName = "clustering",
              fluidRow(box(title = "🎯 Lancement", width = 12, status = "primary", solidHeader = TRUE,
                           fluidRow(
                             column(6, h4("📋 Config"), verbatimTextOutput("config_summary")),
                             column(6, h4("🚀 Action"), br(), actionButton("run_clustering", "▶ LANCER", class = "btn-success btn-lg", style = "width:100%;height:80px;font-size:20px;"))
                           )
              )),
              fluidRow(valueBoxOutput("vbox_status", 4), valueBoxOutput("vbox_k", 4), valueBoxOutput("vbox_vars", 4))
      ),
      
      tabItem(tabName = "results",
              fluidRow(box(title = "📊 Résumé", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("model_summary"))),
              fluidRow(tabBox(title = "📈 Visualisations", width = 12,
                              tabPanel("Dendrogramme", plotOutput("plot_dendrogram", height = "600px")),
                              tabPanel("Heatmap", plotlyOutput("plot_heatmap", height = "600px")),
                              tabPanel("Distribution", plotlyOutput("plot_distribution", height = "500px"))
              )),
              fluidRow(box(title = "📋 Détails", width = 12, status = "success", solidHeader = TRUE, DTOutput("clusters_table")))
      ),
      
      tabItem(tabName = "diagnostics",
              fluidRow(box(title = "🔍 Analyse", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("diagnostics_text"))),
              fluidRow(box(title = "📊 Qualité", width = 12, status = "info", solidHeader = TRUE, plotlyOutput("quality_by_cluster", height = "400px")))
      ),
      
      tabItem(tabName = "export",
              fluidRow(box(title = "📥 Export", width = 12, status = "primary", solidHeader = TRUE,
                           h4("Téléchargements"), fluidRow(
                             column(4, downloadButton("download_clusters", "📋 CSV", class = "btn-primary btn-lg btn-block")),
                             column(4, downloadButton("download_plots", "📊 PNG", class = "btn-info btn-lg btn-block")),
                             column(4, downloadButton("download_pdf", "📄 PDF", class = "btn-success btn-lg btn-block"))
                           ), hr(),
                           textInput("pdf_title", "Titre PDF", "Rapport de Clustering"),
                           textInput("pdf_author", "Auteur", "Application Shiny")
              ))
      )
    )
  )
)

