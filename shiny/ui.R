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
})

# ==============================================================================
# CHARGEMENT DES CLASSES R6
# ==============================================================================

# Source des fichiers (à adapter selon votre structure)
# source("ClusterAnalysis.R")
# source("VarClustAdvanced.R")
# source("VAR_CAH.R")
# source("KmodesVarClust.R")
# source("VARCLUS.R")
# source("utils_clustering.R")

# ==============================================================================
# INTERFACE UTILISATEUR (UI)
# ==============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # ============================================================================
  # HEADER
  # ============================================================================
  dashboardHeader(
    title = "Clustering de Variables",
    titleWidth = 300,
    tags$li(
      class = "dropdown",
      tags$style(HTML("
        .main-header .logo { font-weight: bold; font-size: 20px; }
        .content-wrapper { background-color: #f4f6f9; }
      "))
    )
  ),
  
  # ============================================================================
  # SIDEBAR
  # ============================================================================
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      
      menuItem("📊 Accueil", tabName = "home", icon = icon("home")),
      menuItem("📁 Données", tabName = "data", icon = icon("database")),
      menuItem("⚙️ Configuration", tabName = "config", icon = icon("cogs")),
      menuItem("🎯 Clustering", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("📈 Résultats", tabName = "results", icon = icon("chart-line")),
      menuItem("🔍 Diagnostics", tabName = "diagnostics", icon = icon("search")),
      menuItem("⚖️ Comparaison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("📥 Export", tabName = "export", icon = icon("download")),
      menuItem("ℹ️ Aide", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # ============================================================================
  # BODY
  # ============================================================================
  dashboardBody(
    
    # CSS personnalisé pour améliorer l'apparence
    tags$head(
      tags$style(HTML("
        .box { border-top: 3px solid #3c8dbc; }
        .box-header { background-color: #f7f7f7; }
        .info-box { min-height: 90px; }
        .small-box { border-radius: 5px; }
        .btn-primary { background-color: #3c8dbc; border-color: #367fa9; }
        .btn-success { background-color: #00a65a; border-color: #008d4c; }
        .nav-tabs-custom { background: #fff; }
        .alert-info { background-color: #d9edf7; border-color: #bce8f1; color: #31708f; }
      "))
    ),
    
    tabItems(
      
      # ========================================================================
      # ONGLET ACCUEIL
      # ========================================================================
      tabItem(
        tabName = "home",
        
        fluidRow(
          box(
            title = "🎯 Bienvenue dans l'Application de Clustering de Variables",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h3("Qu'est-ce que le clustering de variables ?"),
            p("Le clustering de variables consiste à regrouper des variables similaires ensemble,",
              "contrairement au clustering classique qui groupe des observations."),
            
            h4("📚 Algorithmes disponibles :"),
            
            fluidRow(
              column(
                width = 6,
                div(
                  style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                  h5(icon("star"), strong(" VarClustAdvanced"), style = "color: #3c8dbc;"),
                  tags$ul(
                    tags$li(strong("PAM :"), "K-médoïdes robustes aux outliers"),
                    tags$li(strong("Hierarchical :"), "CAH avec dendrogramme"),
                    tags$li(strong("Spectral :"), "Structures non-linéaires"),
                    tags$li(strong("PCAmix :"), "Variables mixtes (FAMD)")
                  )
                )
              ),
              
              column(
                width = 6,
                div(
                  style = "background-color: #fff5f0; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                  h5(icon("project-diagram"), strong(" Autres Algorithmes"), style = "color: #d9534f;"),
                  tags$ul(
                    tags$li(strong("VAR_CAH :"), "CAH classique sur variables"),
                    tags$li(strong("KmodesVarClust :"), "K-Modes pour catégorielles"),
                    tags$li(strong("VARCLUS :"), "Clustering descendant (λ₂)")
                  )
                )
              )
            ),
            
            hr(),
            
            h4("🚀 Guide de démarrage rapide :"),
            tags$ol(
              tags$li("📁 ", strong("Données :"), "Importez votre fichier CSV/Excel"),
              tags$li("⚙️ ", strong("Configuration :"), "Sélectionnez l'algorithme et les paramètres"),
              tags$li("🎯 ", strong("Clustering :"), "Lancez l'analyse"),
              tags$li("📈 ", strong("Résultats :"), "Visualisez les clusters"),
              tags$li("📥 ", strong("Export :"), "Téléchargez les résultats")
            )
          )
        ),
        
        # Statistiques rapides
        fluidRow(
          infoBoxOutput("info_algorithms", width = 4),
          infoBoxOutput("info_features", width = 4),
          infoBoxOutput("info_status", width = 4)
        )
      ),
      
      # ========================================================================
      # ONGLET DONNÉES
      # ========================================================================
      tabItem(
        tabName = "data",
        
        fluidRow(
          box(
            title = "📁 Import des Données",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                fileInput(
                  "file_input",
                  "Choisir un fichier",
                  accept = c(".csv", ".txt", ".xlsx", ".xls"),
                  buttonLabel = "Parcourir...",
                  placeholder = "Aucun fichier sélectionné"
                ),
                
                radioButtons(
                  "file_type",
                  "Type de fichier :",
                  choices = c("CSV (virgule)" = "csv_comma",
                              "CSV (point-virgule)" = "csv_semicolon",
                              "CSV (tabulation)" = "csv_tab",
                              "Excel" = "excel"),
                  selected = "csv_comma"
                )
              ),
              
              column(
                width = 6,
                h4("📋 Options d'import"),
                checkboxInput("header", "Première ligne = noms de colonnes", TRUE),
                checkboxInput("row_names", "Première colonne = noms de lignes", FALSE),
                numericInput("skip_rows", "Lignes à ignorer :", 0, min = 0, max = 100),
                
                br(),
                actionButton("load_sample", "📊 Charger données exemple", 
                             class = "btn-info btn-sm")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "🔍 Aperçu des Données",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            DTOutput("data_preview")
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Statistiques Descriptives",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            verbatimTextOutput("data_summary")
          ),
          
          box(
            title = "⚠️ Valeurs Manquantes",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            plotOutput("na_plot", height = "300px"),
            verbatimTextOutput("na_summary")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET CONFIGURATION
      # ========================================================================
      tabItem(
        tabName = "config",
        
        fluidRow(
          box(
            title = "⚙️ Sélection de l'Algorithme",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              "algorithm",
              "Choisir l'algorithme :",
              choices = c(
                "VarClustAdvanced - PAM" = "vca_pam",
                "VarClustAdvanced - Hierarchical" = "vca_hierarchical",
                "VarClustAdvanced - Spectral" = "vca_spectral",
                "VarClustAdvanced - PCAmix" = "vca_pcamix",
                "VAR_CAH" = "var_cah",
                "KmodesVarClust" = "kmodes",
                "VARCLUS" = "varclus"
              ),
              selected = "vca_pam"
            ),
            
            hr(),
            
            uiOutput("algorithm_description")
          ),
          
          box(
            title = "🎛️ Paramètres",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            
            # Paramètres communs
            conditionalPanel(
              condition = "input.algorithm != 'varclus'",
              sliderInput(
                "n_clusters",
                "Nombre de clusters (k) :",
                min = 2,
                max = 10,
                value = 3,
                step = 1
              ),
              checkboxInput("auto_k", "🤖 Détection automatique de k", FALSE)
            ),
            
            # Paramètres spécifiques à VarClustAdvanced
            conditionalPanel(
              condition = "input.algorithm.startsWith('vca')",
              
              selectInput(
                "distance_metric",
                "Métrique de distance :",
                choices = c("Corrélation" = "correlation",
                            "Euclidienne" = "euclidean",
                            "Gower (mixte)" = "gower"),
                selected = "correlation"
              ),
              
              conditionalPanel(
                condition = "input.algorithm == 'vca_hierarchical'",
                selectInput(
                  "linkage",
                  "Méthode de linkage :",
                  choices = c("Ward D2" = "ward.D2",
                              "Complete" = "complete",
                              "Average" = "average",
                              "Single" = "single"),
                  selected = "ward.D2"
                )
              ),
              
              conditionalPanel(
                condition = "input.algorithm == 'vca_spectral'",
                numericInput("sigma", "Paramètre sigma (NULL = auto) :", 
                             value = NULL, min = 0.01, step = 0.1)
              )
            ),
            
            # Gestion des NA
            selectInput(
              "na_strategy",
              "Stratégie pour valeurs manquantes :",
              choices = c("Moyenne" = "mean",
                          "Médiane" = "median",
                          "MICE (imputation)" = "mice",
                          "KNN" = "knn",
                          "Supprimer" = "remove"),
              selected = "mean"
            ),
            
            # Paramètres avancés
            checkboxInput("show_advanced", "⚙️ Paramètres avancés", FALSE),
            
            conditionalPanel(
              condition = "input.show_advanced == true",
              checkboxInput("standardize", "Standardiser les données", TRUE),
              sliderInput("max_iter", "Itérations max :", 
                          min = 50, max = 500, value = 100, step = 50)
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📋 Sélection des Variables",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                h4("Variables Actives (à clustériser)"),
                uiOutput("active_vars_ui")
              ),
              
              column(
                width = 6,
                h4("Variables Illustratives (optionnel)"),
                uiOutput("illustrative_vars_ui")
              )
            ),
            
            hr(),
            
            fluidRow(
              column(
                width = 12,
                actionButton("select_all_active", "✓ Tout sélectionner", 
                             class = "btn-sm btn-info"),
                actionButton("deselect_all_active", "✗ Tout désélectionner", 
                             class = "btn-sm btn-warning"),
                actionButton("select_numeric", "🔢 Sélectionner numériques", 
                             class = "btn-sm btn-success")
              )
            )
          )
        )
      ),
      
      # ========================================================================
      # ONGLET CLUSTERING
      # ========================================================================
      tabItem(
        tabName = "clustering",
        
        fluidRow(
          box(
            title = "🎯 Lancement du Clustering",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                h4("📋 Récapitulatif"),
                verbatimTextOutput("config_summary")
              ),
              
              column(
                width = 6,
                h4("🚀 Action"),
                br(),
                actionButton(
                  "run_clustering",
                  "▶ LANCER LE CLUSTERING",
                  class = "btn-success btn-lg",
                  icon = icon("play"),
                  style = "width: 100%; height: 80px; font-size: 20px;"
                ),
                br(), br(),
                actionButton("reset", "🔄 Réinitialiser", class = "btn-warning")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "⏱️ Progression",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            uiOutput("progress_ui")
          )
        ),
        
        fluidRow(
          valueBoxOutput("vbox_status", width = 4),
          valueBoxOutput("vbox_k", width = 4),
          valueBoxOutput("vbox_quality", width = 4)
        )
      ),
      
      # ========================================================================
      # ONGLET RÉSULTATS
      # ========================================================================
      tabItem(
        tabName = "results",
        
        fluidRow(
          box(
            title = "📊 Vue d'Ensemble",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            verbatimTextOutput("model_summary")
          )
        ),
        
        fluidRow(
          tabBox(
            title = "📈 Visualisations",
            width = 12,
            
            tabPanel(
              "Silhouette",
              plotlyOutput("plot_silhouette", height = "500px")
            ),
            
            tabPanel(
              "Dendrogramme",
              plotOutput("plot_dendrogram", height = "600px")
            ),
            
            tabPanel(
              "Heatmap",
              plotlyOutput("plot_heatmap", height = "600px")
            ),
            
            tabPanel(
              "Distribution",
              plotlyOutput("plot_distribution", height = "500px")
            ),
            
            tabPanel(
              "Corrélation",
              plotOutput("plot_correlation", height = "600px")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📋 Clusters Détaillés",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            DTOutput("clusters_table")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET DIAGNOSTICS
      # ========================================================================
      tabItem(
        tabName = "diagnostics",
        
        fluidRow(
          valueBoxOutput("diag_silhouette", width = 3),
          valueBoxOutput("diag_davies_bouldin", width = 3),
          valueBoxOutput("diag_dunn", width = 3),
          valueBoxOutput("diag_calinski", width = 3)
        ),
        
        fluidRow(
          box(
            title = "⚠️ Variables Mal Classées",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            
            DTOutput("bad_vars_table")
          ),
          
          box(
            title = "🎯 Qualité par Cluster",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            
            plotlyOutput("quality_by_cluster", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "🔍 Analyse Détaillée",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            verbatimTextOutput("diagnostics_text")
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Méthode du Coude",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput("elbow_plot", height = "400px")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET COMPARAISON
      # ========================================================================
      tabItem(
        tabName = "comparison",
        
        fluidRow(
          box(
            title = "⚖️ Comparaison d'Algorithmes",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("Comparez les performances de différents algorithmes sur vos données."),
            
            fluidRow(
              column(
                width = 6,
                checkboxGroupInput(
                  "comparison_algos",
                  "Sélectionner les algorithmes à comparer :",
                  choices = c(
                    "VarClustAdvanced - PAM" = "vca_pam",
                    "VarClustAdvanced - Hierarchical" = "vca_hierarchical",
                    "VarClustAdvanced - Spectral" = "vca_spectral",
                    "VAR_CAH" = "var_cah"
                  ),
                  selected = c("vca_pam", "vca_hierarchical")
                )
              ),
              
              column(
                width = 6,
                sliderInput("comparison_k", "Nombre de clusters (k) :", 
                            min = 2, max = 10, value = 3, step = 1),
                br(),
                actionButton("run_comparison", "▶ Lancer la Comparaison", 
                             class = "btn-success btn-lg", icon = icon("balance-scale"))
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "📊 Tableau Comparatif",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            DTOutput("comparison_table")
          )
        ),
        
        fluidRow(
          box(
            title = "📈 Visualisation Comparative",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput("comparison_plot", height = "400px")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET EXPORT
      # ========================================================================
      tabItem(
        tabName = "export",
        
        fluidRow(
          box(
            title = "📥 Export des Résultats",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            h4("Choisissez les éléments à exporter :"),
            
            fluidRow(
              column(
                width = 6,
                checkboxGroupInput(
                  "export_elements",
                  "Éléments :",
                  choices = c(
                    "Clusters (CSV)" = "clusters_csv",
                    "Clusters (Excel)" = "clusters_xlsx",
                    "Métriques (CSV)" = "metrics_csv",
                    "Graphique Silhouette (PNG)" = "silhouette_png",
                    "Dendrogramme (PNG)" = "dendrogram_png",
                    "Heatmap (PNG)" = "heatmap_png",
                    "Rapport complet (HTML)" = "report_html"
                  ),
                  selected = c("clusters_csv", "metrics_csv", "silhouette_png")
                )
              ),
              
              column(
                width = 6,
                h5("📁 Options d'export"),
                textInput("export_prefix", "Préfixe des fichiers :", 
                          value = "clustering_results"),
                selectInput("export_format_img", "Format images :", 
                            choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg"),
                            selected = "png"),
                sliderInput("export_dpi", "Résolution (DPI) :", 
                            min = 72, max = 600, value = 300, step = 50)
              )
            ),
            
            hr(),
            
            downloadButton("download_results", "📥 Télécharger TOUT", 
                           class = "btn-success btn-lg"),
            downloadButton("download_clusters", "📋 Clusters uniquement", 
                           class = "btn-info"),
            downloadButton("download_plots", "📊 Graphiques uniquement", 
                           class = "btn-primary")
          )
        ),
        
        fluidRow(
          box(
            title = "🎨 Aperçu avant Export",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            tabsetPanel(
              tabPanel("Clusters", DTOutput("export_preview_clusters")),
              tabPanel("Métriques", DTOutput("export_preview_metrics")),
              tabPanel("Silhouette", plotOutput("export_preview_silhouette"))
            )
          )
        )
      ),
      
      # ========================================================================
      # ONGLET AIDE
      # ========================================================================
      tabItem(
        tabName = "help",
        
        fluidRow(
          box(
            title = "ℹ️ Guide d'Utilisation",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            h3("📚 Documentation"),
            
            h4("1️⃣ Import des données"),
            p("Formats supportés : CSV (virgule, point-virgule, tabulation), Excel (.xlsx, .xls)"),
            p("Les données doivent être organisées avec les", strong("observations en lignes"), 
              "et les", strong("variables en colonnes"), "."),
            
            h4("2️⃣ Configuration"),
            tags$ul(
              tags$li(strong("Algorithme :"), "Choisissez parmi 7 méthodes différentes"),
              tags$li(strong("Nombre de clusters :"), "2-10 ou détection automatique"),
              tags$li(strong("Variables actives :"), "Variables à inclure dans le clustering"),
              tags$li(strong("NA :"), "Stratégie de gestion des valeurs manquantes")
            ),
            
            h4("3️⃣ Interprétation"),
            tags$ul(
              tags$li(strong("Silhouette :"), "[-1, 1] - Plus proche de 1 = meilleur"),
              tags$li(strong("Davies-Bouldin :"), "[0, ∞] - Plus bas = meilleur"),
              tags$li(strong("Dunn Index :"), "[0, ∞] - Plus haut = meilleur")
            ),
            
            hr(),
            
            h4("🆘 Support"),
            p("Pour toute question, consultez la documentation ou contactez l'équipe.")
          )
        ),
        
        fluidRow(
          box(
            title = "📖 Références",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            
            h5("Algorithmes"),
            tags$ul(
              tags$li("Kaufman & Rousseeuw (1990) - PAM"),
              tags$li("Ward (1963) - CAH"),
              tags$li("Ng et al. (2001) - Spectral Clustering"),
              tags$li("Chavent et al. (2012) - ClustOfVar")
            )
          ),
          
          box(
            title = "🔗 Liens Utiles",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            tags$ul(
              tags$li(tags$a(href = "https://CRAN.R-project.org/package=cluster", 
                             "Package cluster", target = "_blank")),
              tags$li(tags$a(href = "https://CRAN.R-project.org/package=factoextra", 
                             "Package factoextra", target = "_blank")),
              tags$li(tags$a(href = "https://CRAN.R-project.org/package=ClustOfVar", 
                             "Package ClustOfVar", target = "_blank"))
            )
          )
        )
      )
    )
  )
)