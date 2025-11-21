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
      
      menuItem("Accueil", tabName = "home", icon = icon("home")),
      menuItem("Données", tabName = "data", icon = icon("database")),
      menuItem("⚙️ Configuration", tabName = "config", icon = icon("cogs")),
      menuItem("Clustering", tabName = "clustering", icon = icon("project-diagram")),
      menuItem("Résultats", tabName = "results", icon = icon("chart-line")),
      menuItem("Prédiction", tabName = "prediction", icon = icon("magic")),
      menuItem("Diagnostics", tabName = "diagnostics", icon = icon("search")),
      menuItem("Comparaison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Historique", tabName = "history", icon = icon("history")),
      menuItem("Export", tabName = "export", icon = icon("download")),
      menuItem("Aide", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  # ============================================================================
  # BODY
  # ============================================================================
  dashboardBody(
    
    # CSS personnalisé
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
            title = "Bienvenue dans RollerClustR : l'Application de Clustering de Variables",
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
                  style = "background-color: #fff5f0; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                  h5(icon("project-diagram"), strong(" Algorithmes Développés"), style = "color: #d9534f;"),
                  tags$ul(
                    tags$li(strong("VAR_CAH :"), "CAH classique sur variables"),
                    tags$li(strong("KmodesVarClust :"), "K-Modes pour catégorielles"),
                    tags$li(strong("VARCLUS :"), "Clustering descendant (λ₂)")
                  )
                )
              ),
              
              column(
                width = 6,
                div(
                  style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                  h5(icon("star"), strong("Fonctionnalités"), style = "color: #3c8dbc;"),
                  tags$ul(
                    tags$li(strong("Prédiction :"), "Classifier de nouvelles variables"),
                    tags$li(strong("Historique :"), "Sauvegarder vos sessions"),
                    tags$li(strong("Comparaison :"), "Comparer les algorithmes")
                  )
                )
              )
            ),
            
            hr(),
            
            h4("Guide de démarrage rapide :"),
            tags$ol(
              tags$li("", strong("Données :"), "Importez ou générez des données exemple"),
              tags$li("", strong("Configuration :"), "Sélectionnez l'algorithme et les paramètres"),
              tags$li("", strong("Clustering :"), "Lancez l'analyse"),
              tags$li("", strong("Résultats :"), "Visualisez et interprétez"),
              tags$li("", strong("Prédiction :"), "Classez de nouvelles variables"),
              tags$li("📥 ", strong("Export :"), "Téléchargez vos résultats")
            )
          )
        ),
        
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
            title = "Import des Données",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
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
            ),
            
            checkboxInput("header", "Première ligne = noms de colonnes", TRUE),
            checkboxInput("row_names", "Première colonne = noms de lignes", FALSE),
            numericInput("skip_rows", "Lignes à ignorer :", 0, min = 0, max = 100)
          ),
          
          box(
            title = "Générer Données Exemple",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            
            selectInput(
              "sample_type",
              "Type de données :",
              choices = c(
                "Économique" = "economic",
                "Biologique" = "biological",
                "Marketing" = "marketing",
                "Mixte (num + cat)" = "mixed",
                "Catégoriel pur" = "categorical"
              ),
              selected = "economic"
            ),
            
            sliderInput("sample_n", "Nombre d'observations :", 
                        min = 50, max = 500, value = 100, step = 50),
            
            sliderInput("sample_noise", "Niveau de bruit (NA) :", 
                        min = 0, max = 0.3, value = 0.1, step = 0.05),
            
            numericInput("sample_seed", "Seed (reproductibilité) :", 
                         value = 42, min = 1, max = 9999),
            
            br(),
            actionButton("load_sample", "Générer", 
                         class = "btn-success btn-lg", icon = icon("dice"))
          )
        ),
        
        fluidRow(
          box(
            title = "Aperçu des Données",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            DTOutput("data_preview")
          )
        ),
        
        fluidRow(
          box(
            title = "Statistiques Descriptives",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            verbatimTextOutput("data_summary")
          ),
          
          box(
            title = "Valeurs Manquantes",
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
            title = "Sélection de l'Algorithme",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            selectInput(
              "algorithm",
              "Choisir l'algorithme :",
              choices = c(
                "VAR_CAH" = "var_cah",
                "KmodesVarClust" = "kmodes",
                "VARCLUS" = "varclus"
              ),
              selected = "var_cah"
            ),
            
            hr(),
            
            uiOutput("algorithm_description")
          ),
          
          box(
            title = "️ Paramètres",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            
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
              checkboxInput("auto_k", "Détection automatique de k", FALSE)
            ),
            
            conditionalPanel(
              condition = "input.algorithm == 'var_cah'",
              selectInput(
                "linkage",
                "Méthode de linkage :",
                choices = c("Ward D2" = "ward.D2",
                            "Complete" = "complete",
                            "Average" = "average",
                            "Single" = "single"),
                selected = "ward.D2"
              ),
              
              selectInput(
                "distance_metric",
                "Métrique de distance :",
                choices = c("Corrélation" = "correlation",
                            "Euclidienne" = "euclidean"),
                selected = "correlation"
              )
            ),
            
            selectInput(
              "na_strategy",
              "Stratégie pour valeurs manquantes :",
              choices = c("Moyenne" = "mean",
                          "Médiane" = "median",
                          "Supprimer" = "remove"),
              selected = "mean"
            ),
            
            checkboxInput("show_advanced", " Paramètres avancés", FALSE),
            
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
            title = " Sélection des Variables",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 12,
                h4("Variables Actives (à clustériser)"),
                uiOutput("active_vars_ui")
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
            title = "Lancement du Clustering",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                h4("Récapitulatif"),
                verbatimTextOutput("config_summary")
              ),
              
              column(
                width = 6,
                h4(" Action"),
                br(),
                actionButton(
                  "run_clustering",
                  "▶ LANCER LE CLUSTERING",
                  class = "btn-success btn-lg",
                  icon = icon("play"),
                  style = "width: 100%; height: 80px; font-size: 20px;"
                ),
                br(), br(),
                actionButton("reset", "Réinitialiser", class = "btn-warning")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = " Progression",
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
            title = "Résumé du Modèle",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            verbatimTextOutput("model_summary")
          )
        ),
        
        fluidRow(
          tabBox(
            title = " Visualisations",
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
            ),
            
            tabPanel(
              "Projection 2D",
              
              fluidRow(
                column(
                  width = 3,
                  
                  wellPanel(
                    h4("Paramètres"),
                    
                    selectInput(
                      "projection_method",
                      "Méthode :",
                      choices = c(
                        "ACP (Rapide)" = "pca",
                        "MDS" = "mds",
                        "t-SNE" = "tsne",
                        "UMAP" = "umap"
                      ),
                      selected = "pca"
                    ),
                    
                    conditionalPanel(
                      condition = "input.projection_method == 'tsne'",
                      sliderInput("tsne_perplexity", "Perplexité :", 
                                  min = 5, max = 50, value = 30, step = 5),
                      helpText("Ajuster si nécessaire (5-50)")
                    ),
                    
                    conditionalPanel(
                      condition = "input.projection_method == 'umap'",
                      sliderInput("umap_neighbors", "Voisins :", 
                                  min = 5, max = 50, value = 15, step = 5)
                    ),
                    
                    hr(),
                    
                    checkboxInput("show_var_labels", "Noms variables", TRUE),
                    sliderInput("proj_point_size", "Taille points :", 
                                min = 3, max = 12, value = 6, step = 1),
                    
                    br(),
                    actionButton("compute_projection", "🔄 Calculer", 
                                 class = "btn-primary btn-block", icon = icon("refresh"))
                  ),
                  
                  # Informations qualité
                  uiOutput("projection_quality_box")
                ),
                
                column(
                  width = 9,
                  plotlyOutput("projection_plot", height = "550px"),
                  br(),
                  verbatimTextOutput("projection_summary")
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = " Clusters Détaillés",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            DTOutput("clusters_table")
          )
        )
      ),
      
      # ========================================================================
      # ONGLET PRÉDICTION (NOUVEAU)
      # ========================================================================
      tabItem(
        tabName = "prediction",
        
        fluidRow(
          box(
            title = " Prédiction pour Nouvelles Variables",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("Cette fonctionnalité permet de classifier de nouvelles variables dans les clusters existants."),
            
            fluidRow(
              column(
                width = 6,
                h4("Import Nouvelles Variables"),
                fileInput(
                  "predict_file",
                  "Fichier avec nouvelles variables",
                  accept = c(".csv", ".xlsx")
                ),
                
                hr(),
                
                h5("Ou créer manuellement :"),
                textInput("new_var_name", "Nom de la variable :", "Nouvelle_Var"),
                numericInput("new_var_n", "Nombre de valeurs :", 100, min = 10, max = 1000),
                actionButton("generate_new_var", "Générer Variable Aléatoire", 
                             class = "btn-info")
              ),
              
              column(
                width = 6,
                h4(" Lancer Prédiction"),
                br(),
                actionButton(
                  "run_prediction",
                  "▶ PRÉDIRE CLUSTERS",
                  class = "btn-success btn-lg",
                  icon = icon("magic"),
                  style = "width: 100%; height: 60px;"
                ),
                
                br(), br(),
                
                uiOutput("prediction_status")
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = " Résultats de Prédiction",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            DTOutput("prediction_results_table")
          )
        ),
        
        fluidRow(
          box(
            title = " Visualisation",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            plotlyOutput("prediction_plot", height = "400px")
          )
        )
      ),
      
      # Autres onglets (Diagnostics, Comparaison, etc.) - identiques à avant
      # Je garde la structure mais ajoute l'onglet Historique
      
      # ========================================================================
      # ONGLET HISTORIQUE (NOUVEAU)
      # ========================================================================
      tabItem(
        tabName = "history",
        
        fluidRow(
          box(
            title = " Historique des Sessions",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("Sauvegardez et chargez vos sessions de clustering pour reprendre votre travail plus tard."),
            
            fluidRow(
              column(
                width = 6,
                h4(" Sauvegarder Session Actuelle"),
                textInput("session_name", "Nom de la session :", 
                          value = paste0("Session_", format(Sys.time(), "%Y%m%d_%H%M%S"))),
                textAreaInput("session_notes", "Notes (optionnel) :", 
                              placeholder = "Description de cette analyse...", rows = 3),
                br(),
                actionButton("save_session", "Sauvegarder", 
                             class = "btn-success btn-lg", icon = icon("save"))
              ),
              
              column(
                width = 6,
                h4(" Charger Session"),
                uiOutput("session_selector"),
                br(),
                actionButton("load_session", " Charger", 
                             class = "btn-info btn-lg", icon = icon("folder-open")),
                br(), br(),
                actionButton("delete_session", "🗑️ Supprimer", 
                             class = "btn-danger", icon = icon("trash"))
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = " Sessions Sauvegardées",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            DTOutput("sessions_table")
          )
        ),
        
        fluidRow(
          box(
            title = " Export/Import Sessions",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            fluidRow(
              column(
                width = 6,
                h5("Exporter toutes les sessions"),
                downloadButton("export_all_sessions", "📥 Exporter (RDS)", 
                               class = "btn-primary")
              ),
              column(
                width = 6,
                h5("Importer sessions"),
                fileInput("import_sessions_file", "Importer fichier RDS", 
                          accept = ".rds")
              )
            )
          )
        )
      ),
      
      # Autres onglets existants...
      # (Export, Aide, etc.)
      
      # ========================================================================
      # ONGLET EXPORT
      # ========================================================================
      tabItem(
        tabName = "export",
        
        fluidRow(
          box(
            title = " Export des Résultats",
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
                    "Rapport complet (HTML)" = "report_html"
                  ),
                  selected = c("clusters_csv", "metrics_csv")
                )
              ),
              
              column(
                width = 6,
                h5(" Options d'export"),
                textInput("export_prefix", "Préfixe des fichiers :", 
                          value = "clustering_results"),
                selectInput("export_format_img", "Format images :", 
                            choices = c("PNG" = "png", "PDF" = "pdf"),
                            selected = "png"),
                sliderInput("export_dpi", "Résolution (DPI) :", 
                            min = 72, max = 600, value = 300, step = 50)
              )
            ),
            
            hr(),
            
            downloadButton("download_results", " Télécharger TOUT", 
                           class = "btn-success btn-lg"),
            downloadButton("download_clusters", " Clusters uniquement", 
                           class = "btn-info"),
            downloadButton("download_plots", " Graphiques uniquement", 
                           class = "btn-primary")
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
            title = "Variables Mal Classées",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            
            DTOutput("bad_vars_table")
          ),
          
          box(
            title = "Qualité par Cluster",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            
            plotlyOutput("quality_by_cluster", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = " Analyse Détaillée",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            verbatimTextOutput("diagnostics_text")
          )
        ),
        
        fluidRow(
          box(
            title = " Méthode du Coude",
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
            title = "Comparaison d'Algorithmes",
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
                    "VAR_CAH" = "var_cah",
                    "KmodesVarClust" = "kmodes",
                    "VARCLUS" = "varclus"
                  ),
                  selected = c("var_cah", "kmodes")
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
            title = "Tableau Comparatif",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            DTOutput("comparison_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Visualisation Comparative",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput("comparison_plot", height = "400px")
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
            title = " Guide d'Utilisation",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            h3("📚 Documentation"),
            
            h4("1️⃣ Import des données"),
            p("Formats supportés : CSV (virgule, point-virgule, tabulation), Excel (.xlsx, .xls)"),
            p("Les données doivent être organisées avec les", strong("observations en lignes"), 
              "et les", strong("variables en colonnes"), "."),
            
            h4("2️⃣ Génération de données exemple"),
            tags$ul(
              tags$li(strong("Économique :"), "12 variables (PIB, Revenu, Emploi, etc.)"),
              tags$li(strong("Biologique :"), "12 gènes groupés par fonction"),
              tags$li(strong("Marketing :"), "8 variables de comportement client"),
              tags$li(strong("Mixte :"), "Variables numériques et catégorielles"),
              tags$li(strong("Catégoriel :"), "8 variables catégorielles pures")
            ),
            
            h4("3️⃣ Configuration"),
            tags$ul(
              tags$li(strong("VAR_CAH :"), "CAH avec PC1 comme variable synthétique"),
              tags$li(strong("KmodesVarClust :"), "Pour variables catégorielles uniquement"),
              tags$li(strong("VARCLUS :"), "Division successive avec critère λ₂ ≥ 1")
            ),
            
            h4("4️⃣ Prédiction"),
            p("Après le clustering, vous pouvez classifier de nouvelles variables :"),
            tags$ul(
              tags$li("Importer un fichier avec nouvelles variables"),
              tags$li("Ou générer une variable aléatoire pour tester"),
              tags$li("Le modèle assignera automatiquement le cluster le plus proche")
            ),
            
            h4("5️⃣ Historique"),
            p("Sauvegardez vos sessions pour reprendre votre travail :"),
            tags$ul(
              tags$li("Donnez un nom descriptif à votre session"),
              tags$li("Ajoutez des notes pour vous rappeler le contexte"),
              tags$li("Exportez/importez vos sessions entre ordinateurs")
            ),
            
            h4("6️⃣ Interprétation"),
            tags$ul(
              tags$li(strong("Silhouette :"), "[-1, 1] - Plus proche de 1 = meilleur"),
              tags$li(strong("Davies-Bouldin :"), "[0, ∞] - Plus bas = meilleur"),
              tags$li(strong("Dunn Index :"), "[0, ∞] - Plus haut = meilleur"),
              tags$li(strong("Calinski-Harabasz :"), "[0, ∞] - Plus haut = meilleur")
            ),
            
            hr(),
            
            h4("🆘 Support"),
            p("Pour toute question, consultez la documentation des algorithmes ou contactez l'équipe.")
          )
        ),
        
        fluidRow(
          box(
            title = "Références Algorithmes",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            
            h5("VAR_CAH"),
            p("Classification Ascendante Hiérarchique sur variables avec première composante principale."),
            
            h5("KmodesVarClust"),
            p("Extension du K-Modes pour le clustering de variables catégorielles (Huang, 1998)."),
            
            h5("VARCLUS"),
            p("Clustering descendant par division successive basé sur le critère λ₂ (SAS Institute).")
          ),
          
          box(
            title = " Liens Utiles",
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