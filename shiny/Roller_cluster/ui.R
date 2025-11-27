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
      
      menuItem("Accueil", tabName = "home"),
      menuItem("Données", tabName = "data"),
      menuItem("Configuration", tabName = "config"),
      menuItem("Clustering", tabName = "clustering"),
      
      menuItem(" Résultats & Analyses", startExpanded = FALSE,
               menuSubItem("Résultats Principaux", tabName = "results"),
               menuSubItem(" Contribution Variables", tabName = "contribution"),
               menuSubItem("Diagnostics", tabName = "diagnostics"),
               menuSubItem("Stabilité Bootstrap", tabName = "stability")
      ),
      
      #menuItem(" Visualisations", startExpanded = FALSE,
      #         menuSubItem("Projection 2D", tabName = "projection_2d"),
      #         menuSubItem("Projection 3D", tabName = "projection_3d")
      #),
      
      menuItem("Prédiction", tabName = "prediction"),
      menuItem("Comparaison", tabName = "comparison"),
      menuItem(" Historique", tabName = "history"),
      menuItem(" Export", tabName = "export"),
      menuItem("Aide", tabName = "help")
    )
  )
  ,
  
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
                  h5(strong(" Algorithmes Développés"), style = "color: #d9534f;"),
                  tags$ul(
                    tags$li(strong("VAR_CAH :"), "CAH classique sur variables"),
                    tags$li(strong("VAR_KMEANS :"), "K-Means"),
                    tags$li(strong("TandemVarClust :"), "TandemVarClust : Approche Tandem (ACM + CAH) pour variables mixtes")
                    
                  )
                )
              ),
              
              column(
                width = 6,
                div(
                  style = "background-color: #f0f8ff; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                  h5(strong("Fonctionnalités"), style = "color: #3c8dbc;"),
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
              tags$li("", strong("Export :"), "Téléchargez vos résultats")
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
              accept = c(".csv", ".txt", ".xlsx", ".xls"),  # ← Ajouter .txt
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
                "=== Données Générées ===" = "",
                "Économique" = "economic",
                "Biologique" = "biological",
                "Marketing" = "marketing",
                "Mixte (num + cat)" = "mixed",
                "Catégoriel pur" = "categorical",
                "=== Datasets R ===" = "",
                "iris (fleurs)" = "r_iris",
                "mtcars (voitures)" = "r_mtcars",
                "USArrests (criminalité)" = "r_usarrests",
                "swiss (fertilité)" = "r_swiss",
                "state.x77 (USA états)" = "r_statex77",
                "airquality (qualité air)" = "r_airquality"
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
                         class = "btn-success btn-lg")
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
        
        # conversion des données
        fluidRow(
          box(
            title = " Conversion des Types de Variables",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            p("Convertissez automatiquement les types de variables détectés incorrectement."),
            
            fluidRow(
              column(
                width = 6,
                actionButton("auto_convert_types", " Conversion Automatique", 
                             class = "btn-warning"),
                br(), br(),
                verbatimTextOutput("conversion_report")
              ),
              
              column(
                width = 6,
                h5("Conversion Manuelle"),
                uiOutput("manual_type_conversion_ui")
              )
            )
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
                "VAR_CAH (Hiérarchique)" = "var_cah",
                "VAR_KMEANS (Réallocation)" = "var_kmeans",           
                "TandemVarClust (Mixte)" = "tandem"
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
            title = "Sélection des Variables",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 6,
                h4("Variables Actives (à clustériser)"),
                uiOutput("active_vars_ui"),
                
                hr(),
                
                actionButton("select_all_active", "✓ Tout sélectionner", 
                             class = "btn-sm btn-info"),
                actionButton("deselect_all_active", "✗ Tout désélectionner", 
                             class = "btn-sm btn-warning"),
                actionButton("select_numeric", "🔢 Sélectionner numériques", 
                             class = "btn-sm btn-success")
              ),
              
              column(
                width = 6,
                h4("Variables Illustratives (optionnel)"),
                p(class = "text-muted", 
                  "Variables qui ne participent pas au clustering mais seront projetées."),
                
                uiOutput("illustrative_vars_ui"),
                
                hr(),
                
                actionButton("swap_vars", "🔄 Inverser Actives ↔ Illustratives", 
                             class = "btn-sm btn-primary")
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
                        "Algorithme (Optimal)" = "algo_specific",  # ← NOUVEAU
                        "ACP (Standard)" = "pca",
                        "MDS" = "mds",
                        "t-SNE" = "tsne",
                        "UMAP" = "umap"
                      ),
                      selected = "algo_specific"  # ← Par défaut
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
                                 class = "btn-primary btn-block")
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
            ),
            
            tabPanel(
              "Projection 3D",
              
              fluidRow(
                # ═══════════════════════════════════════════════════════════
                # COLONNE GAUCHE : Contrôles
                # ═══════════════════════════════════════════════════════════
                column(
                  width = 3,
                  
                  wellPanel(
                    h4(" Paramètres 3D"),
                    
                    selectInput(
                      "projection_3d_method",
                      "Méthode de projection :",
                      choices = c(
                        "ACP (3 axes)" = "pca",
                        "MDS 3D" = "mds",
                        "t-SNE 3D" = "tsne",
                        "UMAP 3D" = "umap"
                      ),
                      selected = "pca"
                    ),
                    
                    hr(),
                    
                    h5(" Apparence"),
                    
                    sliderInput(
                      "point_size_3d",
                      "Taille des points :",
                      min = 3,
                      max = 15,
                      value = 8,
                      step = 1
                    ),
                    
                    checkboxInput(
                      "show_labels_3d",
                      "Afficher noms variables",
                      value = TRUE
                    ),
                    
                    selectInput(
                      "color_scheme_3d",
                      "Palette de couleurs :",
                      choices = c(
                        "Viridis" = "Viridis",
                        "Set2" = "Set2",
                        "Dark2" = "Dark2",
                        "Pastel" = "Pastel1",
                        "Accent" = "Accent"
                      ),
                      selected = "Set2"
                    ),
                    
                    hr(),
                    
                    h5(" Animation"),
                    
                    checkboxInput(
                      "enable_animation_3d",
                      "Activer l'animation Avant/Après",
                      value = TRUE
                    ),
                    
                    conditionalPanel(
                      condition = "input.enable_animation_3d == true",
                      
                      sliderInput(
                        "animation_speed",
                        "Vitesse (ms par frame) :",
                        min = 50,
                        max = 500,
                        value = 200,
                        step = 50
                      ),
                      
                      actionButton(
                        "play_animation",
                        " Lancer Animation",
                        class = "btn-success btn-block"
                      ),
                      
                      br(),
                      
                      div(
                        class = "alert alert-info",
                        style = "padding: 8px; font-size: 12px;",
                        
                        " L'animation montre la transformation des variables non-clustérisées 
            vers leur regroupement final."
                      )
                    ),
                    
                    hr(),
                    
                    actionButton(
                      "compute_3d", " Calculer Projection",
                      class = "btn-primary btn-block"
                    )
                  ),
                  
                  # ═══════════════════════════════════════════════════════════
                  # Boîte qualité
                  # ═══════════════════════════════════════════════════════════
                  uiOutput("projection_3d_quality")
                ),
                
                # ═══════════════════════════════════════════════════════════
                # COLONNE DROITE : Visualisations
                # ═══════════════════════════════════════════════════════════
                column(
                  width = 9,
                  
                  tabBox(
                    width = 12,
                    
                    # ─────────────────────────────────────────────────────────
                    # TAB 1 : Projection 3D Principale
                    # ─────────────────────────────────────────────────────────
                    tabPanel(
                      title = tagList(" Projection 3D Interactive"),
                      
                      div(
                        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                     padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h4(style = "color: white; margin: 0;",  " Exploration 3D des Variables")
                      ),
                      
                      plotlyOutput("plot_3d_main", height = "650px"),
                      
                      br(),
                      
                      fluidRow(
                        column(
                          width = 6,
                          wellPanel(
                            style = "background-color: #f8f9fa;",
                            h5(" Variance Expliquée"),
                            uiOutput("variance_3d_bars")
                          )
                        ),
                        column(
                          width = 6,
                          wellPanel(
                            style = "background-color: #f8f9fa;",
                            h5(" Statistiques"),
                            verbatimTextOutput("stats_3d")
                          )
                        )
                      )
                    ),
                    
                    # ─────────────────────────────────────────────────────────
                    # TAB 2 : Animation Avant/Après
                    # ─────────────────────────────────────────────────────────
                    tabPanel(
                      title = tagList(" Animation Avant/Après"),
                      
                      div(
                        style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                     padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                        h4(style = "color: white; margin: 0;", 
                           " Transformation Progressive des Variables")
                      ),
                      
                      fluidRow(
                        column(
                          width = 6,
                          div(
                            style = "border: 3px solid #667eea; border-radius: 8px; 
                         padding: 10px; background: white;",
                            h4(style = "text-align: center; color: #667eea;",  " AVANT Clustering"),
                            plotlyOutput("plot_3d_before", height = "500px")
                          )
                        ),
                        
                        column(
                          width = 6,
                          div(
                            style = "border: 3px solid #f5576c; border-radius: 8px; 
                         padding: 10px; background: white;",
                            h4(style = "text-align: center; color: #f5576c;", 
                               " APRÈS Clustering"),
                            plotlyOutput("plot_3d_after", height = "500px")
                          )
                        )
                      ),
                      
                      br(),
                      
                      div(
                        class = "alert alert-success",
                        style = "font-size: 15px;",
                        strong(" Interprétation : "),
                        "Observez comment les variables se regroupent naturellement après le clustering. 
            Les variables proches dans l'espace 3D partagent des caractéristiques similaires."
                      )
                    ),
                    
                    # ─────────────────────────────────────────────────────────
                    # TAB 3 : Analyse par Cluster
                    # ─────────────────────────────────────────────────────────
                    tabPanel(
                      title = tagList( " Analyse par Cluster"),
                      
                      h4(" Visualisation Cluster par Cluster"),
                      
                      br(),
                      
                      fluidRow(
                        column(
                          width = 3,
                          wellPanel(
                            h5("Sélectionner un cluster :"),
                            uiOutput("cluster_selector_3d")
                          )
                        ),
                        
                        column(
                          width = 9,
                          plotlyOutput("plot_3d_cluster_focus", height = "500px"),
                          
                          br(),
                          
                          wellPanel(
                            h5(" Variables dans ce cluster :"),
                            verbatimTextOutput("cluster_vars_list")
                          )
                        )
                      )
                    ),
                    
                    # ─────────────────────────────────────────────────────────
                    # TAB 4 : Trajectoires de Variables
                    # ─────────────────────────────────────────────────────────
                    tabPanel(
                      title = tagList(" Trajectoires"),
                      
                      h4(" Trajectoires des Variables dans l'Espace 3D"),
                      
                      p(class = "text-muted", 
                        "Visualisez comment chaque variable se déplace vers son cluster."),
                      
                      br(),
                      
                      plotlyOutput("plot_3d_trajectories", height = "600px"),
                      
                      br(),
                      
                      wellPanel(
                        style = "background-color: #fff3cd;",
                        h5(" Lecture du graphique :"),
                        tags$ul(
                          tags$li("Chaque ligne représente la trajectoire d'une variable"),
                          tags$li("Point de départ : position initiale (avant clustering)"),
                          tags$li("Point d'arrivée : position finale (après clustering)"),
                          tags$li("Couleur : cluster d'appartenance final")
                        )
                      )
                    )
                  )
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
      
      
      ##### contributions
      # ============================================================================
      # NOUVEL ONGLET : ANALYSE DE CONTRIBUTION DES VARIABLES
      # À ajouter dans ui.R après "Résultats"
      # ============================================================================
      
      tabItem(
        tabName = "contribution",
        
        fluidRow(
          box(
            title = "Analyse de Contribution des Variables",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("Cette analyse identifie les variables les plus représentatives de chaque cluster 
        et évalue leur importance dans la structure de clustering."),
            
            fluidRow(
              column(
                width = 12,
                
                tabBox(
                  width = 12,
                  
                  # ─────────────────────────────────────────────────────────
                  # TAB 1 : Contribution Globale
                  # ─────────────────────────────────────────────────────────
                  tabPanel(
                    title = tagList(" Contribution Globale"),
                    
                    fluidRow(
                      column(
                        width = 8,
                        div(
                          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                             padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                          h4(style = "color: white; margin: 0;", 
                              " Importance des Variables")
                        ),
                        plotlyOutput("plot_contribution_global", height = "500px")
                      ),
                      
                      column(
                        width = 4,
                        wellPanel(
                          style = "background-color: #f8f9fa;",
                          h5(" Top 10 Variables"),
                          DTOutput("table_top_variables")
                        ),
                        
                        wellPanel(
                          style = "background-color: #fff3cd;",
                          h5(" Interprétation"),
                          p(style = "font-size: 13px;",
                            "Les variables avec une contribution élevée sont 
                      fortement corrélées avec leur cluster et peu 
                      corrélées avec les autres clusters.")
                        )
                      )
                    )
                  ),
                  
                  # ─────────────────────────────────────────────────────────
                  # TAB 2 : Contribution par Cluster
                  # ─────────────────────────────────────────────────────────
                  tabPanel(
                    title = tagList(" Par Cluster"),
                    
                    fluidRow(
                      column(
                        width = 3,
                        wellPanel(
                          h5("Sélectionner un cluster :"),
                          uiOutput("cluster_selector_contrib"),
                          
                          hr(),
                          
                          h5( " Statistiques"),
                          verbatimTextOutput("cluster_contrib_stats")
                        )
                      ),
                      
                      column(
                        width = 9,
                        h4(" Variables du Cluster Sélectionné"),
                        plotlyOutput("plot_contribution_cluster", height = "400px"),
                        
                        br(),
                        
                        h5(" Détails des Contributions"),
                        DTOutput("table_contribution_cluster")
                      )
                    )
                  ),
                  
                  # ─────────────────────────────────────────────────────────
                  # TAB 3 : Analyse Discriminante
                  # ─────────────────────────────────────────────────────────
                  tabPanel(
                    title = tagList(" Variables Discriminantes"),
                    
                    p("Identifie les variables qui différencient le mieux les clusters."),
                    
                    fluidRow(
                      column(
                        width = 6,
                        h4(" Top Variables Discriminantes"),
                        plotlyOutput("plot_discriminant_vars", height = "400px")
                      ),
                      
                      column(
                        width = 6,
                        h4(" Ratio Inter/Intra Variance"),
                        plotlyOutput("plot_variance_ratio", height = "400px")
                      )
                    ),
                    
                    br(),
                    
                    wellPanel(
                      h5(" Tableau Complet"),
                      DTOutput("table_discriminant_analysis")
                    )
                  ),
                  
                  # ─────────────────────────────────────────────────────────
                  # TAB 4 : Similarité Intra-Cluster
                  # ─────────────────────────────────────────────────────────
                  tabPanel(
                    title = tagList(" Cohésion Intra-Cluster"),
                    
                    p("Mesure la similarité (corrélation) entre variables au sein de chaque cluster."),
                    
                    plotlyOutput("plot_intra_cluster_similarity", height = "400px"),
                    
                    br(),
                    
                    fluidRow(
                      column(
                        width = 6,
                        wellPanel(
                          h5(" Statistiques par Cluster"),
                          DTOutput("table_intra_similarity")
                        )
                      ),
                      
                      column(
                        width = 6,
                        wellPanel(
                          style = "background-color: #d1ecf1;",
                          h5(" Critères de Qualité"),
                          tags$ul(
                            tags$li(strong("Corrélation moyenne > 0.7 :"), " Excellent"),
                            tags$li(strong("Corrélation moyenne > 0.5 :"), " Bon"),
                            tags$li(strong("Corrélation moyenne < 0.5 :"), " Faible cohésion")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
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
                             class = "btn-success btn-lg")
              ),
              
              column(
                width = 6,
                h4(" Charger Session"),
                uiOutput("session_selector"),
                br(),
                actionButton("load_session", " Charger", 
                             class = "btn-info btn-lg"),
                br(), br(),
                actionButton("delete_session", "️ Supprimer", 
                             class = "btn-danger")
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
      
      # ============================================================================
      # NOUVEL ONGLET : ANALYSE DE STABILITÉ (Bootstrap)
      # À ajouter dans ui.R après l'onglet "Diagnostics"
      # ============================================================================
      
      tabItem(
        tabName = "stability",
        
        fluidRow(
          box(
            title = "Analyse de Stabilité par Bootstrap",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("L'analyse de stabilité évalue la robustesse de votre clustering en re-clustérisant 
        des échantillons bootstrap des observations. Un clustering stable produit des résultats 
        similaires même avec des variations dans les données."),
            
            fluidRow(
              column(
                width = 4,
                wellPanel(
                  h4(" Paramètres Bootstrap"),
                  
                  sliderInput(
                    "n_bootstrap",
                    "Nombre d'itérations :",
                    min = 10,
                    max = 100,
                    value = 50,
                    step = 10
                  ),
                  
                  sliderInput(
                    "bootstrap_sample_pct",
                    "Pourcentage d'échantillonnage :",
                    min = 50,
                    max = 100,
                    value = 80,
                    step = 5
                  ),
                  
                  numericInput(
                    "bootstrap_seed",
                    "Seed (reproductibilité) :",
                    value = 123,
                    min = 1,
                    max = 9999
                  ),
                  
                  hr(),
                  
                  actionButton(
                    "run_bootstrap",
                    "▶ LANCER ANALYSE",
                    class = "btn-success btn-lg btn-block"
                  ),
                  
                  br(),
                  
                  uiOutput("bootstrap_status")
                )
              ),
              
              column(
                width = 8,
                
                div(
                  style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                     padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h4(style = "color: white; margin: 0;", " Résultats de Stabilité")
                ),
                
                tabBox(
                  width = 12,
                  
                  tabPanel(
                    "📊 Scores de Stabilité",
                    plotlyOutput("plot_stability_scores", height = "400px"),
                    br(),
                    DTOutput("table_stability_by_cluster")
                  ),
                  
                  tabPanel(
                    "Heatmap de Co-clustering",
                    p("Cette heatmap montre à quelle fréquence les paires de variables 
                sont clustérisées ensemble. Plus la couleur est chaude, plus la 
                co-occurrence est fréquente."),
                    plotlyOutput("plot_coclustering_heatmap", height = "600px")
                  ),
                  
                  tabPanel(
                    " Distribution ARI",
                    p("L'Adjusted Rand Index (ARI) mesure la similarité entre deux partitions. 
                Un ARI proche de 1 indique une grande stabilité."),
                    plotlyOutput("plot_ari_distribution", height = "400px"),
                    br(),
                    wellPanel(
                      h5(" Statistiques ARI"),
                      verbatimTextOutput("ari_stats")
                    )
                  )
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Interprétation de la Stabilité",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            
            h4("Comment interpréter ces résultats ?"),
            
            tags$ul(
              tags$li(
                strong("Score de stabilité > 0.8 :"),
                " Excellent. Le clustering est très robuste."
              ),
              tags$li(
                strong("Score de stabilité entre 0.6 et 0.8 :"),
                " Bon. Le clustering est raisonnablement stable."
              ),
              tags$li(
                strong("Score de stabilité < 0.6 :"),
                " Faible. Le clustering est sensible aux variations. 
          Considérez d'autres valeurs de k ou un autre algorithme."
              )
            ),
            
            hr(),
            
            h4("Que faire si la stabilité est faible ?"),
            
            tags$ol(
              tags$li("Essayer un nombre différent de clusters (k)"),
              tags$li("Utiliser un algorithme différent"),
              tags$li("Retirer les variables bruitées"),
              tags$li("Augmenter la taille de l'échantillon si possible")
            )
          )
        )
      ),
      
      # ============================================================================
      # ONGLET PROJECTION 3D INTERACTIVE - À AJOUTER DANS ui.R
      # Ajouter après l'onglet "Projection 2D"
      # ============================================================================
      
      tabPanel(
        "Projection 3D",
        
        fluidRow(
          # ═══════════════════════════════════════════════════════════
          # COLONNE GAUCHE : Contrôles
          # ═══════════════════════════════════════════════════════════
          column(
            width = 3,
            
            wellPanel(
              h4(" Paramètres 3D"),
              
              selectInput(
                "projection_3d_method",
                "Méthode de projection :",
                choices = c(
                  "ACP (3 axes)" = "pca",
                  "MDS 3D" = "mds",
                  "t-SNE 3D" = "tsne",
                  "UMAP 3D" = "umap"
                ),
                selected = "pca"
              ),
              
              hr(),
              
              h5(" Apparence"),
              
              sliderInput(
                "point_size_3d",
                "Taille des points :",
                min = 3,
                max = 15,
                value = 8,
                step = 1
              ),
              
              checkboxInput(
                "show_labels_3d",
                "Afficher noms variables",
                value = TRUE
              ),
              
              selectInput(
                "color_scheme_3d",
                "Palette de couleurs :",
                choices = c(
                  "Viridis" = "Viridis",
                  "Set2" = "Set2",
                  "Dark2" = "Dark2",
                  "Pastel" = "Pastel1",
                  "Accent" = "Accent"
                ),
                selected = "Set2"
              ),
              
              hr(),
              
              h5(" Animation"),
              
              checkboxInput(
                "enable_animation_3d",
                "Activer l'animation Avant/Après",
                value = TRUE
              ),
              
              conditionalPanel(
                condition = "input.enable_animation_3d == true",
                
                sliderInput(
                  "animation_speed",
                  "Vitesse (ms par frame) :",
                  min = 50,
                  max = 500,
                  value = 200,
                  step = 50
                ),
                
                actionButton(
                  "play_animation",
                   " Lancer Animation",
                  class = "btn-success btn-block"
                ),
                
                br(),
                
                div(
                  class = "alert alert-info",
                  style = "padding: 8px; font-size: 12px;",
                  
                  " L'animation montre la transformation des variables non-clustérisées 
            vers leur regroupement final."
                )
              ),
              
              hr(),
              
              actionButton(
                "compute_3d", " Calculer Projection",
                class = "btn-primary btn-block"
              )
            ),
            
            # ═══════════════════════════════════════════════════════════
            # Boîte qualité
            # ═══════════════════════════════════════════════════════════
            uiOutput("projection_3d_quality")
          ),
          
          # ═══════════════════════════════════════════════════════════
          # COLONNE DROITE : Visualisations
          # ═══════════════════════════════════════════════════════════
          column(
            width = 9,
            
            tabBox(
              width = 12,
              
              # ─────────────────────────────────────────────────────────
              # TAB 1 : Projection 3D Principale
              # ─────────────────────────────────────────────────────────
              tabPanel(
                title = tagList(" Projection 3D Interactive"),
                
                div(
                  style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                     padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h4(style = "color: white; margin: 0;",  " Exploration 3D des Variables")
                ),
                
                plotlyOutput("plot_3d_main", height = "650px"),
                
                br(),
                
                fluidRow(
                  column(
                    width = 6,
                    wellPanel(
                      style = "background-color: #f8f9fa;",
                      h5(" Variance Expliquée"),
                      uiOutput("variance_3d_bars")
                    )
                  ),
                  column(
                    width = 6,
                    wellPanel(
                      style = "background-color: #f8f9fa;",
                      h5(" Statistiques"),
                      verbatimTextOutput("stats_3d")
                    )
                  )
                )
              ),
              
              # ─────────────────────────────────────────────────────────
              # TAB 2 : Animation Avant/Après
              # ─────────────────────────────────────────────────────────
              tabPanel(
                title = tagList(" Animation Avant/Après"),
                
                div(
                  style = "background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); 
                     padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                  h4(style = "color: white; margin: 0;", 
                      " Transformation Progressive des Variables")
                ),
                
                fluidRow(
                  column(
                    width = 6,
                    div(
                      style = "border: 3px solid #667eea; border-radius: 8px; 
                         padding: 10px; background: white;",
                      h4(style = "text-align: center; color: #667eea;",  " AVANT Clustering"),
                      plotlyOutput("plot_3d_before", height = "500px")
                    )
                  ),
                  
                  column(
                    width = 6,
                    div(
                      style = "border: 3px solid #f5576c; border-radius: 8px; 
                         padding: 10px; background: white;",
                      h4(style = "text-align: center; color: #f5576c;", 
                          " APRÈS Clustering"),
                      plotlyOutput("plot_3d_after", height = "500px")
                    )
                  )
                ),
                
                br(),
                
                div(
                  class = "alert alert-success",
                  style = "font-size: 15px;",
                  strong(" Interprétation : "),
                  "Observez comment les variables se regroupent naturellement après le clustering. 
            Les variables proches dans l'espace 3D partagent des caractéristiques similaires."
                )
              ),
              
              # ─────────────────────────────────────────────────────────
              # TAB 3 : Analyse par Cluster
              # ─────────────────────────────────────────────────────────
              tabPanel(
                title = tagList( " Analyse par Cluster"),
                
                h4(" Visualisation Cluster par Cluster"),
                
                br(),
                
                fluidRow(
                  column(
                    width = 3,
                    wellPanel(
                      h5("Sélectionner un cluster :"),
                      uiOutput("cluster_selector_3d")
                    )
                  ),
                  
                  column(
                    width = 9,
                    plotlyOutput("plot_3d_cluster_focus", height = "500px"),
                    
                    br(),
                    
                    wellPanel(
                      h5(" Variables dans ce cluster :"),
                      verbatimTextOutput("cluster_vars_list")
                    )
                  )
                )
              ),
              
              # ─────────────────────────────────────────────────────────
              # TAB 4 : Trajectoires de Variables
              # ─────────────────────────────────────────────────────────
              tabPanel(
                title = tagList(" Trajectoires"),
                
                h4(" Trajectoires des Variables dans l'Espace 3D"),
                
                p(class = "text-muted", 
                  "Visualisez comment chaque variable se déplace vers son cluster."),
                
                br(),
                
                plotlyOutput("plot_3d_trajectories", height = "600px"),
                
                br(),
                
                wellPanel(
                  style = "background-color: #fff3cd;",
                  h5(" Lecture du graphique :"),
                  tags$ul(
                    tags$li("Chaque ligne représente la trajectoire d'une variable"),
                    tags$li("Point de départ : position initiale (avant clustering)"),
                    tags$li("Point d'arrivée : position finale (après clustering)"),
                    tags$li("Couleur : cluster d'appartenance final")
                  )
                )
              )
            )
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
                    "VAR_KMEANS" = "var_kmeans",
                    "TandemVarClust" = "tandem"
                  ),
                  selected = c("var_cah", "var_kmeans")
                )
              ),
              
              column(
                width = 6,
                sliderInput("comparison_k", "Nombre de clusters (k) :", 
                            min = 2, max = 10, value = 3, step = 1),
                br(),
                actionButton("run_comparison", "▶ Lancer la Comparaison", 
                             class = "btn-success btn-lg")
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