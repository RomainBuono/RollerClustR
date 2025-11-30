# ==============================================================================
# INTERFACE UTILISATEUR (UI) - VERSION CORRIGÉE
# ==============================================================================
# Modifications appliquées :
# 1. ✅ Retiré onglet "Comparaison" de la sidebar (ligne 49)
# 2. ✅ Retiré section tabItem "comparison" complète (lignes 1740-1803)
# 3. ✅ Mis à jour descriptions algorithmes (lignes 104-106)
# 4. ✅ Retiré mention "Comparaison" des fonctionnalités
#
# Algorithmes conservés : VAR_CAH, VAR_KMEANS, TandemVarClust
# ==============================================================================

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
    div(
      style = "text-align: center; margin-bottom: 20px;",
      tags$img(
        src = "Logo_RollerClusteR.jpg",
        alt = "RollerClustR Logo",
        height = "120px",  # Ajuster selon la taille souhaitée
        style = "border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
      )
    ),
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Accueil", tabName = "home"),
      menuItem("Données", tabName = "data"),
      menuItem("Configuration", tabName = "config"),
      menuItem("Clustering", tabName = "clustering"),
      
      menuItem("Résultats & Analyses", startExpanded = FALSE,
               #menuSubItem("Résultats Principaux", tabName = "results"),
               menuSubItem("Résultats", tabName = "algo_graphs"), # ← NOUVEAU
               #menuSubItem("Contribution Variables", tabName = "contribution"),
               menuSubItem("Diagnostics", tabName = "diagnostics"),
               menuSubItem("Stabilité Bootstrap", tabName = "stability")
      ),
      
      #menuItem(" Visualisations", startExpanded = FALSE,
      #         menuSubItem("Projection 2D", tabName = "projection_2d"),
      #         menuSubItem("Projection 3D", tabName = "projection_3d")
      #),
      
      menuItem("Prédiction", tabName = "prediction"),
      menuItem("Historique", tabName = "history"),
      menuItem("Export", tabName = "export"),
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
            
            div(
              style = "text-align: center; margin-bottom: 20px;",
              tags$img(
                src = "Logo_RollerClusteR.jpg",
                alt = "RollerClustR Logo",
                height = "220px",  # Ajuster selon la taille souhaitée
                style = "border-radius: 10px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
              )
            ),
            
            h3("Qu'est-ce que le clustering de variables ?"),
            p("Le clustering de variables consiste à regrouper des variables similaires ensemble,",
              "contrairement au clustering classique qui groupe des observations."),
            
            h4("Algorithmes disponibles :"),
            
            fluidRow(
              column(
                width = 6,
                div(
                  style = "background-color: #fff5f0; padding: 15px; border-radius: 5px; margin-bottom: 10px;",
                  h5(strong(" Algorithmes Développés"), style = "color: #d9534f;"),
                  tags$ul(
                    tags$li(strong("VAR_CAH :"), "Matrice de corrélation → Dissimilarité (1-R²) → CAH"),
                    tags$li(strong("VAR_KMEANS :"), "Attribution aléatoire → Itérations → Convergence (proximité = R²)"),
                    tags$li(strong("TandemVarClust :"), "AFDM → CAH sur valeurs propres")
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
                        min = 50, max = 500, value = 500, step = 50),
            
            sliderInput("sample_noise", "Niveau de bruit (NA) :", 
                        min = 0, max = 0.3, value = 0.0, step = 0.05),
            
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
      # ═══════════════════════════════════════════════════════════════════════════
      # ONGLET CONFIG - VERSION CORRIGÉE AVEC 3 TYPES DE VARIABLES
      # À remplacer dans ui.R
      # ═══════════════════════════════════════════════════════════════════════════
      
      tabItem(
        tabName = "config",
        
        # ═══════════════════════════════════════════════════════════════════════
        # SECTION 1 : Algorithme et Paramètres (INCHANGÉ)
        # ═══════════════════════════════════════════════════════════════════════
        
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
        
        # ═══════════════════════════════════════════════════════════════════════
        # SECTION 2 : NOUVELLE - Sélection des Variables (3 TYPES)
        # ═══════════════════════════════════════════════════════════════════════
        
        fluidRow(
          box(
            title = "📊 Classification des Variables",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            
            # Message explicatif
            wellPanel(
              style = "background-color: #f0f8ff; border-left: 4px solid #3c8dbc;",
              h4(icon("info-circle"), " Définissez le rôle de chaque variable"),
              tags$ul(
                tags$li(
                  tags$strong(style = "color: #00a65a;", icon("chart-bar"), " Active :"), 
                  " Utilisée pour construire le clustering"
                ),
                tags$li(
                  tags$strong(style = "color: #f39c12;", icon("eye"), " Illustrative :"), 
                  " Affichée dans les résultats mais n'influence pas le clustering"
                ),
                tags$li(
                  tags$strong(style = "color: #dd4b39;", icon("crystal-ball"), " Predict :"), 
                  " Sera prédite dans l'onglet Prédiction (après clustering)"
                )
              ),
              tags$p(
                class = "text-muted",
                icon("exclamation-triangle"),
                " Chaque variable ne peut avoir qu'UN SEUL rôle."
              )
            ),
            
            # Zone de configuration des variables (3 colonnes)
            fluidRow(
              # Colonne 1 : Variables Actives
              column(
                width = 4,
                box(
                  title = tagList(icon("chart-bar"), " Variables Actives"),
                  width = NULL,
                  status = "success",
                  solidHeader = TRUE,
                  
                  uiOutput("active_vars_ui"),
                  
                  hr(),
                  
                  actionButton(
                    "select_all_active", 
                    "✓ Tout sélectionner", 
                    class = "btn-sm btn-success btn-block"
                  ),
                  actionButton(
                    "select_numeric", 
                    "🔢 Sélectionner numériques", 
                    class = "btn-sm btn-info btn-block"
                  )
                )
              ),
              
              # Colonne 2 : Variables Illustratives
              column(
                width = 4,
                box(
                  title = tagList(icon("eye"), " Variables Illustratives"),
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  
                  tags$p(
                    class = "text-muted small",
                    "Variables projetées mais n'influençant pas le clustering"
                  ),
                  
                  uiOutput("illustrative_vars_ui"),
                  
                  hr(),
                  
                  actionButton(
                    "select_all_illus", 
                    "✓ Tout sélectionner", 
                    class = "btn-sm btn-warning btn-block"
                  )
                )
              ),
              
              # Colonne 3 : Variables à Prédire (NOUVEAU !)
              column(
                width = 4,
                box(
                  title = tagList(icon("crystal-ball"), " Variables à Prédire"),
                  width = NULL,
                  status = "danger",
                  solidHeader = TRUE,
                  
                  tags$p(
                    class = "text-muted small",
                    "Variables dont le cluster sera prédit dans l'onglet Prédiction"
                  ),
                  
                  uiOutput("predict_vars_ui"),
                  
                  hr(),
                  
                  actionButton(
                    "select_all_predict", 
                    "✓ Tout sélectionner", 
                    class = "btn-sm btn-danger btn-block"
                  )
                )
              )
            ),
            
            # Boutons d'action
            fluidRow(
              column(12,
                     hr(),
                     actionButton(
                       "validate_roles",
                       "✓ Valider la Configuration",
                       icon = icon("check-circle"),
                       class = "btn-success btn-lg btn-block",
                       style = "margin-top: 10px;"
                     )
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════════════════
        # SECTION 3 : Résumé de la Configuration
        # ═══════════════════════════════════════════════════════════════════════
        
        fluidRow(
          column(12,
                 uiOutput("roles_summary")
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
      
      # ========================================================================
      # ONGLET GRAPHIQUES SPÉCIFIQUES PAR ALGORITHME
      # ========================================================================
      tabItem(
        tabName = "algo_graphs",
        
        fluidRow(
          box(
            title = "Graphiques Spécifiques à l'Algorithme",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            p("Ces graphiques sont adaptés à l'algorithme de clustering que vous avez choisi."),
            
            # AFFICHER LE NOM DE L'ALGORITHME
            uiOutput("current_algorithm_display")
          ),
          
          box(
            title = "Résumé du Modèle",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            verbatimTextOutput("model_summary")
          )
        ),
        
        # ═══════════════════════════════════════════════════════════
        # GRAPHIQUES VAR_CAH
        # ═══════════════════════════════════════════════════════════
        conditionalPanel(
          condition = "input.algorithm == 'var_cah'",
          
          fluidRow(
            box(
              title = "Dendrogramme",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Arbre hiérarchique montrant les regroupements successifs des variables."),
              plotOutput("plot_dendrogram_cah", height = "600px")
            )
          ),
          
          fluidRow(
            box(
              title = "Matrice de Corrélation",
              width = 6,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Heatmap des corrélations entre variables."),
              plotlyOutput("plot_correlation_cah", height = "500px")
            ),
            
            box(
              title = "Homogénéité des Clusters",
              width = 6,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Mesure de la cohésion interne de chaque cluster."),
              DTOutput("table_homogeneity_cah"),
              
              br(),
              
              wellPanel(
                style = "background-color: #d1ecf1;",
                h5("Interprétation"),
                tags$ul(
                  tags$li(strong("Homogénéité > 0.7 :"), " Cluster très cohérent"),
                  tags$li(strong("0.5 < Homogénéité < 0.7 :"), " Cluster moyennement cohérent"),
                  tags$li(strong("Homogénéité < 0.5 :"), " Cluster peu cohérent")
                )
              )
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════
        # GRAPHIQUES VAR_KMEANS
        # ═══════════════════════════════════════════════════════════
        conditionalPanel(
          condition = "input.algorithm == 'var_kmeans'",
          
          fluidRow(
            box(
              title = "Méthode du Coude - Inertie Intra-classe",
              width = 8,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Graphique montrant l'inertie intra-classe en fonction du nombre de clusters K."),
              p(strong("Axes :"), "X = Nombre de clusters (K), Y = Inertie intra-classe"),
              p(strong("Objectif :"), "Identifier le 'coude' pour choisir le nombre optimal de clusters."),
              
              plotlyOutput("plot_inertia_kmeans", height = "500px"),
              
              br(),
              
              wellPanel(
                style = "background-color: #fff3cd;",
                h5("Comment interpréter ?"),
                p("Le nombre optimal de clusters se situe au 'coude' de la courbe, ",
                  "là où l'ajout d'un cluster supplémentaire n'améliore plus significativement l'inertie.")
              )
            ),
            
            box(
              title = "Tableau des Inerties",
              width = 4,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Valeurs numériques des inerties."),
              DTOutput("table_inertia_kmeans")
            )
          )
        ),
        
        # ═══════════════════════════════════════════════════════════
        # GRAPHIQUES TANDEMVARCLUST
        # ═══════════════════════════════════════════════════════════
        conditionalPanel(
          condition = "input.algorithm == 'tandem'",
          
          fluidRow(
            box(
              title = "Projection Factorielle",
              width = 6,
              status = "info",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Projection des variables dans l'espace factoriel de l'AFDM."),
              p(strong("Axes :"), "Axes factoriels 1 et 2"),
              
              plotlyOutput("plot_projection_tandem", height = "500px")
            ),
            
            box(
              title = "Dendrogramme sur Valeurs Propres",
              width = 6,
              status = "success",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Arbre hiérarchique construit sur les valeurs propres de l'AFDM."),
              plotOutput("plot_dendrogram_tandem", height = "500px")
            )
          ),
          
          fluidRow(
            box(
              title = "Tableau des Inerties",
              width = 12,
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              
              p(strong("Description :"), "Décomposition de l'inertie totale."),
              DTOutput("table_inertia_tandem"),
              
              br(),
              
              fluidRow(
                column(
                  width = 6,
                  wellPanel(
                    h5("Composantes de l'Inertie"),
                    tags$ul(
                      tags$li(strong("Inertie Totale :"), " Variance totale des données"),
                      tags$li(strong("Inertie Intra :"), " Variance à l'intérieur des clusters"),
                      tags$li(strong("Inertie Inter :"), " Variance entre les clusters")
                    )
                  )
                ),
                
                column(
                  width = 6,
                  wellPanel(
                    style = "background-color: #d4edda;",
                    h5("Critère de Qualité"),
                    p("Un bon clustering maximise l'inertie inter (clusters bien séparés) ",
                      "et minimise l'inertie intra (clusters homogènes).")
                  )
                )
              )
            )
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
        
        tabPanel(
          "Prédiction",
          icon = icon("crystal-ball"),
          
          # ───────────────────────────────────────────────────────────────────────
          # SECTION 1 : Choix de la source de données
          # ───────────────────────────────────────────────────────────────────────
          
          fluidRow(
            column(12,
                   wellPanel(
                     h3(icon("database"), "Source des Données à Prédire"),
                     
                     radioButtons(
                       inputId = "predict_source",
                       label = "Choisissez la source :",
                       choices = list(
                         "Variables marquées 'Predict' du fichier initial" = "internal",
                         "Importer un nouveau fichier (CSV/Excel/TXT)" = "external"
                       ),
                       selected = "internal"
                     )
                   )
            )
          ),
          
          # ───────────────────────────────────────────────────────────────────────
          # SECTION 2A : Variables internes (marquées Predict)
          # ───────────────────────────────────────────────────────────────────────
          
          conditionalPanel(
            condition = "input.predict_source == 'internal'",
            
            fluidRow(
              column(12,
                     wellPanel(
                       h4(icon("list"), "Variables Disponibles"),
                       
                       # Message si aucune variable
                       uiOutput("predict_vars_available_msg"),
                       
                       # Liste des variables avec checkboxes
                       uiOutput("predict_vars_selection")
                     )
              )
            )
          ),
          
          # ───────────────────────────────────────────────────────────────────────
          # SECTION 2B : Import fichier externe
          # ───────────────────────────────────────────────────────────────────────
          
          conditionalPanel(
            condition = "input.predict_source == 'external'",
            
            fluidRow(
              column(12,
                     wellPanel(
                       h4(icon("file-upload"), "Importer un Fichier"),
                       
                       fileInput(
                         inputId = "predict_file",
                         label = "Choisir un fichier (CSV, Excel, TXT)",
                         accept = c(
                           ".csv",
                           ".txt",
                           ".xlsx",
                           ".xls"
                         ),
                         buttonLabel = "Parcourir...",
                         placeholder = "Aucun fichier sélectionné"
                       ),
                       
                       # Paramètres CSV
                       conditionalPanel(
                         condition = "input.predict_file",
                         
                         fluidRow(
                           column(4,
                                  selectInput(
                                    "predict_separator",
                                    "Séparateur",
                                    choices = list(
                                      "Virgule (,)" = ",",
                                      "Point-virgule (;)" = ";",
                                      "Tabulation" = "\t",
                                      "Espace" = " "
                                    ),
                                    selected = ","
                                  )
                           ),
                           
                           column(4,
                                  selectInput(
                                    "predict_decimal",
                                    "Décimale",
                                    choices = list(
                                      "Point (.)" = ".",
                                      "Virgule (,)" = ","
                                    ),
                                    selected = "."
                                  )
                           ),
                           
                           column(4,
                                  checkboxInput(
                                    "predict_header",
                                    "Première ligne = noms colonnes",
                                    value = TRUE
                                  )
                           )
                         )
                       ),
                       
                       # Aperçu des données importées
                       uiOutput("predict_file_preview")
                     )
              )
            ),
            
            # Sélection des variables du fichier externe
            fluidRow(
              column(12,
                     uiOutput("predict_external_vars_selection")
              )
            )
          ),
          
          # ───────────────────────────────────────────────────────────────────────
          # SECTION 3 : Bouton de prédiction
          # ───────────────────────────────────────────────────────────────────────
          
          fluidRow(
            column(12,
                   tags$hr(),
                   
                   actionButton(
                     inputId = "predict_btn",
                     label = tagList(
                       icon("magic"),
                       "Prédire les Clusters"
                     ),
                     class = "btn-primary btn-lg btn-block",
                     style = "margin-top: 20px; margin-bottom: 20px;"
                   )
            )
          ),
          
          # ───────────────────────────────────────────────────────────────────────
          # SECTION 4 : Résultats
          # ───────────────────────────────────────────────────────────────────────
          
          fluidRow(
            column(12,
                   uiOutput("prediction_results")
            )
          ),
          
          # ───────────────────────────────────────────────────────────────────────
          # SECTION 5 : Export des résultats
          # ───────────────────────────────────────────────────────────────────────
          
          conditionalPanel(
            condition = "output.prediction_results",
            
            fluidRow(
              column(12,
                     tags$hr(),
                     
                     wellPanel(
                       h4(icon("download"), "Exporter les Résultats"),
                       
                       downloadButton(
                         outputId = "download_predictions",
                         label = "Télécharger (CSV)",
                         class = "btn-success"
                       )
                     )
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
          valueBoxOutput("diag_silhouette", width = 6),
          #valueBoxOutput("diag_davies_bouldin", width = 3),
          #valueBoxOutput("diag_dunn", width = 3),
          #valueBoxOutput("diag_calinski", width = 3)
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
        
        #fluidRow(
        #  box(
        #    title = " Analyse Détaillée",
        #    width = 12,
        #    status = "primary",
        #    solidHeader = TRUE,
            
        #    verbatimTextOutput("diagnostics_text")
        #  )
        #),
        
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
                    "Scores de Stabilité",
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
      
      
      
      # ========================================================================
      # ONGLET AIDE
      # ========================================================================
      tabItem(
        tabName = "help",
        
        # ═════════════════════════════════════════════════════════════════
        # GUIDE D'UTILISATION PRINCIPAL
        # ═════════════════════════════════════════════════════════════════
        fluidRow(
          box(
            title = "Guide d'Utilisation",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            
            h3("Documentation RollerClustR", style = "color: #3c8dbc;"),
            
            # ───────────────────────────────────────────────────────────────
            # 1. Import des Données
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("1️⃣ Import des Données"),
              tags$ul(
                tags$li(
                  strong("Formats supportés :"), 
                  "CSV (virgule, point-virgule, tabulation), Excel (.xlsx, .xls)"
                ),
                tags$li(
                  strong("Structure requise :"), 
                  "Observations en lignes, variables en colonnes"
                ),
                tags$li(
                  strong("Valeurs manquantes :"), 
                  "Choisissez votre stratégie (moyenne, médiane, suppression)"
                )
              )
            ),
            
            # ───────────────────────────────────────────────────────────────
            # 2. Jeux de Données Exemple
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("2️⃣ Génération de Données Exemple"),
              p("Testez l'application avec des jeux de données pré-configurés :"),
              tags$ul(
                tags$li(strong("Économique :"), "12 variables (PIB, Revenu, Emploi, Population, etc.)"),
                tags$li(strong("Biologique :"), "12 gènes groupés par fonction (Métabolisme, Croissance, Stress, Immunité)"),
                tags$li(strong("Marketing :"), "8 variables de comportement client (Visites, Achats, Satisfaction, etc.)"),
                tags$li(strong("Mixte :"), "Variables numériques et catégorielles combinées"),
                tags$li(strong("Catégoriel :"), "8 variables catégorielles pures")
              )
            ),
            
            # ───────────────────────────────────────────────────────────────
            # 3. Configuration des Algorithmes
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("3️⃣ Configuration des Algorithmes"),
              
              # VAR_CAH
              div(
                style = "margin-left: 20px; margin-top: 15px;",
                h5(
                  style = "color: #3c8dbc;",
                  icon("project-diagram"), 
                  strong(" VAR_CAH - Classification Ascendante Hiérarchique")
                ),
                tags$ul(
                  tags$li(strong("Principe :"), "Matrice de corrélation → Distance (1-R²) → CAH"),
                  tags$li(strong("Linkage :"), "Complete (codé en dur)"),
                  tags$li(strong("Distance :"), "1 - |Corrélation|"),
                  tags$li(strong("Variables synthétiques :"), "PC1 de chaque cluster"),
                  tags$li(strong("Adapté pour :"), "Variables numériques continues")
                )
              ),
              
              # VAR_KMEANS
              div(
                style = "margin-left: 20px; margin-top: 15px;",
                h5(
                  style = "color: #3c8dbc;",
                  icon("circle-notch"), 
                  strong(" VAR_KMEANS - K-Means pour Variables")
                ),
                tags$ul(
                  tags$li(strong("Principe :"), "Attribution aléatoire → Itérations → Convergence"),
                  tags$li(strong("Proximité :"), "Corrélation R²"),
                  tags$li(strong("Centres :"), "PC1 de chaque cluster"),
                  tags$li(strong("Avantage :"), "Rapide et efficace"),
                  tags$li(strong("Adapté pour :"), "Variables numériques continues")
                )
              ),
              
              # TandemVarClust
              div(
                style = "margin-left: 20px; margin-top: 15px;",
                h5(
                  style = "color: #3c8dbc;",
                  icon("layer-group"), 
                  strong(" TandemVarClust - Approche Tandem AFDM + CAH")
                ),
                tags$ul(
                  tags$li(strong("Principe :"), "AFDM → CAH sur valeurs propres"),
                  tags$li(strong("Étape 1 :"), "Analyse Factorielle des Données Mixtes"),
                  tags$li(strong("Étape 2 :"), "CAH sur les coordonnées factorielles"),
                  tags$li(strong("Spécificité :"), "Gère les variables mixtes (numériques + catégorielles)"),
                  tags$li(strong("Adapté pour :"), "Données hétérogènes")
                )
              )
            ),
            
            # ───────────────────────────────────────────────────────────────
            # 4. Visualisations et Résultats
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("4️⃣ Visualisations et Analyses"),
              
              tags$ul(
                tags$li(
                  strong("Résultats Principaux :"),
                  "Silhouette, dendrogramme, heatmap, distribution, projections 2D/3D"
                ),
                tags$li(
                  strong("Graphiques Algorithmes :"),
                  "Visualisations spécifiques à chaque algorithme",
                  tags$ul(
                    tags$li("VAR_CAH : Dendrogramme, matrice corrélation, homogénéité clusters"),
                    tags$li("VAR_KMEANS : Méthode du coude (inertie), tableau inerties"),
                    tags$li("TandemVarClust : Projection factorielle, dendrogramme valeurs propres")
                  )
                ),
                tags$li(
                  strong("Contribution Variables :"),
                  "Analyse de l'importance des variables dans chaque cluster"
                ),
                tags$li(
                  strong("Diagnostics :"),
                  "Variables discriminantes, qualité du clustering"
                ),
                tags$li(
                  strong("Stabilité Bootstrap :"),
                  "Évaluation de la robustesse du clustering"
                )
              )
            ),
            
            # ───────────────────────────────────────────────────────────────
            # 5. Prédiction (Optionnel)
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("5️⃣ Prédiction (si implémentée)"),
              p("Après le clustering, classifiez de nouvelles variables :"),
              tags$ul(
                tags$li("Importer un fichier avec nouvelles variables"),
                tags$li("Ou générer une variable aléatoire pour tester"),
                tags$li("Le modèle assignera automatiquement au cluster le plus proche")
              ),
              div(
                style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; margin-top: 10px;",
                icon("exclamation-triangle"),
                strong(" Note :"), 
                " La prédiction nécessite que la méthode predict() soit implémentée 
           dans les classes R6."
              )
            ),
            
            # ───────────────────────────────────────────────────────────────
            # 6. Historique
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("6️⃣ Historique et Sauvegarde"),
              p("Gérez vos sessions de travail :"),
              tags$ul(
                tags$li("Donnez un nom descriptif à votre session"),
                tags$li("Ajoutez des notes pour vous rappeler le contexte"),
                tags$li("Exportez/importez vos sessions entre ordinateurs"),
                tags$li("Comparez différentes configurations")
              )
            ),
            
            # ───────────────────────────────────────────────────────────────
            # 7. Interprétation des Métriques
            # ───────────────────────────────────────────────────────────────
            div(
              style = "margin-top: 20px;",
              h4("7️⃣ Interprétation des Métriques de Qualité"),
              
              tags$table(
                class = "table table-striped",
                style = "margin-top: 15px;",
                tags$thead(
                  tags$tr(
                    tags$th("Métrique"),
                    tags$th("Plage"),
                    tags$th("Interprétation"),
                    tags$th("Objectif")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td(strong("Silhouette")),
                    tags$td("[-1, 1]"),
                    tags$td("Cohésion et séparation des clusters"),
                    tags$td("Plus proche de 1 = meilleur")
                  ),
                  tags$tr(
                    tags$td(strong("Davies-Bouldin")),
                    tags$td("[0, ∞]"),
                    tags$td("Ratio distance intra/inter"),
                    tags$td("Plus bas = meilleur")
                  ),
                  tags$tr(
                    tags$td(strong("Dunn Index")),
                    tags$td("[0, ∞]"),
                    tags$td("Ratio distance min inter / max intra"),
                    tags$td("Plus haut = meilleur")
                  ),
                  tags$tr(
                    tags$td(strong("Calinski-Harabasz")),
                    tags$td("[0, ∞]"),
                    tags$td("Ratio variance inter/intra"),
                    tags$td("Plus haut = meilleur")
                  )
                )
              )
            ),
            
            hr(style = "margin-top: 30px; margin-bottom: 20px;"),
            
            # ───────────────────────────────────────────────────────────────
            # Support
            # ───────────────────────────────────────────────────────────────
            div(
              style = "background-color: #d4edda; padding: 15px; border-radius: 5px; border-left: 4px solid #28a745;",
              h4(icon("life-ring"), " Support", style = "margin-top: 0;"),
              p("Pour toute question ou suggestion d'amélioration, consultez la 
          documentation des algorithmes ou contactez l'équipe de développement.")
            )
          )
        ),
        
        # ═════════════════════════════════════════════════════════════════
        # RÉFÉRENCES ET LIENS
        # ═════════════════════════════════════════════════════════════════
        fluidRow(
          # Références Algorithmes
          box(
            title = "Références Algorithmes",
            width = 6,
            status = "success",
            solidHeader = TRUE,
            
            div(
              style = "margin-bottom: 15px;",
              h5(strong("VAR_CAH"), style = "color: #28a745;"),
              p(
                "Classification Ascendante Hiérarchique adaptée pour le clustering 
          de variables avec utilisation de la première composante principale 
          comme variable synthétique."
              )
            ),
            
            div(
              style = "margin-bottom: 15px;",
              h5(strong("VAR_KMEANS"), style = "color: #28a745;"),
              p(
                "Adaptation de l'algorithme K-Means pour le clustering de variables 
          en utilisant la corrélation comme mesure de proximité."
              )
            ),
            
            div(
              style = "margin-bottom: 15px;",
              h5(strong("TandemVarClust"), style = "color: #28a745;"),
              p(
                "Approche en deux étapes combinant l'Analyse Factorielle des Données 
          Mixtes (AFDM) et la Classification Ascendante Hiérarchique sur les 
          valeurs propres."
              )
            )
          ),
          
          # Liens Utiles
          box(
            title = "Membres d'équipe",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            #h5("Romain BUONO"),
            tags$ul(
              tags$li(
                tags$a(
                  href = "r.buono@univ-lyon2.fr", 
                  "Romain BUONO",
                  target = "_blank"
                )
              ),
              tags$li(
                tags$a(
                  href = "nico.dena@univ-lyon2.fr", 
                  "Nico DENA",
                  target = "_blank"
                )
              ),
              tags$li(
                tags$a(
                  href = "h.bah@univ-lyon2.fr", 
                  "Habib BAH",
                  target = "_blank"
                )
            ),
            
            hr(),
            
            h5("Ressources Complémentaires"),
            tags$ul(
              tags$li("Documentation Shiny"),
              tags$li("Tutoriels clustering de variables"),
              tags$li("Forums R et Stack Overflow")
            )
          )
        )
      )
      )
    )
  )
)