# Vérifie quels packages ne sont pas installés
packages <- c(
  "shiny",
  #"votre_package_clustering",
  "DT",
  "plotly",
  "shinydashboard"
)
packages_manquants <- packages[!(packages %in% installed.packages()[, "Package"])]

# Installe uniquement ceux qui manquent
if (length(packages_manquants) > 0) {
  cat("Installation des packages manquants :", paste(packages_manquants, collapse = ", "), "\n")
  install.packages(packages_manquants, dependencies = TRUE)
} else {
  cat("Tous les packages sont déjà installés.\n")
}

# Charge tous les packages
invisible(lapply(packages, library, character.only = TRUE))
cat("Tous les packages ont été chargés avec succès.\n")

########################
# interface utilisateur
########################

ui <- fluidPage(
  titlePanel("Application de Clustering et Prédiction"),
  
  sidebarLayout(
    sidebarPanel(
      # Upload de données
      fileInput("file", "Charger les données (CSV)", 
                accept = c(".csv")),
      
      # Sélection de l'algorithme
      selectInput("algo", "Algorithme de clustering",
                  choices = c("K-means" = "kmeans",
                            "Hiérarchique" = "hclust",
                            "DBSCAN" = "dbscan")),
      
      # Paramètres dynamiques selon l'algorithme
      uiOutput("parametres_algo"),
      
      actionButton("run_clustering", "Lancer le clustering",
                   class = "btn-primary"),
      
      hr(),
      
      # Section prédiction
      h4("Prédiction"),
      fileInput("new_data", "Nouvelles données pour prédiction"),
      actionButton("run_prediction", "Prédire")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Données", DTOutput("data_table")),
        tabPanel("Résultats Clustering", 
                 plotlyOutput("cluster_plot"),
                 verbatimTextOutput("cluster_stats")),
        tabPanel("Prédictions", 
                 DTOutput("predictions_table"))
      )
    )
  )
)

##################
# Partie Serveur
##################

server <- function(input, output, session) {
  
  # Stockage réactif des données et modèles
  donnees <- reactiveVal(NULL)
  modele_clustering <- reactiveVal(NULL)
  resultats_clustering <- reactiveVal(NULL)
  
  # Chargement des données
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    donnees(df)
  })
  
  # Affichage des données
  output$data_table <- renderDT({
    req(donnees())
    datatable(donnees())
  })

  ####################################
  # Paramètres dynamiques selon l'algo
  ####################################

  output$parametres_algo <- renderUI({
    req(input$algo)
    
    if(input$algo == "kmeans") {
      tagList(
        numericInput("k", "Nombre de clusters (k)", 
                     value = 3, min = 2, max = 10),
        numericInput("nstart", "Nombre d'initialisations", 
                     value = 25)
      )
    } else if(input$algo == "hclust") {
      tagList(
        selectInput("method", "Méthode de liaison",
                   choices = c("complete", "single", "average")),
        numericInput("h", "Hauteur de coupe", value = 3)
      )
    } else if(input$algo == "dbscan") {
      tagList(
        numericInput("eps", "Epsilon (rayon)", value = 0.5),
        numericInput("minPts", "Points minimum", value = 5)
      )
    }
  })