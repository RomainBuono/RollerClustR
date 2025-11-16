# ============================================================
# app.R – Variable Clustering Studio
# Clustering de VARIABLES (colonnes) avec :
#  - VAR_CAH (numérique, CAH sur corrélation)
#  - KmodesVarClust (catégoriel / mixte)
#  - VARCLUS (descendant type VARCLUS SAS)
# ============================================================

# ---- Packages ----
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(bsicons)
library(readr)
library(R6)

# ---- SOURCES (adapte les noms si besoin) ----
source("ClusterAnalysis_parentclass.R")
source("KmodesVarClust.R")
source("VAR_CAH.R")
source("VARCLUS.R")
source("user_functions.R")   # fonctions utilitaires
source("wrapper.R")         # ClusteringHelper, etc.

# ---- THEME ----
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins"),
  code_font = font_google("JetBrains Mono"),
  primary   = "#2E86DE",
  secondary = "#6C757D",
  success   = "#00C853",
  info      = "#17A2B8",
  warning   = "#FFC107",
  danger    = "#FF5252",
  light     = "#F8F9FA",
  dark      = "#212529"
)

colorblind_palette <- c(
  "#0072B2", "#E69F00", "#009E73", "#F0E442",
  "#56B4E9", "#D55E00", "#CC79A7", "#999999"
)

nice_card <- function(title, ..., height = NULL) {
  card(
    card_header(
      div(
        bs_icon("three-dots"),
        title,
        class = "d-flex align-items-center gap-2"
      )
    ),
    ...,
    height = height,
    class = "shadow-sm rounded-4"
  )
}

# ---- Helper qualité clusters VAR_CAH ----
compute_cluster_quality_varcah <- function(X, groupes) {
  vars <- colnames(X)
  if (is.null(names(groupes))) {
    names(groupes) <- vars
  }
  clusters_uniques <- sort(unique(groupes[!is.na(groupes)]))
  
  res <- data.frame(
    cluster = integer(),
    n_vars = integer(),
    mean_abs_cor = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (cl in clusters_uniques) {
    v_in <- names(groupes)[groupes == cl]
    if (length(v_in) >= 2) {
      subX <- X[, v_in, drop = FALSE]
      cor_mat <- cor(subX, use = "pairwise.complete.obs")
      upper <- abs(cor_mat[upper.tri(cor_mat)])
      mean_cor <- mean(upper, na.rm = TRUE)
      res <- rbind(res, data.frame(
        cluster = cl,
        n_vars = length(v_in),
        mean_abs_cor = mean_cor
      ))
    } else {
      res <- rbind(res, data.frame(
        cluster = cl,
        n_vars = length(v_in),
        mean_abs_cor = NA_real_
      ))
    }
  }
  res
}

# ---- UI ----
ui <- page_navbar(
  title = div(
    bs_icon("grid-1x2"),
    "Variable Clustering Studio",
    class = "d-flex align-items-center gap-2"
  ),
  theme = app_theme,
  collapsible = TRUE,
  
  # --- CSS GLOBAL POUR UN DESIGN PLUS PROPRE ---
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #eef2f7 100%);
      }

      .navbar {
        box-shadow: 0 4px 14px rgba(0,0,0,0.08);
      }

      .card {
        border-radius: 1rem !important;
        border: none !important;
        box-shadow: 0 4px 18px rgba(0,0,0,0.04);
        transition: transform 0.15s ease, box-shadow 0.15s ease;
      }

      .card:hover {
        transform: translateY(-3px);
        box-shadow: 0 10px 22px rgba(0,0,0,0.08);
      }

      .card-header {
        border-bottom: none !important;
        background-color: transparent !important;
        font-weight: 600;
      }

      .sidebar {
        background: #ffffff !important;
        box-shadow: 4px 0 12px rgba(0,0,0,0.04);
      }

      .form-label {
        font-weight: 500;
      }

      .btn-primary, .btn-success {
        border-radius: 999px !important;
      }

      .shiny-input-container {
        margin-bottom: 0.8rem;
      }

      .dataTables_wrapper .dataTables_paginate .paginate_button {
        border-radius: 999px !important;
      }
    "))
  ),
  
  # ---------- SIDEBAR ----------
  sidebar = sidebar(
    open = "always",
    width = 360,
    class = "pt-3",
    
    h4(bs_icon("sliders"), "Configuration du clustering"),
    tags$small(
      "Clustering de VARIABLES (colonnes) avec VAR_CAH, K-modes ou VARCLUS."
    ),
    br(),
    
    h5("1) Importer des données"),
    fileInput("file", "Fichier (CSV/TSV)", accept = c(".csv", ".tsv", ".txt")),
    radioButtons(
      "sep", "Séparateur", inline = TRUE,
      choices = c("," = ",", ";" = ";", "\\t" = "\t", "Auto" = "auto"),
      selected = "auto"
    ),
    materialSwitch("header", "Première ligne = en-têtes", value = TRUE),
    fluidRow(
      column(
        6,
        actionBttn("load_demo_mtcars", "Demo mtcars", style = "bordered",
                   color = "primary", size = "sm")
      ),
      column(
        6,
        actionBttn("load_demo_iris", "Demo iris", style = "bordered",
                   color = "primary", size = "sm")
      )
    ),
    hr(),
    
    h5("2) Choix de l'algorithme"),
    selectInput(
      "algo", "Algorithme de clustering de variables",
      choices = c(
        "VAR_CAH (numérique, CAH sur corrélations)" = "var_cah",
        "K-modes (variables catégorielles / mixtes)" = "kmodes_varclust",
        "VARCLUS (descendant, type SAS)"           = "varclus"
      ),
      selected = "var_cah"
    ),
    
    h5("3) Sélection des variables"),
    uiOutput("var_picker"),
    
    hr(),
    h5("4) Paramètres de l'algorithme"),
    
    conditionalPanel(
      condition = "input.algo != 'varclus'",
      sliderInput("k", "Nombre de clusters (k)",
                  min = 2, max = 12, value = 3, step = 1)
    ),
    
    conditionalPanel(
      condition = "input.algo == 'var_cah'",
      prettySwitch("scale_varcah", "Standardiser (z-score)",
                   value = TRUE, status = "info")
    ),
    
    conditionalPanel(
      condition = "input.algo == 'kmodes_varclust'",
      numericInput("maxiter_kmodes", "Itérations max (K-modes)",
                   value = 100, min = 10, step = 10)
    ),
    
    conditionalPanel(
      condition = "input.algo == 'varclus'",
      numericInput("stop_eig", "Seuil λ₂ (stop_eigenvalue)",
                   value = 1.0, min = 0.5, max = 3, step = 0.1),
      selectInput("dist_metric", "Distance / structure interne",
                  choices = c("correlation"), selected = "correlation"),
      tags$small(
        "VARCLUS détermine automatiquement le nombre de clusters en fonction de λ₂."
      )
    ),
    
    hr(),
    h5("5) Lancer le clustering"),
    actionButton("run", bs_icon("play-fill"), class = "btn btn-primary w-100"),
    br(), br(),
    downloadButton("export_clusters", "Exporter les clusters (CSV)",
                   class = "btn-success w-100")
  ),
  
  # ---------- Onglet Accueil / Aperçu ----------
  nav_panel(
    "Aperçu",
    layout_columns(
      nice_card(
        "Aperçu des données",
        DTOutput("preview") %>% withSpinner(type = 4),
        height = "380px"
      ),
      nice_card(
        "Résumé",
        verbatimTextOutput("summary_data") %>% withSpinner(type = 4),
        height = "380px"
      )
    )
  ),
  
  # ---------- Onglet Résultats ----------
  nav_panel(
    "Résultats",
    layout_columns(
      nice_card(
        "Résumé du modèle",
        verbatimTextOutput("model_summary") %>% withSpinner(type = 4)
      ),
      nice_card(
        "Affectations de variables",
        DTOutput("cluster_table") %>% withSpinner(type = 4)
      )
    )
  ),
  
  # ---------- Onglet Visualisations ----------
  nav_panel(
    "Visualisations",
    layout_columns(
      nice_card(
        "Matrice de corrélation (si variables numériques)",
        plotOutput("cor_plot", height = "420px") %>% withSpinner(type = 4)
      ),
      nice_card(
        "Dendrogramme des variables (si numérique)",
        plotOutput("dendro_plot", height = "420px") %>% withSpinner(type = 4)
      ),
      nice_card(
        "Tailles des clusters de variables",
        plotOutput("bar_sizes", height = "380px") %>% withSpinner(type = 4)
      ),
      nice_card(
        "Inertie / qualité du clustering (VAR_CAH)",
        plotOutput("inertia_plot", height = "380px") %>% withSpinner(type = 4)
      )
    )
  ),
  
  # ---------- Onglet Aide au choix de k ----------
  nav_panel(
    "Aide au choix de k",
    layout_columns(
      nice_card(
        "VAR_CAH – Qualité moyenne intra-cluster (|corr|)",
        plotOutput("k_quality_plot", height = "420px") %>% withSpinner(type = 4)
      ),
      nice_card(
        "Explications",
        htmlOutput("k_help_text")
      )
    )
  ),
  
  # ---------- Onglet Rapport ----------
  nav_panel(
    "Rapport",
    layout_columns(
      nice_card(
        "Rapport textuel du clustering",
        verbatimTextOutput("report_text") %>% withSpinner(type = 4),
        height = "480px"
      ),
      nice_card(
        "Téléchargement du rapport",
        div(
          p("Génère un rapport synthétique à partir de l’objet de clustering."),
          downloadButton("download_report", "Télécharger le rapport (.txt)",
                         class = "btn btn-outline-primary")
        ),
        height = "200px"
      )
    )
  ),
  
  # ---------- Onglet Guide ----------
  nav_panel(
    "Guide",
    layout_columns(
      nice_card(
        "Comment utiliser l'application ?",
        div(
          tags$ol(
            tags$li("Importer un fichier ou utiliser un des jeux de démonstration."),
            tags$li("Choisir l'algorithme : VAR_CAH, K-modes ou VARCLUS."),
            tags$li("Sélectionner les variables à clusteriser (les colonnes)."),
            tags$li("Régler les paramètres : k (sauf VARCLUS), standardisation, etc."),
            tags$li("Cliquer sur « Lancer » puis analyser les résultats et visualisations."),
            tags$li("Exporter les clusters et le rapport si besoin.")
          ),
          tags$hr(),
          tags$strong("Important : "),
          " ici, on regroupe des ",
          tags$em("variables entre elles"),
          ", pas des individus."
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Modal de bienvenue
  showModal(
    modalDialog(
      title = tagList(bs_icon("stars"), "Bienvenue dans Variable Clustering Studio"),
      easyClose = TRUE,
      size = "m",
      tagList(
        p("Cette application est dédiée au ", strong("clustering de variables"), 
          " (colonnes) via :"),
        tags$ul(
          tags$li(strong("VAR_CAH :"), " CAH sur matrice de corrélation (variables numériques)."),
          tags$li(strong("K-modes :"), " variables catégorielles ou mixtes (numériques + facteurs)."),
          tags$li(strong("VARCLUS :"), " clustering descendant de variables (style SAS).")
        ),
        p("Suivez les étapes dans la barre latérale.")
      ),
      footer = modalButton("Commencer")
    )
  )
  
  data_rv  <- reactiveVal(NULL)
  model_rv <- reactiveVal(NULL)
  helper   <- ClusteringHelper$new()
  
  # ---------- Import des données ----------
  observeEvent(input$file, {
    req(input$file)
    path <- input$file$datapath
    sep_choice <- input$sep
    if (sep_choice == "auto") {
      first <- readLines(path, n = 1)
      delim <- if (grepl("\t", first)) "\t" else if (grepl(";", first)) ";" else ","
    } else {
      delim <- sep_choice
    }
    df <- readr::read_delim(path, delim = delim,
                            col_names = input$header,
                            show_col_types = FALSE)
    data_rv(as.data.frame(df))
  })
  
  observeEvent(input$load_demo_mtcars, {
    data_rv(mtcars)
  })
  
  observeEvent(input$load_demo_iris, {
    df <- iris
    data_rv(df)
  })
  
  # ---------- Sélection des variables ----------
  output$var_picker <- renderUI({
    df <- data_rv()
    if (is.null(df)) {
      return(helpText("Charge un jeu de données pour voir les variables disponibles."))
    }
    
    col_choices <- colnames(df)
    alg <- input$algo
    
    if (alg %in% c("var_cah", "varclus")) {
      is_num <- sapply(df, is.numeric)
      col_choices <- colnames(df)[is_num]
      if (length(col_choices) == 0) {
        return(helpText("Aucune variable numérique disponible pour cet algorithme."))
      }
    }
    
    pickerInput(
      "vars", "Variables à clusteriser (colonnes)",
      choices = col_choices,
      selected = col_choices,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE
      )
    )
  })
  
  # ---------- Aperçu & résumé ----------
  output$preview <- renderDT({
    req(data_rv())
    datatable(head(data_rv(), 15),
              options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$summary_data <- renderPrint({
    req(data_rv())
    df <- data_rv()
    cat("Dimensions :", nrow(df), "lignes x", ncol(df), "colonnes\n\n")
    print(summary(df))
  })
  
  # ---------- Data utilisée pour le clustering ----------
  X_vars <- reactive({
    req(data_rv(), input$vars)
    df <- data_rv()[, input$vars, drop = FALSE]
    validate(need(ncol(df) >= 2, "Sélectionne au moins 2 variables."))
    df
  })
  
  # ---------- Lancer le clustering ----------
  observeEvent(input$run, {
    req(X_vars())
    df  <- X_vars()
    alg <- input$algo
    
    if (alg == "var_cah") {
      num_cols <- sapply(df, is.numeric)
      if (!all(num_cols)) {
        showNotification("VAR_CAH : seules les variables numériques sont utilisées.",
                         type = "warning")
      }
      df_num <- df[, num_cols, drop = FALSE]
      validate(need(ncol(df_num) >= 2, "VAR_CAH nécessite au moins 2 variables numériques."))
      obj <- VAR_CAH$new(scale = input$scale_varcah, k = input$k)
      obj$fit(df_num)
      model_rv(list(
        model = obj,
        algo  = "var_cah",
        X_used = df_num
      ))
    } else if (alg == "kmodes_varclust") {
      obj <- KmodesVarClust$new(k = input$k, max_iter = input$maxiter_kmodes)
      obj$fit(df)
      model_rv(list(
        model = obj,
        algo  = "kmodes_varclust",
        X_used = df
      ))
    } else if (alg == "varclus") {
      num_cols <- sapply(df, is.numeric)
      df_num <- df[, num_cols, drop = FALSE]
      validate(need(ncol(df_num) >= 3,
                    "VARCLUS nécessite au moins 3 variables numériques."))
      obj <- VARCLUS$new(
        stop_eigenvalue = input$stop_eig,
        distance_metric = input$dist_metric,
        cr = FALSE
      )
      obj$fit(df_num)
      model_rv(list(
        model = obj,
        algo  = "varclus",
        X_used = df_num
      ))
    }
    
    showNotification("Clustering terminé.", type = "message")
  })
  
  # ---------- Résumé du modèle ----------
  output$model_summary <- renderPrint({
    res <- model_rv()
    req(res)
    obj <- res$model
    alg <- res$algo
    
    algo_label <- switch(
      alg,
      var_cah         = "VAR_CAH (CAH sur corrélations)",
      kmodes_varclust = "KmodesVarClust (K-modes)",
      varclus         = "VARCLUS (clustering descendant)",
      alg
    )
    
    cat("Algorithme :", algo_label, "\n")
    if (alg != "varclus") cat("k =", input$k, "\n\n") else cat("\n")
    
    cat("Résumé du modèle :\n")
    cat("--------------------------------------------------\n")
    capture <- utils::capture.output(obj$summary())
    cat(paste(capture, collapse = "\n"))
  })
  
  # ---------- Table des clusters ----------
  output$cluster_table <- renderDT({
    res <- model_rv()
    req(res)
    obj <- res$model
    groupes <- obj$Groupes
    df <- data.frame(
      Variable = names(groupes),
      Cluster  = as.integer(groupes),
      stringsAsFactors = FALSE
    )
    df <- df[order(df$Cluster, df$Variable), ]
    datatable(df, options = list(pageLength = 20, scrollX = TRUE))
  })
  
  # ---------- Visualisation : corrélation ----------
  output$cor_plot <- renderPlot({
    res <- model_rv()
    req(res)
    if (!res$algo %in% c("var_cah", "varclus")) {
      plot.new()
      title("Matrice de corrélation : VAR_CAH ou VARCLUS (variables numériques) uniquement.")
      return(invisible(NULL))
    }
    X <- res$X_used
    validate(need(ncol(X) >= 2, "Au moins 2 variables numériques nécessaires."))
    plot_correlation_matrix(X, method = "pearson", cluster_vars = TRUE)
  })
  
  # ---------- Visualisation : dendrogramme ----------
  output$dendro_plot <- renderPlot({
    res <- model_rv()
    req(res)
    if (!res$algo %in% c("var_cah", "varclus")) {
      plot.new()
      title("Dendrogramme : VAR_CAH ou VARCLUS (variables numériques) uniquement.")
      return(invisible(NULL))
    }
    X <- res$X_used
    validate(need(ncol(X) >= 2, "Au moins 2 variables numériques nécessaires."))
    plot_dendrogram(X, method = "correlation", linkage = "ward.D2",
                    k = if (res$algo == "var_cah") input$k else NULL)
  })
  
  # ---------- Taille des clusters ----------
  output$bar_sizes <- renderPlot({
    res <- model_rv()
    req(res)
    obj <- res$model
    groupes <- obj$Groupes
    tab <- as.data.frame(table(groupes))
    colnames(tab) <- c("Cluster", "Size")
    ggplot(tab, aes(x = factor(Cluster), y = Size, fill = factor(Cluster))) +
      geom_col() +
      geom_text(aes(label = Size), vjust = -0.3) +
      scale_fill_manual(values = colorblind_palette) +
      theme_minimal(base_size = 13) +
      labs(title = "Taille des clusters de variables",
           x = "Cluster", y = "Nombre de variables") +
      theme(legend.position = "none")
  })
  
  # ---------- Inertie / Qualité (pour VAR_CAH) ----------
  output$inertia_plot <- renderPlot({
    df <- X_vars()
    req(df)
    alg <- input$algo
    
    if (alg != "var_cah") {
      plot.new()
      title("Inertie / qualité : disponible pour VAR_CAH (variables numériques) uniquement.")
      return(invisible(NULL))
    }
    
    num_cols <- sapply(df, is.numeric)
    df_num <- df[, num_cols, drop = FALSE]
    validate(need(ncol(df_num) >= 2, "VAR_CAH nécessite au moins 2 variables numériques."))
    
    k_min <- 2
    k_max <- max(3, min(10, ncol(df_num)))
    kvals <- k_min:k_max
    quality <- numeric(length(kvals))
    
    for (i in seq_along(kvals)) {
      k <- kvals[i]
      objk <- VAR_CAH$new(scale = input$scale_varcah, k = k)
      objk$fit(df_num)
      groupes_k <- objk$Groupes
      q_df <- compute_cluster_quality_varcah(df_num, groupes_k)
      quality[i] <- mean(q_df$mean_abs_cor, na.rm = TRUE)
    }
    
    q_data <- data.frame(
      k = kvals,
      inertia_expliquee = quality * 100  # en %
    )
    
    ggplot(q_data, aes(x = k, y = inertia_expliquee)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      theme_minimal(base_size = 13) +
      labs(
        title = "Inertie expliquée (proxy) en fonction de k – VAR_CAH",
        x = "Nombre de clusters (k)",
        y = "Inertie expliquée (%) \n(basée sur la cohérence intra-cluster)"
      ) +
      scale_x_continuous(breaks = kvals) +
      ylim(0, 100)
  })
  
  # ---------- Aide au choix de k ----------
  output$k_quality_plot <- renderPlot({
    df <- X_vars()
    req(df)
    alg <- input$algo
    
    if (alg != "var_cah") {
      plot.new()
      title("Analyse de k implémentée pour VAR_CAH uniquement (variables numériques).")
      return(invisible(NULL))
    }
    
    num_cols <- sapply(df, is.numeric)
    df_num <- df[, num_cols, drop = FALSE]
    validate(need(ncol(df_num) >= 2, "VAR_CAH nécessite au moins 2 variables numériques."))
    
    k_min <- 2
    k_max <- max(3, min(10, ncol(df_num)))
    kvals <- k_min:k_max
    quality <- numeric(length(kvals))
    
    for (i in seq_along(kvals)) {
      k <- kvals[i]
      objk <- VAR_CAH$new(scale = input$scale_varcah, k = k)
      objk$fit(df_num)
      groupes_k <- objk$Groupes
      q_df <- compute_cluster_quality_varcah(df_num, groupes_k)
      quality[i] <- mean(q_df$mean_abs_cor, na.rm = TRUE)
    }
    
    q_data <- data.frame(k = kvals, mean_abs_cor = quality)
    ggplot(q_data, aes(x = k, y = mean_abs_cor)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      theme_minimal(base_size = 13) +
      labs(
        title = "VAR_CAH – Qualité moyenne intra-cluster (|corr|)",
        x = "Nombre de clusters (k)",
        y = "Qualité moyenne (corrélation absolue)"
      ) +
      scale_x_continuous(breaks = kvals) +
      ylim(0, 1)
  })
  
  output$k_help_text <- renderUI({
    alg <- input$algo
    if (alg != "var_cah") {
      HTML(
        "<p><b>Aide au choix de k</b> :</p>
         <p>L'analyse automatique de k (via corrélation intra-cluster) est implémentée pour 
         <b>VAR_CAH</b> uniquement.</p>
         <p>Pour <b>K-modes</b> et <b>VARCLUS</b> :</p>
         <ul>
           <li>Pour K-modes : teste plusieurs valeurs de k et observe la cohérence des groupes.</li>
           <li>Pour VARCLUS : le nombre de clusters est déterminé automatiquement par le critère λ₂ ≥ seuil.</li>
         </ul>"
      )
    } else {
      HTML(
        "<p><b>Interprétation du graphique :</b></p>
         <ul>
           <li>La courbe montre la <b>qualité moyenne intra-cluster</b> 
           (corrélation absolue entre variables d’un même cluster et leur composante principale).</li>
           <li>Plus la valeur est élevée (proche de 1), plus les variables d’un cluster sont homogènes.</li>
           <li>Choisis un k où la courbe commence à se stabiliser (idée du <i>coude</i>).</li>
         </ul>"
      )
    }
  })
  
  # ---------- Rapport textuel ----------
  output$report_text <- renderPrint({
    res <- model_rv()
    req(res)
    helper$generate_report(res$model, file = NULL)
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("rapport_clustering_variables_", Sys.Date(), ".txt")
    },
    content = function(file) {
      res <- model_rv()
      req(res)
      helper$generate_report(res$model, file = file)
    }
  )
  
  # ---------- Export des clusters ----------
  output$export_clusters <- downloadHandler(
    filename = function() paste0("clusters_variables_", Sys.Date(), ".csv"),
    content = function(file) {
      res <- model_rv()
      req(res)
      obj <- res$model
      groupes <- obj$Groupes
      df <- data.frame(
        Variable = names(groupes),
        Cluster  = as.integer(groupes),
        stringsAsFactors = FALSE
      )
      df <- df[order(df$Cluster, df$Variable), ]
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
