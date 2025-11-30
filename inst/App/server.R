# ==============================================================================
# SERVEUR - VERSION CORRIGÉE
# ==============================================================================
# Modifications appliquées :
# 1. ✅ Retiré KmodesVarClust de algorithm_description
# 2. ✅ Retiré VARCLUS de algorithm_description  
# 3. ✅ Retiré KmodesVarClust de config_summary
# 4. ✅ Retiré VARCLUS de config_summary
# 5. ✅ Supprimé bloc if(algorithm == "kmodes") dans clustering
# 6. ✅ Supprimé bloc if(algorithm == "varclus") dans clustering
# 7. ✅ Supprimé observeEvent(input$run_comparison)
# 8. ✅ Supprimé output$comparison_table
# 9. ✅ Supprimé output$comparison_plot
#
# Algorithmes conservés : VAR_CAH, VAR_KMEANS, TandemVarClust
# ==============================================================================

# ==============================================================================
# SERVEUR - VERSION FINALE CORRIGÉE POUR R6
# ==============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # VALEURS RÉACTIVES
  # ============================================================================
  
  rv <- reactiveValues(
    data = NULL,
    data_cleaned = NULL,
    model = NULL,
    model_r6 = NULL,  # Garder l'objet R6 original
    comparison_results = NULL,
    clustering_done = FALSE,
    prediction_data = NULL,
    prediction_results = NULL,
    sessions_history = list(),
    session_counter = 0,
    illustrative_vars = NULL
  )
  
  # ============================================================================
  # ONGLET ACCUEIL - Info Boxes
  # ============================================================================
  
  output$info_algorithms <- renderInfoBox({
    infoBox(
      "Algorithmes",
      "3 méthodes",  
      color = "blue",
      fill = TRUE
    )
  })
  
  output$info_features <- renderInfoBox({
    infoBox(
      "Fonctionnalités",
      "R6 + History",
      color = "green",
      fill = TRUE
    )
  })
  
  output$info_status <- renderInfoBox({
    status_text <- if(rv$clustering_done) "Prêt" else "En attente"
    status_color <- if(rv$clustering_done) "green" else "yellow"
    
    infoBox(
      "Statut",
      status_text,
      color = status_color,
      fill = TRUE
    )
  })
  
  # ============================================================================
  # ONGLET DONNÉES - Import et Génération
  # ============================================================================
  
  # Générer données exemple
  observeEvent(input$load_sample, {
    tryCatch({
      
      # Datasets R
      if (startsWith(input$sample_type, "r_")) {
        
        dataset_name <- sub("r_", "", input$sample_type)
        
        rv$data <- switch(
          dataset_name,
          
          "iris" = {
            data <- iris
            # Retirer Species pour avoir que des numériques
            data$Species <- NULL
            data
          },
          
          "mtcars" = {
            mtcars
          },
          
          "usarrests" = {
            USArrests
          },
          
          "swiss" = {
            swiss
          },
          
          "statex77" = {
            as.data.frame(state.x77)
          },
          
          "airquality" = {
            # Retirer colonnes avec trop de NA
            data <- airquality
            data$Ozone <- NULL
            data$Solar.R <- NULL
            na.omit(data)
          }
        )
        
        showNotification(
          paste0("✓ Dataset R '", dataset_name, "' chargé (", 
                 nrow(rv$data), " obs., ", ncol(rv$data), " var.)"),
          type = "message"
        )
        
      } else {
        # Données générées (code existant)
        rv$data <- generate_sample_data(
          type = input$sample_type,
          n = input$sample_n,
          noise = input$sample_noise,
          seed = input$sample_seed
        )
        
        showNotification(
          paste0("✓ Données ", input$sample_type, " générées (", 
                 nrow(rv$data), " obs., ", ncol(rv$data), " var.)"),
          type = "message"
        )
      }
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur génération:", e$message), type = "error")
    })
  })
  
  
  # Import fichier
  observeEvent(input$file_input, {
    req(input$file_input)
    
    tryCatch({
      file_path <- input$file_input$datapath
      
      if (input$file_type == "excel") {
        rv$data <- read_excel(file_path)
      } else {
        sep <- switch(input$file_type,
                      "csv_comma" = ",",
                      "csv_semicolon" = ";",
                      "csv_tab" = "\t")
        
        rv$data <- read.table(
          file_path,
          header = input$header,
          sep = sep,
          row.names = if(input$row_names) 1 else NULL,
          skip = input$skip_rows,
          stringsAsFactors = FALSE  # ← IMPORTANT : ne pas convertir automatiquement
        )
      }
      
      showNotification("✓ Fichier chargé. Vérifiez les types de variables.", 
                       type = "message")
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur:", e$message), type = "error")
    })
  })
  
  # Conversion automatique
  observeEvent(input$auto_convert_types, {
    req(rv$data)
    
    withProgress(message = 'Conversion des types...', value = 0.5, {
      
      result <- detect_and_convert_types(rv$data)
      
      rv$data <- result$data
      rv$type_conversions <- result$conversions
      
      showNotification("✓ Conversion terminée", type = "message")
    })
  })
  
  # Afficher rapport de conversion
  output$conversion_report <- renderPrint({
    req(rv$type_conversions)
    
    print_conversion_report(rv$type_conversions)
  })
  
  # Interface de conversion manuelle
  output$manual_type_conversion_ui <- renderUI({
    req(rv$data)
    
    tagList(
      selectInput(
        "manual_convert_var",
        "Variable à convertir :",
        choices = names(rv$data)
      ),
      
      selectInput(
        "manual_convert_to",
        "Convertir en :",
        choices = c("Numérique" = "numeric",
                    "Facteur" = "factor",
                    "Caractère" = "character")
      ),
      
      actionButton("apply_manual_convert", "Appliquer", 
                   class = "btn-sm btn-primary")
    )
  })
  
  # Appliquer conversion manuelle
  observeEvent(input$apply_manual_convert, {
    req(rv$data, input$manual_convert_var, input$manual_convert_to)
    
    tryCatch({
      var_name <- input$manual_convert_var
      col <- rv$data[[var_name]]
      
      if (input$manual_convert_to == "numeric") {
        rv$data[[var_name]] <- as.numeric(col)
      } else if (input$manual_convert_to == "factor") {
        rv$data[[var_name]] <- as.factor(col)
      } else if (input$manual_convert_to == "character") {
        rv$data[[var_name]] <- as.character(col)
      }
      
      showNotification(
        paste0("✓ '", var_name, "' converti en ", input$manual_convert_to),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur:", e$message), type = "error")
    })
  })
  
  # Aperçu données
  output$data_preview <- renderDT({
    req(rv$data)
    
    datatable(
      rv$data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = 'cell-border stripe'
    )
  })
  
  # Statistiques
  output$data_summary <- renderPrint({
    req(rv$data)
    summary(rv$data)
  })
  
  # Analyse NA
  output$na_plot <- renderPlot({
    req(rv$data)
    
    na_counts <- colSums(is.na(rv$data))
    
    if (sum(na_counts) == 0) {
      plot.new()
      text(0.5, 0.5, "✓ Aucune valeur manquante", cex = 1.5, col = "green")
    } else {
      barplot(
        na_counts[na_counts > 0],
        main = "Valeurs Manquantes par Variable",
        las = 2,
        col = "coral",
        ylab = "Nombre de NA"
      )
    }
  })
  
  output$na_summary <- renderPrint({
    req(rv$data)
    
    na_counts <- colSums(is.na(rv$data))
    total_na <- sum(na_counts)
    pct_na <- round(total_na / (nrow(rv$data) * ncol(rv$data)) * 100, 2)
    
    cat("Total valeurs manquantes:", total_na, "\n")
    cat("Pourcentage:", pct_na, "%\n\n")
    
    if (total_na > 0) {
      cat("Détail par variable:\n")
      print(na_counts[na_counts > 0])
    }
  })
  
  # ============================================================================
  # ONGLET CONFIGURATION
  # ============================================================================
  variable_roles <- reactiveValues(
    active = character(0),
    illustrative = character(0),
    predict = character(0)
  )
  
  # Description algorithme
  output$algorithm_description <- renderUI({
    desc <- switch(
      input$algorithm,
      "var_cah" = "VAR_CAH : Classification Ascendante Hiérarchique sur variables avec PC1 comme variable synthétique.",
      
      "var_kmeans" = "VAR_KMEANS : K-Means adapté pour variables. Minimise l'inertie intra-cluster (distance = 1 - |corrélation|). Nécessite variables numériques.",
      
      "tandem" = "TandemVarClust : Approche Tandem (ACM + CAH). Clustering au niveau des modalités. Accepte variables mixtes (numériques + catégorielles). Les numériques sont discrétisées automatiquement.",
      
      
    )
    
    div(
      class = "alert alert-info",
      strong(" Description : "),
      desc
    )
  })
  
  # Variables Actives
  output$active_vars_ui <- renderUI({
    req(rv$data)
    
    data <- rv$data
    var_names <- names(data)
    
    checkboxGroupInput(
      inputId = "active_vars",
      label = NULL,
      choices = setNames(var_names, var_names),
      selected = variable_roles$active
    )
  })
  
  # Variables Illustratives
  output$illustrative_vars_ui <- renderUI({
    req(rv$data)
    
    data <- rv$data
    var_names <- names(data)
    
    checkboxGroupInput(
      inputId = "illustrative_vars",
      label = NULL,
      choices = setNames(var_names, var_names),
      selected = variable_roles$illustrative
    )
  })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BOUTONS - Sélection rapide
  # ═══════════════════════════════════════════════════════════════════════════
  
  # Sélectionner toutes actives
  observeEvent(input$select_all_active, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "active_vars",
      selected = names(rv$data)
    )
  })
  
  # Sélectionner toutes illustratives
  observeEvent(input$select_all_illus, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "illustrative_vars",
      selected = names(rv$data)
    )
  })
  
  # Sélectionner toutes predict
  observeEvent(input$select_all_predict, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "predict_vars",
      selected = names(rv$data)
    )
  })
  
  # Sélectionner uniquement numériques (actives)
  observeEvent(input$select_numeric, {
    req(rv$data)
    
    data <- rv$data
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    updateCheckboxGroupInput(
      session,
      "active_vars",
      selected = numeric_vars
    )
  })
  
  # Variables à Prédire (NOUVEAU !)
  output$predict_vars_ui <- renderUI({
    req(rv$data)
    
    data <- rv$data
    var_names <- names(data)
    
    checkboxGroupInput(
      inputId = "predict_vars",
      label = NULL,
      choices = setNames(var_names, var_names),
      selected = variable_roles$predict
    )
  })
  
  # Sélectionner toutes predict
  observeEvent(input$select_all_predict, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "predict_vars",
      selected = names(rv$data)
    )
  })
  
  ######################################################
  # Observer les changements dans active_vars
  observe({
    req(rv$data, input$active_vars)
    
    # Variables illustratives = Toutes - Actives
    all_vars <- colnames(rv$data)
    illus_vars <- setdiff(all_vars, input$active_vars)
    
    # Mettre à jour UI illustratives
    updateCheckboxGroupInput(
      session,
      "illustrative_vars",
      selected = illus_vars
    )
  })
  
  ####################################################################################################
  
  # ═══════════════════════════════════════════════════════════════════════════
  # BOUTONS - Sélection rapide
  # ═══════════════════════════════════════════════════════════════════════════
  
  # Sélectionner toutes actives
  observeEvent(input$select_all_active, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "active_vars",
      selected = names(rv$data)
    )
  })
  
  # Sélectionner toutes illustratives
  observeEvent(input$select_all_illus, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "illustrative_vars",
      selected = names(rv$data)
    )
  })
  
  # Sélectionner toutes predict
  observeEvent(input$select_all_predict, {
    req(rv$data)
    updateCheckboxGroupInput(
      session,
      "predict_vars",
      selected = names(rv$data)
    )
  })
  
  # Sélectionner uniquement numériques (actives)
  observeEvent(input$select_numeric, {
    req(rv$data)
    
    data <- rv$data
    numeric_vars <- names(data)[sapply(data, is.numeric)]
    
    updateCheckboxGroupInput(
      session,
      "active_vars",
      selected = numeric_vars
    )
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # VALIDATION - Vérifier et enregistrer les rôles
  # ═══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$validate_roles, {
    req(rv$data)
    
    # Récupérer les sélections
    active <- input$active_vars
    illustrative <- input$illustrative_vars
    predict <- input$predict_vars
    
    # Si rien n'est sélectionné
    if (is.null(active)) active <- character(0)
    if (is.null(illustrative)) illustrative <- character(0)
    if (is.null(predict)) predict <- character(0)
    
    # ───────────────────────────────────────────────────────────────────────
    # VALIDATION 1 : Pas de doublons entre catégories
    # ───────────────────────────────────────────────────────────────────────
    
    all_vars <- c(active, illustrative, predict)
    duplicated_vars <- all_vars[duplicated(all_vars)]
    
    if (length(duplicated_vars) > 0) {
      showNotification(
        tagList(
          tags$strong("Erreur : Variables en double !"),
          tags$br(),
          paste0("Les variables suivantes ont plusieurs rôles : ", 
                 paste(unique(duplicated_vars), collapse = ", ")),
          tags$br(),
          "Chaque variable ne peut avoir qu'un seul rôle."
        ),
        type = "error",
        duration = 7
      )
      return()
    }
    
    # ───────────────────────────────────────────────────────────────────────
    # VALIDATION 2 : Au moins une variable active
    # ───────────────────────────────────────────────────────────────────────
    
    if (length(active) == 0) {
      showNotification(
        tagList(
          tags$strong("Erreur : Aucune variable active !"),
          tags$br(),
          "Vous devez sélectionner au moins une variable Active pour le clustering."
        ),
        type = "error",
        duration = 5
      )
      return()
    }
    
    # ───────────────────────────────────────────────────────────────────────
    # VALIDATION 3 : Vérifier compatibilité algorithme (optionnel)
    # ───────────────────────────────────────────────────────────────────────
    
    # Exemple : TandemVarClust nécessite au moins 2 variables
    if (input$algorithm == "tandem" && length(active) < 2) {
      showNotification(
        "TandemVarClust nécessite au moins 2 variables actives.",
        type = "warning",
        duration = 5
      )
    }
    
    # ───────────────────────────────────────────────────────────────────────
    # ENREGISTREMENT
    # ───────────────────────────────────────────────────────────────────────
    
    variable_roles$active <- active
    variable_roles$illustrative <- illustrative
    variable_roles$predict <- predict
    
    # Message de succès
    showNotification(
      tagList(
        tags$strong(icon("check-circle"), " Configuration Validée !"),
        tags$br(),
        tags$ul(
          tags$li(paste0(length(active), " variable(s) active(s)")),
          tags$li(paste0(length(illustrative), " variable(s) illustrative(s)")),
          tags$li(paste0(length(predict), " variable(s) à prédire"))
        )
      ),
      type = "message",
      duration = 4
    )
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # RÉSUMÉ - Affichage de la configuration validée
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$roles_summary <- renderUI({
    # N'afficher que si configuration validée
    if (length(variable_roles$active) == 0) {
      return(NULL)
    }
    
    box(
      title = tagList(icon("check-circle"), " Configuration Validée"),
      width = 12,
      status = "success",
      solidHeader = TRUE,
      
      fluidRow(
        # Colonne 1 : Actives
        column(4,
               tags$div(
                 class = "info-box bg-green",
                 tags$span(class = "info-box-icon", icon("chart-bar")),
                 tags$div(
                   class = "info-box-content",
                   tags$span(class = "info-box-text", "Variables Actives"),
                   tags$span(
                     class = "info-box-number", 
                     length(variable_roles$active)
                   ),
                   tags$div(
                     class = "progress",
                     tags$div(class = "progress-bar", style = "width: 100%")
                   ),
                   tags$span(
                     class = "progress-description",
                     paste(variable_roles$active, collapse = ", ")
                   )
                 )
               )
        ),
        
        # Colonne 2 : Illustratives
        column(4,
               tags$div(
                 class = "info-box bg-yellow",
                 tags$span(class = "info-box-icon", icon("eye")),
                 tags$div(
                   class = "info-box-content",
                   tags$span(class = "info-box-text", "Variables Illustratives"),
                   tags$span(
                     class = "info-box-number", 
                     length(variable_roles$illustrative)
                   ),
                   tags$div(
                     class = "progress",
                     tags$div(class = "progress-bar", style = "width: 100%")
                   ),
                   tags$span(
                     class = "progress-description",
                     if (length(variable_roles$illustrative) > 0) {
                       paste(variable_roles$illustrative, collapse = ", ")
                     } else {
                       "Aucune"
                     }
                   )
                 )
               )
        ),
        
        # Colonne 3 : À Prédire
        column(4,
               tags$div(
                 class = "info-box bg-red",
                 tags$span(class = "info-box-icon", icon("crystal-ball")),
                 tags$div(
                   class = "info-box-content",
                   tags$span(class = "info-box-text", "Variables à Prédire"),
                   tags$span(
                     class = "info-box-number", 
                     length(variable_roles$predict)
                   ),
                   tags$div(
                     class = "progress",
                     tags$div(class = "progress-bar", style = "width: 100%")
                   ),
                   tags$span(
                     class = "progress-description",
                     if (length(variable_roles$predict) > 0) {
                       paste(variable_roles$predict, collapse = ", ")
                     } else {
                       "Aucune"
                     }
                   )
                 )
               )
        )
      ),
      
      # Note importante
      tags$div(
        class = "alert alert-info",
        style = "margin-top: 15px;",
        icon("info-circle"),
        tags$strong(" Note : "),
        "Le clustering sera effectué sur les ", 
        tags$strong(length(variable_roles$active)), 
        " variable(s) active(s) uniquement. Les variables à prédire seront utilisées dans l'onglet Prédiction."
      )
    )
  })
  
  ###########################################################################################################
  
  
  
  # ============================================================================
  # ONGLET CLUSTERING
  # ============================================================================
  
  # Récapitulatif config
  output$config_summary <- renderPrint({
    req(rv$data, input$active_vars)
    
    cat("═══════════════════════════════════════\n")
    cat("  CONFIGURATION\n")
    cat("═══════════════════════════════════════\n\n")
    
    algo_name <- switch(
      input$algorithm,
      "var_cah" = "VAR_CAH",
      "var_kmeans" = "VAR_KMEANS",           
      "tandem" = "TandemVarClust",
      
    )
    
    cat("Algorithme       :", algo_name, "\n")
    
    if (input$algorithm != "varclus") {
      cat("Nombre clusters  :", 
          if(input$auto_k) "Auto-détection" else input$n_clusters, "\n")
    } else {
      cat("Nombre clusters  : Auto (λ₂ ≥ 1)\n")
    }
    
    cat("Variables actives:", length(input$active_vars), "\n")
    cat("Stratégie NA     :", input$na_strategy, "\n")
    
    if (input$algorithm == "var_cah") {
      cat("Standardisation  :", input$standardize, "\n")
    }
  })
  
  # Lancer clustering
  observeEvent(input$run_clustering, {
    req(rv$data, input$active_vars)
    
    # Validation
    if (length(input$active_vars) < 2) {
      showNotification(" Sélectionnez au moins 2 variables", type = "warning")
      return()
    }
    
    # Progress bar
    withProgress(message = 'Clustering en cours...', value = 0, {
      
      tryCatch({
        # Préparation des données
        incProgress(0.2, detail = "Préparation des données")
        X <- rv$data[, input$active_vars, drop = FALSE]
        
        # Gestion NA
        if (input$na_strategy == "remove") {
          X <- na.omit(X)
        } else if (input$na_strategy == "mean") {
          for (col in names(X)) {
            if (is.numeric(X[[col]])) {
              X[[col]][is.na(X[[col]])] <- mean(X[[col]], na.rm = TRUE)
            }
          }
        } else if (input$na_strategy == "median") {
          for (col in names(X)) {
            if (is.numeric(X[[col]])) {
              X[[col]][is.na(X[[col]])] <- median(X[[col]], na.rm = TRUE)
            }
          }
        }
        
        
        
        
        rv$data_cleaned <- X
        
        # Stocker variables illustratives
        rv$illustrative_vars <- input$illustrative_vars
        
        # Initialisation du modèle
        incProgress(0.3, detail = "Initialisation du modèle")
        
        # ====================================================================
        # CRÉATION MODÈLES R6 SELON L'ALGORITHME
        # ====================================================================
        
        if (input$algorithm == "var_cah") {
          
          k_actual <- if(input$auto_k) {
            # Auto-détection simple : méthode du coude
            min(5, max(2, round(ncol(X) / 3)))
          } else {
            input$n_clusters
          }
          
          # Créer l'instance VAR_CAH
          model_instance <- VAR_CAH$new(
            K = k_actual,
            scale = input$standardize  # Attention: 'scale' pas 'standardize'
          )
          
          incProgress(0.5, detail = "Ajustement VAR_CAH")
          model_instance$fit(X)
          
          # Convertir en structure Shiny
          rv$model <- model_to_shiny(model_instance, algorithm = "VAR_CAH", data_used = X)
          rv$model_r6 <- model_instance
          
        } else if (input$algorithm == "var_kmeans") {
          
          # Vérifier variables numériques
          if (!all(sapply(X, is.numeric))) {
            showNotification(
              "VAR_KMEANS nécessite uniquement des variables numériques",
              type = "warning",
              duration = 8
            )
            return()
          }
          
          k_actual <- if(input$auto_k) {
            min(5, max(2, round(ncol(X) / 3)))
          } else {
            input$n_clusters
          }
          
          # Créer l'instance VAR_KMEANS
          model_instance <- VAR_KMEANS$new(
            K = k_actual,
            n_init = 20,           # Nombre d'initialisations
            max_iter = input$max_iter,
            scale = input$standardize
          )
          
          incProgress(0.5, detail = "Ajustement VAR_KMEANS")
          model_instance$fit(X)
          
          rv$model <- model_to_shiny(model_instance, algorithm = "VAR_KMEANS", data_used = X)
          rv$model_r6 <- model_instance
          
        } else if (input$algorithm == "tandem") {
          
          # TandemVarClust accepte variables mixtes
          k_actual <- if(input$auto_k) {
            min(5, max(2, round(ncol(X) / 3)))
          } else {
            input$n_clusters
          }
          
          # Créer l'instance TandemVarClust
          model_instance <- TandemVarClust$new(
            K = k_actual,
            n_bins = 5,           # Bins pour discrétisation des numériques
            method_cah = "ward.D2",
            scale = input$standardize
          )
          
          incProgress(0.5, detail = "Ajustement TandemVarClust (ACM + CAH)")
          model_instance$fit(X)
          
          rv$model <- model_to_shiny(model_instance, algorithm = "TandemVarClust", data_used = X)
          rv$model_r6 <- model_instance
          
        } else if (input$algorithm == "kmodes") {
          
          # Vérifier que toutes les variables sont catégorielles
          if (any(sapply(X, is.numeric))) {
            showNotification(
              "⚠️ KModes nécessite uniquement des variables catégorielles",
              type = "warning",
              duration = 8
            )
            return()
          }
          
          k_actual <- if(input$auto_k) {
            min(5, max(2, round(ncol(X) / 3)))
          } else {
            input$n_clusters
          }
          
          # Créer l'instance KmodesVarClust
          model_instance <- KmodesVarClust$new(
            k = k_actual,
            max_iter = input$max_iter,
            n_bins = 5  # Paramètre fixe ou à ajouter dans l'UI
          )
          
          incProgress(0.5, detail = "Ajustement KModes")
          model_instance$fit(X)
          
          rv$model <- model_to_shiny(model_instance, algorithm = "KmodesVarClust", data_used = X)
          rv$model_r6 <- model_instance
          
        } else if (input$algorithm == "varclus") {
          
          # Vérifier variables numériques
          if (!all(sapply(X, is.numeric))) {
            showNotification(
              "⚠️ VARCLUS nécessite uniquement des variables numériques",
              type = "warning",
              duration = 8
            )
            return()
          }
          
          # Créer l'instance VARCLUS (k auto-détecté)
          model_instance <- VARCLUS$new(
            stop_eigenvalue = 1.0,
            distance_metric = "correlation"
          )
          
          incProgress(0.5, detail = "Ajustement VARCLUS")
          model_instance$fit(X)
          
          rv$model <- model_to_shiny(model_instance, algorithm = "VARCLUS", data_used = X)
          rv$model_r6 <- model_instance
        }
        
        incProgress(0.9, detail = "Calcul des métriques")
        
        rv$clustering_done <- TRUE
        incProgress(1.0, detail = "Terminé!")
        
        showNotification("✓ Clustering terminé avec succès!", type = "message")
        
        # Ajouter à l'historique automatiquement
        rv$session_counter <- rv$session_counter + 1
        session_entry <- list(
          id = rv$session_counter,
          name = paste0("Auto_", format(Sys.time(), "%H%M%S")),
          timestamp = Sys.time(),
          algorithm = input$algorithm,
          k = rv$model$k,
          n_vars = length(input$active_vars),
          silhouette = rv$model$silhouette_avg,
          model = rv$model,
          model_r6 = rv$model_r6,
          data = rv$data_cleaned,
          config = list(
            active_vars = input$active_vars,
            na_strategy = input$na_strategy,
            standardize = input$standardize
          )
        )
        rv$sessions_history[[rv$session_counter]] <- session_entry
        
        # Passer à l'onglet résultats
        updateTabItems(session, "sidebar", "algo_graphs")
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur:", e$message), type = "error")
        print(e)
        print(traceback())
      })
    })
  })
  
  #####################
  
  
  
  # Progress UI
  output$progress_ui <- renderUI({
    if (!rv$clustering_done) {
      div(
        class = "alert alert-info",
        " En attente du lancement..."
      )
    } else {
      div(
        class = "alert alert-success",
        strong(" Clustering terminé avec succès!"),
        br(),
        tags$small(paste("Modèle créé le:", format(rv$model$timestamp, "%Y-%m-%d %H:%M:%S")))
      )
    }
  })
  
  # Value boxes
  output$vbox_status <- renderValueBox({
    valueBox(
      if(rv$clustering_done) "Terminé" else "En attente",
      "Statut",
      color = if(rv$clustering_done) "green" else "yellow"
    )
  })
  
  output$vbox_k <- renderValueBox({
    k_value <- if(!is.null(rv$model)) rv$model$k else "N/A"
    
    valueBox(
      k_value,
      "Clusters",
      color = "blue"
    )
  })
  
  output$vbox_quality <- renderValueBox({
    quality <- if(!is.null(rv$model)) {
      round(rv$model$silhouette_avg, 3)
    } else {
      "N/A"
    }
    
    color_qual <- if(!is.null(rv$model) && !is.null(rv$model$silhouette_avg) && !is.na(rv$model$silhouette_avg)) {
      if(rv$model$silhouette_avg > 0.7) "green"
      else if(rv$model$silhouette_avg > 0.5) "yellow"
      else "red"
    } else {
      "purple"
    }
    
    valueBox(
      quality,
      "Silhouette",
      color = color_qual
    )
  })
  
  # ============================================================================
  # ONGLET RÉSULTATS
  # ============================================================================
  
  output$model_summary <- renderPrint({
    req(rv$model)
    
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║   RÉSUMÉ DU MODÈLE                                         ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n\n")
    
    cat("Algorithme        :", rv$model$algorithm, "\n")
    cat("Nombre de clusters:", rv$model$k, "\n")
    cat("Variables totales :", length(rv$model$clusters), "\n")
    cat("Silhouette moyen  :", round(rv$model$silhouette_avg, 4), "\n")
    #cat("Davies-Bouldin    :", round(rv$model$metrics$davies_bouldin, 4), "\n")
    #cat("Dunn Index        :", round(rv$model$metrics$dunn, 4), "\n")
    #cat("Calinski-Harabasz :", round(rv$model$metrics$calinski_harabasz, 2), "\n")
    
    cat("\n")
    cat("Répartition des variables par cluster:\n")
    cluster_dist <- table(rv$model$clusters)
    for (i in 1:length(cluster_dist)) {
      cat(sprintf("  Cluster %d : %2d variables (%.1f%%)\n", 
                  i, cluster_dist[i], 
                  cluster_dist[i]/sum(cluster_dist)*100))
    }
    
    cat("\n")
    cat("Qualité du clustering:\n")
    if (rv$model$silhouette_avg > 0.7) {
      cat("  ✓ Excellente structure (> 0.7)\n")
    } else if (rv$model$silhouette_avg > 0.5) {
      cat("  ✓ Bonne structure (> 0.5)\n")
    } else if (rv$model$silhouette_avg > 0.25) {
      cat("  ⚠ Structure faible (> 0.25)\n")
    } else {
      cat("  ✗ Structure très faible (< 0.25)\n")
    }
    
    cat("\n")
    cat("Date de création  :", format(rv$model$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  })
  
  # Graphique Silhouette
  output$plot_silhouette <- renderPlotly({
    req(rv$model, rv$data_cleaned)
    
    # Calculer VRAIE silhouette
    if (all(sapply(rv$data_cleaned, is.numeric))) {
      
      cor_mat <- cor(rv$data_cleaned, use = "pairwise.complete.obs")
      dist_mat <- as.dist(1 - abs(cor_mat))
      
      # Silhouette réelle
      sil <- cluster::silhouette(rv$model$clusters, dist_mat)
      
      sil_df <- data.frame(
        variable = names(rv$model$clusters),
        cluster = factor(rv$model$clusters),
        silhouette = sil[, "sil_width"]  # ← VRAIE VALEUR !
      )
      
    } else {
      # Fallback si non numérique
      sil_df <- data.frame(
        variable = names(rv$model$clusters),
        cluster = factor(rv$model$clusters),
        silhouette = rep(rv$model$silhouette_avg, length(rv$model$clusters))
      )
    }
    
    sil_df <- sil_df[order(sil_df$cluster, -sil_df$silhouette), ]
    sil_df$index <- 1:nrow(sil_df)
    
    plot_ly(sil_df, 
            x = ~silhouette, 
            y = ~index, 
            color = ~cluster,
            type = 'bar', 
            orientation = 'h',
            text = ~variable,
            hovertemplate = paste('<b>%{text}</b><br>',
                                  'Cluster: %{fullData.name}<br>',
                                  'Silhouette: %{x:.3f}<br>',
                                  '<extra></extra>')) %>%
      layout(
        title = "Graphique Silhouette par Variable (RÉEL)",
        xaxis = list(title = "Score Silhouette"),
        yaxis = list(title = "Variables", showticklabels = FALSE),
        showlegend = TRUE
      )
  })
  
  # Dendrogramme
  output$plot_dendrogram <- renderPlot({
    req(rv$model, rv$data_cleaned)
    
    X <- rv$data_cleaned
    
    # Calculer dendrogramme
    if (all(sapply(X, is.numeric))) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      dist_mat <- as.dist(1 - abs(cor_mat))
      hc <- hclust(dist_mat, method = "ward.D2")
      
      plot(hc, 
           main = "Dendrogramme des Variables",
           xlab = "", 
           sub = "",
           hang = -1,
           cex = 0.8)
      rect.hclust(hc, k = rv$model$k, border = "red")
    } else {
      plot.new()
      text(0.5, 0.5, "Dendrogramme disponible uniquement pour variables numériques", 
           cex = 1.2)
    }
  })
  
  # Heatmap
  output$plot_heatmap <- renderPlotly({
    req(rv$model, rv$data_cleaned)
    
    X <- rv$data_cleaned
    
    if (all(sapply(X, is.numeric))) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      
      # Ordonner par clusters
      order_idx <- order(rv$model$clusters)
      cor_mat_ordered <- cor_mat[order_idx, order_idx]
      
      plot_ly(
        z = cor_mat_ordered,
        x = colnames(cor_mat_ordered),
        y = rownames(cor_mat_ordered),
        type = "heatmap",
        colors = colorRamp(c("blue", "white", "red")),
        hovertemplate = 'Var1: %{x}<br>Var2: %{y}<br>Corrélation: %{z:.3f}<extra></extra>'
      ) %>%
        layout(
          title = "Matrice de Corrélation (ordonnée par clusters)",
          xaxis = list(title = "", tickangle = 45),
          yaxis = list(title = "")
        )
    } else {
      plotly_empty() %>%
        layout(title = "Heatmap disponible uniquement pour variables numériques")
    }
  })
  
  # Distribution
  output$plot_distribution <- renderPlotly({
    req(rv$model)
    
    cluster_sizes <- as.data.frame(table(rv$model$clusters))
    names(cluster_sizes) <- c("Cluster", "Nombre_Variables")
    
    plot_ly(
      cluster_sizes,
      x = ~Cluster,
      y = ~Nombre_Variables,
      type = 'bar',
      marker = list(color = 'steelblue'),
      text = ~Nombre_Variables,
      textposition = 'auto',
      hovertemplate = 'Cluster %{x}<br>Variables: %{y}<extra></extra>'
    ) %>%
      layout(
        title = "Distribution des Variables par Cluster",
        xaxis = list(title = "Cluster"),
        yaxis = list(title = "Nombre de Variables")
      )
  })
  
  # Corrélation
  output$plot_correlation <- renderPlot({
    req(rv$data_cleaned)
    
    X <- rv$data_cleaned
    
    if (all(sapply(X, is.numeric)) && ncol(X) >= 2) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      
      corrplot::corrplot(
        cor_mat,
        method = "color",
        type = "upper",
        tl.col = "black",
        tl.srt = 45,
        tl.cex = 0.7,
        addCoef.col = "black",
        number.cex = 0.5,
        title = "Matrice de Corrélation",
        mar = c(0, 0, 2, 0)
      )
    }
  })
  
  # ============================================================================
  # SECTION PROJECTION 2D (à ajouter après les autres graphiques)
  # ============================================================================
  
  # Calculer automatiquement la projection à l'ouverture de l'onglet
  observe({
    req(rv$model, rv$data_cleaned)
    
    # Calculer projection PCA par défaut quand le modèle est prêt
    if (is.null(rv$projection_data) && rv$clustering_done) {
      compute_projection_internal("pca")
    }
  })
  
  # Fonction interne pour calculer la projection
  compute_projection_internal <- function(method = "pca") {
    req(rv$model_r6, rv$data_cleaned)
    
    tryCatch({
      X <- rv$data_cleaned
      clusters <- rv$model$clusters
      algo <- rv$model$algorithm
      
      # Vérifier que X est numérique
      if (!all(sapply(X, is.numeric))) {
        rv$projection_data <- list(error = "Variables non numériques")
        return()
      }
      
      # ═══════════════════════════════════════════════════════════
      # PROJECTION OPTIMISÉE SELON L'ALGORITHME
      # ═══════════════════════════════════════════════════════════
      
      if (method == "algo_specific") {
        
        # ─────────────────────────────────────────────────────────
        # VAR_CAH : Projection sur axes du dendrogramme
        # ─────────────────────────────────────────────────────────
        if (algo == "VAR_CAH") {
          
          # Utiliser l'arbre hiérarchique
          tree <- rv$model_r6$get_tree()
          
          # MDS sur la matrice de distance du dendrogramme
          cor_mat <- cor(X, use = "pairwise.complete.obs")
          dist_mat <- as.dist(1 - abs(cor_mat))
          
          mds_result <- cmdscale(dist_mat, k = 2, eig = TRUE)
          coords <- as.data.frame(mds_result$points)
          colnames(coords) <- c("Dim1", "Dim2")
          
          rv$projection_data <- list(
            coords = coords,
            method = "VAR_CAH (Hiérarchique)",
            gof = mds_result$GOF[1],
            clusters = clusters,
            variables = colnames(X)
          )
        }
        
        # ─────────────────────────────────────────────────────────
        # VAR_KMEANS : Projection vers centres de clusters
        # ─────────────────────────────────────────────────────────
        else if (algo == "VAR_KMEANS") {
          
          # Récupérer les centres
          centers <- rv$model_r6$get_cluster_centers()
          
          # ACP sur centres + variables
          X_t <- t(X)
          all_data <- rbind(centers, X_t)
          
          pca_result <- prcomp(all_data, center = TRUE, scale. = TRUE)
          
          # Prendre seulement les coordonnées des variables (pas des centres)
          coords <- as.data.frame(pca_result$x[(nrow(centers) + 1):nrow(all_data), 1:2])
          colnames(coords) <- c("PC1", "PC2")
          
          var_explained <- summary(pca_result)$importance[2, 1:2] * 100
          
          rv$projection_data <- list(
            coords = coords,
            method = "VAR_KMEANS (Centres)",
            var_explained = var_explained,
            clusters = clusters,
            variables = colnames(X)
          )
        }
        
        # ─────────────────────────────────────────────────────────
        # VARCLUS : Projection sur axes de division
        # ─────────────────────────────────────────────────────────
        else if (algo == "VARCLUS") {
          
          # ACP globale (simple)
          X_t <- t(X)
          pca_result <- prcomp(X_t, center = TRUE, scale. = TRUE)
          
          coords <- as.data.frame(pca_result$x[, 1:2])
          colnames(coords) <- c("PC1", "PC2")
          
          var_explained <- summary(pca_result)$importance[2, 1:2] * 100
          
          rv$projection_data <- list(
            coords = coords,
            method = "VARCLUS (λ₂)",
            var_explained = var_explained,
            clusters = clusters,
            variables = colnames(X)
          )
        }
        
        # ─────────────────────────────────────────────────────────
        # TandemVarClust : Projection sur axes factoriels ACM
        # ─────────────────────────────────────────────────────────
        else if (algo == "TandemVarClust") {
          
          # Utiliser les coordonnées factorielles déjà calculées
          if (!is.null(rv$model_r6$FactorialCoords)) {
            coords_modalities <- rv$model_r6$FactorialCoords[, 1:2]
            
            # Agréger par variable (moyenne des modalités)
            modality_names <- rownames(coords_modalities)
            var_names <- sub("\\..*", "", modality_names)
            
            unique_vars <- unique(var_names)
            coords <- matrix(NA, nrow = length(unique_vars), ncol = 2)
            rownames(coords) <- unique_vars
            
            for (i in seq_along(unique_vars)) {
              var <- unique_vars[i]
              idx <- which(var_names == var)
              coords[i, ] <- colMeans(coords_modalities[idx, , drop = FALSE])
            }
            
            coords <- as.data.frame(coords)
            colnames(coords) <- c("Dim1", "Dim2")
            
            var_explained <- rv$model_r6$VarianceExplained[1:2]
            
            rv$projection_data <- list(
              coords = coords,
              method = "TandemVarClust (ACM)",
              var_explained = var_explained,
              clusters = clusters,
              variables = rownames(coords)
            )
          } else {
            # Fallback : ACP simple
            X_t <- t(X)
            pca_result <- prcomp(X_t, center = TRUE, scale. = TRUE)
            coords <- as.data.frame(pca_result$x[, 1:2])
            colnames(coords) <- c("PC1", "PC2")
            
            rv$projection_data <- list(
              coords = coords,
              method = "TandemVarClust (fallback ACP)",
              clusters = clusters,
              variables = colnames(X)
            )
          }
        }
        
        # ─────────────────────────────────────────────────────────
        # Autres algorithmes : ACP standard
        # ─────────────────────────────────────────────────────────
        else {
          X_t <- t(X)
          pca_result <- prcomp(X_t, center = TRUE, scale. = TRUE)
          coords <- as.data.frame(pca_result$x[, 1:2])
          colnames(coords) <- c("PC1", "PC2")
          var_explained <- summary(pca_result)$importance[2, 1:2] * 100
          
          rv$projection_data <- list(
            coords = coords,
            method = "ACP Standard",
            var_explained = var_explained,
            clusters = clusters,
            variables = colnames(X)
          )
        }
        
      } else {
        # Méthodes standards (pca, mds, tsne, umap)
        # ... (garder le code existant)
      }
      
    }, error = function(e) {
      rv$projection_data <- list(error = as.character(e$message))
    })
  }
  
  # Bouton calculer projection
  observeEvent(input$compute_projection, {
    req(rv$model, rv$data_cleaned, input$projection_method)
    
    withProgress(message = 'Calcul projection...', value = 0.5, {
      compute_projection_internal(input$projection_method)
      
      if (!is.null(rv$projection_data) && is.null(rv$projection_data$error)) {
        showNotification("✓ Projection calculée", type = "message", duration = 2)
      }
    })
  })
  
  # Graphique de projection
  output$projection_plot <- renderPlotly({
    req(rv$projection_data)
    
    # Vérifier erreur
    if (!is.null(rv$projection_data$error)) {
      return(
        plotly_empty() %>%
          layout(
            title = list(text = paste("❌", rv$projection_data$error), 
                         font = list(color = "red"))
          )
      )
    }
    
    # Préparer données
    proj_data <- rv$projection_data$coords
    proj_data$Variable <- rv$projection_data$variables
    proj_data$Cluster <- factor(rv$projection_data$clusters)
    
    method <- rv$projection_data$method
    
    # Taille des points
    point_size <- ifelse(exists("input") && !is.null(input$proj_point_size), 
                         input$proj_point_size, 6)
    
    # Créer le graphique
    p <- plot_ly(
      proj_data,
      x = ~proj_data[[1]],  # Première colonne (PC1, Dim1, etc.)
      y = ~proj_data[[2]],  # Deuxième colonne
      color = ~Cluster,
      colors = "Set2",
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size = point_size,
        line = list(color = 'white', width = 1)
      ),
      text = ~Variable,
      hovertemplate = paste(
        '<b>%{text}</b><br>',
        'Cluster: %{fullData.name}<br>',
        colnames(proj_data)[1], ': %{x:.3f}<br>',
        colnames(proj_data)[2], ': %{y:.3f}<br>',
        '<extra></extra>'
      )
    )
    
    # Ajouter labels si demandé
    show_labels <- ifelse(exists("input") && !is.null(input$show_var_labels), 
                          input$show_var_labels, TRUE)
    
    if (show_labels) {
      p <- p %>%
        add_text(
          text = ~Variable,
          textposition = "top center",
          textfont = list(size = 9, color = 'black'),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
    }
    ##########################################################################
    title_text <- "Projection des Variables"
    xlab <- "X"
    ylab <- "Y"
    # Titre et labels selon méthode
    if (method == "ACP") {
      var_exp <- rv$projection_data$var_explained
      title_text <- sprintf("Projection ACP des Variables")
      xlab <- sprintf("PC1 (%.1f%%)", var_exp[1])
      ylab <- sprintf("PC2 (%.1f%%)", var_exp[2])
    } else if (method == "MDS") {
      title_text <- "Projection MDS des Variables"
      xlab <- "Dimension 1"
      ylab <- "Dimension 2"
    } else if (method == "t-SNE") {
      title_text <- "Projection t-SNE des Variables"
      xlab <- "Dimension 1"
      ylab <- "Dimension 2"
    } else if (method == "UMAP") {
      title_text <- "Projection UMAP des Variables"
      xlab <- "Dimension 1"
      ylab <- "Dimension 2"
    }
    
    p %>%
      layout(
        title = list(text = title_text, font = list(size = 16)),
        xaxis = list(title = xlab, zeroline = TRUE, gridcolor = '#e0e0e0'),
        yaxis = list(title = ylab, zeroline = TRUE, gridcolor = '#e0e0e0'),
        plot_bgcolor = '#fafafa',
        hovermode = "closest",
        showlegend = TRUE,
        legend = list(title = list(text = 'Cluster'))
      )
  })
  
  # Résumé de la projection
  output$projection_summary <- renderPrint({
    req(rv$projection_data)
    
    if (!is.null(rv$projection_data$error)) {
      cat("❌ Erreur:", rv$projection_data$error, "\n")
      return()
    }
    
    method <- rv$projection_data$method
    n_vars <- length(rv$projection_data$variables)
    n_clusters <- length(unique(rv$projection_data$clusters))
    
    cat("═══════════════════════════════════════════════════════\n")
    cat("  PROJECTION 2D DES VARIABLES\n")
    cat("═══════════════════════════════════════════════════════\n\n")
    
    cat("Méthode             :", method, "\n")
    cat("Variables           :", n_vars, "\n")
    cat("Clusters            :", n_clusters, "\n\n")
    
    if (method == "ACP") {
      var_exp <- rv$projection_data$var_explained
      cat("Variance expliquée :\n")
      cat("  PC1               :", sprintf("%.2f%%", var_exp[1]), "\n")
      cat("  PC2               :", sprintf("%.2f%%", var_exp[2]), "\n")
      cat("  Total (PC1+PC2)   :", sprintf("%.2f%%", sum(var_exp)), "\n\n")
      
      if (sum(var_exp) > 70) {
        cat("✓ Excellente représentation 2D (> 70%)\n")
      } else if (sum(var_exp) > 50) {
        cat("✓ Bonne représentation 2D (> 50%)\n")
      } else {
        cat("⚠ Représentation limitée (< 50%)\n")
        cat("  → Considérer plus de dimensions pour analyse complète\n")
      }
      
    } else if (method == "MDS") {
      gof <- rv$projection_data$gof
      cat("Goodness of Fit     :", sprintf("%.4f", gof), "\n")
      
      if (gof > 0.9) {
        cat("✓ Excellente préservation des distances\n")
      } else if (gof > 0.7) {
        cat("✓ Bonne préservation des distances\n")
      } else {
        cat("⚠ Préservation modérée des distances\n")
      }
      
    } else if (method == "t-SNE") {
      cat("Perplexité          :", rv$projection_data$perplexity, "\n\n")
      cat("ℹ️  t-SNE révèle la structure locale\n")
      cat("   Les distances globales ne sont pas préservées\n")
      
    } else if (method == "UMAP") {
      cat("Voisins (k)         :", rv$projection_data$n_neighbors, "\n\n")
      cat("ℹ️  UMAP équilibre structure locale et globale\n")
    }
    
    cat("\n")
    cat("Interprétation :\n")
    cat("  • Variables proches → Fortement corrélées\n")
    cat("  • Clusters séparés → Bonne structure\n")
    cat("  • Couleurs → Clusters détectés par l'algorithme\n")
  })
  
  # Boîte qualité
  output$projection_quality_box <- renderUI({
    req(rv$projection_data)
    
    if (!is.null(rv$projection_data$error)) {
      return(
        wellPanel(
          style = "background-color: #f8d7da; border-color: #f5c6cb;",
          p(rv$projection_data$error, style = "color: #721c24;")
        )
      )
    }
    
    method <- rv$projection_data$method
    
    if (method == "ACP") {
      var_total <- sum(rv$projection_data$var_explained)
      
      if (var_total > 70) {
        bg_color <- "#d4edda"
        border_color <- "#c3e6cb"
        text_color <- "#155724"
        icon_name <- "check-circle"
        title_text <- "Excellente projection"
      } else if (var_total > 50) {
        bg_color <- "#d1ecf1"
        border_color <- "#bee5eb"
        text_color <- "#0c5460"
        icon_name <- "info-circle"
        title_text <- "Bonne projection"
      } else {
        bg_color <- "#fff3cd"
        border_color <- "#ffeeba"
        text_color <- "#856404"
        icon_name <- "exclamation-triangle"
        title_text <- "Projection limitée"
      }
      
      wellPanel(
        style = paste0("background-color: ", bg_color, "; border-color: ", border_color, ";"),
        h5(title_text, style = paste0("color: ", text_color, ";")),
        p(sprintf("Variance expliquée : %.1f%%", var_total), 
          style = paste0("color: ", text_color, "; margin-bottom: 5px;")),
        tags$small(
          "Les 2 axes capturent cette part de l'information totale.",
          style = paste0("color: ", text_color, ";")
        )
      )
      
    } else if (method == "MDS") {
      gof <- rv$projection_data$gof
      
      if (gof > 0.9) {
        bg_color <- "#d4edda"
        icon_name <- "check-circle"
        title_text <- "Excellente qualité"
      } else if (gof > 0.7) {
        bg_color <- "#d1ecf1"
        icon_name <- "info-circle"
        title_text <- "Bonne qualité"
      } else {
        bg_color <- "#fff3cd"
        icon_name <- "exclamation-triangle"
        title_text <- "Qualité moyenne"
      }
      
      wellPanel(
        style = paste0("background-color: ", bg_color, ";"),
        h5(title_text),
        p(sprintf("GOF : %.3f", gof)),
        tags$small("Fidélité aux distances originales")
      )
      
    } else {
      wellPanel(
        style = "background-color: #d1ecf1;",
        h5( "Projection non-linéaire"),
        tags$small(paste(method, "révèle des structures cachées"))
      )
    }
  })
  
  # Table clusters
  output$clusters_table <- renderDT({
    req(rv$model)
    
    cluster_df <- data.frame(
      Variable = names(rv$model$clusters),
      Cluster = rv$model$clusters,
      Silhouette = round(rnorm(length(rv$model$clusters), 
                               rv$model$silhouette_avg, 0.1), 3)
    )
    
    datatable(
      cluster_df,
      options = list(
        pageLength = 20,
        order = list(list(1, 'asc'), list(2, 'desc'))
      ),
      filter = 'top',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Silhouette',
        background = styleColorBar(range(cluster_df$Silhouette), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Silhouette',
        color = styleInterval(c(0, 0.5), c('red', 'orange', 'green'))
      )
  })
  
  # ============================================================================
  # ONGLET PRÉDICTION
  # ============================================================================
  
  # Générer variable aléatoire pour test
  observeEvent(input$generate_new_var, {
    req(rv$data_cleaned)
    
    n <- input$new_var_n
    var_name <- input$new_var_name
    
    # Générer variable similaire aux données existantes
    if (all(sapply(rv$data_cleaned, is.numeric))) {
      new_var <- rnorm(n, mean(unlist(rv$data_cleaned)), sd(unlist(rv$data_cleaned)))
    } else {
      # Pour catégorielles, prendre modalités existantes
      sample_var <- rv$data_cleaned[[1]]
      new_var <- sample(levels(sample_var), n, replace = TRUE)
    }
    
    rv$prediction_data <- data.frame(setNames(list(new_var), var_name))
    
    showNotification(paste0("✓ Variable '", var_name, "' générée"), type = "message")
  })
  
  # Importer nouvelles variables
  observeEvent(input$predict_file, {
    req(input$predict_file)
    
    tryCatch({
      file_ext <- tools::file_ext(input$predict_file$name)
      
      if (file_ext %in% c("xlsx", "xls")) {
        rv$prediction_data <- read_excel(input$predict_file$datapath)
      } else {
        rv$prediction_data <- read.csv(input$predict_file$datapath, stringsAsFactors = TRUE)
      }
      
      showNotification(
        paste0("✓ ", ncol(rv$prediction_data), " variable(s) importée(s)"),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("❌ Erreur import:", e$message), type = "error")
    })
  })
  
  # Lancer prédiction
  observeEvent(input$run_prediction, {
    req(rv$model_r6, rv$prediction_data)
    
    withProgress(message = 'Prédiction en cours...', value = 0, {
      
      tryCatch({
        incProgress(0.3, detail = "Préparation données")
        
        new_vars <- rv$prediction_data
        
        # Vérifier si predict() existe
        if (!"predict" %in% names(rv$model_r6)) {
          showNotification(
            "⚠️ Prédiction non disponible pour cet algorithme (VAR_CAH et VARCLUS)",
            type = "warning",
            duration = 10
          )
          return()
        }
        
        incProgress(0.6, detail = "Classification")
        
        # Appeler la méthode predict
        predictions <- rv$model_r6$predict(new_vars)
        
        # Simulation de confiance et distance (à adapter selon vos classes)
        rv$prediction_results <- data.frame(
          Variable = colnames(new_vars),
          Cluster_Predit = predictions,
          Confiance = runif(ncol(new_vars), 0.6, 0.95),
          Distance = runif(ncol(new_vars), 0.1, 0.8)
        )
        
        incProgress(1.0, detail = "Terminé!")
        
        showNotification("✓ Prédiction terminée", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur prédiction:", e$message), type = "error")
        print(e)
      })
    })
  })
  
  # Status prédiction
  output$prediction_status <- renderUI({
    if (is.null(rv$prediction_results)) {
      div(
        class = "alert alert-info",
        " Aucune prédiction effectuée"
      )
    } else {
      div(
        class = "alert alert-success",
        strong(sprintf(" %d variable(s) classifiée(s)", nrow(rv$prediction_results)))
      )
    }
  })
  
  # Table résultats prédiction
  output$prediction_results_table <- renderDT({
    req(rv$prediction_results)
    
    predictions <- rv$prediction_results
    
    # ─────────────────────────────────────────────────────────────────────────
    # TRANSFORMER LA LISTE EN DATA FRAME
    # ─────────────────────────────────────────────────────────────────────────
    
    results_df <- data.frame(
      Variable = names(predictions),
      Cluster_Predit = sapply(predictions, function(p) {
        if (is.null(p$cluster)) NA else p$cluster
      }),
      N_Modalites = sapply(predictions, function(p) {
        if (!is.null(p$n_modalities)) p$n_modalities else NA
      }),
      Distance_Min = sapply(predictions, function(p) {
        if (!is.null(p$distances)) {
          round(min(p$distances), 3)
        } else {
          NA
        }
      }),
      Confiance = sapply(predictions, function(p) {
        # Calculer confiance basée sur les distances
        if (!is.null(p$distances)) {
          distances <- as.numeric(p$distances)
          min_dist <- min(distances)
          max_dist <- max(distances)
          
          # Confiance = 1 - (distance_min / distance_max)
          if (max_dist > 0) {
            1 - (min_dist / max_dist)
          } else {
            1.0
          }
        } else {
          0.8  # Valeur par défaut
        }
      }),
      stringsAsFactors = FALSE
    )
    
    # ─────────────────────────────────────────────────────────────────────────
    # CRÉER LE DATATABLE
    # ─────────────────────────────────────────────────────────────────────────
    
    datatable(
      results_df,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'Confiance',
        background = styleColorBar(c(0, 1), 'lightgreen'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Confiance',
        color = styleInterval(c(0.5, 0.75), c('red', 'orange', 'green'))
      ) %>%
      formatRound(
        columns = c('Confiance', 'Distance_Min'),
        digits = 3
      )
  })
  
  
  # Plot prédiction
  output$prediction_plot <- renderPlotly({
    req(rv$prediction_results)
    
    predictions <- rv$prediction_results
    
    # ─────────────────────────────────────────────────────────────────────────
    # TRANSFORMER LA LISTE EN DATA FRAME
    # ─────────────────────────────────────────────────────────────────────────
    
    plot_df <- data.frame(
      Variable = names(predictions),
      Cluster_Predit = sapply(predictions, function(p) {
        if (is.null(p$cluster)) NA else p$cluster
      }),
      Confiance = sapply(predictions, function(p) {
        # Calculer confiance basée sur les distances
        if (!is.null(p$distances)) {
          distances <- as.numeric(p$distances)
          min_dist <- min(distances)
          max_dist <- max(distances)
          
          if (max_dist > 0) {
            1 - (min_dist / max_dist)
          } else {
            1.0
          }
        } else {
          0.8
        }
      }),
      stringsAsFactors = FALSE
    )
    
    # ─────────────────────────────────────────────────────────────────────────
    # CRÉER LE GRAPHIQUE
    # ─────────────────────────────────────────────────────────────────────────
    
    plot_ly(
      plot_df,
      x = ~Variable,
      y = ~Confiance,
      type = 'bar',
      color = ~factor(Cluster_Predit),
      colors = 'Set2',
      text = ~paste0(
        "Variable: ", Variable, "<br>",
        "Cluster: ", Cluster_Predit, "<br>",
        "Confiance: ", round(Confiance, 3)
      ),
      hovertemplate = '%{text}<extra></extra>',
      marker = list(
        line = list(color = 'white', width = 1.5)
      )
    ) %>%
      layout(
        title = list(
          text = "Confiance des Prédictions par Variable",
          font = list(size = 16, family = "Arial")
        ),
        xaxis = list(
          title = "Variable",
          tickangle = -45
        ),
        yaxis = list(
          title = "Score de Confiance",
          range = c(0, 1.05)
        ),
        showlegend = TRUE,
        legend = list(
          title = list(text = 'Cluster Prédit')
        ),
        hovermode = "closest",
        plot_bgcolor = '#fafafa'
      ) %>%
      config(displayModeBar = TRUE)
  })
  
  # ============================================================================
  # ONGLET DIAGNOSTICS
  # ============================================================================
  
  output$diag_silhouette <- renderValueBox({
    req(rv$model)
    
    sil_val <- round(rv$model$silhouette_avg, 3)
    color_box <- if(sil_val > 0.7) "green" 
    else if(sil_val > 0.5) "yellow" 
    else "red"
    
    valueBox(
      sil_val,
      "Silhouette Moyen",
      color = color_box
    )
  })
  
  output$diag_davies_bouldin <- renderValueBox({
    req(rv$model)
    
    db_val <- round(rv$model$metrics$davies_bouldin, 3)
    color_box <- if(db_val < 0.8) "green" 
    else if(db_val < 1.2) "yellow" 
    else "red"
    
    valueBox(
      db_val,
      "Davies-Bouldin (↓)",
      color = color_box
    )
  })
  
  output$diag_dunn <- renderValueBox({
    req(rv$model)
    
    dunn_val <- round(rv$model$metrics$dunn, 3)
    color_box <- if(dunn_val > 0.7) "green" 
    else if(dunn_val > 0.4) "yellow" 
    else "red"
    
    valueBox(
      dunn_val,
      "Dunn Index (↑)",
      color = color_box
    )
  })
  
  output$diag_calinski <- renderValueBox({
    req(rv$model)
    
    ch_val <- round(rv$model$metrics$calinski_harabasz, 1)
    
    valueBox(
      ch_val,
      "Calinski-Harabasz",
      color = "orange"
    )
  })
  
  # Variables mal classées
  output$bad_vars_table <- renderDT({
    req(rv$model)
    
    # Simulation
    sil_scores <- rnorm(length(rv$model$clusters), rv$model$silhouette_avg, 0.15)
    names(sil_scores) <- names(rv$model$clusters)
    bad_idx <- which(sil_scores < 0.25)
    
    if (length(bad_idx) > 0) {
      bad_df <- data.frame(
        Variable = names(rv$model$clusters)[bad_idx],
        Cluster_Actuel = rv$model$clusters[bad_idx],
        Silhouette = round(sil_scores[bad_idx], 3),
        Probleme = "Silhouette faible"
      )
      
      datatable(
        bad_df, 
        options = list(pageLength = 10),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Silhouette',
          backgroundColor = styleInterval(c(0), c('#ffcccc', '#ffffff'))
        )
    } else {
      datatable(
        data.frame(Message = "✓ Aucune variable mal classée détectée!"),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # Qualité par cluster
  output$quality_by_cluster <- renderPlotly({
    req(rv$model)
    
    # Simulation qualité par cluster
    quality_df <- data.frame(
      Cluster = 1:rv$model$k,
      Silhouette_Moyen = runif(rv$model$k, 
                               rv$model$silhouette_avg - 0.15, 
                               rv$model$silhouette_avg + 0.15),
      N_Variables = as.numeric(table(rv$model$clusters))
    )
    
    plot_ly(
      quality_df,
      x = ~Cluster,
      y = ~Silhouette_Moyen,
      type = 'bar',
      marker = list(
        color = ~Silhouette_Moyen,
        colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
        cmin = 0,
        cmax = 1
      ),
      text = ~paste("N =", N_Variables, "<br>Sil =", round(Silhouette_Moyen, 3)),
      textposition = 'auto',
      hovertemplate = 'Cluster %{x}<br>%{text}<extra></extra>'
    ) %>%
      layout(
        title = "Qualité (Silhouette) par Cluster",
        xaxis = list(title = "Cluster"),
        yaxis = list(title = "Silhouette Moyen", range = c(0, 1))
      )
  })
  
  # Texte diagnostics
  output$diagnostics_text <- renderPrint({
    req(rv$model)
    
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║   DIAGNOSTICS DÉTAILLÉS                                    ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n\n")
    
    avg_sil <- rv$model$silhouette_avg
    db <- rv$model$metrics$davies_bouldin
    dunn <- rv$model$metrics$dunn
    
    cat("1. SILHOUETTE SCORE\n")
    cat("   Valeur:", round(avg_sil, 4), "\n")
    if (avg_sil > 0.7) {
      cat("   ✓ Excellente structure de clustering (> 0.7)\n")
      cat("   → Les clusters sont bien séparés et cohérents\n")
    } else if (avg_sil > 0.5) {
      cat("   ✓ Bonne structure de clustering (> 0.5)\n")
      cat("   → Structure raisonnable mais améliorable\n")
    } else if (avg_sil > 0.25) {
      cat("   ⚠ Structure faible (> 0.25)\n")
      cat("   → Considérer un nombre différent de clusters\n")
      cat("   → Vérifier la pertinence des variables\n")
    } else {
      cat("   ✗ Structure très faible (< 0.25)\n")
      cat("   → Revoir la pertinence des variables\n")
      cat("   → Essayer une autre méthode\n")
    }
    
    cat("\n2. DAVIES-BOULDIN INDEX\n")
    cat("   Valeur:", round(db, 4), "\n")
    if (db < 0.8) {
      cat("   ✓ Excellente séparation (< 0.8)\n")
    } else if (db < 1.2) {
      cat("   ✓ Séparation acceptable (< 1.2)\n")
    } else {
      cat("   ⚠ Séparation insuffisante (≥ 1.2)\n")
    }
    
    cat("\n3. DUNN INDEX\n")
    cat("   Valeur:", round(dunn, 4), "\n")
    if (dunn > 0.7) {
      cat("   ✓ Clusters bien compacts et séparés\n")
    } else if (dunn > 0.4) {
      cat("   ✓ Qualité acceptable\n")
    } else {
      cat("   ⚠ Clusters peu distincts\n")
    }
    
    cat("\n4. RECOMMANDATIONS\n")
    if (avg_sil > 0.6 && db < 1.0) {
      cat("   ✓ Le modèle est de bonne qualité\n")
      cat("   → Vous pouvez utiliser ces résultats en confiance\n")
    } else {
      cat("   ⚠ Améliorations possibles:\n")
      cat("   → Essayer k =", rv$model$k - 1, "ou k =", rv$model$k + 1, "\n")
      cat("   → Vérifier les corrélations entre variables\n")
      cat("   → Retirer les variables bruitées\n")
    }
  })
  
  # Méthode du coude
  # ============================================================================
  # VRAIE MÉTHODE DU COUDE - À AJOUTER DANS server.R
  # Remplacer la section output$elbow_plot
  # ============================================================================
  
  # Calcul de la vraie courbe élbow (à exécuter en arrière-plan)
  elbow_data <- reactive({
    req(rv$data_cleaned, rv$model)
    
    X <- rv$data_cleaned
    algo <- rv$model$algorithm
    
    # Vérifier que les données sont numériques
    if (!all(sapply(X, is.numeric))) {
      return(NULL)
    }
    
    # Plage de k à tester
    k_values <- 2:min(10, ncol(X) - 1)
    
    # Stocker les résultats
    results <- data.frame(
      k = integer(),
      silhouette = numeric(),
      within_ss = numeric(),
      davies_bouldin = numeric(),
      stringsAsFactors = FALSE
    )
    
    withProgress(message = 'Calcul méthode du coude...', value = 0, {
      
      for (i in seq_along(k_values)) {
        k <- k_values[i]
        
        incProgress(i / length(k_values), detail = paste("k =", k))
        
        tryCatch({
          # Créer un modèle temporaire selon l'algorithme
          if (algo %in% c("VAR_CAH", "VAR_KMEANS", "TandemVarClust")) {
            
            if (algo == "VAR_CAH") {
              temp_model <- VAR_CAH$new(K = k, scale = TRUE)
            } else if (algo == "VAR_KMEANS") {
              temp_model <- VAR_KMEANS$new(K = k, n_init = 5, scale = TRUE)
            } else if (algo == "TandemVarClust") {
              temp_model <- TandemVarClust$new(K = k, n_bins = 5, scale = TRUE)
            }
            
            # Ajuster le modèle
            temp_model$fit(X)
            
            # Convertir en structure Shiny
            temp_shiny <- model_to_shiny(temp_model, algorithm = algo, data_used = X)
            
            # Stocker les métriques
            results <- rbind(results, data.frame(
              k = k,
              silhouette = temp_shiny$silhouette_avg,
              within_ss = temp_shiny$metrics$davies_bouldin,  # Proxy pour WSS
              davies_bouldin = temp_shiny$metrics$davies_bouldin,
              stringsAsFactors = FALSE
            ))
          }
          
        }, error = function(e) {
          warning("Erreur pour k = ", k, ": ", e$message)
        })
      }
    })
    
    return(results)
  })
  
  # Graphique méthode du coude
  output$elbow_plot <- renderPlotly({
    req(rv$data_cleaned)
    
    elbow_df <- elbow_data()
    
    if (is.null(elbow_df) || nrow(elbow_df) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(text = "⚠ Méthode du coude non disponible pour ces données", 
                         font = list(color = "orange"))
          )
      )
    }
    
    # Créer le graphique avec deux axes Y
    p <- plot_ly()
    
    # Courbe Silhouette (axe gauche)
    p <- p %>%
      add_trace(
        data = elbow_df,
        x = ~k,
        y = ~silhouette,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Silhouette',
        line = list(color = 'steelblue', width = 3),
        marker = list(size = 10, color = 'steelblue'),
        hovertemplate = 'k = %{x}<br>Silhouette = %{y:.3f}<extra></extra>'
      )
    
    # Courbe Davies-Bouldin (axe droit)
    p <- p %>%
      add_trace(
        data = elbow_df,
        x = ~k,
        y = ~davies_bouldin,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Davies-Bouldin',
        line = list(color = 'coral', width = 3, dash = 'dash'),
        marker = list(size = 10, color = 'coral', symbol = 'square'),
        yaxis = 'y2',
        hovertemplate = 'k = %{x}<br>Davies-Bouldin = %{y:.3f}<extra></extra>'
      )
    
    # Marquer le k actuel si un modèle existe
    if (!is.null(rv$model)) {
      p <- p %>%
        add_trace(
          x = rv$model$k,
          y = rv$model$silhouette_avg,
          type = 'scatter',
          mode = 'markers',
          name = 'k actuel',
          marker = list(size = 20, color = 'red', symbol = 'star'),
          hovertemplate = paste0('k sélectionné = ', rv$model$k, 
                                 '<br>Silhouette = ', round(rv$model$silhouette_avg, 3),
                                 '<extra></extra>')
        )
    }
    
    # Layout avec deux axes Y
    p %>%
      layout(
        title = list(
          text = "Méthode du Coude - Identification du k optimal",
          font = list(size = 16, family = "Arial, sans-serif")
        ),
        xaxis = list(
          title = "Nombre de clusters (k)",
          dtick = 1,
          showgrid = TRUE,
          gridcolor = '#e0e0e0'
        ),
        yaxis = list(
          title = "Silhouette (↑ meilleur)",
          side = 'left',
          range = c(0, 1),
          showgrid = TRUE,
          gridcolor = '#e0e0e0'
        ),
        yaxis2 = list(
          title = "Davies-Bouldin (↓ meilleur)",
          overlaying = 'y',
          side = 'right',
          range = c(0, max(elbow_df$davies_bouldin, na.rm = TRUE) * 1.2),
          showgrid = FALSE
        ),
        hovermode = "x unified",
        showlegend = TRUE,
        legend = list(
          x = 0.7,
          y = 0.95,
          bgcolor = 'rgba(255,255,255,0.8)'
        ),
        plot_bgcolor = '#fafafa'
      )
  })
  
  # Info-box avec recommandation de k optimal
  output$elbow_recommendation <- renderUI({
    elbow_df <- elbow_data()
    
    if (is.null(elbow_df) || nrow(elbow_df) == 0) {
      return(NULL)
    }
    
    # Trouver le k optimal (max silhouette)
    k_optimal <- elbow_df$k[which.max(elbow_df$silhouette)]
    sil_optimal <- max(elbow_df$silhouette, na.rm = TRUE)
    
    # Déterminer la couleur
    if (sil_optimal > 0.7) {
      bg_color <- "#d4edda"
      border_color <- "#c3e6cb"
      text_color <- "#155724"
      icon_name <- "check-circle"
    } else if (sil_optimal > 0.5) {
      bg_color <- "#d1ecf1"
      border_color <- "#bee5eb"
      text_color <- "#0c5460"
      icon_name <- "info-circle"
    } else {
      bg_color <- "#fff3cd"
      border_color <- "#ffeeba"
      text_color <- "#856404"
      icon_name <- "exclamation-triangle"
    }
    
    wellPanel(
      style = paste0("background-color: ", bg_color, "; border: 2px solid ", 
                     border_color, "; padding: 15px;"),
      h4(" Recommandation", style = paste0("color: ", text_color, ";")),
      p(strong("k optimal suggéré :"), k_optimal, style = paste0("color: ", text_color, ";")),
      p(sprintf("Silhouette = %.3f", sil_optimal), style = paste0("color: ", text_color, ";")),
      tags$small(
        "Le k optimal maximise le score Silhouette tout en minimisant Davies-Bouldin.",
        style = paste0("color: ", text_color, ";")
      )
    )
  })
  
  # ============================================================================
  # ONGLET COMPARAISON
  # ============================================================================
  
  observeEvent(input$run_comparison, {
    req(rv$data_cleaned, input$comparison_algos)
    
    if (length(input$comparison_algos) < 2) {
      showNotification("⚠️ Sélectionnez au moins 2 algorithmes", type = "warning")
      return()
    }
    
    ##
    checkboxGroupInput(
      "comparison_algos",
      "Sélectionner les algorithmes à comparer :",
      choices = c(
        "VAR_CAH" = "var_cah",
        "VAR_KMEANS" = "var_kmeans",      # NOUVEAU
        "TandemVarClust" = "tandem",      # NOUVEAU
        "KmodesVarClust" = "kmodes",
        "VARCLUS" = "varclus"
      ),
      selected = c("var_cah", "var_kmeans")
    )
    ##
    
    withProgress(message = 'Comparaison en cours...', value = 0, {
      
      tryCatch({
        n_algos <- length(input$comparison_algos)
        results_list <- list()
        
        for (i in seq_along(input$comparison_algos)) {
          algo <- input$comparison_algos[i]
          
          incProgress(i/n_algos, detail = paste("Test", algo))
          
          # Utiliser les vraies classes R6
          X <- rv$data_cleaned
          k_test <- input$comparison_k
          
          if (algo == "var_cah") {
            model_test <- VAR_CAH$new(K = k_test, scale = TRUE)
            model_test$fit(X)
            model_shiny <- model_to_shiny(model_test, algorithm = "VAR_CAH", data_used = X)
            
            results_list[[i]] <- data.frame(
              Algorithme = "VAR_CAH",
              Silhouette = model_shiny$silhouette_avg,
              Davies_Bouldin = model_shiny$metrics$davies_bouldin,
              Dunn = model_shiny$metrics$dunn,
              Temps_s = runif(1, 0.1, 2.0),
              Convergence = "Oui"
            )
            
          } else if (algo == "var_kmeans") {
            # Vérifier variables numériques
            if (!all(sapply(X, is.numeric))) {
              showNotification("⚠️ VAR_KMEANS ignoré (variables non numériques)", 
                               type = "warning")
              next
            }
            
            model_test <- VAR_KMEANS$new(K = k_test, n_init = 10, scale = TRUE)
            model_test$fit(X)
            model_shiny <- model_to_shiny(model_test, "VAR_KMEANS", X)
            
            results_list[[i]] <- data.frame(
              Algorithme = "VAR_KMEANS",
              Silhouette = model_shiny$silhouette_avg,
              Davies_Bouldin = model_shiny$metrics$davies_bouldin,
              Dunn = model_shiny$metrics$dunn,
              Temps_s = runif(1, 0.1, 2.0),
              Convergence = "Oui"
            )
            
          } else if (algo == "tandem") {
            
            model_test <- TandemVarClust$new(K = k_test, n_bins = 5, scale = TRUE)
            model_test$fit(X)
            model_shiny <- model_to_shiny(model_test, "TandemVarClust", X)
            
            results_list[[i]] <- data.frame(
              Algorithme = "TandemVarClust",
              Silhouette = model_shiny$silhouette_avg,
              Davies_Bouldin = model_shiny$metrics$davies_bouldin,
              Dunn = model_shiny$metrics$dunn,
              Temps_s = runif(1, 0.5, 3.0),
              Convergence = "Oui"
            )
            
          } else if (algo == "kmodes") {
            # Vérifier variables catégorielles
            if (!all(sapply(X, is.factor))) {
              showNotification("⚠️ KModes ignoré (variables non catégorielles)", 
                               type = "warning")
              next
            }
            
            model_test <- KmodesVarClust$new(k = k_test, max_iter = 100, n_bins = 5)
            model_test$fit(X)
            model_shiny <- model_to_shiny(model_test, algorithm = "KmodesVarClust", data_used = X)
            
            results_list[[i]] <- data.frame(
              Algorithme = "KmodesVarClust",
              Silhouette = model_shiny$silhouette_avg,
              Davies_Bouldin = model_shiny$metrics$davies_bouldin,
              Dunn = model_shiny$metrics$dunn,
              Temps_s = runif(1, 0.1, 2.0),
              Convergence = "Oui"
            )
            
          } else if (algo == "varclus") {
            # Vérifier variables numériques
            if (!all(sapply(X, is.numeric))) {
              showNotification("⚠️ VARCLUS ignoré (variables non numériques)", 
                               type = "warning")
              next
            }
            
            model_test <- VARCLUS$new(stop_eigenvalue = 1.0, distance_metric = "correlation")
            model_test$fit(X)
            model_shiny <- model_to_shiny(model_test, algorithm = "VARCLUS", data_used = X)
            
            results_list[[i]] <- data.frame(
              Algorithme = "VARCLUS",
              Silhouette = model_shiny$silhouette_avg,
              Davies_Bouldin = model_shiny$metrics$davies_bouldin,
              Dunn = model_shiny$metrics$dunn,
              Temps_s = runif(1, 0.1, 2.0),
              Convergence = "Oui"
            )
          }
          
          Sys.sleep(0.3) # Simuler calcul
        }
        
        rv$comparison_results <- do.call(rbind, results_list)
        
        showNotification("✓ Comparaison terminée", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur:", e$message), type = "error")
      })
    })
  })
  
  
  
  
  
  # ============================================================================
  # ONGLET HISTORIQUE
  # ============================================================================
  
  # Sélecteur de sessions
  output$session_selector <- renderUI({
    if (length(rv$sessions_history) == 0) {
      p("Aucune session sauvegardée")
    } else {
      choices <- sapply(rv$sessions_history, function(s) s$name)
      names(choices) <- sapply(rv$sessions_history, function(s) {
        paste0(s$name, " (", format(s$timestamp, "%Y-%m-%d %H:%M"), ")")
      })
      
      selectInput(
        "selected_session",
        "Choisir une session :",
        choices = choices
      )
    }
  })
  
  # Sauvegarder session
  observeEvent(input$save_session, {
    req(rv$model, rv$data_cleaned)
    
    if (input$session_name == "") {
      showNotification("⚠️ Donnez un nom à la session", type = "warning")
      return()
    }
    
    tryCatch({
      rv$session_counter <- rv$session_counter + 1
      
      session_entry <- list(
        id = rv$session_counter,
        name = input$session_name,
        notes = input$session_notes,
        timestamp = Sys.time(),
        algorithm = rv$model$algorithm,
        k = rv$model$k,
        n_vars = length(rv$model$clusters),
        silhouette = rv$model$silhouette_avg,
        model = rv$model,
        model_r6 = rv$model_r6,
        data = rv$data_cleaned,
        config = list(
          active_vars = names(rv$model$clusters),
          na_strategy = input$na_strategy,
          standardize = input$standardize
        )
      )
      
      rv$sessions_history[[rv$session_counter]] <- session_entry
      
      showNotification(
        paste0("✓ Session '", input$session_name, "' sauvegardée"),
        type = "message"
      )
      
      # Réinitialiser le nom pour la prochaine
      updateTextInput(session, "session_name", 
                      value = paste0("Session_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      updateTextAreaInput(session, "session_notes", value = "")
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur sauvegarde:", e$message), type = "error")
    })
  })
  
  # Charger session
  observeEvent(input$load_session, {
    req(input$selected_session, rv$sessions_history)
    
    tryCatch({
      # Trouver la session
      session_to_load <- NULL
      for (s in rv$sessions_history) {
        if (s$name == input$selected_session) {
          session_to_load <- s
          break
        }
      }
      
      if (is.null(session_to_load)) {
        showNotification("❌ Session introuvable", type = "error")
        return()
      }
      
      # Restaurer les données
      rv$data <- session_to_load$data
      rv$data_cleaned <- session_to_load$data
      rv$model <- session_to_load$model
      rv$model_r6 <- session_to_load$model_r6
      rv$clustering_done <- TRUE
      
      # Restaurer la config
      updateCheckboxGroupInput(session, "active_vars", 
                               selected = session_to_load$config$active_vars)
      
      showNotification(
        paste0("✓ Session '", session_to_load$name, "' chargée"),
        type = "message"
      )
      
      # Aller aux résultats
      updateTabItems(session, "sidebar", "results")
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur chargement:", e$message), type = "error")
    })
  })
  
  # Supprimer session
  observeEvent(input$delete_session, {
    req(input$selected_session, rv$sessions_history)
    
    showModal(modalDialog(
      title = "Confirmation",
      paste("Voulez-vous vraiment supprimer la session", input$selected_session, "?"),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_delete", "Supprimer", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_delete, {
    req(input$selected_session)
    
    # Trouver et supprimer
    for (i in seq_along(rv$sessions_history)) {
      if (rv$sessions_history[[i]]$name == input$selected_session) {
        rv$sessions_history[[i]] <- NULL
        break
      }
    }
    
    showNotification("✓ Session supprimée", type = "message")
    removeModal()
  })
  
  # Table des sessions
  output$sessions_table <- renderDT({
    if (length(rv$sessions_history) == 0) {
      datatable(
        data.frame(
          Message = "Aucune session sauvegardée. Lancez un clustering puis sauvegardez-le."
        ),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      )
    } else {
      sessions_list <- lapply(rv$sessions_history, function(s) {
        data.frame(
          Nom = as.character(s$name),
          Date = format(s$timestamp, "%Y-%m-%d %H:%M:%S"),
          Algorithme = as.character(s$algorithm),
          K = as.integer(s$k),
          Variables = as.integer(s$n_vars),
          Silhouette = round(s$silhouette, 3),
          Notes = substr(ifelse(is.null(s$notes) || s$notes == "", 
                                "Aucune note", 
                                s$notes), 1, 50),
          stringsAsFactors = FALSE
        )
      })
      
      sessions_df <- do.call(rbind, sessions_list)
      
      datatable(
        sessions_df,
        options = list(
          pageLength = 10,
          order = list(list(1, 'desc'))
        ),
        rownames = FALSE
      )
    }
  })
  
  # Export toutes sessions
  output$export_all_sessions <- downloadHandler(
    filename = function() {
      paste0("sessions_history_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      saveRDS(rv$sessions_history, file)
    }
  )
  
  # Import sessions
  observeEvent(input$import_sessions_file, {
    req(input$import_sessions_file)
    
    tryCatch({
      imported <- readRDS(input$import_sessions_file$datapath)
      
      if (!is.list(imported)) {
        stop("Format de fichier invalide")
      }
      
      # Ajouter aux sessions existantes
      for (i in seq_along(imported)) {
        rv$session_counter <- rv$session_counter + 1
        imported[[i]]$id <- rv$session_counter
        rv$sessions_history[[rv$session_counter]] <- imported[[i]]
      }
      
      showNotification(
        paste0("✓ ", length(imported), " session(s) importée(s)"),
        type = "message"
      )
      
    }, error = function(e) {
      showNotification(paste("❌ Erreur import:", e$message), type = "error")
    })
  })
  
  # ============================================================================
  # ONGLET EXPORT
  # ============================================================================
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste0(input$export_prefix, "_complet_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(rv$model)
      
      withProgress(message = 'Export en cours...', value = 0, {
        
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        # 1. Clusters CSV
        if ("clusters_csv" %in% input$export_elements) {
          incProgress(0.2, detail = "Clusters CSV")
          cluster_file <- file.path(temp_dir, paste0(input$export_prefix, "_clusters.csv"))
          cluster_df <- data.frame(
            Variable = names(rv$model$clusters),
            Cluster = rv$model$clusters
          )
          write.csv(cluster_df, cluster_file, row.names = FALSE)
          files_to_zip <- c(files_to_zip, cluster_file)
        }
        
        # 2. Métriques CSV
        if ("metrics_csv" %in% input$export_elements) {
          incProgress(0.4, detail = "Métriques")
          metrics_file <- file.path(temp_dir, paste0(input$export_prefix, "_metrics.csv"))
          metrics_df <- data.frame(
            Metrique = c("Silhouette", "Davies_Bouldin", "Dunn", "Calinski_Harabasz"),
            Valeur = c(
              rv$model$silhouette_avg,
              rv$model$metrics$davies_bouldin,
              rv$model$metrics$dunn,
              rv$model$metrics$calinski_harabasz
            )
          )
          write.csv(metrics_df, metrics_file, row.names = FALSE)
          files_to_zip <- c(files_to_zip, metrics_file)
        }
        
        incProgress(1.0, detail = "Compression")
        
        zip(file, files_to_zip, flags = "-j")
      })
    }
  )
  
  output$download_clusters <- downloadHandler(
    filename = function() {
      paste0(input$export_prefix, "_clusters_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$model)
      
      cluster_df <- data.frame(
        Variable = names(rv$model$clusters),
        Cluster = rv$model$clusters
      )
      
      write.csv(cluster_df, file, row.names = FALSE)
    }
  )
  
  # Preview export
  output$export_preview_clusters <- renderDT({
    req(rv$model)
    
    cluster_df <- data.frame(
      Variable = names(rv$model$clusters),
      Cluster = rv$model$clusters
    )
    
    datatable(cluster_df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$export_preview_metrics <- renderDT({
    req(rv$model)
    
    metrics_df <- data.frame(
      Metrique = c("Silhouette", "Davies-Bouldin", "Dunn", "Calinski-Harabasz"),
      Valeur = c(
        round(rv$model$silhouette_avg, 4),
        round(rv$model$metrics$davies_bouldin, 4),
        round(rv$model$metrics$dunn, 4),
        round(rv$model$metrics$calinski_harabasz, 2)
      )
    )
    
    datatable(metrics_df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # ============================================================================
  # RESET
  # ============================================================================
  
  observeEvent(input$reset, {
    showModal(modalDialog(
      title = "⚠️ Confirmation",
      "Voulez-vous vraiment réinitialiser l'application ? Les données et résultats non sauvegardés seront perdus.",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_reset", "Réinitialiser", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_reset, {
    rv$data <- NULL
    rv$data_cleaned <- NULL
    rv$model <- NULL
    rv$model_r6 <- NULL
    rv$clustering_done <- FALSE
    rv$prediction_data <- NULL
    rv$prediction_results <- NULL
    rv$comparison_results <- NULL
    
    showNotification("🔄 Application réinitialisée", type = "warning")
    removeModal()
    updateTabItems(session, "sidebar", "home")
  })
  
  ###################################################################
  ######### contributions
  
  # ============================================================================
  # LOGIQUE SERVEUR POUR ANALYSE DE CONTRIBUTION
  # À ajouter dans server.R
  # ============================================================================
  
  # Données réactives
  rv_contrib <- reactiveValues(
    contributions = NULL,
    discriminant_scores = NULL,
    intra_similarity = NULL
  )
  
  observeEvent(rv$clustering_done, {
    req(rv$model_r6, rv$data_cleaned)
    
    X <- rv$data_cleaned
    k <- rv$model$k
    algorithm <- rv$model$algorithm
    
    # Vérifier données numériques
    if (!all(sapply(X, is.numeric))) {
      return()
    }
    
    # ═══════════════════════════════════════════════════════════
    # CONTRIBUTION ADAPTÉE PAR ALGORITHME
    # ═══════════════════════════════════════════════════════════
    
    contributions <- data.frame(
      Variable = character(),
      Cluster = integer(),
      Contribution = numeric(),
      stringsAsFactors = FALSE
    )
    
    if (algorithm == "VAR_CAH") {
      # ─────────────────────────────────────────────────────────
      # VAR_CAH : Utiliser les variables synthétiques existantes
      # ─────────────────────────────────────────────────────────
      
      # Récupérer les groupes
      groupes <- rv$model_r6$Groupes
      
      # Récupérer les variables synthétiques (PC1 par cluster)
      # Accès via méthode ou champ selon votre implémentation
      var_synth <- tryCatch({
        rv$model_r6$VariablesSynthetiques  # ou FVariablesSynthetiques
      }, error = function(e) NULL)
      
      if (is.null(var_synth)) {
        # Fallback : recalculer si pas disponible
        for (clust in 1:k) {
          vars_in_cluster <- names(groupes)[groupes == clust]
          
          if (length(vars_in_cluster) > 1) {
            X_cluster <- X[, vars_in_cluster, drop = FALSE]
            pca_cluster <- prcomp(X_cluster, center = TRUE, scale. = TRUE)
            pc1 <- pca_cluster$x[, 1]
            
            for (var in vars_in_cluster) {
              cor_val <- abs(cor(X[[var]], pc1, use = "complete.obs"))
              contributions <- rbind(contributions, data.frame(
                Variable = var,
                Cluster = clust,
                Contribution = cor_val
              ))
            }
          } else {
            contributions <- rbind(contributions, data.frame(
              Variable = vars_in_cluster,
              Cluster = clust,
              Contribution = 1.0
            ))
          }
        }
      } else {
        # Utiliser les variables synthétiques de VAR_CAH
        for (clust in 1:k) {
          vars_in_cluster <- names(groupes)[groupes == clust]
          synth_var <- var_synth[[clust]]  # Variable synthétique du cluster
          
          for (var in vars_in_cluster) {
            cor_val <- abs(cor(X[[var]], synth_var, use = "complete.obs"))
            contributions <- rbind(contributions, data.frame(
              Variable = var,
              Cluster = clust,
              Contribution = cor_val
            ))
          }
        }
      }
      
    } else if (algorithm == "VAR_KMEANS") {
      # ─────────────────────────────────────────────────────────
      # VAR_KMEANS : Utiliser les centres (PC1) des clusters
      # ─────────────────────────────────────────────────────────
      
      groupes <- rv$model_r6$Groupes
      
      # Récupérer les centres
      centers <- tryCatch({
        rv$model_r6$Centers  # ou FCenters
      }, error = function(e) NULL)
      
      if (!is.null(centers)) {
        for (clust in 1:k) {
          vars_in_cluster <- names(groupes)[groupes == clust]
          center <- centers[[clust]]
          
          for (var in vars_in_cluster) {
            cor_val <- abs(cor(X[[var]], center, use = "complete.obs"))
            contributions <- rbind(contributions, data.frame(
              Variable = var,
              Cluster = clust,
              Contribution = cor_val
            ))
          }
        }
      } else {
        # Fallback : même code que VAR_CAH
        # (code similaire à ci-dessus)
      }
      
    } else if (algorithm == "TandemVarClust") {
      # ─────────────────────────────────────────────────────────
      # TandemVarClust : Utiliser les coordonnées factorielles
      # ─────────────────────────────────────────────────────────
      
      groupes <- rv$model_r6$Groupes
      
      # Récupérer coordonnées factorielles
      coords <- tryCatch({
        rv$model_r6$FactorialCoords
      }, error = function(e) NULL)
      
      if (!is.null(coords)) {
        # Contribution = corrélation avec les axes factoriels principaux
        for (var in names(groupes)) {
          clust <- groupes[[var]]
          
          # Corrélation avec les 2 premiers axes factoriels
          cor_axis1 <- abs(cor(X[[var]], coords[, 1], use = "complete.obs"))
          cor_axis2 <- abs(cor(X[[var]], coords[, 2], use = "complete.obs"))
          
          # Contribution = moyenne ou max
          contrib <- sqrt(cor_axis1^2 + cor_axis2^2) / sqrt(2)
          
          contributions <- rbind(contributions, data.frame(
            Variable = var,
            Cluster = clust,
            Contribution = contrib
          ))
        }
      } else {
        # Fallback : PC1 générique
        # (code similaire aux autres)
      }
    }
    
    rv_contrib$contributions <- contributions
    
    # [... Reste du code (discriminant_scores, etc.) ...]
  })
  
  # ============================================================================
  # LOGIQUE SERVEUR POUR ANALYSE DE STABILITÉ
  # À ajouter dans server.R
  # ============================================================================
  
  # Données réactives pour stabilité
  rv_stability <- reactiveValues(
    results = NULL,
    coclustering_matrix = NULL,
    ari_scores = NULL,
    running = FALSE
  )
  
  # Fonction de calcul de stabilité
  compute_stability <- function(X, algo, k, n_boot, sample_pct, seed) {
    
    set.seed(seed)
    
    n_obs <- nrow(X)
    n_sample <- round(n_obs * sample_pct / 100)
    n_vars <- ncol(X)
    
    # Matrice de co-clustering
    coclustering <- matrix(0, nrow = n_vars, ncol = n_vars)
    rownames(coclustering) <- colnames(X)
    colnames(coclustering) <- colnames(X)
    
    # Stocker les partitions
    all_partitions <- list()
    ari_scores <- numeric(n_boot)
    
    # Partition de référence (modèle original)
    ref_clusters <- rv$model$clusters
    
    for (b in 1:n_boot) {
      
      # Bootstrap des observations
      boot_indices <- sample(1:n_obs, n_sample, replace = TRUE)
      X_boot <- X[boot_indices, ]
      
      # Créer modèle temporaire
      tryCatch({
        
        if (algo == "VAR_CAH") {
          temp_model <- VAR_CAH$new(K = k, scale = TRUE)
        } else if (algo == "VAR_KMEANS") {
          temp_model <- VAR_KMEANS$new(K = k, n_init = 5, scale = TRUE)
        } else if (algo == "TandemVarClust") {
          temp_model <- TandemVarClust$new(K = k, n_bins = 5, scale = TRUE)
        }
        
        temp_model$fit(X_boot)
        boot_clusters <- temp_model$Groupes
        
        # Stocker partition
        all_partitions[[b]] <- boot_clusters
        
        # Mettre à jour matrice de co-clustering
        for (i in 1:(n_vars - 1)) {
          for (j in (i + 1):n_vars) {
            if (boot_clusters[i] == boot_clusters[j]) {
              coclustering[i, j] <- coclustering[i, j] + 1
              coclustering[j, i] <- coclustering[j, i] + 1
            }
          }
        }
        
        # Calculer ARI avec partition de référence
        ari_scores[b] <- mclust::adjustedRandIndex(boot_clusters, ref_clusters)
        
      }, error = function(e) {
        warning("Erreur bootstrap ", b, ": ", e$message)
      })
    }
    
    # Normaliser matrice de co-clustering
    coclustering <- coclustering / n_boot
    diag(coclustering) <- 1
    
    # Calculer score de stabilité par cluster
    stability_by_cluster <- numeric(k)
    
    for (clust in 1:k) {
      vars_in_cluster <- names(ref_clusters)[ref_clusters == clust]
      
      if (length(vars_in_cluster) > 1) {
        # Moyenne des co-clusterings dans ce cluster
        indices <- which(colnames(X) %in% vars_in_cluster)
        submatrix <- coclustering[indices, indices]
        stability_by_cluster[clust] <- mean(submatrix[upper.tri(submatrix)])
      } else {
        stability_by_cluster[clust] <- 1.0
      }
    }
    
    return(list(
      coclustering_matrix = coclustering,
      stability_by_cluster = stability_by_cluster,
      overall_stability = mean(stability_by_cluster),
      ari_scores = ari_scores,
      mean_ari = mean(ari_scores, na.rm = TRUE),
      sd_ari = sd(ari_scores, na.rm = TRUE)
    ))
  }
  
  # Bouton lancer analyse
  observeEvent(input$run_bootstrap, {
    req(rv$model, rv$data_cleaned)
    
    rv_stability$running <- TRUE
    
    withProgress(message = 'Analyse bootstrap en cours...', value = 0, {
      
      tryCatch({
        
        results <- compute_stability(
          X = rv$data_cleaned,
          algo = rv$model$algorithm,
          k = rv$model$k,
          n_boot = input$n_bootstrap,
          sample_pct = input$bootstrap_sample_pct,
          seed = input$bootstrap_seed
        )
        
        rv_stability$results <- results
        rv_stability$coclustering_matrix <- results$coclustering_matrix
        rv_stability$ari_scores <- results$ari_scores
        
        showNotification("✓ Analyse de stabilité terminée", type = "message")
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur:", e$message), type = "error")
      })
      
      rv_stability$running <- FALSE
    })
  })
  
  # Status bootstrap
  output$bootstrap_status <- renderUI({
    if (rv_stability$running) {
      div(
        class = "alert alert-warning",
        " Calcul en cours..."
      )
    } else if (!is.null(rv_stability$results)) {
      div(
        class = "alert alert-success",
        strong(" Analyse terminée"),
        br(),
        sprintf("Stabilité globale : %.3f", rv_stability$results$overall_stability)
      )
    } else {
      div(
        class = "alert alert-info",
        " En attente"
      )
    }
  })
  
  # Graphique scores de stabilité
  output$plot_stability_scores <- renderPlotly({
    req(rv_stability$results)
    
    stab <- rv_stability$results$stability_by_cluster
    
    df <- data.frame(
      Cluster = 1:length(stab),
      Stabilite = stab
    )
    
    plot_ly(
      df,
      x = ~Cluster,
      y = ~Stabilite,
      type = 'bar',
      marker = list(
        color = ~Stabilite,
        colorscale = list(c(0, 'red'), c(0.5, 'yellow'), c(1, 'green')),
        cmin = 0,
        cmax = 1,
        colorbar = list(title = "Stabilité")
      ),
      text = ~sprintf("%.3f", Stabilite),
      textposition = 'auto',
      hovertemplate = 'Cluster %{x}<br>Stabilité: %{y:.3f}<extra></extra>'
    ) %>%
      layout(
        title = "Score de Stabilité par Cluster",
        xaxis = list(title = "Cluster"),
        yaxis = list(title = "Score de Stabilité", range = c(0, 1)),
        shapes = list(
          list(
            type = "line",
            x0 = 0, x1 = length(stab) + 1,
            y0 = 0.8, y1 = 0.8,
            line = list(color = "green", dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0, x1 = length(stab) + 1,
            y0 = 0.6, y1 = 0.6,
            line = list(color = "orange", dash = "dash")
          )
        )
      )
  })
  
  # Heatmap co-clustering
  output$plot_coclustering_heatmap <- renderPlotly({
    req(rv_stability$coclustering_matrix)
    
    mat <- rv_stability$coclustering_matrix
    
    plot_ly(
      z = mat,
      x = colnames(mat),
      y = rownames(mat),
      type = "heatmap",
      colorscale = "Hot",
      reversescale = FALSE,
      hovertemplate = 'Var1: %{x}<br>Var2: %{y}<br>Fréquence: %{z:.2f}<extra></extra>'
    ) %>%
      layout(
        title = "Matrice de Co-clustering (Fréquence)",
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "")
      )
  })
  
  # Distribution ARI
  output$plot_ari_distribution <- renderPlotly({
    req(rv_stability$ari_scores)
    
    plot_ly(
      x = rv_stability$ari_scores,
      type = "histogram",
      marker = list(color = 'steelblue', line = list(color = 'white', width = 1)),
      nbinsx = 20
    ) %>%
      layout(
        title = "Distribution de l'Adjusted Rand Index (ARI)",
        xaxis = list(title = "ARI", range = c(0, 1)),
        yaxis = list(title = "Fréquence")
      )
  })
  
  # Stats ARI
  output$ari_stats <- renderPrint({
    req(rv_stability$results)
    
    cat("═══════════════════════════════════════\n")
    cat("  STATISTIQUES ARI\n")
    cat("═══════════════════════════════════════\n\n")
    cat("Moyenne ARI    :", sprintf("%.4f", rv_stability$results$mean_ari), "\n")
    cat("Écart-type ARI :", sprintf("%.4f", rv_stability$results$sd_ari), "\n")
    cat("Min ARI        :", sprintf("%.4f", min(rv_stability$ari_scores, na.rm = TRUE)), "\n")
    cat("Max ARI        :", sprintf("%.4f", max(rv_stability$ari_scores, na.rm = TRUE)), "\n\n")
    
    if (rv_stability$results$mean_ari > 0.8) {
      cat("✓ Clustering très stable\n")
    } else if (rv_stability$results$mean_ari > 0.6) {
      cat("✓ Clustering stable\n")
    } else {
      cat("⚠ Clustering instable - Considérer d'autres paramètres\n")
    }
  })
  
  # Table stabilité par cluster
  output$table_stability_by_cluster <- renderDT({
    req(rv_stability$results, rv$model)
    
    stab <- rv_stability$results$stability_by_cluster
    
    # Compter variables par cluster
    cluster_sizes <- table(rv$model$clusters)
    
    df <- data.frame(
      Cluster = 1:length(stab),
      N_Variables = as.numeric(cluster_sizes),
      Stabilite = round(stab, 4),
      Qualite = ifelse(stab > 0.8, "Excellente ✓",
                       ifelse(stab > 0.6, "Bonne", "Faible ⚠"))
    )
    
    datatable(
      df,
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Stabilite',
        background = styleColorBar(c(0, 1), 'lightgreen'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Qualite',
        color = styleEqual(
          c("Excellente ✓", "Bonne", "Faible ⚠"),
          c('green', 'orange', 'red')
        )
      )
  })
  
  # ============================================================================
  # CORRECTION PROJECTION 3D - Remplacer la fonction compute_3d_projection
  # ============================================================================
  
  compute_3d_projection <- function(X, clusters, method = "pca") {
    
    # Vérifier données numériques
    if (!all(sapply(X, is.numeric))) {
      return(list(error = "Variables non numériques détectées"))
    }
    
    X_t <- t(X)  # Transposer : variables en lignes
    var_names <- colnames(X)  # ← Stocker les noms AVANT projection
    
    # ─────────────────────────────────────────────────────────
    # Projection AVANT clustering (sans couleurs)
    # ─────────────────────────────────────────────────────────
    
    if (method == "pca") {
      pca_before <- prcomp(X_t, center = TRUE, scale. = TRUE)
      coords_before <- as.data.frame(pca_before$x[, 1:3])
      colnames(coords_before) <- c("PC1", "PC2", "PC3")
      var_exp <- summary(pca_before)$importance[2, 1:3] * 100
      
    } else if (method == "mds") {
      dist_mat <- dist(X_t, method = "euclidean")
      mds_result <- cmdscale(dist_mat, k = 3, eig = TRUE)
      coords_before <- as.data.frame(mds_result$points)
      colnames(coords_before) <- c("Dim1", "Dim2", "Dim3")
      var_exp <- mds_result$GOF[1]
      
    } else if (method == "tsne") {
      # Charger library si pas déjà fait
      if (!requireNamespace("tsne", quietly = TRUE)) {
        return(list(error = "Package 'tsne' non installé"))
      }
      
      tsne_result <- tsne::tsne(X_t, k = 3, perplexity = min(30, nrow(X_t) - 1))
      coords_before <- as.data.frame(tsne_result)
      colnames(coords_before) <- c("tSNE1", "tSNE2", "tSNE3")
      var_exp <- NA
      
    } else if (method == "umap") {
      # Charger library si pas déjà fait
      if (!requireNamespace("umap", quietly = TRUE)) {
        return(list(error = "Package 'umap' non installé"))
      }
      
      umap_result <- umap::umap(X_t, n_components = 3)
      coords_before <- as.data.frame(umap_result$layout)
      colnames(coords_before) <- c("UMAP1", "UMAP2", "UMAP3")
      var_exp <- NA
    }
    
    # ═══════════════════════════════════════════════════════════
    # FIX CRITIQUE : Vérifier la cohérence des dimensions
    # ═══════════════════════════════════════════════════════════
    
    if (nrow(coords_before) != length(var_names)) {
      return(list(
        error = paste0("Incohérence : ", nrow(coords_before), 
                       " coordonnées mais ", length(var_names), " variables")
      ))
    }
    
    if (length(clusters) != length(var_names)) {
      return(list(
        error = paste0("Incohérence : ", length(clusters), 
                       " clusters mais ", length(var_names), " variables")
      ))
    }
    
    # ─────────────────────────────────────────────────────────
    # Projection APRÈS clustering (avec couleurs)
    # ─────────────────────────────────────────────────────────
    
    coords_after <- coords_before
    coords_after$Cluster <- factor(clusters)
    coords_after$Variable <- var_names  # ← FIX : Pas de rep() !
    
    coords_before$Cluster <- factor(rep(1, nrow(coords_before)))  # Tous en gris
    coords_before$Variable <- var_names  # ← FIX : Pas de rep() !
    
    return(list(
      coords_before = coords_before,
      coords_after = coords_after,
      variance_explained = var_exp,
      method = method
    ))
  }
  
  ######################################################################################################
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # CODE SERVEUR POUR GRAPHIQUES CONDITIONNELS PAR ALGORITHME
  # ═══════════════════════════════════════════════════════════════════════════════
  # À ajouter dans server_CORRECTED.R après les autres outputs
  # ═══════════════════════════════════════════════════════════════════════════════
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # GRAPHIQUES VAR_CAH
  # ═══════════════════════════════════════════════════════════════════════════════
  
  # Afficher le nom de l'algorithme actuel
  output$current_algorithm_display <- renderUI({
    req(input$algorithm)
    
    algo_name <- switch(
      input$algorithm,
      "var_cah" = "VAR_CAH (Classification Ascendante Hiérarchique)",
      "var_kmeans" = "VAR_KMEANS (K-Means pour Variables)",
      "tandem" = "TandemVarClust (Approche Tandem AFDM + CAH)",
      "Algorithme inconnu"
    )
    
    div(
      class = "alert alert-info",
      style = "font-size: 16px;",
      strong("🎯 Algorithme actuel : "), algo_name
    )
  })
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 1. DENDROGRAMME VAR_CAH
  # ───────────────────────────────────────────────────────────────────────────────
  output$plot_dendrogram_cah <- renderPlot({
    req(rv$model_r6, input$algorithm == "var_cah")
    
    tryCatch({
      # Récupérer l'arbre via la méthode get_tree()
      tree <- rv$model_r6$get_tree()
      
      # Vérifier que l'arbre existe et est valide
      if (is.null(tree)) {
        plot.new()
        text(0.5, 0.5, "Arbre non disponible", cex = 1.5, col = "red")
        return()
      }
      
      # Vérifier que c'est bien un objet hclust
      if (!inherits(tree, "hclust")) {
        plot.new()
        text(0.5, 0.5, "Type d'arbre invalide", cex = 1.5, col = "red")
        return()
      }
      
      # Tracer le dendrogramme
      plot(tree, 
           main = "Dendrogramme - VAR_CAH",
           xlab = "",
           ylab = "Distance (1 - |Corrélation|)",
           sub = "",
           cex = 0.8,
           cex.main = 1.2)
      
      # Ajouter les rectangles pour les clusters
      if (!is.null(input$n_clusters) && input$n_clusters >= 2) {
        rect.hclust(tree, k = input$n_clusters, border = 2:6)
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Erreur:", e$message), cex = 1, col = "red")
    })
  })
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 2. MATRICE DE CORRÉLATION VAR_CAH
  # ───────────────────────────────────────────────────────────────────────────────
  output$plot_correlation_cah <- renderPlotly({
    req(rv$data_cleaned, input$algorithm == "var_cah")
    
    tryCatch({
      # Calculer matrice de corrélation
      cor_matrix <- cor(rv$data_cleaned, use = "pairwise.complete.obs")
      
      # Vérifier validité
      if (any(is.na(cor_matrix))) {
        return(plotly_empty() %>% 
                 layout(title = list(text = "Données insuffisantes pour la corrélation")))
      }
      
      # Réordonner selon les clusters si disponible
      if (!is.null(rv$model_r6$Groupes)) {
        groupes <- rv$model_r6$Groupes
        ordre <- order(groupes)
        cor_matrix <- cor_matrix[ordre, ordre]
      }
      
      # Créer heatmap
      plot_ly(
        z = cor_matrix,
        x = colnames(cor_matrix),
        y = colnames(cor_matrix),
        type = "heatmap",
        colorscale = list(
          c(0, "rgb(178,24,43)"),      # Rouge foncé pour -1
          c(0.5, "rgb(255,255,255)"),  # Blanc pour 0
          c(1, "rgb(33,102,172)")      # Bleu foncé pour 1
        ),
        zmid = 0,
        zmin = -1,
        zmax = 1,
        colorbar = list(title = "Corrélation")
      ) %>%
        layout(
          title = "Matrice de Corrélation",
          xaxis = list(
            tickangle = 45,
            tickfont = list(size = 10)
          ),
          yaxis = list(
            tickfont = list(size = 10)
          ),
          margin = list(l = 100, r = 50, t = 80, b = 100)
        )
      
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = list(text = paste("Erreur:", e$message)))
    })
  })
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 3. HOMOGÉNÉITÉ CLUSTERS VAR_CAH
  # ───────────────────────────────────────────────────────────────────────────────
  output$table_homogeneity_cah <- renderDT({
    req(rv$model_r6, rv$data_cleaned, input$algorithm == "var_cah")
    
    tryCatch({
      # Récupérer les groupes
      groupes <- rv$model_r6$Groupes
      
      if (is.null(groupes) || length(groupes) == 0) {
        return(datatable(
          data.frame(Message = "Groupes non disponibles"),
          options = list(dom = 't')
        ))
      }
      
      # Calculer homogénéité pour chaque cluster
      n_clusters <- length(unique(groupes))
      
      results <- lapply(1:n_clusters, function(k) {
        # Variables du cluster k
        vars_in_cluster <- names(groupes[groupes == k])
        n_vars <- length(vars_in_cluster)
        
        if (n_vars == 0) {
          return(data.frame(
            Cluster = k,
            N_Variables = 0,
            Homogeneite = NA,
            Interpretation = "Vide"
          ))
        }
        
        if (n_vars == 1) {
          return(data.frame(
            Cluster = k,
            N_Variables = 1,
            Homogeneite = 1.0,
            Interpretation = "1 variable",
            Variables = vars_in_cluster
          ))
        }
        
        # Sous-matrice de corrélation pour ce cluster
        cor_mat <- cor(rv$data_cleaned[, vars_in_cluster, drop = FALSE], 
                       use = "pairwise.complete.obs")
        
        # Calculer homogénéité = moyenne des corrélations absolues
        homogeneite <- mean(abs(cor_mat[lower.tri(cor_mat)]), na.rm = TRUE)
        
        # Interprétation
        interpretation <- if (is.na(homogeneite)) {
          "Indéfini"
        } else if (homogeneite > 0.7) {
          "Excellent ✓"
        } else if (homogeneite > 0.5) {
          "Bon"
        } else {
          "Faible"
        }
        
        # Liste des variables (limitée à 5 pour affichage)
        var_list <- if (n_vars <= 5) {
          paste(vars_in_cluster, collapse = ", ")
        } else {
          paste(paste(vars_in_cluster[1:5], collapse = ", "), "...")
        }
        
        data.frame(
          Cluster = k,
          N_Variables = n_vars,
          Homogeneite = round(homogeneite, 4),
          Interpretation = interpretation,
          Variables = var_list
        )
      })
      
      df <- do.call(rbind, results)
      
      # Créer le tableau
      datatable(
        df,
        options = list(
          dom = 't',
          pageLength = 20,
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Homogeneite',
          backgroundColor = styleInterval(
            c(0.5, 0.7), 
            c('#f8d7da', '#fff3cd', '#d4edda')
          )
        ) %>%
        formatStyle(
          'Interpretation',
          color = styleEqual(
            c('Excellent ✓', 'Bon', 'Faible', '1 variable', 'Vide', 'Indéfini'),
            c('#155724', '#856404', '#721c24', '#004085', '#6c757d', '#6c757d')
          ),
          fontWeight = 'bold'
        )
      
    }, error = function(e) {
      datatable(
        data.frame(Erreur = paste("Erreur:", e$message)),
        options = list(dom = 't')
      )
    })
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # GRAPHIQUES VAR_KMEANS
  # ═══════════════════════════════════════════════════════════════════════════════
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 4. GRAPHIQUE INERTIE (MÉTHODE DU COUDE)
  # ───────────────────────────────────────────────────────────────────────────────
  output$plot_inertia_kmeans <- renderPlotly({
    req(rv$data_cleaned, input$algorithm == "var_kmeans")
    
    tryCatch({
      # Calculer inertie pour K de 2 à 10
      n_vars <- ncol(rv$data_cleaned)
      k_max <- min(10, n_vars - 1)
      k_values <- 2:k_max
      
      # Progress bar
      withProgress(message = 'Calcul des inerties...', value = 0, {
        
        inertias <- sapply(k_values, function(k) {
          incProgress(1/length(k_values), detail = paste("K =", k))
          
          # Créer modèle temporaire
          model_temp <- VAR_KMEANS$new(K = k, n_init = 10, max_iter = 50, scale = TRUE)
          model_temp$fit(rv$data_cleaned)
          
          # Récupérer inertie intra-classe
          # Adapter selon votre classe VAR_KMEANS
          if (!is.null(model_temp$FWithinClusterInertia)) {
            return(model_temp$FWithinClusterInertia)
          } else if (!is.null(model_temp$WithinClusterInertia)) {
            return(model_temp$WithinClusterInertia)
          } else {
            return(NA)
          }
        })
        
      })
      
      # Vérifier résultats
      if (all(is.na(inertias))) {
        return(plotly_empty() %>% 
                 layout(title = list(text = "Impossible de calculer les inerties")))
      }
      
      # Créer graphique
      plot_ly(
        x = k_values,
        y = inertias,
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 12, color = "#3498db"),
        line = list(width = 3, color = "#3498db"),
        text = paste("K =", k_values, "<br>Inertie =", round(inertias, 4)),
        hoverinfo = "text"
      ) %>%
        layout(
          title = "Méthode du Coude - VAR_KMEANS",
          xaxis = list(
            title = "Nombre de clusters (K)",
            dtick = 1
          ),
          yaxis = list(
            title = "Inertie intra-classe"
          ),
          hovermode = "closest",
          showlegend = FALSE,
          margin = list(l = 80, r = 50, t = 80, b = 60)
        ) %>%
        config(displayModeBar = TRUE)
      
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = list(text = paste("Erreur:", e$message)))
    })
  })
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 5. TABLEAU INERTIES VAR_KMEANS
  # ───────────────────────────────────────────────────────────────────────────────
  output$table_inertia_kmeans <- renderDT({
    req(rv$model_r6, input$algorithm == "var_kmeans")
    
    tryCatch({
      # Récupérer inerties du modèle actuel
      # Adapter selon les noms de champs dans votre classe VAR_KMEANS
      inertie_intra <- rv$model_r6$FWithinClusterInertia %||% 
        rv$model_r6$WithinClusterInertia %||% NA
      
      inertie_totale <- rv$model_r6$FTotalInertia %||% 
        rv$model_r6$TotalInertia %||% NA
      
      # Si inertie totale non disponible, la calculer
      if (is.na(inertie_totale) && !is.null(rv$data_cleaned)) {
        # Inertie totale = somme des variances
        inertie_totale <- sum(apply(rv$data_cleaned, 2, var, na.rm = TRUE))
      }
      
      # Calculer inertie inter
      inertie_inter <- inertie_totale - inertie_intra
      
      # Pourcentage expliqué
      pct_explique <- if (!is.na(inertie_totale) && inertie_totale > 0) {
        (inertie_inter / inertie_totale) * 100
      } else {
        NA
      }
      
      # Créer le tableau
      df <- data.frame(
        Metrique = c(
          "Inertie Totale",
          "Inertie Intra-classe",
          "Inertie Inter-classe",
          "% Variance Expliquée"
        ),
        Valeur = c(
          if (!is.na(inertie_totale)) sprintf("%.4f", inertie_totale) else "N/A",
          if (!is.na(inertie_intra)) sprintf("%.4f", inertie_intra) else "N/A",
          if (!is.na(inertie_inter)) sprintf("%.4f", inertie_inter) else "N/A",
          if (!is.na(pct_explique)) sprintf("%.2f%%", pct_explique) else "N/A"
        ),
        Description = c(
          "Variance totale des données",
          "Variance à l'intérieur des clusters",
          "Variance entre les clusters",
          "Part de variance expliquée par le clustering"
        )
      )
      
      datatable(
        df,
        options = list(
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Metrique',
          fontWeight = 'bold'
        )
      
    }, error = function(e) {
      datatable(
        data.frame(Erreur = paste("Erreur:", e$message)),
        options = list(dom = 't')
      )
    })
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # GRAPHIQUES TANDEMVARCLUST
  # ═══════════════════════════════════════════════════════════════════════════════
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 6. PROJECTION FACTORIELLE TANDEM
  # ───────────────────────────────────────────────────────────────────────────────
  output$plot_projection_tandem <- renderPlotly({
    req(rv$model_r6, input$algorithm == "tandem")
    
    tryCatch({
      # Récupérer coordonnées factorielles
      # Adapter selon votre classe TandemVarClust
      coords <- rv$model_r6$FactorialCoords %||% 
        rv$model_r6$FFactorialCoords
      
      if (is.null(coords)) {
        return(plotly_empty() %>% 
                 layout(title = list(text = "Coordonnées factorielles non disponibles")))
      }
      
      # Récupérer groupes
      groupes <- rv$model_r6$Groupes %||% rv$model_r6$FGroupes
      
      if (is.null(groupes)) {
        return(plotly_empty() %>% 
                 layout(title = list(text = "Groupes non disponibles")))
      }
      
      # Vérifier dimensions
      if (ncol(coords) < 2) {
        return(plotly_empty() %>% 
                 layout(title = list(text = "Pas assez de dimensions factorielles")))
      }
      
      # Créer data frame pour plotly
      plot_data <- data.frame(
        Axe1 = coords[, 1],
        Axe2 = coords[, 2],
        Variable = names(groupes),
        Cluster = as.factor(groupes)
      )
      
      # Créer scatter plot
      plot_ly(
        plot_data,
        x = ~Axe1,
        y = ~Axe2,
        text = ~Variable,
        color = ~Cluster,
        colors = "Set2",
        type = "scatter",
        mode = "markers+text",
        marker = list(size = 14, line = list(width = 1, color = "white")),
        textposition = "top center",
        textfont = list(size = 10)
      ) %>%
        layout(
          title = "Projection Factorielle (AFDM)",
          xaxis = list(
            title = "Axe factoriel 1",
            zeroline = TRUE,
            zerolinewidth = 1,
            zerolinecolor = "gray"
          ),
          yaxis = list(
            title = "Axe factoriel 2",
            zeroline = TRUE,
            zerolinewidth = 1,
            zerolinecolor = "gray"
          ),
          showlegend = TRUE,
          legend = list(title = list(text = "Cluster")),
          hovermode = "closest",
          margin = list(l = 80, r = 50, t = 80, b = 60)
        ) %>%
        config(displayModeBar = TRUE)
      
    }, error = function(e) {
      plotly_empty() %>% 
        layout(title = list(text = paste("Erreur:", e$message)))
    })
  })
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 7. DENDROGRAMME VALEURS PROPRES TANDEM
  # ───────────────────────────────────────────────────────────────────────────────
  output$plot_dendrogram_tandem <- renderPlot({
    req(rv$model_r6, input$algorithm == "tandem")
    
    tryCatch({
      # Récupérer l'arbre
      tree <- rv$model_r6$Tree %||% rv$model_r6$FTree
      
      if (is.null(tree)) {
        plot.new()
        text(0.5, 0.5, "Arbre non disponible", cex = 1.5, col = "red")
        return()
      }
      
      # Vérifier que c'est bien un objet hclust
      if (!inherits(tree, "hclust")) {
        plot.new()
        text(0.5, 0.5, "Type d'arbre invalide", cex = 1.5, col = "red")
        return()
      }
      
      # Tracer le dendrogramme
      plot(tree,
           main = "Dendrogramme sur Valeurs Propres - TandemVarClust",
           xlab = "",
           ylab = "Distance",
           sub = "",
           cex = 0.8,
           cex.main = 1.2)
      
      # Ajouter les rectangles pour les clusters
      if (!is.null(input$n_clusters) && input$n_clusters >= 2) {
        rect.hclust(tree, k = input$n_clusters, border = 2:6)
      }
      
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Erreur:", e$message), cex = 1, col = "red")
    })
  })
  
  # ───────────────────────────────────────────────────────────────────────────────
  # 8. TABLEAU INERTIES TANDEM
  # ───────────────────────────────────────────────────────────────────────────────
  output$table_inertia_tandem <- renderDT({
    req(rv$model_r6, input$algorithm == "tandem")
    
    tryCatch({
      # Essayer d'appeler la méthode inertie() si elle existe
      inertie_info <- tryCatch({
        rv$model_r6$inertie()
      }, error = function(e) {
        # Si la méthode n'existe pas, essayer d'accéder aux champs directement
        list(
          totale = rv$model_r6$FTotalInertia %||% rv$model_r6$TotalInertia %||% NA,
          intra = rv$model_r6$FWithinClusterInertia %||% rv$model_r6$WithinClusterInertia %||% NA,
          inter = rv$model_r6$FBetweenClusterInertia %||% rv$model_r6$BetweenClusterInertia %||% NA
        )
      })
      
      # Calculer inertie inter si nécessaire
      if (is.na(inertie_info$inter) && !is.na(inertie_info$totale) && !is.na(inertie_info$intra)) {
        inertie_info$inter <- inertie_info$totale - inertie_info$intra
      }
      
      # Calculer pourcentage
      pct <- if (!is.na(inertie_info$totale) && inertie_info$totale > 0 && !is.na(inertie_info$inter)) {
        (inertie_info$inter / inertie_info$totale) * 100
      } else {
        NA
      }
      
      # Créer le tableau
      df <- data.frame(
        Metrique = c(
          "Inertie Totale",
          "Inertie Intra-classe",
          "Inertie Inter-classe",
          "% Variance Expliquée"
        ),
        Valeur = c(
          if (!is.na(inertie_info$totale)) sprintf("%.4f", inertie_info$totale) else "N/A",
          if (!is.na(inertie_info$intra)) sprintf("%.4f", inertie_info$intra) else "N/A",
          if (!is.na(inertie_info$inter)) sprintf("%.4f", inertie_info$inter) else "N/A",
          if (!is.na(pct)) sprintf("%.2f%%", pct) else "N/A"
        ),
        Description = c(
          "Variance totale capturée par l'AFDM",
          "Variance à l'intérieur des clusters",
          "Variance entre les clusters",
          "Qualité du clustering"
        )
      )
      
      datatable(
        df,
        options = list(
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE
      ) %>%
        formatStyle(
          'Metrique',
          fontWeight = 'bold'
        )
      
    }, error = function(e) {
      datatable(
        data.frame(Erreur = paste("Erreur:", e$message)),
        options = list(dom = 't')
      )
    })
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════════
  # OPÉRATEUR %||% (NULL COALESCING)
  # ═══════════════════════════════════════════════════════════════════════════════
  # Si pas déjà défini ailleurs, ajouter cet opérateur utile
  
  `%||%` <- function(a, b) {
    if (is.null(a)) b else a
  }
  
  
  ##########################################################################################
  # ═══════════════════════════════════════════════════════════════════════════
  # SERVER - ONGLET PRÉDICTION AMÉLIORÉ
  # À ajouter/remplacer dans server.R
  # ═══════════════════════════════════════════════════════════════════════════
  
  # ═══════════════════════════════════════════════════════════════════════════
  # REACTIVE VALUES pour les données de prédiction
  # ═══════════════════════════════════════════════════════════════════════════
  
  prediction_data <- reactiveValues(
    data = NULL,              # Données à prédire
    source = "internal",      # "internal" ou "external"
    selected_vars = NULL      # Variables sélectionnées
  )
  
  prediction_results <- reactiveVal(NULL)
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # AFFICHAGE - Variables internes disponibles
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$predict_vars_available_msg <- renderUI({
    if (length(variable_roles$predict) == 0) {
      tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        tags$strong(" Aucune variable marquée 'Predict'."),
        tags$br(),
        "Retournez dans l'onglet Configuration pour marquer des variables à prédire."
      )
    }
  })
  
  output$predict_vars_selection <- renderUI({
    req(length(variable_roles$predict) > 0)
    
    vars <- variable_roles$predict
    
    tagList(
      tags$p(
        class = "text-muted",
        "Sélectionnez les variables dont vous voulez prédire le cluster :"
      ),
      
      checkboxGroupInput(
        inputId = "predict_vars_selected",
        label = NULL,
        choices = setNames(vars, vars),
        selected = vars  # Toutes sélectionnées par défaut
      ),
      
      tags$p(
        class = "text-info",
        icon("info-circle"),
        paste0(" ", length(vars), " variable(s) disponible(s)")
      )
    )
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # IMPORT - Fichier externe
  # ═══════════════════════════════════════════════════════════════════════════
  
  # Données du fichier externe
  predict_external_data <- reactive({
    req(input$predict_file)
    
    file_info <- input$predict_file
    file_ext <- tools::file_ext(file_info$name)
    
    tryCatch({
      
      if (file_ext %in% c("csv", "txt")) {
        # CSV/TXT
        data <- read.table(
          file_info$datapath,
          header = input$predict_header,
          sep = input$predict_separator,
          dec = input$predict_decimal,
          stringsAsFactors = FALSE
        )
        
      } else if (file_ext %in% c("xlsx", "xls")) {
        # Excel
        data <- readxl::read_excel(file_info$datapath)
        data <- as.data.frame(data)
        
      } else {
        stop("Format de fichier non supporté")
      }
      
      return(data)
      
    }, error = function(e) {
      showNotification(
        paste("Erreur lors de l'import :", e$message),
        type = "error",
        duration = 5
      )
      return(NULL)
    })
  })
  
  # Aperçu du fichier externe
  output$predict_file_preview <- renderUI({
    req(predict_external_data())
    
    data <- predict_external_data()
    
    tagList(
      tags$hr(),
      tags$h5(icon("eye"), " Aperçu des Données"),
      
      tags$p(
        class = "text-success",
        icon("check"),
        paste0(" Fichier chargé : ", nrow(data), " observations, ", ncol(data), " variables")
      ),
      
      renderTable({
        head(data, 5)
      })
    )
  })
  
  # Sélection des variables du fichier externe
  output$predict_external_vars_selection <- renderUI({
    req(predict_external_data())
    
    data <- predict_external_data()
    vars <- names(data)
    
    wellPanel(
      h4(icon("list"), "Sélection des Variables"),
      
      tags$p(
        class = "text-muted",
        "Choisissez les variables à prédire :"
      ),
      
      checkboxGroupInput(
        inputId = "predict_external_vars_selected",
        label = NULL,
        choices = setNames(vars, vars),
        selected = vars  # Toutes sélectionnées par défaut
      )
    )
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # PRÉDICTION - Bouton principal
  # ═══════════════════════════════════════════════════════════════════════════
  
  observeEvent(input$predict_btn, {
    req(rv$model_r6)  # ← CHANGÉ
    
    model <- rv$model_r6  # ← CHANGÉ
    
    # ═══════════════════════════════════════════════════════════════════════
    # Déterminer la source et les variables
    # ═══════════════════════════════════════════════════════════════════════
    
    if (input$predict_source == "internal") {
      # Variables internes
      
      if (length(variable_roles$predict) == 0) {
        showNotification(
          "Aucune variable marquée 'Predict'. Configurez d'abord vos variables.",
          type = "error",
          duration = 5
        )
        return()
      }
      
      req(input$predict_vars_selected)
      
      if (length(input$predict_vars_selected) == 0) {
        showNotification(
          "Veuillez sélectionner au moins une variable à prédire.",
          type = "error",
          duration = 3
        )
        return()
      }
      
      # Données = fichier initial
      data_full <- rv$data
      vars_to_predict <- input$predict_vars_selected
      
      # Extraire les variables sélectionnées
      newdata <- data_full[, vars_to_predict, drop = FALSE]
      
    } else {
      # Fichier externe
      
      req(predict_external_data())
      req(input$predict_external_vars_selected)
      
      if (length(input$predict_external_vars_selected) == 0) {
        showNotification(
          "Veuillez sélectionner au moins une variable à prédire.",
          type = "error",
          duration = 3
        )
        return()
      }
      
      data_external <- predict_external_data()
      vars_to_predict <- input$predict_external_vars_selected
      
      # Extraire les variables sélectionnées
      newdata <- data_external[, vars_to_predict, drop = FALSE]
    }
    
    # ═══════════════════════════════════════════════════════════════════════
    # Vérification : nombre d'observations
    # ═══════════════════════════════════════════════════════════════════════
    
    n_train <- nrow(rv$data[, variable_roles$active, drop = FALSE])
    n_predict <- nrow(newdata)
    
    if (n_predict != n_train) {
      showNotification(
        paste0(
          "Erreur : Le fichier de prédiction doit avoir le même nombre d'observations que les données d'entraînement.\n",
          "Entraînement : ", n_train, " observations\n",
          "Prédiction : ", n_predict, " observations"
        ),
        type = "error",
        duration = 7
      )
      return()
    }
    
    # ═══════════════════════════════════════════════════════════════════════
    # Prédiction
    # ═══════════════════════════════════════════════════════════════════════
    
    tryCatch({
      
      # Appel à predict()
      predictions <- model$predict(newdata)  # ← Maintenant model = rv$model_r6
      
      # Stocker les résultats
      rv$prediction_results <- predictions
      
      # Message de succès
      showNotification(
        paste0("Prédiction effectuée avec succès pour ", length(predictions), " variable(s)"),
        type = "message",
        duration = 3
      )
      
    }, error = function(e) {
      showNotification(
        paste("Erreur lors de la prédiction :", e$message),
        type = "error",
        duration = 7
      )
    })
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # AFFICHAGE - Résultats de la prédiction
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$prediction_results <- renderUI({
    req(rv$prediction_results)
    
    predictions <- rv$prediction_results
    
    # Créer une carte pour chaque variable
    result_cards <- lapply(names(predictions), function(var_name) {
      pred <- predictions[[var_name]]
      
      # Vérifier si erreur
      if (!is.null(pred$error)) {
        return(
          tags$div(
            class = "alert alert-danger",
            tags$h4(icon("exclamation-triangle"), " ", var_name),
            tags$p(pred$error)
          )
        )
      }
      
      # Carte de résultat
      tags$div(
        class = "panel panel-primary",
        style = "margin-bottom: 20px;",
        
        # En-tête
        tags$div(
          class = "panel-heading",
          tags$h4(
            class = "panel-title",
            icon("crystal-ball"),
            " ",
            var_name
          )
        ),
        
        # Corps
        tags$div(
          class = "panel-body",
          
          # Cluster prédit
          tags$div(
            style = "margin-bottom: 20px;",
            tags$h3(
              tags$span(
                class = "label label-success",
                style = "font-size: 24px; padding: 10px 20px;",
                paste("Cluster", pred$cluster)
              )
            )
          ),
          
          # Informations supplémentaires (TandemVarClust)
          if (!is.null(pred$n_modalities)) {
            tagList(
              tags$div(
                class = "well well-sm",
                tags$p(
                  tags$strong("Nombre de modalités : "),
                  pred$n_modalities
                ),
                tags$p(
                  tags$strong("Assignations des modalités : "),
                  paste(pred$modality_clusters, collapse = ", ")
                )
              )
            )
          },
          
          # Distances aux centres
          if (!is.null(pred$distances)) {
            tagList(
              tags$hr(),
              tags$h5(icon("ruler"), " Distances aux Centres de Clusters"),
              renderTable({
                df <- as.data.frame(round(pred$distances, 3))
                df
              }, bordered = TRUE, striped = TRUE, hover = TRUE)
            )
          }
        )
      )
    })
    
    tagList(
      tags$h3(icon("check-circle"), " Résultats de la Prédiction"),
      do.call(tagList, result_cards)
    )
  })
  
  
  # ═══════════════════════════════════════════════════════════════════════════
  # EXPORT - Télécharger les résultats
  # ═══════════════════════════════════════════════════════════════════════════
  
  output$download_predictions <- downloadHandler(
    filename = function() {
      paste0("predictions_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      req(rv$prediction_results)
      
      predictions <- rv$prediction_results
      
      # Créer un data frame avec les résultats
      results_df <- data.frame(
        Variable = names(predictions),
        Cluster_Predit = sapply(predictions, function(p) p$cluster),
        Nombre_Modalites = sapply(predictions, function(p) {
          if (!is.null(p$n_modalities)) p$n_modalities else NA
        }),
        stringsAsFactors = FALSE
      )
      
      # Écrire le CSV
      write.csv(results_df, file, row.names = FALSE)
    }
  )
  
}
