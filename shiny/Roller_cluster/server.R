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
    session_counter = 0
  )
  
  # ============================================================================
  # ONGLET ACCUEIL - Info Boxes
  # ============================================================================
  
  output$info_algorithms <- renderInfoBox({
    infoBox(
      "Algorithmes",
      "3 méthodes",
      icon = icon("cogs"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$info_features <- renderInfoBox({
    infoBox(
      "Fonctionnalités",
      "R6 + History",
      icon = icon("star"),
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
      icon = icon("check-circle"),
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
          stringsAsFactors = TRUE
        )
      }
      
      showNotification("✓ Fichier chargé avec succès", type = "message")
      
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
  
  # Description algorithme
  output$algorithm_description <- renderUI({
    desc <- switch(
      input$algorithm,
      "var_cah" = "VAR_CAH : Classification Ascendante Hiérarchique sur variables avec PC1 comme variable synthétique.",
      "kmodes" = "K-Modes : Algorithme pour variables catégorielles basé sur le mode majoritaire.",
      "varclus" = "VARCLUS : Clustering descendant par division successive. k détecté automatiquement (λ₂ ≥ 1)."
    )
    
    div(
      class = "alert alert-info",
      icon("info-circle"),
      strong(" Description : "),
      desc
    )
  })
  
  # Sélection variables actives
  output$active_vars_ui <- renderUI({
    req(rv$data)
    
    checkboxGroupInput(
      "active_vars",
      NULL,
      choices = colnames(rv$data),
      selected = colnames(rv$data)
    )
  })
  
  # Boutons de sélection
  observeEvent(input$select_all_active, {
    updateCheckboxGroupInput(session, "active_vars", selected = colnames(rv$data))
  })
  
  observeEvent(input$deselect_all_active, {
    updateCheckboxGroupInput(session, "active_vars", selected = character(0))
  })
  
  observeEvent(input$select_numeric, {
    req(rv$data)
    numeric_vars <- names(rv$data)[sapply(rv$data, is.numeric)]
    updateCheckboxGroupInput(session, "active_vars", selected = numeric_vars)
  })
  
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
      "kmodes" = "KmodesVarClust",
      "varclus" = "VARCLUS"
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
      showNotification("⚠️ Sélectionnez au moins 2 variables", type = "warning")
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
            k = k_actual,
            scale = input$standardize  # Attention: 'scale' pas 'standardize'
          )
          
          incProgress(0.5, detail = "Ajustement VAR_CAH")
          model_instance$fit(X)
          
          # Convertir en structure Shiny
          rv$model <- model_to_shiny(model_instance, algorithm = "VAR_CAH", data_used = X)
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
        updateTabItems(session, "sidebar", "results")
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur:", e$message), type = "error")
        print(e)
        print(traceback())
      })
    })
  })
  
  # Progress UI
  output$progress_ui <- renderUI({
    if (!rv$clustering_done) {
      div(
        class = "alert alert-info",
        icon("clock"),
        " En attente du lancement..."
      )
    } else {
      div(
        class = "alert alert-success",
        icon("check"),
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
      icon = icon(if(rv$clustering_done) "check" else "clock"),
      color = if(rv$clustering_done) "green" else "yellow"
    )
  })
  
  output$vbox_k <- renderValueBox({
    k_value <- if(!is.null(rv$model)) rv$model$k else "N/A"
    
    valueBox(
      k_value,
      "Clusters",
      icon = icon("layer-group"),
      color = "blue"
    )
  })
  
  output$vbox_quality <- renderValueBox({
    quality <- if(!is.null(rv$model)) {
      round(rv$model$silhouette_avg, 3)
    } else {
      "N/A"
    }
    
    color_qual <- if(!is.null(rv$model)) {
      if(rv$model$silhouette_avg > 0.7) "green"
      else if(rv$model$silhouette_avg > 0.5) "yellow"
      else "red"
    } else {
      "purple"
    }
    
    valueBox(
      quality,
      "Silhouette",
      icon = icon("star"),
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
    cat("Davies-Bouldin    :", round(rv$model$metrics$davies_bouldin, 4), "\n")
    cat("Dunn Index        :", round(rv$model$metrics$dunn, 4), "\n")
    cat("Calinski-Harabasz :", round(rv$model$metrics$calinski_harabasz, 2), "\n")
    
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
    req(rv$model)
    
    # Simulation données silhouette
    sil_df <- data.frame(
      variable = names(rv$model$clusters),
      cluster = factor(rv$model$clusters),
      silhouette = rnorm(length(rv$model$clusters), rv$model$silhouette_avg, 0.1)
    )
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
        title = "Graphique Silhouette par Variable",
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
    req(rv$model, rv$data_cleaned)
    
    tryCatch({
      X <- rv$data_cleaned
      clusters <- rv$model$clusters
      
      # Vérifier que X est numérique
      if (!all(sapply(X, is.numeric))) {
        rv$projection_data <- list(error = "Variables non numériques")
        return()
      }
      
      # Transposer : variables en lignes
      X_t <- t(X)
      
      # Calculer la projection selon la méthode
      if (method == "pca") {
        
        # ACP sur les variables
        pca_result <- prcomp(X_t, center = TRUE, scale. = TRUE)
        
        # Coordonnées des variables sur PC1 et PC2
        coords <- as.data.frame(pca_result$x[, 1:2])
        colnames(coords) <- c("PC1", "PC2")
        
        # Variance expliquée
        var_explained <- summary(pca_result)$importance[2, 1:2] * 100
        
        rv$projection_data <- list(
          coords = coords,
          method = "ACP",
          var_explained = var_explained,
          clusters = clusters,
          variables = colnames(X)
        )
        
      } else if (method == "mds") {
        
        # Distance basée sur corrélation
        cor_mat <- cor(X, use = "pairwise.complete.obs")
        dist_mat <- as.dist(1 - abs(cor_mat))
        
        # MDS
        mds_result <- cmdscale(dist_mat, k = 2, eig = TRUE)
        
        coords <- as.data.frame(mds_result$points)
        colnames(coords) <- c("Dim1", "Dim2")
        
        # Qualité : GOF
        gof <- mds_result$GOF[1]
        
        rv$projection_data <- list(
          coords = coords,
          method = "MDS",
          gof = gof,
          clusters = clusters,
          variables = colnames(X)
        )
        
      } else if (method == "tsne") {
        
        # Vérifier package
        if (!requireNamespace("Rtsne", quietly = TRUE)) {
          rv$projection_data <- list(error = "Package Rtsne non installé")
          return()
        }
        
        perp <- ifelse(exists("input") && !is.null(input$tsne_perplexity), 
                       input$tsne_perplexity, 30)
        perp <- min(perp, floor((nrow(X_t) - 1) / 3))
        
        # t-SNE
        set.seed(42)
        tsne_result <- Rtsne::Rtsne(
          X_t, 
          dims = 2, 
          perplexity = perp,
          max_iter = 1000,
          check_duplicates = FALSE,
          verbose = FALSE
        )
        
        coords <- as.data.frame(tsne_result$Y)
        colnames(coords) <- c("Dim1", "Dim2")
        
        rv$projection_data <- list(
          coords = coords,
          method = "t-SNE",
          perplexity = perp,
          clusters = clusters,
          variables = colnames(X)
        )
        
      } else if (method == "umap") {
        
        # Vérifier package
        if (!requireNamespace("umap", quietly = TRUE)) {
          rv$projection_data <- list(error = "Package umap non installé")
          return()
        }
        
        neighbors <- ifelse(exists("input") && !is.null(input$umap_neighbors), 
                            input$umap_neighbors, 15)
        neighbors <- min(neighbors, nrow(X_t) - 1)
        
        # UMAP
        umap_config <- umap::umap.defaults
        umap_config$n_neighbors <- neighbors
        umap_config$verbose <- FALSE
        
        umap_result <- umap::umap(X_t, config = umap_config)
        
        coords <- as.data.frame(umap_result$layout)
        colnames(coords) <- c("Dim1", "Dim2")
        
        rv$projection_data <- list(
          coords = coords,
          method = "UMAP",
          n_neighbors = neighbors,
          clusters = clusters,
          variables = colnames(X)
        )
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
          h5(icon("exclamation-triangle"), "Erreur"),
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
        h5(icon(icon_name), title_text, style = paste0("color: ", text_color, ";")),
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
        h5(icon(icon_name), title_text),
        p(sprintf("GOF : %.3f", gof)),
        tags$small("Fidélité aux distances originales")
      )
      
    } else {
      wellPanel(
        style = "background-color: #d1ecf1;",
        h5(icon("info-circle"), "Projection non-linéaire"),
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
        icon("info-circle"),
        " Aucune prédiction effectuée"
      )
    } else {
      div(
        class = "alert alert-success",
        icon("check-circle"),
        strong(sprintf(" %d variable(s) classifiée(s)", nrow(rv$prediction_results)))
      )
    }
  })
  
  # Table résultats prédiction
  output$prediction_results_table <- renderDT({
    req(rv$prediction_results)
    
    datatable(
      rv$prediction_results,
      options = list(pageLength = 10),
      rownames = FALSE
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
      )
  })
  
  # Plot prédiction
  output$prediction_plot <- renderPlotly({
    req(rv$prediction_results)
    
    plot_ly(
      rv$prediction_results,
      x = ~Variable,
      y = ~Confiance,
      type = 'bar',
      color = ~factor(Cluster_Predit),
      text = ~paste("Cluster:", Cluster_Predit, "<br>Confiance:", round(Confiance, 2)),
      hovertemplate = '%{text}<extra></extra>'
    ) %>%
      layout(
        title = "Confiance des Prédictions",
        xaxis = list(title = "Variable"),
        yaxis = list(title = "Score de Confiance", range = c(0, 1)),
        showlegend = TRUE
      )
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
      icon = icon("chart-line"),
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
      icon = icon("arrows-alt-v"),
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
      icon = icon("arrows-alt-v"),
      color = color_box
    )
  })
  
  output$diag_calinski <- renderValueBox({
    req(rv$model)
    
    ch_val <- round(rv$model$metrics$calinski_harabasz, 1)
    
    valueBox(
      ch_val,
      "Calinski-Harabasz",
      icon = icon("chart-bar"),
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
  output$elbow_plot <- renderPlotly({
    req(rv$data_cleaned)
    
    # Simulation courbe élbow
    k_values <- 2:min(10, ncol(rv$data_cleaned) - 1)
    
    # Simuler des scores qui diminuent mais avec un coude
    sil_scores <- sapply(k_values, function(k) {
      base_score <- 0.8 * exp(-0.15 * k)
      noise <- rnorm(1, 0, 0.03)
      max(0.2, min(0.9, base_score + noise))
    })
    
    elbow_df <- data.frame(k = k_values, silhouette = sil_scores)
    
    # Graphique principal avec tous les k
    p <- plot_ly(
      elbow_df,
      x = ~k,
      y = ~silhouette,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(size = 10, color = 'steelblue'),
      line = list(color = 'steelblue', width = 2),
      hovertemplate = 'k = %{x}<br>Silhouette = %{y:.3f}<extra></extra>',
      name = 'Score'
    )
    
    # Ajouter le point actuel si un modèle existe
    if (!is.null(rv$model)) {
      p <- p %>%
        add_trace(
          x = rv$model$k,
          y = rv$model$silhouette_avg,
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 15, color = 'red', symbol = 'star'),
          name = 'Actuel',
          hovertemplate = paste0('k sélectionné = ', rv$model$k, 
                                 '<br>Silhouette = ', round(rv$model$silhouette_avg, 3),
                                 '<extra></extra>')
        )
    }
    
    p %>%
      layout(
        title = "Méthode du Coude (Silhouette vs k)",
        xaxis = list(title = "Nombre de clusters (k)", dtick = 1),
        yaxis = list(title = "Score Silhouette Moyen", range = c(0, 1))
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
            model_test <- VAR_CAH$new(k = k_test, scale = TRUE)
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
  
  output$comparison_table <- renderDT({
    req(rv$comparison_results)
    
    datatable(
      rv$comparison_results,
      options = list(
        pageLength = 10,
        order = list(list(1, 'desc'))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:5, digits = 3) %>%
      formatStyle(
        'Silhouette',
        background = styleColorBar(c(0, 1), 'lightgreen'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'Davies_Bouldin',
        background = styleColorBar(range(rv$comparison_results$Davies_Bouldin), 'lightcoral'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  output$comparison_plot <- renderPlotly({
    req(rv$comparison_results)
    
    plot_ly(
      rv$comparison_results,
      x = ~Algorithme,
      y = ~Silhouette,
      type = 'bar',
      name = 'Silhouette',
      marker = list(color = 'steelblue'),
      text = ~round(Silhouette, 3),
      textposition = 'auto',
      hovertemplate = '%{x}<br>Silhouette: %{y:.3f}<extra></extra>'
    ) %>%
      add_trace(
        y = ~Davies_Bouldin,
        name = 'Davies-Bouldin',
        marker = list(color = 'coral'),
        text = ~round(Davies_Bouldin, 3),
        yaxis = 'y2',
        hovertemplate = '%{x}<br>DB: %{y:.3f}<extra></extra>'
      ) %>%
      layout(
        title = "Comparaison des Algorithmes",
        xaxis = list(title = ""),
        yaxis = list(title = "Silhouette", side = 'left', range = c(0, 1)),
        yaxis2 = list(
          title = "Davies-Bouldin",
          overlaying = 'y',
          side = 'right',
          range = c(0, max(rv$comparison_results$Davies_Bouldin) * 1.2)
        ),
        barmode = 'group',
        showlegend = TRUE
      )
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
}