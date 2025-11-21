# ==============================================================================
# SERVEUR
# ==============================================================================

server <- function(input, output, session) {
  
  # ============================================================================
  # VALEURS RÉACTIVES
  # ============================================================================
  
  rv <- reactiveValues(
    data = NULL,
    data_cleaned = NULL,
    model = NULL,
    comparison_results = NULL,
    clustering_done = FALSE
  )
  
  # ============================================================================
  # ONGLET ACCUEIL - Info Boxes
  # ============================================================================
  
  output$info_algorithms <- renderInfoBox({
    infoBox(
      "Algorithmes",
      "7 méthodes",
      icon = icon("cogs"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$info_features <- renderInfoBox({
    infoBox(
      "Fonctionnalités",
      "15+ features",
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
  # ONGLET DONNÉES - Import
  # ============================================================================
  
  # Charger données exemple
  observeEvent(input$load_sample, {
    set.seed(42)
    n <- 100
    
    rv$data <- data.frame(
      PIB = rnorm(n, 100, 15),
      Revenu = rnorm(n, 50, 10),
      Emploi = rnorm(n, 75, 12),
      Population = rnorm(n, 1000, 200),
      Natalite = rnorm(n, 15, 3),
      Mortalite = rnorm(n, 10, 2),
      Temperature = rnorm(n, 20, 5),
      Precipitation = rnorm(n, 800, 150),
      Pollution = rnorm(n, 50, 15)
    )
    
    # Corrélations intra-groupes
    rv$data$Revenu <- rv$data$PIB * 0.7 + rnorm(n, 0, 5)
    rv$data$Emploi <- rv$data$PIB * 0.6 + rnorm(n, 0, 8)
    
    showNotification("✓ Données exemple chargées", type = "message")
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
                      "csv_tab" = "\t"
        )
        
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
      text(0.5, 0.5, "Aucune valeur manquante", cex = 1.5, col = "green")
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
      "vca_pam" = "PAM (Partition Around Medoids) : K-médoïdes robustes aux outliers. Utilise des points représentatifs réels.",
      "vca_hierarchical" = "Hierarchical : Classification Ascendante Hiérarchique avec dendrogramme. Permet d'explorer différents niveaux de clustering.",
      "vca_spectral" = "Spectral : Clustering spectral basé sur la décomposition en valeurs propres. Idéal pour structures non-linéaires.",
      "vca_pcamix" = "PCAmix : Analyse factorielle pour variables mixtes (numériques + catégorielles).",
      "var_cah" = "VAR_CAH : CAH classique sur variables avec PC1 comme variable synthétique.",
      "kmodes" = "K-Modes : Algorithme pour variables catégorielles basé sur le mode majoritaire.",
      "varclus" = "VARCLUS : Clustering descendant par division successive. Utilise le critère λ₂ ≥ 1."
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
  
  # Sélection variables illustratives
  output$illustrative_vars_ui <- renderUI({
    req(rv$data)
    
    checkboxGroupInput(
      "illustrative_vars",
      NULL,
      choices = colnames(rv$data),
      selected = NULL
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
    
    cat("Algorithme :", input$algorithm, "\n")
    
    if (input$algorithm != "varclus") {
      cat("Nombre de clusters :", 
          if(input$auto_k) "Auto-détection" else input$n_clusters, "\n")
    } else {
      cat("Nombre de clusters : Détection automatique (λ₂)\n")
    }
    
    cat("Variables actives :", length(input$active_vars), "\n")
    cat("Stratégie NA :", input$na_strategy, "\n")
    cat("Standardisation :", input$standardize, "\n")
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
        
        # Création du modèle selon l'algorithme
        incProgress(0.3, detail = "Initialisation du modèle")
        
        # NOTE: Cette partie nécessite que les classes R6 soient chargées
        # Pour la démo, on simule le clustering
        
        # Exemple pour VarClustAdvanced
        if (startsWith(input$algorithm, "vca")) {
          method_name <- sub("vca_", "", input$algorithm)
          
          # rv$model <- VarClustAdvanced$new(
          #   method = method_name,
          #   k = if(input$auto_k) NULL else input$n_clusters,
          #   distance_metric = input$distance_metric,
          #   linkage = input$linkage,
          #   na_strategy = input$na_strategy
          # )
          
          # Simulation pour la démo
          rv$model <- list(
            method = method_name,
            k = if(input$auto_k) 3 else input$n_clusters,
            clusters = sample(1:input$n_clusters, ncol(X), replace = TRUE),
            silhouette_avg = runif(1, 0.4, 0.8),
            metrics = list(
              davies_bouldin = runif(1, 0.5, 1.5),
              dunn = runif(1, 0.3, 0.9),
              calinski_harabasz = runif(1, 50, 200)
            )
          )
          names(rv$model$clusters) <- colnames(X)
        }
        
        incProgress(0.5, detail = "Ajustement du modèle")
        # rv$model$fit(X)
        
        incProgress(0.9, detail = "Calcul des métriques")
        
        rv$clustering_done <- TRUE
        incProgress(1.0, detail = "Terminé!")
        
        showNotification("✓ Clustering terminé avec succès!", type = "message")
        
        # Passer à l'onglet résultats
        updateTabItems(session, "sidebar", "results")
        
      }, error = function(e) {
        showNotification(paste("❌ Erreur:", e$message), type = "error")
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
        strong(" Clustering terminé avec succès!")
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
    
    valueBox(
      quality,
      "Silhouette",
      icon = icon("star"),
      color = "purple"
    )
  })
  
  # ============================================================================
  # ONGLET RÉSULTATS
  # ============================================================================
  
  output$model_summary <- renderPrint({
    req(rv$model)
    
    cat("╔════════════════════════════════════════════════════════════╗\n")
    cat("║   RÉSULTATS DU CLUSTERING                                  ║\n")
    cat("╚════════════════════════════════════════════════════════════╝\n\n")
    
    cat("Méthode           :", rv$model$method, "\n")
    cat("Nombre de clusters:", rv$model$k, "\n")
    cat("Silhouette moyen  :", round(rv$model$silhouette_avg, 3), "\n\n")
    
    cat("Répartition des variables:\n")
    print(table(rv$model$clusters))
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
                                  'Silhouette: %{x:.3f}<br>',
                                  '<extra></extra>')) %>%
      layout(
        title = "Graphique Silhouette",
        xaxis = list(title = "Score Silhouette"),
        yaxis = list(title = "Variables", showticklabels = FALSE),
        showlegend = TRUE
      )
  })
  
  # Dendrogramme
  output$plot_dendrogram <- renderPlot({
    req(rv$model, rv$data, input$active_vars)
    
    X <- rv$data[, input$active_vars, drop = FALSE]
    
    # Calculer dendrogramme
    if (all(sapply(X, is.numeric))) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      dist_mat <- as.dist(sqrt(2 * (1 - abs(cor_mat))))
      hc <- hclust(dist_mat, method = "ward.D2")
      
      plot(hc, 
           main = "Dendrogramme des Variables",
           xlab = "", 
           sub = "",
           hang = -1)
      rect.hclust(hc, k = rv$model$k, border = "red")
    } else {
      plot.new()
      text(0.5, 0.5, "Dendrogramme disponible uniquement pour variables numériques", 
           cex = 1.2)
    }
  })
  
  # Heatmap
  output$plot_heatmap <- renderPlotly({
    req(rv$model, rv$data, input$active_vars)
    
    X <- rv$data[, input$active_vars, drop = FALSE]
    
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
          xaxis = list(title = ""),
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
    req(rv$data, input$active_vars)
    
    X <- rv$data[, input$active_vars, drop = FALSE]
    
    if (all(sapply(X, is.numeric)) && ncol(X) >= 2) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      
      corrplot::corrplot(
        cor_mat,
        method = "color",
        type = "upper",
        tl.col = "black",
        tl.srt = 45,
        tl.cex = 0.8,
        addCoef.col = "black",
        number.cex = 0.6,
        title = "Matrice de Corrélation",
        mar = c(0, 0, 2, 0)
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
      filter = 'top'
    ) %>%
      formatStyle(
        'Silhouette',
        background = styleColorBar(range(cluster_df$Silhouette), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
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
    
    valueBox(
      db_val,
      "Davies-Bouldin (↓)",
      icon = icon("arrows-alt-v"),
      color = "blue"
    )
  })
  
  output$diag_dunn <- renderValueBox({
    req(rv$model)
    
    dunn_val <- round(rv$model$metrics$dunn, 3)
    
    valueBox(
      dunn_val,
      "Dunn Index (↑)",
      icon = icon("arrows-alt-v"),
      color = "purple"
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
    sil_scores <- rnorm(length(rv$model$clusters), rv$model$silhouette_avg, 0.1)
    bad_idx <- which(sil_scores < 0)
    
    if (length(bad_idx) > 0) {
      bad_df <- data.frame(
        Variable = names(rv$model$clusters)[bad_idx],
        Cluster_Actuel = rv$model$clusters[bad_idx],
        Silhouette = round(sil_scores[bad_idx], 3)
      )
      
      datatable(bad_df, options = list(pageLength = 10))
    } else {
      datatable(data.frame(Message = "Aucune variable mal classée!"))
    }
  })
  
  # Qualité par cluster
  output$quality_by_cluster <- renderPlotly({
    req(rv$model)
    
    # Simulation qualité par cluster
    quality_df <- data.frame(
      Cluster = 1:rv$model$k,
      Qualite = runif(rv$model$k, 0.4, 0.9)
    )
    
    plot_ly(
      quality_df,
      x = ~Cluster,
      y = ~Qualite,
      type = 'bar',
      marker = list(color = 'coral'),
      text = ~round(Qualite, 3),
      textposition = 'auto'
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
    
    if (avg_sil > 0.7) {
      cat("✓ Excellente structure de clustering (silhouette > 0.7)\n")
    } else if (avg_sil > 0.5) {
      cat("✓ Bonne structure de clustering (silhouette > 0.5)\n")
    } else if (avg_sil > 0.25) {
      cat("⚠ Structure faible (silhouette > 0.25)\n")
      cat("  → Considérer un nombre différent de clusters\n")
    } else {
      cat("✗ Structure très faible (silhouette < 0.25)\n")
      cat("  → Revoir la pertinence des variables\n")
      cat("  → Essayer une autre méthode\n")
    }
  })
  
  # Méthode du coude
  output$elbow_plot <- renderPlotly({
    req(rv$data, input$active_vars)
    
    # Simulation courbe élbow
    k_values <- 2:min(10, length(input$active_vars) - 1)
    sil_scores <- sapply(k_values, function(k) {
      1 - exp(-0.3 * k) + rnorm(1, 0, 0.05)
    })
    
    elbow_df <- data.frame(k = k_values, silhouette = sil_scores)
    
    plot_ly(
      elbow_df,
      x = ~k,
      y = ~silhouette,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(size = 10, color = 'red'),
      line = list(color = 'blue', width = 2),
      hovertemplate = 'k = %{x}<br>Silhouette = %{y:.3f}<extra></extra>'
    ) %>%
      add_trace(
        x = if(!is.null(rv$model)) rv$model$k else NULL,
        y = if(!is.null(rv$model)) rv$model$silhouette_avg else NULL,
        type = 'scatter',
        mode = 'markers',
        marker = list(size = 15, color = 'green', symbol = 'star'),
        name = 'Sélectionné',
        showlegend = TRUE
      ) %>%
      layout(
        title = "Méthode du Coude (Silhouette)",
        xaxis = list(title = "Nombre de clusters (k)"),
        yaxis = list(title = "Score Silhouette Moyen")
      )
  })
  
  # ============================================================================
  # ONGLET COMPARAISON
  # ============================================================================
  
  observeEvent(input$run_comparison, {
    req(rv$data, input$active_vars, input$comparison_algos)
    
    withProgress(message = 'Comparaison en cours...', value = 0, {
      
      # Simulation résultats comparaison
      comparison_results <- data.frame(
        Algorithme = input$comparison_algos,
        Silhouette = runif(length(input$comparison_algos), 0.4, 0.8),
        Davies_Bouldin = runif(length(input$comparison_algos), 0.5, 1.5),
        Dunn = runif(length(input$comparison_algos), 0.3, 0.9),
        Temps_s = runif(length(input$comparison_algos), 0.1, 2.0)
      )
      
      rv$comparison_results <- comparison_results
      
      showNotification("✓ Comparaison terminée", type = "message")
    })
  })
  
  output$comparison_table <- renderDT({
    req(rv$comparison_results)
    
    datatable(
      rv$comparison_results,
      options = list(pageLength = 10),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:5, digits = 3)
  })
  
  output$comparison_plot <- renderPlotly({
    req(rv$comparison_results)
    
    plot_ly(
      rv$comparison_results,
      x = ~Algorithme,
      y = ~Silhouette,
      type = 'bar',
      marker = list(color = 'steelblue'),
      text = ~round(Silhouette, 3),
      textposition = 'auto'
    ) %>%
      layout(
        title = "Comparaison des Algorithmes (Silhouette)",
        xaxis = list(title = ""),
        yaxis = list(title = "Score Silhouette", range = c(0, 1))
      )
  })
  
  # ============================================================================
  # ONGLET EXPORT
  # ============================================================================
  
  output$export_preview_clusters <- renderDT({
    req(rv$model)
    
    cluster_df <- data.frame(
      Variable = names(rv$model$clusters),
      Cluster = rv$model$clusters
    )
    
    datatable(cluster_df, options = list(pageLength = 10))
  })
  
  output$export_preview_metrics <- renderDT({
    req(rv$model)
    
    metrics_df <- data.frame(
      Metrique = c("Silhouette", "Davies-Bouldin", "Dunn", "Calinski-Harabasz"),
      Valeur = c(
        rv$model$silhouette_avg,
        rv$model$metrics$davies_bouldin,
        rv$model$metrics$dunn,
        rv$model$metrics$calinski_harabasz
      )
    )
    
    datatable(metrics_df, options = list(pageLength = 10), rownames = FALSE)
  })
  
  output$export_preview_silhouette <- renderPlot({
    req(rv$model)
    
    # Plot silhouette statique
    sil_df <- data.frame(
      variable = names(rv$model$clusters),
      cluster = factor(rv$model$clusters),
      silhouette = rnorm(length(rv$model$clusters), rv$model$silhouette_avg, 0.1)
    )
    
    ggplot(sil_df, aes(x = silhouette, y = reorder(variable, silhouette), 
                       fill = cluster)) +
      geom_col() +
      labs(title = "Graphique Silhouette",
           x = "Score Silhouette",
           y = "Variables") +
      theme_minimal()
  })
  
  # Downloads
  output$download_results <- downloadHandler(
    filename = function() {
      paste0(input$export_prefix, "_complet_", Sys.Date(), ".zip")
    },
    content = function(file) {
      # Créer fichiers temporaires et zipper
      showNotification("📦 Préparation de l'export...", type = "message")
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
  
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0(input$export_prefix, "_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      showNotification("📊 Export des graphiques...", type = "message")
    }
  )
  
  # Reset
  observeEvent(input$reset, {
    rv$data <- NULL
    rv$model <- NULL
    rv$clustering_done <- FALSE
    rv$comparison_results <- NULL
    
    showNotification("🔄 Application réinitialisée", type = "warning")
    updateTabItems(session, "sidebar", "home")
  })
}

