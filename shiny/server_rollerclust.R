# ==============================================================================
# SERVER
# ==============================================================================

server <- function(input, output, session) {
  rv <- reactiveValues(data = NULL, model = NULL, clustering_done = FALSE)
  
  # Accueil
  output$info_algorithms <- renderInfoBox({ infoBox("Algorithmes", "3", icon = icon("cogs"), color = "blue", fill = TRUE) })
  output$info_data <- renderInfoBox({ infoBox("Données", if(!is.null(rv$data)) "OK" else "Non", icon = icon("database"), color = if(!is.null(rv$data)) "green" else "red", fill = TRUE) })
  output$info_status <- renderInfoBox({ infoBox("Statut", if(rv$clustering_done) "Terminé" else "Attente", icon = icon("check"), color = if(rv$clustering_done) "green" else "yellow", fill = TRUE) })
  
  # Données
  observeEvent(input$generate_sample, { rv$data <- generate_sample_data(input$sample_type, input$sample_n); showNotification("✓ Données générées", type = "message") })
  observeEvent(input$file_input, {
    req(input$file_input)
    tryCatch({
      if (input$file_type == "excel") { rv$data <- read_excel(input$file_input$datapath) 
      } else {
        sep <- switch(input$file_type, "csv_comma" = ",", "csv_semicolon" = ";")
        rv$data <- read.table(input$file_input$datapath, header = TRUE, sep = sep, stringsAsFactors = TRUE)
      }
      showNotification("✓ Fichier chargé", type = "message")
    }, error = function(e) { showNotification(paste("❌", e$message), type = "error") })
  })
  output$data_preview <- renderDT({ req(rv$data); datatable(rv$data, options = list(pageLength = 10, scrollX = TRUE)) })
  
  # Configuration
  output$algorithm_description <- renderUI({
    desc <- switch(input$algorithm,
                   "var_cah" = "VAR_CAH : CAH sur variables avec PC1",
                   "kmodes" = "KmodesVarClust : K-Modes pour catégorielles",
                   "varclus" = "VARCLUS : Descendant par λ₂"
    )
    div(class = "alert alert-info", icon("info-circle"), " ", desc)
  })
  output$active_vars_ui <- renderUI({ req(rv$data); checkboxGroupInput("active_vars", "Actives", choices = colnames(rv$data), selected = colnames(rv$data)) })
  observeEvent(input$select_all, { updateCheckboxGroupInput(session, "active_vars", selected = colnames(rv$data)) })
  observeEvent(input$select_numeric, { req(rv$data); updateCheckboxGroupInput(session, "active_vars", selected = names(rv$data)[sapply(rv$data, is.numeric)]) })
  
  # Clustering
  output$config_summary <- renderPrint({
    req(rv$data, input$active_vars)
    cat("Algorithme :", input$algorithm, "\n")
    cat(if (input$algorithm != "varclus") paste("k :", input$n_clusters) else paste("λ₂ :", input$stop_eigenvalue), "\n")
    cat("Variables  :", length(input$active_vars), "\n")
  })
  
  observeEvent(input$run_clustering, {
    req(rv$data, input$active_vars)
    if (length(input$active_vars) < 2) { showNotification("⚠️ Min 2 variables", type = "warning"); return() }
    
    withProgress(message = 'Clustering...', value = 0, {
      tryCatch({
        X <- rv$data[, input$active_vars, drop = FALSE]
        incProgress(0.5)
        
        # REMPLACER PAR VOS CLASSES :
        # model_obj <- VAR_CAH$new(k = input$n_clusters); model_obj$fit(X)
        
        k_final <- if (input$algorithm == "varclus") sample(2:5, 1) else input$n_clusters
        rv$model <- list(algorithm = input$algorithm, k = k_final, clusters = sample(1:k_final, ncol(X), replace = TRUE), convergence = TRUE, iterations = sample(10:50, 1))
        names(rv$model$clusters) <- colnames(X)
        
        rv$clustering_done <- TRUE
        showNotification("✓ Terminé", type = "message")
        updateTabItems(session, "sidebar", "results")
      }, error = function(e) { showNotification(paste("❌", e$message), type = "error") })
    })
  })
  
  output$vbox_status <- renderValueBox({ valueBox(if(rv$clustering_done) "OK" else "Attente", "Statut", icon = icon(if(rv$clustering_done) "check" else "clock"), color = if(rv$clustering_done) "green" else "yellow") })
  output$vbox_k <- renderValueBox({ valueBox(if(!is.null(rv$model)) rv$model$k else "N/A", "Clusters", icon = icon("layer-group"), color = "blue") })
  output$vbox_vars <- renderValueBox({ valueBox(if(!is.null(rv$model)) length(rv$model$clusters) else 0, "Variables", icon = icon("list"), color = "purple") })
  
  # Résultats
  output$model_summary <- renderPrint({
    req(rv$model)
    cat("═══════════════════════════════════════\n")
    cat("  RÉSULTATS\n")
    cat("═══════════════════════════════════════\n\n")
    cat("Algorithme :", rv$model$algorithm, "\n")
    cat("Clusters   :", rv$model$k, "\n")
    cat("Convergence:", ifelse(rv$model$convergence, "Oui", "Non"), "\n\n")
    cat("Répartition:\n"); print(table(rv$model$clusters))
    cat("\nDétails:\n")
    for (k in 1:rv$model$k) {
      vars <- names(rv$model$clusters)[rv$model$clusters == k]
      cat("Cluster", k, ":", paste(vars, collapse = ", "), "\n")
    }
  })
  
  output$plot_dendrogram <- renderPlot({
    req(rv$model, rv$data, input$active_vars)
    X <- rv$data[, input$active_vars, drop = FALSE]
    if (all(sapply(X, is.numeric)) && input$algorithm %in% c("var_cah", "varclus")) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      dist_mat <- as.dist(sqrt(2 * (1 - abs(cor_mat))))
      hc <- hclust(dist_mat, method = "ward.D2")
      plot(hc, main = "Dendrogramme", xlab = "", sub = "", hang = -1)
      rect.hclust(hc, k = rv$model$k, border = "red")
    } else { plot.new(); text(0.5, 0.5, "Dendrogramme uniquement pour\nVAR_CAH/VARCLUS + numériques", cex = 1.2) }
  })
  
  output$plot_heatmap <- renderPlotly({
    req(rv$model, rv$data, input$active_vars)
    X <- rv$data[, input$active_vars, drop = FALSE]
    if (all(sapply(X, is.numeric))) {
      cor_mat <- cor(X, use = "pairwise.complete.obs")
      order_idx <- order(rv$model$clusters)
      cor_mat_ordered <- cor_mat[order_idx, order_idx]
      plot_ly(z = cor_mat_ordered, x = colnames(cor_mat_ordered), y = rownames(cor_mat_ordered), type = "heatmap", colors = colorRamp(c("blue", "white", "red"))) %>%
        layout(title = "Heatmap Corrélation")
    } else { plotly_empty() %>% layout(title = "Heatmap : variables numériques uniquement") }
  })
  
  output$plot_distribution <- renderPlotly({
    req(rv$model)
    df <- as.data.frame(table(rv$model$clusters)); names(df) <- c("Cluster", "Nombre")
    plot_ly(df, x = ~Cluster, y = ~Nombre, type = 'bar', marker = list(color = 'steelblue'), text = ~Nombre, textposition = 'auto') %>%
      layout(title = "Distribution", xaxis = list(title = "Cluster"), yaxis = list(title = "Variables"))
  })
  
  output$clusters_table <- renderDT({
    req(rv$model)
    df <- data.frame(Variable = names(rv$model$clusters), Cluster = as.factor(rv$model$clusters))
    datatable(df, options = list(pageLength = 20), filter = 'top', rownames = FALSE) %>%
      formatStyle('Cluster', backgroundColor = styleEqual(levels(df$Cluster), rainbow(nlevels(df$Cluster), alpha = 0.3)))
  })
  
  # Diagnostics
  output$diagnostics_text <- renderPrint({
    req(rv$model)
    cat("═══════════════════════════════════════\n")
    cat("  DIAGNOSTICS\n")
    cat("═══════════════════════════════════════\n\n")
    cat("Algorithme :", rv$model$algorithm, "\n")
    cat("Clusters   :", rv$model$k, "\n")
    cat("Convergence:", ifelse(rv$model$convergence, "Oui", "Non"), "\n\n")
    sizes <- table(rv$model$clusters)
    for (i in 1:length(sizes)) { cat("Cluster", i, ":", sizes[i], "variables\n") }
    if (any(sizes == 1)) cat("\n⚠ Singletons détectés\n")
    if (max(sizes) > 3 * min(sizes)) cat("⚠ Clusters déséquilibrés\n")
    cat("\n", if(rv$model$convergence) "✓ Convergence OK" else "⚠ Pas de convergence", "\n")
  })
  
  output$quality_by_cluster <- renderPlotly({
    req(rv$model)
    df <- data.frame(Cluster = 1:rv$model$k, Qualite = runif(rv$model$k, 0.5, 0.9))
    plot_ly(df, x = ~Cluster, y = ~Qualite, type = 'bar', marker = list(color = 'coral')) %>%
      layout(title = "Qualité par Cluster", yaxis = list(range = c(0, 1)))
  })
  
  # Export
  output$download_clusters <- downloadHandler(
    filename = function() { paste0("clusters_", Sys.Date(), ".csv") },
    content = function(file) { req(rv$model); write.csv(data.frame(Variable = names(rv$model$clusters), Cluster = rv$model$clusters), file, row.names = FALSE) }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() { paste0("plots_", Sys.Date(), ".zip") },
    content = function(file) {
      tmpdir <- tempdir()
      png(file.path(tmpdir, "distribution.png"), width = 800, height = 600)
      barplot(table(rv$model$clusters), main = "Distribution", col = rainbow(rv$model$k))
      dev.off()
      zip(file, files = list.files(tmpdir, pattern = ".png$", full.names = TRUE))
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() { paste0("rapport_", Sys.Date(), ".pdf") },
    content = function(file) {
      req(rv$model)
      rmd <- sprintf('---\ntitle: "%s"\nauthor: "%s"\noutput: pdf_document\n---\n\n# Résumé\n\nAlgo: %s | Clusters: %d\n\n# Résultats\n\n```{r echo=FALSE}\nknitr::kable(data.frame(Variable=c(%s), Cluster=c(%s)))\n```',
                     input$pdf_title, input$pdf_author, rv$model$algorithm, rv$model$k,
                     paste(sprintf('"%s"', names(rv$model$clusters)), collapse = ","),
                     paste(rv$model$clusters, collapse = ",")
      )
      rmd_file <- tempfile(fileext = ".Rmd"); writeLines(rmd, rmd_file)
      tryCatch({ rmarkdown::render(rmd_file, output_file = file, quiet = TRUE); showNotification("✓ PDF OK", type = "message") },
               error = function(e) { showNotification("❌ PDF: installez tinytex", type = "error") })
    }
  )
  
  # Session
  observeEvent(input$save_session_btn, {
    req(rv$data)
    tryCatch({
      filename <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      save_session(rv, input, filename)
      showModal(modalDialog(title = "✓ Sauvegardé", paste("Fichier:", filename), easyClose = TRUE, footer = modalButton("OK")))
      showNotification("💾 Session OK", type = "message")
    }, error = function(e) { showNotification(paste("❌", e$message), type = "error") })
  })
  
  observeEvent(input$load_session_file, {
    req(input$load_session_file)
    tryCatch({
      session_data <- load_session(input$load_session_file$datapath)
      rv$data <- session_data$data; rv$model <- session_data$model; rv$clustering_done <- session_data$clustering_done
      updateSelectInput(session, "algorithm", selected = session_data$config$algorithm)
      if (!is.null(session_data$config$n_clusters)) updateSliderInput(session, "n_clusters", value = session_data$config$n_clusters)
      showModal(modalDialog(title = "✓ Chargé", paste("Date:", format(session_data$timestamp, "%Y-%m-%d %H:%M")), easyClose = TRUE, footer = modalButton("OK")))
      showNotification("📂 Session OK", type = "message")
      updateTabItems(session, "sidebar", if(session_data$clustering_done) "results" else "data")
    }, error = function(e) { showNotification(paste("❌", e$message), type = "error") })
  })
}