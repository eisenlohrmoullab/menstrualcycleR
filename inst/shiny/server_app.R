library(shiny)
library(menstrualcycleR)
library(dplyr)
library(rlang)
library(ggplot2)
library(shinyjs)
library(cpass)
library(purrr)




server <- function(input, output, session) {
  user_data <- reactiveVal(NULL)
  
  observeEvent(input$load_data, {
    req(input$file)
    data <- read.csv(input$file$datapath)
    user_data(data)
    
    updateSelectInput(session, "id_col", choices = names(data))
    updateSelectInput(session, "date_col", choices = names(data))
    updateSelectInput(session, "menses_col", choices = names(data))
    updateSelectInput(session, "ovtoday_col", choices = names(data))
    updateSelectInput(session, "symptom_col_plot", choices = names(data))
  })
  
  observeEvent(input$id_col, {
    req(user_data(), input$id_col)
    updateSelectInput(session, "selected_id", choices = unique(user_data()[[input$id_col]]))
  })
  
  output$data_preview <- renderTable({ req(user_data()); head(user_data()) })
  
  processed_data <- reactiveVal(NULL)
  
  observeEvent(input$process_data, {
    req(user_data(), input$id_col, input$date_col, input$menses_col, input$ovtoday_col, input$lower_bound, input$upper_bound)
    
    data <- user_data()
    id_col <- sym(input$id_col)
    date_col <- sym(input$date_col)
    menses_col <- sym(input$menses_col)
    ovtoday_col <- sym(input$ovtoday_col)
    
    if (!inherits(data[[input$date_col]], "Date")) {
      data[[input$date_col]] <- lubridate::ymd(data[[input$date_col]])
    }
    
    processed <- data %>%
      menstrualcycleR::calculate_mcyclength(
        id = !!id_col,
        daterated = !!date_col,
        menses = !!menses_col,
        ovtoday = !!ovtoday_col
      ) %>%
      menstrualcycleR::calculate_cycletime(
        id = !!id_col,
        daterated = !!date_col,
        menses = !!menses_col,
        ovtoday = !!ovtoday_col,
        lower_cyclength_bound = as.numeric(input$lower_bound),
        upper_cyclength_bound = as.numeric(input$upper_bound)
      )
    
    processed_data(processed)
  })
  

  # === CYCLE DATA CHECK TAB ===
  
  # Update available symptom checkboxes when processed_data is updated
  observeEvent(processed_data(), {
    symptom_choices <- setdiff(
      names(processed_data()),
      c(
        "id",
        "cyclenum",
        "menses",
        "scaled_cycleday",
        "scaled_cycleday_impute",
        "scaled_cycleday_ov",
        "scaled_cycleday_imp_ov",
        "ovtoday",
        "m2mcount",
        "ovtoday_impute",
        "daterated",
        "cyclenum",
        "mcyclength",
        "cycle_incomplete"
      )
    )
    updateCheckboxGroupInput(session, "cyclecheck_symptoms", choices = symptom_choices)
  })
  
  # Reactive to store results of cycledata_check
  cyclecheck_result <- reactiveVal(NULL)
  
  # Run cycledata_check when button clicked
  observeEvent(input$run_cyclecheck, {
    req(processed_data(), input$cyclecheck_symptoms)
    
    result <- menstrualcycleR::cycledata_check(
      data = processed_data(),
      symptom_columns = input$cyclecheck_symptoms
    )
    
    cyclecheck_result(result)
  })
  
  # Render summary tables
  output$cyclecheck_by_id <- renderTable({
    req(cyclecheck_result())
    cyclecheck_result()$by_id
  })
  
  output$cyclecheck_overall <- renderTable({
    req(cyclecheck_result())
    cyclecheck_result()$overall
  })
  
  # Download handlers for tables
  output$download_cyclecheck_by_id <- downloadHandler(
    filename = function() paste0("cyclecheck_by_id_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(cyclecheck_result()$by_id, file, row.names = FALSE)
    }
  )
  
  output$download_cyclecheck_overall <- downloadHandler(
    filename = function() paste0("cyclecheck_overall_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(cyclecheck_result()$overall, file, row.names = FALSE)
    }
  )
  
  # Dynamic rendering and downloading of plots
  output$cyclecheck_plots <- renderUI({
    req(cyclecheck_result())
    plots <- cyclecheck_result()$data_symptom_plots
    
    plot_outputs <- lapply(names(plots), function(symptom_name) {
      plot_id <- paste0("cycleplot_", symptom_name)
      download_id <- paste0("download_", plot_id)
      
      # Assign outputs dynamically
      local({
        s <- symptom_name
        output[[plot_id]] <- renderPlot({ plots[[s]] })
        
        output[[download_id]] <- downloadHandler(
          filename = function() paste0("cyclecheck_plot_", s, "_", Sys.Date(), ".png"),
          content = function(file) {
            ggsave(file, plot = plots[[s]], device = "png", width = 8, height = 7, dpi = 300)
          }
        )
      })
      
      tagList(
        tags$h5(symptom_name),
        plotOutput(plot_id),
        downloadButton(download_id, "Download Plot"),
        tags$hr()
      )
    })
    
    do.call(tagList, plot_outputs)
  })
  
  
  
  ovulation_summary <- reactive({ req(processed_data()); menstrualcycleR::summary_ovulation(processed_data()) })
  output$ovulation_summary <- renderTable({ req(ovulation_summary()); ovulation_summary()$ovstatus_total })
  output$ovulation_summary_id <- renderTable({ req(ovulation_summary()); ovulation_summary()$ovstatus_id })
  
  cycle_plot_data <- reactiveVal(NULL)
  
  plot_inputs <- reactive({
    list(
      symptom = input$symptom_col_plot,
      centering = input$plot_centering,
      include_impute = input$plot_impute,
      y_scale = input$plot_y_scale,
      rollingavg = input$rollingavg
    )
  })
  
  observeEvent(input$update_plot, {
    req(processed_data(),plot_inputs())
    
    plot_result <- menstrualcycleR::cycle_plot(
      data = processed_data(),
      symptom = input$symptom_col_plot,
      centering = input$plot_centering,
      include_impute = isTRUE(input$plot_impute),
      y_scale = input$plot_y_scale,
      rollingavg = as.numeric(input$rollingavg)
    )
    
    cycle_plot_data(plot_result$plot)
  })
  
  output$cycle_plot <- renderPlot({ req(cycle_plot_data()); cycle_plot_data() })
  output$download_plot <- downloadHandler(
    filename = function() { paste("cycle_plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = cycle_plot_data(), device = "png", width = 8, height = 6, dpi = 300)
    }
  )
  
  observeEvent(processed_data(), {
    updateSelectInput(session, "selected_id", choices = unique(processed_data()$id))
    symptom_choices <- setdiff(
      names(processed_data()),
      c(
        "id",
        "cyclenum",
        "menses",
        "scaled_cycleday",
        "scaled_cycleday_impute",
        "scaled_cycleday_ov",
        "scaled_cycleday_imp_ov",
        "m2mcount",
        "mcyclength",
        "ovtoday",
        "ovtoday_impute",
        "cycle_incomplete", 
        "daterated"
      )
    )
    updateCheckboxGroupInput(session, "selected_symptoms", choices = symptom_choices)
  })
  
  observeEvent(input$run_individual_plot, {
    req(processed_data(), input$selected_id, input$selected_symptoms)
    
    results <- menstrualcycleR::cycle_plot_individual(
      data = processed_data(),
      id = input$selected_id,
      symptoms = input$selected_symptoms,
      centering = input$centering_mode,
      y_scale = input$y_scale_mode,
      include_impute = isTRUE(input$include_impute_toggle),
      rollingavg = as.numeric(input$rollingavg_input)
    )
    
    output$individual_plot_output <- renderUI({
      output_list <- list()
      
      for (symptom in names(results)) {
        for (cycle_name in names(results[[symptom]])) {
          plot_id <- paste0("plot_", symptom, "_", cycle_name)
          summary_id <- paste0("summary_", symptom, "_", cycle_name)
          toggle_button_id <- paste0("toggle_", summary_id)
          download_summary_id <- paste0("download_", summary_id)
          download_plot_id <- paste0("download_plot_", symptom, "_", cycle_name)
          
          local({
            s <- symptom
            c <- cycle_name
            p_id <- plot_id
            s_id <- summary_id
            d_id <- download_summary_id
            dp_id <- download_plot_id
            toggle_id <- paste0(s_id, "_container")
            
            output[[p_id]] <- renderPlot({ results[[s]][[c]]$plot })
            output[[s_id]] <- renderTable({ results[[s]][[c]]$summary })
            
            output[[d_id]] <- downloadHandler(
              filename = function() paste0("summary_", s, "_", c, ".csv"),
              content = function(file) {
                write.csv(results[[s]][[c]]$summary, file, row.names = FALSE)
              }
            )
            
            output[[dp_id]] <- downloadHandler(
              filename = function() paste0("plot_", s, "_", c, ".png"),
              content = function(file) {
                ggsave(file, plot = results[[s]][[c]]$plot, device = "png", width = 8, height = 6, dpi = 300)
              }
            )
            
            observeEvent(input[[toggle_button_id]], {
              shinyjs::toggle(toggle_id)
            })
          })
          
          output_list[[length(output_list) + 1]] <- tagList(
            tags$h4(paste("Symptom:", symptom, "|", cycle_name)),
            plotOutput(plot_id),
            downloadButton(download_plot_id, "Download Plot"),
            actionButton(toggle_button_id, "View Summary"),
            hidden(div(id = paste0(summary_id, "_container"), tableOutput(summary_id))),
            downloadButton(download_summary_id, "Download Summary"),
            tags$hr()
          )
        }
      }
      
      do.call(tagList, output_list)
    })
  })
  
  
  # CPASS Section
  # ---- CPASS Helper Function ----
  add_cpass_count <- function(data, days_before = 8, days_after = 10) {
    # Step 1: Identify each menses start (first day per episode)
    menses_rows <- data %>%
      dplyr::arrange(id, daterated) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(menses_index = cumsum(menses == 1)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(menses == 1) %>%
      dplyr::group_by(id, menses_index) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(id, menses_index, menses_date = daterated)
    
    # Step 2: For each menses window, label +/- days with its index
    cpass_labeled <- purrr::map_dfr(seq_len(nrow(menses_rows)), function(i) {
      m_row <- menses_rows[i, ]
      data %>%
        dplyr::filter(
          id == m_row$id,
          daterated >= m_row$menses_date - days_before,
          daterated <= m_row$menses_date + days_after
        ) %>%
        dplyr::mutate(cpass_count = m_row$menses_index)
    })
    
    # Step 3: Join back to full data
    result <- data %>%
      dplyr::left_join(
        cpass_labeled %>% dplyr::select(id, daterated, cpass_count),
        by = c("id", "daterated")
      )
    
    return(result)
  }
  
  
  
  

  cpass_process <- function(dataframe, symptom_map, id_number) {
    dataframe <- dataframe %>%
      dplyr::mutate(subject = id, cycle = cpass_count)

    cycleCount <- function(x) {
      inds <- which(x == 1)
      if (!length(inds)) return(0)
      num <- lapply(inds, function(i) {
        num <- seq_along(x) - i
        num[num >= 0] <- num[num >= 0] + 1
        num[num < -7 | num > 10] <- NA
        num
      })
      do.call(coalesce, num)
    }

    dataframe <- dataframe %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(day = cycleCount(menses)) %>%
      dplyr::ungroup()

    df1_long <- dataframe %>%
      tidyr::pivot_longer(
        cols = all_of(names(symptom_map)),
        names_to = "symptom",
        values_to = "drsp_score"
      ) %>%
      dplyr::mutate(item = as.numeric(symptom_map[symptom])) %>%
      dplyr::filter(!is.na(cycle), !is.na(item))

    input1 <- cpass::as_cpass_data(df1_long, sep_event = "menses")

    # Main diagnostic plot
    plots <- list()
    plots[["Diagnosis"]] <- cpass::plot_subject_dx(input1 %>% dplyr::filter(subject == id_number))

    # Add cycle-specific observation plots
    unique_cycles <- input1 %>% dplyr::filter(subject == id_number) %>% dplyr::pull(cycle) %>% unique() %>% na.omit()
    for (cyc in unique_cycles) {
      plots[[paste0("Cycle_", cyc)]] <- cpass::plot_subject_obs(input1 %>% dplyr::filter(subject == id_number, cycle == cyc))
    }

    return(plots)
  }

  # ---- CPASS UI Rendering for Symptom Map ----
  observeEvent(processed_data(), {
    req(processed_data())

    updateSelectInput(session, "cpass_id_select", choices = unique(processed_data()$id))

    symptom_candidates <- setdiff(
      names(processed_data()),
      c(
        input$id_col,
        input$date_col,
        input$menses_col,
        input$ovtoday_col,
        "cyclenum",
        "cpass_count",
        "scaled_cycleday",
        "scaled_cycleday_impute",
        "scaled_cycleday_ov",
        "scaled_cycleday_imp_ov",
        "id",
        "daterated",
        "m2mcount",
        "mcyclength",
        "cycle_incomplete",
        "ovtoday_impute"
      )
    )

    output$cpass_mapping_table <- renderUI({
      tagList(
        lapply(symptom_candidates, function(symptom) {
          fluidRow(
            column(6, tags$label(symptom)),
            column(6, selectInput(
              inputId = paste0("map_", symptom),
              label = NULL,
              choices = c("", cpass_items),
              selected = NULL,
              width = "100%"
            ))
          )
        })
      )
    })
  })

  # ---- Run CPASS Analysis ----
  observeEvent(input$run_cpass, {
    req(processed_data(), input$cpass_id_select)

    isolate({
      # Build symptom_map from dropdown selections
      symptom_inputs <- names(input)[grepl("^map_", names(input))]
      symptom_map <- setNames(
        lapply(symptom_inputs, function(x) as.numeric(input[[x]])),
        gsub("^map_", "", symptom_inputs)
      )
      symptom_map <- symptom_map[!is.na(unlist(symptom_map))]

      # Run CPASS and store plots
      processed_data() = add_cpass_count(processed_data())
      plots <- tryCatch({
        cpass_process(
          dataframe = processed_data(),
          symptom_map = unlist(symptom_map),
          id_number = as.numeric(input$cpass_id_select)
        )
      }, error = function(e) {
        showNotification(paste("CPASS Error:", e$message), type = "error")
        return(NULL)
      })

      # Display CPASS plots dynamically with download buttons
      output$cpass_plot_ui <- renderUI({
        req(plots)

        plot_uis <- lapply(names(plots), function(name) {
          plot_id <- paste0("cpass_plot_", name)
          download_id <- paste0("download_plot_", name)

          local({
            n <- name
            pid <- plot_id
            did <- download_id

            output[[pid]] <- renderPlot({
              plots[[n]]
            })

            output[[did]] <- downloadHandler(
              filename = function() paste0("cpass_plot_", n, "_", Sys.Date(), ".jpeg"),
              content = function(file) {
                ggsave(file, plot = plots[[n]], width = 6, height = 9, dpi = 300)
              }
            )
          })

          wellPanel(
            tags$h4(paste("CPASS Plot:", name), style = "margin-bottom: 15px;"),
            plotOutput(plot_id, height = "800px"),
            br(),
            downloadButton(download_id, "Download Plot"),
            style = "margin-bottom: 50px;"
          )
        })

        do.call(tagList, plot_uis)
      })
    })
  })


  output$download_results <- downloadHandler(
    filename = function() { paste("processed_cycle_data_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(processed_data(), file, row.names = FALSE) }
  )
}

# CPASS item names and labels
cpass_items <- setNames(
  as.character(1:24),
  c(
    "1 - Depressed, Blue",
    "2 - Hopelessness",
    "3 - Worthlessness or Guilt",
    "4 - Anxiety",
    "5 - Mood Swings",
    "6 - Rejection Sensitivity",
    "7 - Anger or Irritability",
    "8 - Interpersonal Conflict",
    "9 - Loss of Interest",
    "10 - Difficulty Concentrating",
    "11 - Lethargy",
    "12 - Change in Appetite/ Overeating",
    "13 - Food Cravings",
    "14 - Hypersomnia",
    "15 - Insomnia",
    "16 - Overwhelmed",
    "17 - Out of Control",
    "18 - Breast Tenderness",
    "19 - Swelling, Bloating",
    "20 - Headache",
    "21 - Joint, Muscle Pain", 
    "22 - Work Interference", 
    "23 - Hobby Interference", 
    "24 - Relationship Interference"
  )
)

