
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import gt
app_ui <- function(request) {
  options(scipen = '999')
  
  tagList(
    mobile_golem_add_external_resources(),
    
    dashboardPage(
      dashboardHeader (title = "SAINT"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(
            text="Main",
            tabName="main"),
          menuItem(
            text="Raw data",
            tabName="raw_data"),
          menuItem(
            text = 'About',
            tabName = 'about'),
          fluidRow(
            column(12,
                   actionButton('action', 'Refresh data'))),
            
          fluidRow(column(12,
                   uiOutput('ts_ui'))
          )
        )),
      dashboardBody(
        # tags$head(
        #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        # ),
        tabItems(
          tabItem(
            tabName="main",
            # navbarPage(title = '',
            #            collapsible = TRUE,
            #            tabPanel(title = "Overview",
            fluidPage(
              fluidRow(
                column(4,
                       h3('Most recent submissions'),
                       helpText('Blue = already submitted today. Yellow = not yet submitted today. Red = never submitted'),
                       gt_output('dt_missing')),
                column(8,
                       h3('Symptoms table'),
                       helpText('The below table shows the number of consecutive days a patient has had a given symptom, as of the most recent observation. Click on the symptom to order by number of days.'),
                       h5('Click a PIN to get more details below on that patient'),
                       DT::dataTableOutput('dt_symptoms'),
                       h3('Invermectin symptoms table'),
                       helpText('The below table shows the number of consecutive days a patient has had a given symptom, as of the most recent observation. Click on the symptom to order by number of days.'),
                       DT::dataTableOutput('dt_ivermectin'))
              ),
              fluidRow(
                uiOutput('participant_ui')
              )
            )
            # ),
            # navbarMenu("Data",
            #            tabPanel("Raw data"),
            #            tabPanel("Meta-data"))
            
            # )
          ),
          tabItem(
            tabName = 'raw_data',
            fluidPage(
              h1('Raw data'),
              helpText('The below table is the data as it appears on the ODK server'),
              DT::dataTableOutput('dt_raw')
            )
          ),
          tabItem(
            tabName = 'about',
            fluidPage(
              fluidRow(
                div(img(src='www/logo_clear.png', align = "center"), style="text-align: center;"),
                h4('Built in partnership with ',
                   a(href = 'http://databrew.cc',
                     target='_blank', 'Databrew'),
                   align = 'center'),
                p('Empowering research and analysis through collaborative data science.', align = 'center'),
                div(a(actionButton(inputId = "email", label = "info@databrew.cc",
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:info@databrew.cc",
                      align = 'center')),
                style = 'text-align:center;'
              )
            )
          )
        )
      )
    )
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
mobile_golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'saint')
  )
  
  
  # share <- list(
  #   title = "Databrew's COVID-19 Data Explorer",
  #   url = "https://datacat.cc/covid19/",
  #   image = "http://www.databrew.cc/images/blog/covid2.png",
  #   description = "Comparing epidemic curves across countries",
  #   twitter_user = "data_brew"
  # )
  
  tags$head(
    
    # # Facebook OpenGraph tags
    # tags$meta(property = "og:title", content = share$title),
    # tags$meta(property = "og:type", content = "website"),
    # tags$meta(property = "og:url", content = share$url),
    # tags$meta(property = "og:image", content = share$image),
    # tags$meta(property = "og:description", content = share$description),
    # 
    # # Twitter summary cards
    # tags$meta(name = "twitter:card", content = "summary"),
    # tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    # tags$meta(name = "twitter:title", content = share$title),
    # tags$meta(name = "twitter:description", content = share$description),
    # tags$meta(name = "twitter:image", content = share$image),
    # 
    # # golem::activate_js(),
    # # golem::favicon(),
    # # Add here all the external resources
    # # Google analytics script
    # includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    # includeScript(system.file('app/www/script.js', package = 'covid19')),
    # includeScript(system.file('app/www/dtselect.js', package = 'saint')),
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
#' @import yaml
#' @import lubridate
#' @import gt
app_server <- function(input, output, session) {

  
  # Get into reactive object
  data_list <- reactiveValues(data = data.frame(),
                              ts = as.character(Sys.time()),
                              participant = NA)
    
  
  # Observe the action button (or app start) to load data
  observeEvent(input$action, {
    # Get data
    df <- get_data(data_file = paste0(getwd(), '/data.csv'),
             user = yaml::read_yaml('credentials/credentials.yaml')$user,
             password = yaml::read_yaml('credentials/credentials.yaml')$password)
    message('Got new df')
    data_list$data <- df
    message('Stuck new df in reactive list')
    data_list$ts <- as.character(Sys.time())
    message('Got time stamp')
    message('data_list$data looks like:')
    print(head(data_list$data))
    # x <- data_list$data
    # save(x, file = '/tmp/tmp.RData')
  }, ignoreNULL = FALSE)
  
  output$dt_raw <- DT::renderDataTable({
    out <- data_list$data
    out
  },
  options = list(scrollX = TRUE))
  
  output$ts_ui <- renderUI({
    out <- data_list$ts
    fluidPage(
      helpText('Updated at:'),
      helpText(out)
    )
  })
  
  output$dt_missing <- render_gt({
    pd <- data_list$data
    # save(pd, file = '/tmp/tmp.RData')
    ok <- FALSE
    if(!is.null(pd)){
      if(nrow(pd) > 0){
        if('end_time' %in% names(pd)){
          ok <- T
        }
      }
    }
    if(ok){
      out <- pd %>%
        arrange(end_time) %>%
        group_by(pin = as.character(pin)) %>%
        summarise(x = dplyr::last(end_time)) %>%
        filter(!is.na(pin)) %>%
        ungroup %>%
        mutate(x = as.POSIXct(x, tz = 'Europe/Paris'))
      left <- tibble(pin = as.character(1:24))
      out <- left_join(left, out)
      out <- out %>%
        mutate(`Hours ago` = round(interval(x, as.POSIXct(Sys.time(), tz = 'Europe/Paris')) / hours(1), digits = 1)) %>%
        mutate(#x = ifelse(is.na(x), '(never)', x),
               `Hours ago` = ifelse(is.na(`Hours ago`), ' ', `Hours ago`)) %>%
        # mutate(ja = NA) %>%
        mutate(x = as.character(x)) %>%
        dplyr::rename(`Last form` = x) 
      
      # Define formatter functions
      hide_na <- function(.x){
        out <- as.POSIXct(.x)
        out <- as.character(out)
        out <- ifelse(is.na(out), '(never)', out)
        out
      }
      
      gt(out) %>%
        # Color rows
        tab_style(
          style = cell_fill(color = "#F7EFB2"),
          locations = cells_body(
            # rows = `Hours ago` > 24
            rows = as.Date(`Last form`) != Sys.Date()
            )
        ) %>%
        tab_style(
          style = cell_fill(color = "#66CCFF"),
          locations = cells_body(
            # rows = `Hours ago` > 24
            rows = as.Date(`Last form`) == Sys.Date()
          )
        ) %>%
        tab_style(
          style = cell_fill(color = "#FF6347"),
          locations = cells_body(
            # rows = `Hours ago` > 24
            rows = is.na(`Last form`)
          )
        ) %>%
        # Bolden pin
        tab_style(
          style = cell_text(size = px(15), weight = "bold", font = "arial"),
          locations = cells_body(vars(pin))
        ) %>%
        tab_style(
          style = cell_text(
            size = px(12),
            # color = "#999",
            font = "arial",
            indent = px(65)
          ),
          locations = cells_body(vars(`Last form`))
        ) %>%
        # Hide NA
        fmt("Last form", fns = hide_na) %>%
        # Align columns
        cols_align(align = 'right',
                   columns = vars(`Last form`)) %>%
        cols_align(align = 'right',
                   columns = vars(`Hours ago`))
    } else {
      NULL
    }
  }#, rownames= FALSE
  )
  
  data_symptoms <- reactive({
    pd <- data_list$data
    pd <- pd %>%
      mutate(date = as.Date(end_time)) %>%
      arrange(pin,
              date) %>%
      filter(!is.na(pin)) %>%
      # Keep only the most recent one for each date
      mutate(dummy = 1) %>%
      group_by(pin, date) %>%
      mutate(cs = cumsum(dummy)) %>%
      filter(dummy == max(dummy)) %>%
      ungroup %>%
      dplyr::select(-dummy)
    # Get the missing dates too
    left <- expand.grid(date = seq(min(pd$date),
                                   max(pd$date),
                                   by = 1),
                        pin = sort(unique(pd$pin)))
    pd <- left_join(left, pd)
    pd <- pd %>% arrange(pin, date)
    
    pd <- pd %>%
      group_by(pin) %>%
      mutate(max_date = max(date[!is.na(instanceID)])) %>%
      ungroup %>%
      # Don't keep anything after max date
      filter(date <= max_date) %>%
      group_by(pin) %>%
      summarise(max_date = max(date[!is.na(instanceID)]),
                congestion_last = dplyr::last(congestion_si_no),
                congestion_days = dplyr::last(sequence(rle(as.character(congestion_si_no))$lengths)),
                congestion_days = ifelse(congestion_last == 'No', 0, congestion_days),
                
                diarrea_last = dplyr::last(diarrea_si_no),
                diarrea_days = dplyr::last(sequence(rle(as.character(diarrea_si_no))$lengths)),
                diarrea_days = ifelse(diarrea_last == 'No', 0, diarrea_days),
                
                fatiga_last = dplyr::last(fatiga_si_no),
                fatiga_days = dplyr::last(sequence(rle(as.character(fatiga_si_no))$lengths)),
                fatiga_days = ifelse(fatiga_last == 'No', 0, fatiga_days),
                
                malestar_last = dplyr::last(malestar_si_no),
                malestar_days = dplyr::last(sequence(rle(as.character(malestar_si_no))$lengths)),
                malestar_days = ifelse(malestar_last == 'No', 0, malestar_days),
                
                medicacion_last = dplyr::last(medicacion_si_no),
                medicacion_days = dplyr::last(sequence(rle(as.character(medicacion_si_no))$lengths)),
                medicacion_days = ifelse(medicacion_last == 'No', 0, medicacion_days),
                
                temp_last = dplyr::last(temp_si_no),
                temp_days = dplyr::last(sequence(rle(as.character(temp_si_no))$lengths)),
                temp_days = ifelse(temp_last == 'No', 0, temp_days),
                
                tos_last = dplyr::last(tos_si_no),
                tos_days = dplyr::last(sequence(rle(as.character(tos_si_no))$lengths)),
                tos_days = ifelse(tos_last == 'No', 0, tos_days),
                
                vomitos_last = dplyr::last(vomitos_si_no),
                vomitos_days = dplyr::last(sequence(rle(as.character(vomitos_si_no))$lengths)),
                vomitos_days = ifelse(vomitos_last == 'No', 0, vomitos_days),
      ) %>%
      ungroup %>%
      # Remove column names with "last"
      dplyr::select(-contains('_last')) %>%
      # Rename column
      dplyr::rename(`Last observation` = max_date)
    # Remove the "days" from column names
    names(pd) <- gsub('_days', '', names(pd))
    # save(pd, file = '/tmp/tmp2.RData')
    
    pd
  })
  
  data_ivermectin <- reactive({
    pd <- data_list$data
    pd <- pd %>%
      mutate(date = as.Date(end_time)) %>%
      arrange(pin,
              date) %>%
      filter(!is.na(pin)) %>%
      # Keep only the most recent one for each date
      mutate(dummy = 1) %>%
      group_by(pin, date) %>%
      mutate(cs = cumsum(dummy)) %>%
      filter(dummy == max(dummy)) %>%
      ungroup %>%
      dplyr::select(-dummy)
    # Get the missing dates too
    left <- expand.grid(date = seq(min(pd$date),
                                   max(pd$date),
                                   by = 1),
                        pin = sort(unique(pd$pin)))
    pd <- left_join(left, pd)
    pd <- pd %>% arrange(pin, date)
    
    # Generate new variables
    sintomas <- unique(c(pd$sintomas_1, pd$sintomas_2, pd$sintomas_3))
    sintomas <- sintomas[!is.na(sintomas)]
    sintomas <- paste0(sort(unique(sintomas)), collapse = ' ')
    sintomas <- sort(unique(unlist(strsplit(sintomas, ' '))))
    
    for(j in 1:length(sintomas)){
      this_sintoma <- sintomas[j]
      var_name <- paste0(this_sintoma, '_iver')
      pd[,var_name] <- NA
      for(i in 1:nrow(pd)){
        has_sintoma <- 
          this_sintoma %in% pd[i,'sintomas_1'] |
          this_sintoma %in% pd[i,'sintomas_2'] |
          this_sintoma %in% pd[i,'sintomas_3'] 
        pd[i,var_name] <- has_sintoma
      }
    }
    
    
    
    pd <- pd %>%
      group_by(pin) %>%
      mutate(max_date = max(date[!is.na(instanceID)])) %>%
      ungroup %>%
      # Don't keep anything after max date
      filter(date <= max_date) %>%
      group_by(pin) %>%
      summarise(max_date = max(date[!is.na(instanceID)]),
                colores_formas_anormales_iver_last = dplyr::last(colores_formas_anormales_iver),
                colores_formas_anormales_iver_days = dplyr::last(sequence(rle(as.character(colores_formas_anormales_iver))$lengths)),
                colores_formas_anormales_iver_days = ifelse(!colores_formas_anormales_iver_last, 0, colores_formas_anormales_iver_days),
                
                
                Confusion_iver_last = dplyr::last(Confusion_iver),
                Confusion_iver_days = dplyr::last(sequence(rle(as.character(Confusion_iver))$lengths)),
                Confusion_iver_days = ifelse(!Confusion_iver_last, 0, Confusion_iver_days),
                
                dificultad_para_enfocar_objetos_iver_last = dplyr::last(dificultad_para_enfocar_objetos_iver),
                dificultad_para_enfocar_objetos_iver_days = dplyr::last(sequence(rle(as.character(dificultad_para_enfocar_objetos_iver))$lengths)),
                dificultad_para_enfocar_objetos_iver_days = ifelse(!dificultad_para_enfocar_objetos_iver_last, 0, dificultad_para_enfocar_objetos_iver_days),
                
                Mareos_iver_last = dplyr::last(Mareos_iver),
                Mareos_iver_days = dplyr::last(sequence(rle(as.character(Mareos_iver))$lengths)),
                Mareos_iver_days = ifelse(!Mareos_iver_last, 0, Mareos_iver_days),
                
                prurito_iver_last = dplyr::last(prurito_iver),
                prurito_iver_days = dplyr::last(sequence(rle(as.character(prurito_iver))$lengths)),
                prurito_iver_days = ifelse(!prurito_iver_last, 0, prurito_iver_days),
                
                puntos_ciegos_iver_last = dplyr::last(puntos_ciegos_iver),
                puntos_ciegos_iver_days = dplyr::last(sequence(rle(as.character(puntos_ciegos_iver))$lengths)),
                puntos_ciegos_iver_days = ifelse(!puntos_ciegos_iver_last, 0, puntos_ciegos_iver_days),
                
                puntos_flotantes_iver_last = dplyr::last(puntos_flotantes_iver),
                puntos_flotantes_iver_days = dplyr::last(sequence(rle(as.character(puntos_flotantes_iver))$lengths)),
                puntos_flotantes_iver_days = ifelse(!puntos_flotantes_iver_last, 0, puntos_flotantes_iver_days),
                
                sarpullido_iver_last = dplyr::last(sarpullido_iver),
                sarpullido_iver_days = dplyr::last(sequence(rle(as.character(sarpullido_iver))$lengths)),
                sarpullido_iver_days = ifelse(!sarpullido_iver_last, 0, sarpullido_iver_days),
                
                Somnolencia_iver_last = dplyr::last(Somnolencia_iver),
                Somnolencia_iver_days = dplyr::last(sequence(rle(as.character(Somnolencia_iver))$lengths)),
                Somnolencia_iver_days = ifelse(!Somnolencia_iver_last, 0, Somnolencia_iver_days),
                
                Temblores_iver_last = dplyr::last(Temblores_iver),
                Temblores_iver_days = dplyr::last(sequence(rle(as.character(Temblores_iver))$lengths)),
                Temblores_iver_days = ifelse(!Temblores_iver_last, 0, Temblores_iver_days),
                
                Vertigo_iver_last = dplyr::last(Vertigo_iver),
                Vertigo_iver_days = dplyr::last(sequence(rle(as.character(Vertigo_iver))$lengths)),
                Vertigo_iver_days = ifelse(!Vertigo_iver_last, 0, Vertigo_iver_days),
                
                vision_borrosa_iver_last = dplyr::last(vision_borrosa_iver),
                vision_borrosa_iver_days = dplyr::last(sequence(rle(as.character(vision_borrosa_iver))$lengths)),
                vision_borrosa_iver_days = ifelse(!vision_borrosa_iver_last, 0, vision_borrosa_iver_days),
                
                vision_de_tunel_iver_last = dplyr::last(vision_de_tunel_iver),
                vision_de_tunel_iver_days = dplyr::last(sequence(rle(as.character(vision_de_tunel_iver))$lengths)),
                vision_de_tunel_iver_days = ifelse(!vision_de_tunel_iver_last, 0, vision_de_tunel_iver_days)
      ) %>%
      ungroup %>%
      # Remove column names with "last"
      dplyr::select(-contains('_last')) %>%
      # Rename column
      dplyr::rename(`Last observation` = max_date)
    # Remove the "days" from column names
    names(pd) <- gsub('_iver', '', names(pd))
    names(pd) <- gsub('_days', '', names(pd))
    
    # save(pd, file = '/tmp/tmp2.RData')
    names(pd) <- gsub('_', ' ', names(pd))
    names(pd) <- tolower(names(pd))
    pd
  })
  
  
  output$dt_symptoms <- DT::renderDataTable({
    pd <- data_symptoms()
    pd
  },
  selection = 'single',
  rownames= FALSE,
  options = list(scrollX = TRUE))
  
  observeEvent(input$dt_symptoms_rows_selected,{
    row = input$dt_symptoms_rows_selected
    # print(row)
    pd <- data_symptoms()
    the_participant <- pd$pin[row]
    data_list$participant <- the_participant
  })
  
  observeEvent(input$dt_ivermectin_rows_selected,{
    row = input$dt_ivermectin_rows_selected
    # print(row)
    pd <- data_ivermectin()
    the_participant <- pd$pin[row]
    data_list$participant <- the_participant
  })
  
  output$dt_ivermectin <- DT::renderDataTable({
    pd <- data_ivermectin()
    pd
  },
  selection = 'single',
  rownames= FALSE,
  options = list(scrollX = TRUE))
  
  output$participant_ui <- renderUI({
    pin <- data_list$participant
    if(is.na(pin)){
      fluidPage(h4('Click a row in the table above to view more details.'))
    } else {
      the_pin <- pin
      full_data <- data_list$data
      temp_data <- 
        full_data <- 
        full_data %>%
        filter(pin == the_pin)
      temp_data <- temp_data %>%
        dplyr::select(date = start_time, temp) %>%
        mutate(date  = as.Date(date))
      
      full_data <- full_data  %>%
        dplyr::select(date = start_time, contains('si_no')) %>%
        mutate(date  = as.Date(date))
      names(full_data) <- gsub('_si_no', '', names(full_data))
      full_data <- full_data %>%
        tidyr::gather(key, value, congestion:vomitos)
      left <- expand.grid(date = seq(min(full_data$date), max(full_data$date), 1),
                          key = sort(unique(full_data$key)))
      joined <- left_join(left, full_data)
      output$plot_grid <- renderPlot({
        ggplot(data = joined,
               aes(x = date,
                   y = key,
                   fill = value)) +
          geom_tile(color = 'black', size = 0.1) +
          scale_fill_manual(name = '',
                            values = c('lightblue', 'darkorange', 'white')) +
          theme_saint() +
          labs(x = 'Date',
               y = 'Symptom',
               title = paste0('Symptoms over time for participant ', data_list$participant))
      })
      
      output$plot_temperature <- 
        renderPlot({
          ok <- FALSE
          if(!is.null(temp_data)){
            temp_range <- range(temp_data$temp, na.rm = TRUE)
            print(temp_range)
            if(!all(is.na(temp_range))){
              if(!all(is.infinite(temp_range))){
                ok <- TRUE
              }
            }
          }
          if(!ok){
            ggplot() +
              theme_saint() +
              labs(title = paste0('No temperature data for participant ', data_list$participant))
          } else {
            lower <- floor(temp_range)
            upper <- ceiling(temp_range)
            temp_seq <- lower:upper
            
            ggplot(data = temp_data,
                   aes(x = date,
                       y = temp)) +
              geom_hline(yintercept = temp_seq, alpha = 0.3) +
              geom_point(color = 'red', size = 3) +
              geom_line() +
              theme_saint() +
              labs(x = 'Date',
                   y = 'Celcius',
                   title = paste0('Temperature over time for participant ', data_list$participant))
          }

        })
      
      # pd <- data_symptoms()
      # pd <- pd %>% filter(pin == the_pin)
      fluidPage(
        column(6,
               plotOutput('plot_grid')),
        column(6,
               plotOutput('plot_temperature'))
      )
    }
  })

  output$plot_forms <- renderPlot({
    pd <- data_list$data
    pd <- pd %>%
      group_by(date = as.Date(end_time),
               pin) %>%
      tally %>%
      ungroup %>%
      filter(!is.na(pin),
             !is.na(date))
    cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))(length(unique(pd$pin)))
    
    ggplot(data = pd,
           aes(x = date,
               y = n,
               fill = factor(pin))) +
      geom_bar(stat = 'identity',
               color = 'black',
               size = 0.2) +
      scale_fill_manual(name = 'ID',
                        values = cols) +
      labs(x = 'Date',
           y = 'Forms filled out',
           title = 'Forms filled out by date') #+
      # guides(fill = guide_legend(reverse = TRUE))

  })
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}