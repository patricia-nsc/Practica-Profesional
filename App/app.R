# ----------------------------------------------------------- #
# Librerias
# ----------------------------------------------------------- #

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(readxl)
library(plotly)    # paraq graf serie sea interactivo

# ----------------------------------------------------------- #
# Datos
# ----------------------------------------------------------- #

ruta_data <- "../Datos/Procesados/agbarometer_ARG_imputado.xlsx"

# ----------------------------------------------------------- #
# Funciones y path del logo
# ----------------------------------------------------------- #

source("../Scripts/funciones.R")

addResourcePath("logo_dir", "../Informe/Logos")

####################################################
# User Interface
####################################################

ui <- page_navbar(
  title = span(style = "font-weight: bold;", "Ag Barometer Austral  |  SIV"),
  bg = "#7D212B",
  theme = bs_theme(
    version = 5,
    primary = "#7D212B",
    success = "#73A800",
    "navbar-bg" = "#7D212B"
  ),
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #F8F9FA;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .card {
        border: none;
        border-radius: 12px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.08);
        transition: transform 0.2s ease;
      }
      .card:hover {
        transform: translateY(-2px);
      }
      .card-header {
        background-color: #7D212B !important;
        color: white !important;
        border-top-left-radius: 12px !important;
        border-top-right-radius: 12px !important;
        font-weight: 600;
        letter-spacing: 0.5px;
      }
      .btn-custom-primary {
        background-color: #73A800;   
        color: white;
        border: none;
        font-weight: 600;
        border-radius: 8px;
        padding: 10px 20px;
        transition: background-color 0.2s;
      }
      .btn-custom-primary:hover {
        background-color: #5E8700;   
        color: white;
      }
      .btn-custom-secondary {
        background-color: #F8D7DA; 
        color: #721C24;
        border: 1px solid #F5C6CB;
        font-weight: 600;
        border-radius: 8px;
        padding: 8px 15px;
        transition: background-color 0.2s;
      }
      .btn-custom-secondary:hover {
        background-color: #F1B0B7;
        color: #491217;
      }
      .welcome-modal .modal-content {
        border-radius: 16px;
        border: 2px solid #7D212B;
      }
      .dashboard-section-title {
        font-size: 1.15rem;
        font-weight: 700;
        color: #7D212B;
        letter-spacing: 0.5px;
        margin-bottom: 15px;
        padding-bottom: 6px;
        border-bottom: 2px solid #7D212B;
        display: flex;
        align-items: center;
        gap: 8px;
      }
      .minimal-card {
        background: #ffffff;
        border-radius: 12px;
        box-shadow: 0 4px 16px rgba(0,0,0,0.04);
        border: 1px solid rgba(0,0,0,0.06);
        padding: 20px;
        margin-bottom: 20px;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }
      .minimal-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 24px rgba(0,0,0,0.08);
      }
      .metric-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 12px 0;
        border-bottom: 1px solid #f4f6f8;
      }
      .metric-row:last-child {
        border-bottom: none;
        padding-bottom: 0;
      }
      .metric-row:first-child {
        padding-top: 0;
      }
      .metric-label {
        font-weight: 600;
        color: #222222;
        font-size: 0.95rem;
      }
      .metric-values {
        display: flex;
        align-items: center;
        gap: 12px;
        font-size: 0.9rem;
        color: #111111;
      }
      .pill-current {
        background: #191919;
        color: white;
        padding: 3px 10px;
        border-radius: 6px;
        font-weight: 600;
        font-size: 0.85rem;
      }
      .pill-past {
        background: #e9ecef;
        color: #212529;
        padding: 3px 10px;
        border-radius: 6px;
        font-weight: 600;
        font-size: 0.85rem;
      }
      .badge-growth-green {
        background-color: rgba(115, 168, 0, 0.18);
        color: #4A7000;
        border: 1px solid rgba(115, 168, 0, 0.5);
        padding: 4px 12px;
        border-radius: 6px;
        font-weight: 700;
        font-size: 0.88rem;
        min-width: 80px;
        text-align: center;
      }
      .badge-growth-red {
        background-color: rgba(217, 83, 79, 0.18);
        color: #C9302C;
        border: 1px solid rgba(217, 83, 79, 0.5);
        padding: 4px 12px;
        border-radius: 6px;
        font-weight: 700;
        font-size: 0.88rem;
        min-width: 80px;
        text-align: center;
      }
      .card-body img, .card-body .shiny-plot-output img {
        max-width: 100% !important;
        height: auto !important;
      }
      .navbar-logo-derecha {
        margin-left: auto;
        display: flex;
        align-items: center;
        padding-right: 15px;
      }
    "))
  ),
  
  footer = tags$script(HTML("
    $(document).ready(function() {
      var logoHtml = '<div class=\"navbar-logo-derecha\"><img src=\"logo_dir/logo_austral_ceag.png\" height=\"44px\" style=\"background: transparent;\"></div>';
      $('.navbar-nav').last().after(logoHtml);
    });
  ")),
  
  # ------------------------------------------------------------- #
  # PESTAÑA 1: carga de datos
  # ------------------------------------------------------------- #
  nav_panel(
    title = "Nuevo Registro",
    fluidRow(
      column(
        width = 4,
        div(
          class = "card mb-4",
          div(class = "card-header", "Ingresar nuevo registro"),
          div(
            class = "card-body",
            p(style = "font-size: 0.9rem; color: #666;", 
              "Ingrese los datos del nuevo relevamiento para actualizar automáticamente el sistema."),
            fluidRow(
              column(
                width = 6,
                selectInput("input_mes", "Mes:", 
                            choices = c("Enero" = 1, "Febrero" = 2,
                                        "Marzo" = 3, "Abril" = 4,
                                        "Mayo" = 5, "Junio" = 6,
                                        "Julio" = 7, "Agosto" = 8,
                                        "Septiembre" = 9, "Octubre" = 10,
                                        "Noviembre" = 11, "Diciembre" = 12))
              ),
              column(
                width = 6,
                numericInput("input_anio", "Año:", value = 2026, min = 2018, max = 2100, step = 1)
              )
            ),
            numericInput("input_agbarometer", "Ag Barometer Austral:", value = 100, min = 0, max = 300, step = 0.1),
            numericInput("input_condiciones", "Condiciones Presentes:", value = 100, min = 0, max = 300, step = 0.1),
            numericInput("input_expectativas", "Expectativas Futuras:", value = 100, min = 0, max = 300, step = 0.1),
            div(
              style = "text-align: center; margin-top: 15px;",
              actionButton("btn_agregar", "Agregar registro", icon = icon("save"), class = "btn btn-custom-primary w-100 mb-2")
            ),
            div(
              style = "text-align: center;",
              actionButton("btn_eliminar", "Eliminar último registro", icon = icon("trash-alt"), class = "btn btn-custom-secondary w-100")
            )
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "card",
          div(class = "card-header", "Vista Previa"),
          div(
            class = "card-body",
            DTOutput("tabla_datos")
          )
        )
      )
    )
  ),
  
  # ------------------------------------------------------------- #
  # PESTAÑA 2: tablero
  # ------------------------------------------------------------- #
  nav_panel(
    title = "Tablero Analítico",
    fluidRow(
      column(
        width = 12,
        div(
          style = "padding: 10px 0px 15px 0px;",
          h4(style = "color: #7D212B; font-weight: bold; letter-spacing: 0.5px;", "Síntesis de Variaciones")
        )
      )
    ),
    
    # VARIACIONES
    fluidRow(
      column(
        width = 6,
        div(
          class = "dashboard-section-title",
          "Variaciones bimestrales"
        ),
        div(
          class = "minimal-card",
          uiOutput("tarjeta_variaciones_bimestrales")
        )
      ),
      column(
        width = 6,
        div(
          class = "dashboard-section-title",
          "Variaciones interanuales"
        ),
        div(
          class = "minimal-card",
          uiOutput("tarjeta_variaciones_interanuales")
        )
      )
    ),
    
    # Evolución del Ag Barometer Austral y Eventos
    fluidRow(
      column(
        width = 12,
        div(
          style = "padding: 10px 0px 5px 0px;",
          h4(style = "color: #7D212B; font-weight: bold; letter-spacing: 0.5px;", "Evolución del Ag Barometer Austral y Eventos")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "card mb-4",
          div(class = "card-header", "Opciones de gráfico y eventos"),
          div(
            class = "card-body",
            checkboxGroupInput("series_sel", "Seleccionar Series:", 
                               choices = c("Ag Barometer Austral", "Condiciones presentes", "Expectativas futuras"),
                               selected = "Ag Barometer Austral"),
            
            conditionalPanel(
              condition = "input.series_sel.includes('Ag Barometer Austral')",
              textInput("color_ag", "Color Ag Barometer Austral:", value = "#8FB933")
            ),
            conditionalPanel(
              condition = "input.series_sel.includes('Condiciones presentes')",
              textInput("color_cond", "Color Condiciones Presentes:", value = "#C7DC99")
            ),
            conditionalPanel(
              condition = "input.series_sel.includes('Expectativas futuras')",
              textInput("color_exp", "Color Expectativas Futuras:", value = "#567E00")
            ),
            
            sliderInput("ancho_sombra", "Ancho del sombreado:", min = 1, max = 6, value = 3, step = 1),
            hr(),
            numericInput("cantidad_eventos", "Cantidad de eventos a mostrar:", value = 2, min = 0, max = 10, step = 1),
            uiOutput("contenedor_eventos_dinamicos")
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "card",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            "Gráfico dinámico",
            div(
              actionButton("btn_zoom_grafico", "", icon = icon("search-plus"), class = "btn btn-sm btn-light me-1", title = "Zoom"),
              downloadButton("btn_descargar_grafico", "", class = "btn btn-sm btn-light", title = "Descargar")
            )
          ),
          div(
            class = "card-body text-center",
            plotlyOutput("grafico_evolucion", height = "450px"),
            div(
              style = "text-align: left; margin-top: 15px; font-size: 0.85rem; color: #333333; line-height: 1.4;",
              uiOutput("referencias_eventos_UI")
            )
          )
        )
      )
    )
  )
)

####################################################
# Server 
####################################################

server <- function(input, output, session) {
  
  showModal(modalDialog(
    title = NULL,
    div(
      style = "text-align: center; padding: 10px;",
      tags$img(src = "logo_dir/logo_austral_ceag.png", 
               height = "90px", style = "margin-bottom: 20px;"),
      h3(style = "color: #7D212B; font-weight: bold; margin-bottom: 15px;", 
         "¡Bienvenido al SIV!"),
      p(style = "font-size: 1.05rem; color: #333; margin-bottom: 20px;",
        "Sistema Interactivo de Visualización (SIV) para el ", 
        tags$b("Ag Barometer Austral"), ".")
    ),
    footer = modalButton("Comenzar"),
    easyClose = TRUE,
    class = "welcome-modal"
  ))
  
  rv <- reactiveValues(
    data = {
      archivo_a_leer <- if (file.exists(ruta_data)) ruta_data else "agbarometer_ARG_imputado.xlsx"
      if (file.exists(archivo_a_leer)) {
        read_excel(archivo_a_leer) |> mutate(Bimestre = as.Date(Bimestre)) |> arrange(Bimestre)
      } else {
        data.frame(
          Bimestre = as.Date(c("2025-09-01", "2025-11-01", "2026-01-01")),
          `Condiciones presentes` = c(87, 124, 132),
          `Expectativas futuras` = c(149, 182, 175),
          `Ag Barometer Austral` = c(124, 159, 158),
          check.names = FALSE
        )
      }
    }
  )
  
  observeEvent(input$btn_agregar, {
    mes_val <- as.numeric(input$input_mes)
    anio_val <- input$input_anio
    nueva_fecha <- as.Date(sprintf("%d-%02d-01", anio_val, mes_val))
    
    if (nueva_fecha %in% rv$data$Bimestre) {
      showNotification("¡Atención! Ya existe un registro para este período.", type = "warning", duration = 4)
      return()
    }
    
    nueva_fila <- data.frame(
      Bimestre = nueva_fecha,
      `Ag Barometer Austral` = input$input_agbarometer,
      `Condiciones presentes` = input$input_condiciones,
      `Expectativas futuras` = input$input_expectativas,
      check.names = FALSE
    )
    
    rv$data <- bind_rows(rv$data, nueva_fila) %>% arrange(Bimestre)
    showNotification("¡Nueva fila agregada y vista previa actualizada con éxito!", type = "message", duration = 3)
  })
  
  observeEvent(input$btn_eliminar, {
    if (nrow(rv$data) > 3) {
      rv$data <- rv$data %>% arrange(Bimestre) %>% slice(-n())
      showNotification("Se ha eliminado el último registro ingresado.", type = "error", duration = 3)
    } else {
      showNotification("No se pueden eliminar más registros (se requiere mantener el histórico mínimo).", type = "warning", duration = 4)
    }
  })
  
  output$tabla_datos <- renderDT({
    df_mostrar <- rv$data %>% 
      arrange(Bimestre) %>% 
      tail(3) %>% 
      mutate(Bimestre = format(Bimestre, "%Y-%m"))
    
    datatable(
      df_mostrar,
      options = list(
        dom = 'tip',
        pageLength = 3,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json')
      ),
      rownames = FALSE
    )
  })
  
  formatear_fecha_amigable <- function(fecha_date) {
    meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    m_idx <- as.numeric(format(fecha_date, "%m"))
    anio_fmt <- format(fecha_date, "%y")
    paste(meses_es[m_idx], anio_fmt)
  }
  
  generar_fila_metrica <- function(nombre_variable, valor_actual, fecha_actual_str, valor_anterior, fecha_anterior_str) {
    if (is.na(valor_actual) || is.na(valor_anterior)) {
      variacion_pct <- 0
      es_aumento <- TRUE
    } else {
      variacion_pct <- ((valor_actual - valor_anterior) / valor_anterior) * 100
      es_aumento <- variacion_pct >= 0
    }
    
    clase_badge <- if (es_aumento) "badge-growth-green" else "badge-growth-red"
    signo_str <- if (es_aumento) "+" else ""
    
    div(
      class = "metric-row",
      div(
        class = "metric-label",
        nombre_variable
      ),
      div(
        class = "metric-values",
        tags$span(style = "font-size: 0.85rem; color: #111111; font-weight: 500;", fecha_actual_str),
        div(class = "pill-current", round(valor_actual, 1)),
        tags$span(style = "color: #333333; font-weight: 600;", "vs"),
        tags$span(style = "font-size: 0.85rem; color: #111111; font-weight: 500;", fecha_anterior_str),
        div(class = "pill-past", round(valor_anterior, 1)),
        div(
          class = clase_badge,
          sprintf("%s%.2f%%", signo_str, variacion_pct)
        )
      )
    )
  }
  
  output$tarjeta_variaciones_bimestrales <- renderUI({
    df <- rv$data %>% arrange(Bimestre)
    n <- nrow(df)
    
    if (n < 2) {
      return(div(style = "padding: 10px; text-align: center; color: #666; font-size: 0.9rem;", "Se requieren al menos dos registros."))
    }
    
    actual <- df[n, ]
    anterior <- df[n - 1, ]
    
    f_act <- formatear_fecha_amigable(actual$Bimestre)
    f_ant <- formatear_fecha_amigable(anterior$Bimestre)                                   
    
    tagList(generar_fila_metrica("Ag Barometer Austral", actual$`Ag Barometer Austral`, f_act, anterior$`Ag Barometer Austral`, f_ant),
                                                                                                   generar_fila_metrica("Condiciones Presentes", actual$`Condiciones presentes`, f_act, anterior$`Condiciones presentes`, f_ant),
                                                                                                   generar_fila_metrica("Expectativas Futuras", actual$`Expectativas futuras`, f_act, anterior$`Expectativas futuras`, f_ant)
    )
  })
  
  output$tarjeta_variaciones_interanuales <- renderUI({
    df <- rv$data %>% arrange(Bimestre) %>% mutate(Bimestre = as.Date(Bimestre))
    n <- nrow(df)
    
    if (n == 0) return(NULL)
    actual <- df[n, ]
    
    anio_actual <- as.numeric(format(actual$Bimestre, "%Y"))
    mes_actual <- as.numeric(format(actual$Bimestre, "%m"))
    
    df_ant_anual <- df %>% filter(as.numeric(format(Bimestre, "%Y")) == (anio_actual - 1) & as.numeric(format(Bimestre, "%m")) == mes_actual)
    
    if (nrow(df_ant_anual) == 0) {
      return(div(style = "padding: 10px; text-align: center; color: #666; font-size: 0.9rem;", "Sin registros interanuales previos para este período."))
    }
    
    anterior <- df_ant_anual[1, ]
    
    f_act <- formatear_fecha_amigable(actual$Bimestre)
    f_ant <- formatear_fecha_amigable(anterior$Bimestre)                                         
    
    tagList(generar_fila_metrica("Ag Barometer Austral", actual$`Ag Barometer Austral`, f_act, anterior$`Ag Barometer Austral`, f_ant),
                                                                                                         generar_fila_metrica("Condiciones Presentes", actual$`Condiciones presentes`, f_act, anterior$`Condiciones presentes`, f_ant),
                                                                                                         generar_fila_metrica("Expectativas Futuras", actual$`Expectativas futuras`, f_act, anterior$`Expectativas futuras`, f_ant)
    )
  })
  
  output$contenedor_eventos_dinamicos <- renderUI({
    num_eventos <- input$cantidad_eventos
    if (is.null(num_eventos) || num_eventos <= 0) return(NULL)
    
    fechas_def <- c("2019-10-01", "2023-11-01", rep("2024-01-01", 8))
    textos_def <- c("Triunfo de Alberto Fernández (Partido Frente de Todos)", 
                    "Triunfo de Javier Milei (Partido La Libertad Avanza) | Ballotage", 
                    rep("", 8))
    
    lapply(1:num_eventos, function(i) {
      f_val <- if(i <= length(fechas_def)) fechas_def[i] else "2024-01-01"
      t_val <- if(i <= length(textos_def)) textos_def[i] else ""
      
      tagList(
        hr(),
        h6(style = "font-weight: bold; color: #7D212B;", paste("Evento", i)),
        dateInput(paste0("fecha_evento_", i), paste("Fecha (Año-Mes) - Evento", i), value = f_val, format = "yyyy-mm"),
        textInput(paste0("desc_evento_", i), paste("Descripción - Evento", i), value = t_val)
      )
    })
  })
  
  eventos_reactivos <- reactive({
    num_eventos <- input$cantidad_eventos
    if (is.null(num_eventos) || num_eventos <= 0) return(NULL)
    
    lista_eventos <- list()
    
    for (i in 1:num_eventos) {
      desc_val <- input[[paste0("desc_evento_", i)]]
      fecha_val <- input[[paste0("fecha_evento_", i)]]
      
      if (!is.null(desc_val) && desc_val != "" && !is.null(fecha_val)) {
        lista_eventos[[length(lista_eventos) + 1]] <- data.frame(
          Bimestre = tsibble::yearmonth(fecha_val),
          Tag = "",
          Descripcion = desc_val,
          stringsAsFactors = FALSE
        )
      }
    }
    
    if (length(lista_eventos) > 0) {
      do.call(rbind, lista_eventos)
    } else {
      NULL
    }
  })
  
  objeto_grafico_reactivo <- reactive({
    df_datos <- rv$data |> mutate(Bimestre = tsibble::yearmonth(Bimestre))
    df_elecciones <- eventos_reactivos()
    
    colores_map <- c(
      "Ag Barometer Austral" = input$color_ag,
      "Condiciones presentes" = input$color_cond,
      "Expectativas futuras" = input$color_exp
    )
    
    grafico_estatico <- graficar_eventos_app(
      data = df_datos,
      elecciones_df = df_elecciones,
      ancho_sombreado = input$ancho_sombra,
      tiempo_col = Bimestre,
      series_seleccionadas = input$series_sel,
      colores_series = colores_map,
      fuente = NULL
    )
    
    # Convertimos a gráfico interactivo con plotly manteniendo las formas de anotación, sombreado y tooltip limpio
    grafico_interactivo <- ggplotly(grafico_estatico, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        legend = list(orientation = "h", x = 0.2, y = -0.15),
        xaxis = list(zeroline = FALSE),
        yaxis = list(zeroline = FALSE)
      )
    
    return(grafico_interactivo)
  })
  
  output$grafico_evolucion <- renderPlotly({
    objeto_grafico_reactivo()
  })
  
  # Renderizado de las referencias debajo del gráfico en la interfaz
  output$referencias_eventos_UI <- renderUI({
    df_elecciones <- eventos_reactivos()
    
    elementos_referencia <- list()
    
    if (!is.null(df_elecciones) && nrow(df_elecciones) > 0) {
      df_ordenado <- df_elecciones %>% arrange(Bimestre) %>% mutate(Tag = as.character(row_number()))
      
      texto_eventos <- paste(
        paste0("<b>Eventos:</b><br>", paste(paste0(df_ordenado$Tag, ": ", df_ordenado$Descripcion), collapse = "<br>")),
        sep = ""
      )
      elementos_referencia[[length(elementos_referencia) + 1]] <- HTML(texto_eventos)
    }
    
    fuente_texto <- "Fuente: Elaboración en base a datos de la Universidad Austral"
    if (length(elementos_referencia) > 0) {
      elementos_referencia[[length(elementos_referencia) + 1]] <- tags$br()
    }
    elementos_referencia[[length(elementos_referencia) + 1]] <- HTML(fuente_texto)
    
    tagList(elementos_referencia)
  })
  
  observeEvent(input$btn_zoom_grafico, {
    showModal(modalDialog(
      title = span(style = "color: #7D212B; font-weight: bold;", "Visualización Ampliada"),
      plotlyOutput("grafico_zoom_modal", height = "550px"),
      footer = modalButton("Cerrar"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  output$grafico_zoom_modal <- renderPlotly({
    objeto_grafico_reactivo()
  })
  
  output$btn_descargar_grafico <- downloadHandler(
    filename = function() {
      paste("evolucion_ag_barometer_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      df_datos <- rv$data |> mutate(Bimestre = tsibble::yearmonth(Bimestre))
      df_elecciones <- eventos_reactivos()
      
      grafico_estatico_descarga <- graficar_eventos_app(
        data = df_datos,
        elecciones_df = df_elecciones,
        ancho_sombreado = input$ancho_sombra,
        tiempo_col = Bimestre,
        series_seleccionadas = input$series_sel,
        colores_series = c(
          "Ag Barometer Austral" = input$color_ag,
          "Condiciones presentes" = input$color_cond,
          "Expectativas futuras" = input$color_exp
        ),
        fuente = 'Fuente: Elaboración en base a datos de la Universidad Austral'
      )
      ggsave(file, plot = grafico_estatico_descarga, width = 10, height = 6, dpi = 120, device = "png")
    }
  )
}

shinyApp(ui, server)

