# ----------------------------------------------------------- #
# Librerias
# ----------------------------------------------------------- #

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(readxl)

# ----------------------------------------------------------- #
# Datos
# ----------------------------------------------------------- #

ruta_data <- "Datos/Procesados/agbarometer_ARG_imputado.xlsx"

####################################################
# User Interface
####################################################

ui <- page_navbar(
  title = span(
    style = "color: white; font-weight: bold; font-family: 'Helvetica Neue', sans-serif;",
    "Ag Barometer Austral | Tablero Interactivo"
  ),
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

      /* Estética Minimalista, Futurista y con Hover para el Tablero */
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
    "))
  ),
  
  # PESTAÑA 1: carga de datos
  nav_panel(
    title = "Gestión de Datos",
    icon = icon("database"),
    fluidRow(
      column(
        width = 4,
        div(
          class = "card mb-4",
          div(class = "card-header", icon("plus-circle"), "Ingresar nuevo registro"),
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
            numericInput("input_condiciones", "Condiciones Presentes:", value = 100, min = 0, max = 300, step = 0.1),
            numericInput("input_expectativas", "Expectativas Futuras:", value = 100, min = 0, max = 300, step = 0.1),
            numericInput("input_agbarometer", "Ag Barometer Austral:", value = 100, min = 0, max = 300, step = 0.1),
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
          div(class = "card-header", icon("table"), " Vista Previa"),
          div(
            class = "card-body",
            DTOutput("tabla_datos")
          )
        )
      )
    )
  ),
  
  # PESTAÑA 2: tablero
  nav_panel(
    title = "Tablero Analítico",
    icon = icon("chart-line"),
    fluidRow(
      column(
        width = 12,
        div(
          style = "padding: 10px 0px 15px 0px;",
          h4(style = "color: #7D212B; font-weight: bold; letter-spacing: 0.5px;", "Síntesis de Variaciones")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        div(
          class = "dashboard-section-title",
          icon("calendar-alt"), "Variaciones bimestrales"
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
          icon("history"), "Variaciones interanuales"
        ),
        div(
          class = "minimal-card",
          uiOutput("tarjeta_variaciones_interanuales")
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
    title = span(style = "color: #7D212B; font-weight: bold;", "Bienvenido al Sistema Interactivo"),
    div(
      style = "text-align: center;",
      tags$img(src = "https://www.austral.edu.ar/rosario/wp-content/uploads/sites/12/2021/03/logo-ceag-bordo.png", 
               height = "90px", style = "margin-bottom: 20px;"),
      p(style = "font-size: 1.05rem; color: #333;",
        "Aplicación desarrollada para la gestión y análisis exploratorio del ", 
        tags$b("Ag Barometer Austral"), "."),
      hr(),
      p(style = "font-size: 0.9rem; color: #666;", 
        "Utilice la pestaña 'Gestión de Datos' para incorporar nuevos relevamientos bimestrales de manera rápida y segura.")
    ),
    footer = modalButton("Comenzar", icon = icon("rocket")),
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
      `Condiciones presentes` = input$input_condiciones,
      `Expectativas futuras` = input$input_expectativas,
      `Ag Barometer Austral` = input$input_agbarometer,
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
    
    tagList(
      generar_fila_metrica("Ag Barometer Austral", actual$`Ag Barometer Austral`, f_act, anterior$`Ag Barometer Austral`, f_ant),
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
    mes_actual <- format(actual$Bimestre, "%m")
    
    fecha_anterior_anual <- as.Date(sprintf("%d-%s-01", anio_actual - 1, mes_actual))
    
    df_ant_anual <- df %>% filter(Bimestre == fecha_anterior_anual)
    
    if (nrow(df_ant_anual) == 0) {
      df_ant_anual <- df %>% filter(as.numeric(format(Bimestre, "%Y")) == (anio_actual - 1)) %>% tail(1)
    }
    
    if (nrow(df_ant_anual) == 0) {
      return(div(style = "padding: 10px; text-align: center; color: #666; font-size: 0.9rem;", "Sin registros interanuales previos."))
    }
    
    anterior <- df_ant_anual[1, ]
    
    f_act <- formatear_fecha_amigable(actual$Bimestre)
    f_ant <- formatear_fecha_amigable(anterior$Bimestre)
    
    tagList(
      generar_fila_metrica("Ag Barometer Austral", actual$`Ag Barometer Austral`, f_act, anterior$`Ag Barometer Austral`, f_ant),
      generar_fila_metrica("Condiciones Presentes", actual$`Condiciones presentes`, f_act, anterior$`Condiciones presentes`, f_ant),
      generar_fila_metrica("Expectativas Futuras", actual$`Expectativas futuras`, f_act, anterior$`Expectativas futuras`, f_ant)
    )
  })
}

shinyApp(ui, server)