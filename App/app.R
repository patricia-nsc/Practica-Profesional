# ----------------------------------------------------------- #
# Librerias
# ----------------------------------------------------------- #

library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(readxl)
library(plotly)
library(ggplot2)
library(lubridate)

# Configurar locale para meses en español
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# ----------------------------------------------------------- #
# Datos
# ----------------------------------------------------------- #

ruta_data <- "../Datos/Procesados/agbarometer_ARG_imputado.xlsx"

# ----------------------------------------------------------- #
# Funciones y path del logo
# ----------------------------------------------------------- #

source("../Scripts/funciones.R")

addResourcePath("logo_dir", "../Informe/Logos")

# ----------------------------------------------------------- #
# Funciones auxiliares (fuera del server)
# ----------------------------------------------------------- #

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
    div(class = "metric-label", nombre_variable),
    div(
      class = "metric-values",
      tags$span(style = "font-size: 0.85rem; color: #111111; font-weight: 500;", fecha_actual_str),
      div(class = "pill-current", round(valor_actual, 1)),
      tags$span(style = "color: #333333; font-weight: 600;", "vs"),
      tags$span(style = "font-size: 0.85rem; color: #111111; font-weight: 500;", fecha_anterior_str),
      div(class = "pill-past", round(valor_anterior, 1)),
      div(class = clase_badge, sprintf("%s%.2f%%", signo_str, variacion_pct))
    )
  )
}

# Las tres filas de métricas con su título interno en negro y negrita
filas_metricas <- function(titulo, actual, anterior) {
  f_act <- formatear_fecha_amigable(actual$Bimestre)
  f_ant <- formatear_fecha_amigable(anterior$Bimestre)         
  
  tagList(
    div(style = "font-size: 1.1rem; font-weight: 700; color: #111111; margin-bottom: 18px; letter-spacing: 0.2px;", titulo),         
    generar_fila_metrica("Ag Barometer Austral", actual$`Ag Barometer Austral`, f_act, anterior$`Ag Barometer Austral`, f_ant),
    generar_fila_metrica("Condiciones Presentes", actual$`Condiciones presentes`, f_act, anterior$`Condiciones presentes`, f_ant),
    generar_fila_metrica("Expectativas Futuras", actual$`Expectativas futuras`, f_act, anterior$`Expectativas futuras`, f_ant)
  )
}

# Fila del año anterior para el mismo mes (o NULL si no existe)
buscar_interanual <- function(df) {
  n <- nrow(df)
  if (n == 0) return(NULL)
  actual <- df[n, ]
  anio_actual <- as.numeric(format(actual$Bimestre, "%Y"))
  mes_actual <- as.numeric(format(actual$Bimestre, "%m"))
  
  previo <- df %>%
    filter(as.numeric(format(Bimestre, "%Y")) == (anio_actual - 1) &
             as.numeric(format(Bimestre, "%m")) == mes_actual)
  
  if (nrow(previo) == 0) return(NULL)
  previo[1, ]
}

# Boxplot estático (se usa en pantalla y en la descarga)
# titulo: opcional; si es NULL o vacío, no se agrega título
armar_boxplot <- function(datos, variable, con_tooltip = TRUE, titulo = NULL) {
  if (nrow(datos) == 0) {
    p <- ggplot() + labs(x = "Año", y = variable, caption = "Sin datos en el rango seleccionado") + theme_bw()
    return(p)
  }
  
  df <- datos |> mutate(anio = factor(year(Bimestre)))
  
  if (con_tooltip) {
    p <- ggplot(df, aes(x = anio, y = .data[[variable]],
                        text = paste0("<b>Año: </b>", anio, "<br><b>", variable, ": </b>",
                                      round(.data[[variable]], 1))))
  } else {
    p <- ggplot(df, aes(x = anio, y = .data[[variable]]))
  }
  
  p <- p +
    geom_boxplot(fill = "#7D212B", alpha = 0.7, color = "#401015") +
    labs(
      x = "Año",
      y = variable,
      caption = "Fuente: Elaboración en base a datos de la Universidad Austral"
    ) +
    scale_y_continuous(breaks = scales::breaks_width(20)) +
    theme_bw() +
    theme(
      plot.caption = element_text(hjust = 0, size = 9, color = "gray20"),
      axis.title = element_text(face = "bold", color = "#333333")
    )
  
  # Título opcional: a la izquierda, en negro y sin negrita
  if (!is.null(titulo) && titulo != "") {
    p <- p + labs(title = titulo) +
      theme(plot.title = element_text(face = "plain", size = 14, hjust = 0, color = "black"))
  }
  p
}

####################################################
# User Interface
####################################################

ui <- page_navbar(
  title = span(style = "font-weight: bold; letter-spacing: 0.5px;", "Ag Barometer Austral  |  SIVA"),
  bg = "#7D212B",
  theme = bs_theme(
    version = 5,
    primary = "#7D212B",
    success = "#73A800",
    "navbar-bg" = "#7D212B"
  ),
  header = tags$head(
    tags$style(HTML("
      /* Fijar la barra de navegación arriba de todo */
      .navbar {
        position: fixed !important;
        top: 0 !important;
        left: 0 !important;
        width: 100% !important;
        z-index: 1030 !important;
        box-shadow: 0 4px 20px rgba(0,0,0,0.15);
      }
      /* Fondo general */
      body {
        background: linear-gradient(135deg, #F4F6F9 0%, #E9ECEF 100%);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        min-height: 100vh;
      }
      /* Espaciado superior */
      .tab-content {
        margin-top: 85px !important;
        padding-top: 20px !important;
        padding-bottom: 40px !important;
      }
      /* ESTILO TARJETAS */
      .card {
        border: 1px solid rgba(125, 33, 43, 0.12);
        border-radius: 18px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.04), 0 1px 3px rgba(0,0,0,0.02);
        background: rgba(255, 255, 255, 0.92);
        backdrop-filter: blur(12px);
        padding: 26px 24px 24px 24px;
        margin-bottom: 30px !important;
        transition: transform 0.3s cubic-bezier(0.16, 1, 0.3, 1), box-shadow 0.3s cubic-bezier(0.16, 1, 0.3, 1), border-color 0.3s ease;
        position: relative;
      }
      .card:hover {
        transform: translateY(-6px);
        box-shadow: 0 16px 40px rgba(125, 33, 43, 0.08), 0 4px 12px rgba(0,0,0,0.05);
        border-color: rgba(125, 33, 43, 0.3);
      }
      .card-sutil {
        background: rgba(255, 255, 255, 0.92);
        backdrop-filter: blur(12px);
        border-radius: 18px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.04), 0 1px 3px rgba(0,0,0,0.02);
        border: 1px solid rgba(125, 33, 43, 0.12);
        padding: 26px 24px 24px 24px;
        margin-bottom: 30px;
        transition: transform 0.3s cubic-bezier(0.16, 1, 0.3, 1), box-shadow 0.3s cubic-bezier(0.16, 1, 0.3, 1), border-color 0.3s ease;
        position: relative;
      }
      .card-sutil:hover {
        transform: translateY(-6px);
        box-shadow: 0 16px 40px rgba(125, 33, 43, 0.08), 0 4px 12px rgba(0,0,0,0.05);
        border-color: rgba(125, 33, 43, 0.3);
      }
      /* Títulos internos de tarjetas en negro y negrita */
      .card-title-custom {
        font-size: 1.15rem;
        font-weight: 700;
        color: #111111;
        margin-bottom: 20px;
        letter-spacing: 0.3px;
        border-bottom: 2px solid rgba(125, 33, 43, 0.08);
        padding-bottom: 10px;
      }
      /* Títulos de sección en bordo, negrita y grandes (sin nubecita) */
      .section-title-clean {
        font-size: 1.35rem;
        font-weight: 800;
        color: #7D212B;
        letter-spacing: 0.5px;
        margin-top: 10px;
        margin-bottom: 15px;
      }
      .btn-custom-primary {
        background-color: #73A800;
        color: white;
        border: none;
        font-weight: 600;
        border-radius: 10px;
        padding: 10px 20px;
        transition: all 0.2s ease;
        box-shadow: 0 4px 12px rgba(115, 168, 0, 0.25);
      }
      .btn-custom-primary:hover {
        background-color: #5E8700;
        color: white;
        box-shadow: 0 6px 16px rgba(115, 168, 0, 0.4);
      }
      .btn-custom-secondary {
        background-color: #F8D7DA;
        color: #721C24;
        border: 1px solid #F5C6CB;
        font-weight: 600;
        border-radius: 10px;
        padding: 8px 15px;
        transition: all 0.2s ease;
      }
      .btn-custom-secondary:hover {
        background-color: #F1B0B7;
        color: #491217;
      }
      .welcome-modal .modal-content {
        border-radius: 18px;
        border: 2px solid #7D212B;
        box-shadow: 0 20px 50px rgba(0,0,0,0.2);
      }
      .metric-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 12px 0;
        border-bottom: 1px solid rgba(0,0,0,0.04);
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
        padding: 4px 10px;
        border-radius: 8px;
        font-weight: 600;
        font-size: 0.85rem;
        box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      }
      .pill-past {
        background: #e9ecef;
        color: #212529;
        padding: 4px 10px;
        border-radius: 8px;
        font-weight: 600;
        font-size: 0.85rem;
      }
      .badge-growth-green {
        background-color: rgba(115, 168, 0, 0.15);
        color: #4A7000;
        border: 1px solid rgba(115, 168, 0, 0.4);
        padding: 4px 12px;
        border-radius: 8px;
        font-weight: 700;
        font-size: 0.88rem;
        min-width: 80px;
        text-align: center;
      }
      .badge-growth-red {
        background-color: rgba(217, 83, 79, 0.15);
        color: #C9302C;
        border: 1px solid rgba(217, 83, 79, 0.4);
        padding: 4px 12px;
        border-radius: 8px;
        font-weight: 700;
        font-size: 0.88rem;
        min-width: 80px;
        text-align: center;
      }
      .card img, .card .shiny-plot-output img {
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
          class = "card",
          div(class = "card-title-custom", "Ingresar nuevo registro"),
          p(style = "font-size: 0.9rem; color: #555;",
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
            actionButton("btn_agregar", "Agregar registro", icon = icon("save"),
                         class = "btn btn-custom-primary w-100 mb-2")
          ),
          div(
            style = "text-align: center;",
            actionButton("btn_eliminar", "Eliminar último registro", icon = icon("trash-alt"),
                         class = "btn btn-custom-secondary w-100")
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "card",
          div(class = "card-title-custom", "Vista previa de los últimos registros"),
          p(style = "font-size: 0.85rem; color: #666; margin-bottom: 10px;", "Haga clic en una fila para editarla."),
          DTOutput("tabla_datos")
        )
      )
    )
  ),
  
  # ------------------------------------------------------------- #
  # PESTAÑA 2: tablero
  # ------------------------------------------------------------- #
  nav_panel(
    title = "Tablero Analítico",
    
    # PERÍODO: Slider horizontal sin tarjeta de fondo, ancho y limpio arriba de todo
    fluidRow(
      column(
        width = 12,
        div(
          style = "display: flex; align-items: center; justify-content: center; gap: 15px; margin-bottom: 25px; padding: 0 10px;",
          tags$span(style = "font-size: 0.85rem; font-weight: 700; color: #7D212B; text-transform: uppercase; letter-spacing: 1px; white-space: nowrap;", "Período:"),
          div(style = "flex-grow: 1; max-width: 800px;", uiOutput("selector_rango_fechas_ui"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12,
        div(class = "section-title-clean", "Síntesis de Variaciones")
      )
    ),
    
    # VARIACIONES
    fluidRow(
      column(
        width = 6,
        div(
          class = "card-sutil",
          uiOutput("tarjeta_variaciones_bimestrales")
        )
      ),
      column(
        width = 6,
        div(
          class = "card-sutil",
          uiOutput("tarjeta_variaciones_interanuales")
        )
      )
    ),
    
    # Evolución del Ag Barometer Austral y Eventos
    fluidRow(
      column(
        width = 12,
        div(class = "section-title-clean", "Evolución del Ag Barometer Austral y Eventos")
      )
    ),
    fluidRow(
      column(
        width = 4,
        div(
          class = "card",
          style = "overflow-x: hidden;",
          div(class = "card-title-custom", "Opciones de gráfico y eventos"),
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
          
          # El slider arranca desde 0
          sliderInput("ancho_sombra", "Ancho del sombreado:", min = 0, max = 6, value = 3, step = 1),
          hr(),
          # Campo para título opcional de descarga (vacío = sin título), arriba de la cantidad de eventos
          textInput("titulo_descarga_grafico", "Título del gráfico (para descarga):", value = ""),
          hr(),
          numericInput("cantidad_eventos", "Cantidad de eventos a mostrar:", value = 2, min = 0, max = 10, step = 1),
          uiOutput("contenedor_eventos_dinamicos")
        )
      ),
      column(
        width = 8,
        div(
          class = "card",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            div(class = "card-title-custom", style = "margin-bottom: 0; border-bottom: none; padding-bottom: 0;", "Gráfico"),
            div(
              actionButton("btn_zoom_grafico", "", icon = icon("search-plus"), class = "btn btn-sm btn-outline-secondary me-1", title = "Zoom"),
              downloadButton("btn_descargar_grafico", "", class = "btn btn-sm btn-outline-secondary", title = "Descargar")
            )
          ),
          plotlyOutput("grafico_evolucion", height = "450px"),
          div(
            style = "text-align: left; margin-top: 15px; font-size: 0.85rem; color: #333333; line-height: 1.4;",
            uiOutput("referencias_eventos_UI")
          )
        )
      )
    ),
    
    # ------------------------------------------------------------- #
    # SECCIÓN: Distribución del indicador por año (con Plotly)
    # ------------------------------------------------------------- #
    fluidRow(
      column(
        width = 12,
        div(class = "section-title-clean", "Distribución de los indicadores por año")
      )
    ),
    fluidRow(
      column(
        width = 4,
        div(
          class = "card",
          div(class = "card-title-custom", "Opciones"),
          selectInput("selector_variable_boxplot", "Seleccionar indicador:",
                      choices = c("Ag Barometer Austral" = "Ag Barometer Austral",
                                  "Condiciones presentes" = "Condiciones presentes",
                                  "Expectativas futuras" = "Expectativas futuras"),
                      selected = "Ag Barometer Austral"),
          hr(),
          # Campo para título opcional de descarga del boxplot (vacío = sin título)
          textInput("titulo_descarga_boxplot", "Título del gráfico (para descarga):", value = "")
        )
      ),
      column(
        width = 8,
        div(
          class = "card",
          div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
            div(class = "card-title-custom", style = "margin-bottom: 0; border-bottom: none; padding-bottom: 0;", "Gráfico"),
            div(
              actionButton("btn_zoom_boxplot", "", icon = icon("search-plus"), class = "btn btn-sm btn-outline-secondary me-1", title = "Zoom"),
              downloadButton("btn_descargar_boxplot", "", class = "btn btn-sm btn-outline-secondary", title = "Descargar")
            )
          ),
          plotlyOutput("grafico_boxplot_interactivo", height = "400px")
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
         "¡Bienvenido al SIVA!"),
      p(style = "font-size: 1.05rem; color: #333; margin-bottom: 20px;",
        "Sistema Interactivo de Visualización y Automatización (SIVA) del ",
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
    },
    fila_a_editar = NULL
  )
  
  # UI dinámico para el slider de fechas horizontal con formato en español
  output$selector_rango_fechas_ui <- renderUI({
    df <- rv$data
    if (nrow(df) == 0) return(NULL)
    min_f <- min(df$Bimestre, na.rm = TRUE)
    max_f <- max(df$Bimestre, na.rm = TRUE)
    
    sliderInput("rango_periodo", label = NULL,
                min = min_f, max = max_f,
                value = c(min_f, max_f),
                timeFormat = "%b %y",
                width = "100%")
  })
  
  # Datos filtrados por el rango de fechas seleccionado en el tablero
  datos_filtrados <- reactive({
    df <- rv$data %>% arrange(Bimestre)
    rango <- input$rango_periodo
    if (is.null(rango)) return(df)
    
    df %>% filter(Bimestre >= rango[1] & Bimestre <= rango[2])
  })
  
  # ----------------------------------------------------------- #
  # Alta, baja y edición de registros
  # ----------------------------------------------------------- #
  
  observeEvent(input$btn_agregar, {
    mes_val <- as.numeric(input$input_mes)
    anio_val <- input$input_anio
    nueva_fecha <- as.Date(sprintf("%d-%02d-01", as.integer(anio_val), as.integer(mes_val)))
    
    # Verificar si ya existe para editar en lugar de duplicar si viene de edición
    if (nueva_fecha %in% rv$data$Bimestre) {
      rv$data <- rv$data %>% filter(Bimestre != nueva_fecha)
    }
    
    nueva_fila <- data.frame(
      Bimestre = nueva_fecha,
      `Ag Barometer Austral` = input$input_agbarometer,
      `Condiciones presentes` = input$input_condiciones,
      `Expectativas futuras` = input$input_expectativas,
      check.names = FALSE
    )
    
    rv$data <- bind_rows(rv$data, nueva_fila) %>% arrange(Bimestre)
    showNotification("¡Registro guardado y vista previa actualizada con éxito!", type = "message", duration = 3)
  })
  
  observeEvent(input$btn_eliminar, {
    if (nrow(rv$data) > 3) {
      showModal(modalDialog(
        title = span(style = "color: #7D212B; font-weight: bold;", "Confirmar eliminación"),
        p("¿Está seguro de que desea eliminar el último registro ingresado?"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("btn_confirmar_eliminacion", "Sí, eliminar", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    } else {
      showNotification("No se pueden eliminar más registros (se requiere mantener el histórico mínimo).",
                       type = "warning", duration = 4)
    }
  })
  
  observeEvent(input$btn_confirmar_eliminacion, {
    removeModal()
    if (nrow(rv$data) > 3) {
      rv$data <- rv$data %>% arrange(Bimestre) %>% slice(-n())
      showNotification("Se ha eliminado el último registro ingresado.", type = "error", duration = 3)
    }
  })
  
  # Vista previa: últimos 7 registros, sin paginación ni textos de entrada
  output$tabla_datos <- renderDT({
    df_mostrar <- rv$data %>%
      arrange(Bimestre) %>%
      tail(7) %>%
      mutate(Bimestre = format(Bimestre, "%Y-%m"))
    
    datatable(
      df_mostrar,
      options = list(
        dom = "t",
        pageLength = 7,
        ordering = FALSE,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json")
      ),
      rownames = FALSE,
      selection = "none"
    )
  })
  
  # Preguntar al hacer clic en una fila si se desea editar
  observeEvent(input$tabla_datos_cell_clicked, {
    clic <- input$tabla_datos_cell_clicked
    if (is.null(clic) || length(clic) == 0 || is.null(clic$row)) return()
    
    row_idx <- clic$row
    
    df_mostrar <- rv$data %>%
      arrange(Bimestre) %>%
      tail(7)
    
    fila_sel <- df_mostrar[row_idx, ]
    
    showModal(modalDialog(
      title = span(style = "color: #7D212B; font-weight: bold;", "Confirmar edición"),
      p(sprintf("¿Está seguro de que desea editar el registro correspondiente a %s?", format(fila_sel$Bimestre, "%Y-%m"))),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_confirmar_edicion", "Sí, editar", class = "btn btn-custom-primary")
      ),
      easyClose = TRUE
    ))
    
    # Guardar temporalmente la fila seleccionada para cargarla al confirmar
    rv$fila_a_editar <- fila_sel
  })
  
  observeEvent(input$btn_confirmar_edicion, {
    removeModal()
    fila <- rv$fila_a_editar
    if (!is.null(fila)) {
      mes_num <- as.numeric(format(fila$Bimestre, "%m"))
      anio_num <- as.numeric(format(fila$Bimestre, "%Y"))
      
      updateSelectInput(session, "input_mes", selected = mes_num)
      updateNumericInput(session, "input_anio", value = anio_num)
      updateNumericInput(session, "input_agbarometer", value = fila$`Ag Barometer Austral`)
      updateNumericInput(session, "input_condiciones", value = fila$`Condiciones presentes`)
      updateNumericInput(session, "input_expectativas", value = fila$`Expectativas futuras`)
      
      showNotification("Los datos se han cargado en el formulario de la izquierda para su edición.", type = "message", duration = 4)
    }
  })
  
  # ----------------------------------------------------------- #
  # Tarjetas de variaciones (usando datos_filtrados)
  # ----------------------------------------------------------- #
  
  output$tarjeta_variaciones_bimestrales <- renderUI({
    df <- datos_filtrados()
    n <- nrow(df)
    
    if (n < 2) {
      return(div(style = "padding: 10px; text-align: center; color: #666; font-size: 0.9rem;",
                 "Se requieren al menos dos registros en el período seleccionado."))
    }
    
    filas_metricas("Variaciones bimestrales", df[n, ], df[n - 1, ])
  })
  
  output$tarjeta_variaciones_interanuales <- renderUI({
    df <- datos_filtrados() %>% mutate(Bimestre = as.Date(Bimestre))
    if (nrow(df) == 0) return(NULL)
    
    anterior <- buscar_interanual(df)
    
    if (is.null(anterior)) {
      return(div(style = "padding: 10px; text-align: center; color: #666; font-size: 0.9rem;",
                 "Sin registros interanuales previos para el último punto del período seleccionado."))
    }
    
    filas_metricas("Variaciones interanuales", df[nrow(df), ], anterior)
  })
  
  # ----------------------------------------------------------- #
  # Eventos dinámicos
  # ----------------------------------------------------------- #
  
  output$contenedor_eventos_dinamicos <- renderUI({
    num_eventos <- input$cantidad_eventos
    if (is.null(num_eventos) || is.na(num_eventos) || num_eventos <= 0) return(NULL)
    
    fechas_def <- c("2019-10-01", "2023-11-01", rep("2024-01-01", 8))
    textos_def <- c("Triunfo de Alberto Fernández (Partido Frente de Todos)",
                    "Triunfo de Javier Milei (Partido La Libertad Avanza) | Ballotage",
                    rep("", 8))
    
    lapply(seq_len(num_eventos), function(i) {
      f_val <- if (i <= length(fechas_def)) fechas_def[i] else "2024-01-01"
      t_val <- if (i <= length(textos_def)) textos_def[i] else ""
      
      tagList(
        hr(),
        h6(style = "font-weight: bold; color: #7D212B;", paste("Evento", i)),
        dateInput(paste0("fecha_evento_", i), paste("Fecha (Año-Mes) - Evento", i),
                  value = f_val, format = "yyyy-mm"),
        textInput(paste0("desc_evento_", i), paste("Descripción - Evento", i), value = t_val)
      )
    })
  })
  
  # Procesamiento de eventos: filtrados por rango, ordenados cronológicamente y enumerados solos
  eventos_reactivos <- reactive({
    num_eventos <- input$cantidad_eventos
    if (is.null(num_eventos) || is.na(num_eventos) || num_eventos <= 0) return(NULL)
    
    lista_eventos <- list()
    rango <- input$rango_periodo
    
    for (i in seq_len(num_eventos)) {
      desc_val <- input[[paste0("desc_evento_", i)]]
      fecha_val <- input[[paste0("fecha_evento_", i)]]
      
      if (!is.null(desc_val) && desc_val != "" && !is.null(fecha_val)) {
        f_date <- as.Date(paste0(fecha_val, "-01"))
        # Filtrar si está dentro del rango temporal seleccionado
        if (is.null(rango) || (f_date >= rango[1] && f_date <= rango[2])) {
          lista_eventos[[length(lista_eventos) + 1]] <- data.frame(
            Bimestre = tsibble::yearmonth(f_date),
            Tag = "",
            FechaReal = f_date,
            Descripcion = desc_val,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(lista_eventos) > 0) {
      df_ev <- do.call(rbind, lista_eventos)
      # Ordenar cronológicamente y asignar tag secuencial
      df_ev <- df_ev %>% arrange(FechaReal) %>% mutate(Tag = as.character(row_number()))
      df_ev$FechaReal <- NULL
      df_ev
    } else {
      NULL
    }
  })
  
  # ----------------------------------------------------------- #
  # Gráfico de evolución (usando datos_filtrados)
  # ----------------------------------------------------------- #
  
  armar_grafico_estatico <- function(titulo_personalizado = NULL) {
    p <- graficar_eventos_app(
      data = datos_filtrados() |> mutate(Bimestre = tsibble::yearmonth(Bimestre)),
      elecciones_df = eventos_reactivos(),
      ancho_sombreado = input$ancho_sombra,
      tiempo_col = Bimestre,
      series_seleccionadas = input$series_sel,
      colores_series = c(
        "Ag Barometer Austral" = input$color_ag,
        "Condiciones presentes" = input$color_cond,
        "Expectativas futuras" = input$color_exp
      ),
      fuente = "Fuente: Elaboración en base a datos de la Universidad Austral"
    )
    
    # Título opcional (solo descarga): a la izquierda, en negro y sin negrita
    if (!is.null(titulo_personalizado) && titulo_personalizado != "") {
      p <- p + labs(title = titulo_personalizado) + 
        theme(plot.title = element_text(face = "plain", size = 14, hjust = 0, color = "black"))
    }
    p
  }
  
  objeto_grafico_reactivo <- reactive({
    ggplotly(armar_grafico_estatico(), tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        # Leyenda anclada debajo del área del gráfico con margen inferior fijo,
        # así el gráfico no se achica al agregar series
        legend = list(orientation = "h", x = 0, y = -0.2, yanchor = "top"),
        margin = list(b = 120),
        xaxis = list(zeroline = FALSE),
        yaxis = list(zeroline = FALSE)
      )
  })
  
  output$grafico_evolucion <- renderPlotly({
    objeto_grafico_reactivo()
  })
  
  output$referencias_eventos_UI <- renderUI({
    df_elecciones <- eventos_reactivos()
    
    elementos_referencia <- list()
    
    if (!is.null(df_elecciones) && nrow(df_elecciones) > 0) {
      texto_eventos <- paste0(
        "<b>Eventos:</b><br>",
        paste(paste0(df_elecciones$Tag, ": ", df_elecciones$Descripcion), collapse = "<br>")
      )
      elementos_referencia[[length(elementos_referencia) + 1]] <- HTML(texto_eventos)
      elementos_referencia[[length(elementos_referencia) + 1]] <- tags$br()
    }
    
    elementos_referencia[[length(elementos_referencia) + 1]] <-
      HTML("Fuente: Elaboración en base a datos de la Universidad Austral")
    
    tagList(elementos_referencia)
  })
  
  # ----------------------------------------------------------- #
  # Boxplot anual (usando datos_filtrados)
  # ----------------------------------------------------------- #
  
  objeto_boxplot_reactivo <- reactive({
    ggplotly(armar_boxplot(datos_filtrados(), input$selector_variable_boxplot, con_tooltip = TRUE),
             tooltip = "text") %>%
      layout(
        xaxis = list(zeroline = FALSE),
        yaxis = list(zeroline = FALSE),
        margin = list(b = 50)
      )
  })
  
  output$grafico_boxplot_interactivo <- renderPlotly({
    objeto_boxplot_reactivo()
  })
  
  # ----------------------------------------------------------- #
  # Modales de zoom
  # ----------------------------------------------------------- #
  
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
  
  observeEvent(input$btn_zoom_boxplot, {
    showModal(modalDialog(
      title = span(style = "color: #7D212B; font-weight: bold;", "Visualización Ampliada - Distribución Anual"),
      plotlyOutput("grafico_zoom_boxplot_modal", height = "550px"),
      footer = modalButton("Cerrar"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  output$grafico_zoom_boxplot_modal <- renderPlotly({
    objeto_boxplot_reactivo()
  })
  
  # ----------------------------------------------------------- #
  # Descargas (Gráficos)
  # ----------------------------------------------------------- #
  
  output$btn_descargar_grafico <- downloadHandler(
    filename = function() {
      paste0("evolucion_ag_barometer_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Si el campo de título está vacío, se descarga sin título
      plot_a_descargar <- armar_grafico_estatico(titulo_personalizado = input$titulo_descarga_grafico)
      ggsave(file, plot = plot_a_descargar, width = 11, height = 8.5, dpi = 150, device = "png")
    }
  )
  
  output$btn_descargar_boxplot <- downloadHandler(
    filename = function() {
      paste0("distribucion_anual_", input$selector_variable_boxplot, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Si el campo de título está vacío, se descarga sin título
      p <- armar_boxplot(datos_filtrados(), input$selector_variable_boxplot,
                         con_tooltip = FALSE, titulo = input$titulo_descarga_boxplot)
      ggsave(file, plot = p, width = 10, height = 6, dpi = 150, device = "png")
    }
  )
}

shinyApp(ui, server)