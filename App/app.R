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
library(shinyWidgets)   # sliderTextInput: permite saltar de bimestre en bimestre y fijar el inicio



# ----------------------------------------------------------- #
# Datos
# ----------------------------------------------------------- #

ruta_data <- "agbarometer_ARG_imputado.xlsx"




# ----------------------------------------------------------- #
# funciones y logo
# ----------------------------------------------------------- #

source("funciones_app.R")


# Logo: se busca en app/www/ o en la carpeta de app.R, y se sirve SOLO esa imagen
# (se copia a una carpeta temporal, así no se expone el resto del proyecto).
archivo_logo <- "logo_austral_ceag.png"
ruta_logo <- c(file.path("www", archivo_logo), archivo_logo)
ruta_logo <- ruta_logo[file.exists(ruta_logo)][1]

if (is.na(ruta_logo)) {
  warning("No se encontró ", archivo_logo, " (buscado en www/ y en la carpeta de la app). Directorio actual: ", getwd())
} else {
  dir_logo <- file.path(tempdir(), "logo_siva")
  dir.create(dir_logo, showWarnings = FALSE)
  file.copy(ruta_logo, file.path(dir_logo, archivo_logo), overwrite = TRUE)
  addResourcePath("logo_dir", dir_logo)
}


# Fecha mínima a la que puede retroceder el fin del período (inclusive)
fecha_min_periodo <- as.Date("2019-11-01")

# Etiquetas en español para el filtro de período (ej: "Nov 19")
etiquetas_bimestres <- function(fechas) {
  meses_abr <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                 "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  paste(meses_abr[month(fechas)], format(fechas, "%y"))
}

# Posición (fila) del primer bimestre >= fecha_min_periodo
indice_minimo_periodo <- function(df) {
  idx <- which(df$Bimestre >= fecha_min_periodo)[1]
  if (is.na(idx)) 1L else idx
}





####################################################
# User Interface
####################################################

ui <- page_navbar(
  # El logo ocupa ahora el lugar del título (izquierda); el título pasa a la derecha vía JS (ver footer)
  title = tags$img(src = "logo_dir/logo_austral_ceag.png", height = "44px",
                   style = "background: transparent;"),
  window_title = "Ag Barometer Austral",
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
      .navbar-titulo-derecha {
        margin-left: auto;
        display: flex;
        align-items: center;
        padding-right: 15px;
      }
      /* Filtro de período: el inicio queda fijo, se oculta su punto para que no parezca movible */
      #selector_rango_fechas_ui .irs-handle.from {
        display: none !important;
        pointer-events: none !important;
      }
    "))
  ),
  
  # Título a la derecha de la barra (intercambiado de lugar con el logo)
  footer = tags$script(HTML("
    $(document).ready(function() {
      var tituloHtml = '<div class=\"navbar-titulo-derecha\"><span style=\"font-weight: bold; letter-spacing: 0.5px; color: white; font-size: 1.25rem;\">Ag Barometer Austral</span></div>';
      $('.navbar-nav').last().after(tituloHtml);
    });
  ")),
  
  # ------------------------------------------------------------- #
  # PESTAÑA 1: inicio (por ahora en blanco, solo va a tener texto)
  # ------------------------------------------------------------- #
  nav_panel(
    title = "Inicio",
    div()
  ),
  
  # ------------------------------------------------------------- #
  # PESTAÑA 2: tablero
  # ------------------------------------------------------------- #
  nav_panel(
    title = "Tablero Analítico",
    
    fluidRow(
      column(
        width = 12,
        div(class = "section-title-clean", "Síntesis de Variaciones")
      )
    ),
    
    # PERÍODO: afecta solo a las tarjetas de variaciones. Inicio fijo, fin movible.
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
    
    # Evolución del Ag Barometer Austral
    fluidRow(
      column(
        width = 12,
        div(class = "section-title-clean", "Evolución del Ag Barometer Austral")
      )
    ),
    fluidRow(
      column(
        width = 4,
        div(
          class = "card",
          style = "overflow-x: hidden;",
          div(class = "card-title-custom", "Opciones de gráfico"),
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
          
          hr(),
          # Campo para título opcional de descarga (vacío = sin título)
          textInput("titulo_descarga_grafico", "Título del gráfico (para descarga):", value = "")
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
            uiOutput("referencias_UI")
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
    }
  )
  
  # UI dinámico para el slider de período: el inicio queda fijo (primer bimestre),
  # el fin se mueve de bimestre en bimestre y no puede retroceder antes de Nov 2019.
  output$selector_rango_fechas_ui <- renderUI({
    df <- rv$data |> arrange(Bimestre)
    if (nrow(df) == 0) return(NULL)
    etiquetas <- etiquetas_bimestres(df$Bimestre)
    idx_min <- indice_minimo_periodo(df)
    
    sliderTextInput(
      inputId = "rango_periodo",
      label = NULL,
      choices = etiquetas,
      selected = c(etiquetas[1], etiquetas[nrow(df)]),
      grid = TRUE,
      from_fixed = TRUE,
      to_min = etiquetas[idx_min],
      dragRange = FALSE,
      width = "100%"
    )
  })
  
  # Seguro extra: si el fin quedara antes de Nov 2019, se lo devuelve a ese límite
  observeEvent(input$rango_periodo, {
    df <- rv$data |> arrange(Bimestre)
    etiquetas <- etiquetas_bimestres(df$Bimestre)
    idx_fin <- match(input$rango_periodo[2], etiquetas)
    idx_min <- indice_minimo_periodo(df)
    if (!is.na(idx_fin) && idx_fin < idx_min) {
      updateSliderTextInput(session, "rango_periodo",
                            selected = c(etiquetas[1], etiquetas[idx_min]))
    }
  })
  
  # Datos filtrados por el período seleccionado: SOLO se usan en las tarjetas de variaciones
  datos_filtrados <- reactive({
    df <- rv$data |> arrange(Bimestre)
    rango <- input$rango_periodo
    if (is.null(rango)) return(df)
    
    etiquetas <- etiquetas_bimestres(df$Bimestre)
    idx_ini <- match(rango[1], etiquetas)
    idx_fin <- match(rango[2], etiquetas)
    if (is.na(idx_ini) || is.na(idx_fin)) return(df)
    
    idx_fin <- max(idx_fin, indice_minimo_periodo(df))
    df[idx_ini:idx_fin, ]
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
    df <- datos_filtrados() |> mutate(Bimestre = as.Date(Bimestre))
    if (nrow(df) == 0) return(NULL)
    
    anterior <- buscar_interanual(df)
    
    if (is.null(anterior)) {
      return(div(style = "padding: 10px; text-align: center; color: #666; font-size: 0.9rem;",
                 "Sin registros interanuales previos para el último punto del período seleccionado."))
    }
    
    filas_metricas("Variaciones interanuales", df[nrow(df), ], anterior)
  })
  
  # ----------------------------------------------------------- #
  # Gráfico de evolución (usa toda la serie, el filtro de período no lo afecta)
  # ----------------------------------------------------------- #
  
  armar_grafico_estatico <- function(titulo_personalizado = NULL) {
    p <- graficar_eventos_app(
      data = rv$data |> arrange(Bimestre) |> mutate(Bimestre = tsibble::yearmonth(Bimestre)),
      elecciones_df = NULL,
      ancho_sombreado = 0,
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
    ggplotly(armar_grafico_estatico(), tooltip = "text") |>
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
  
  output$referencias_UI <- renderUI({
    HTML("Fuente: Elaboración en base a datos de la Universidad Austral")
  })
  
  # ----------------------------------------------------------- #
  # Modal de zoom
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
}

shinyApp(ui, server)