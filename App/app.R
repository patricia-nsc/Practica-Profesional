# ----------------------------------------------------------- #
# Librerias
# ----------------------------------------------------------- #

library(shiny)
library(bslib)
library(dplyr)
library(stringr)
library(DT)
library(readxl)
library(plotly)
library(ggplot2)
library(lubridate)
library(shinyWidgets)   # sliderTextInput: permite saltar de bimestre en bimestre y fijar el inicio
library(writexl)        # write_xlsx: descarga de la base en formato Excel



# ----------------------------------------------------------- #
# Datos
# ----------------------------------------------------------- #

ruta_data <- file.path("Datos", "agbarometer_ARG_imputado.xlsx")

# Base de las 5 preguntas fijas (columnas Q1 a Q5)
ruta_preguntas <- file.path("Datos", "base_preguntas.xlsx")
datos_preguntas <- read_excel(ruta_preguntas, sheet = "Base")


# ----------------------------------------------------------- #
# funciones y logos
# ----------------------------------------------------------- #

source("funciones_app.R")


# Logos: una línea por logo. Se buscan en la carpeta Logos/ (si no, en www/ o en la carpeta de app.R), y se sirven
# SOLO esas imágenes (se copian a una carpeta temporal, así no se expone el resto del proyecto).
# Para agregar un logo nuevo alcanza con sumar una línea acá y usarlo como "logo_dir/<archivo>".
archivos_logos <- c(
  "logo_austral_ceag_transparente.png",
  "logo_agbarometer_blanco.png",
  "logo_agbarometer_color.png",
  "logo_basf.jpg",
  "logo_john_deere.png"
)

dir_logo <- file.path(tempdir(), "logo_siva")
dir.create(dir_logo, showWarnings = FALSE)
for (archivo_logo in archivos_logos) {
  ruta_logo <- c(file.path("Logos", archivo_logo), file.path("www", archivo_logo), archivo_logo)
  ruta_logo <- ruta_logo[file.exists(ruta_logo)][1]
  if (is.na(ruta_logo)) {
    warning("No se encontró ", archivo_logo, " (buscado en Logos/, www/ y en la carpeta de la app). Directorio actual: ", getwd())
  } else {
    file.copy(ruta_logo, file.path(dir_logo, archivo_logo), overwrite = TRUE)
  }
}
addResourcePath("logo_dir", dir_logo)


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


# ----------------------------------------------------------- #
# Sección Resultados de las preguntas: colores y categorías
# ----------------------------------------------------------- #

colores_graficos_dona <- c(
  "Peor" = "#974D55",
  "Mejor" = "#8FB933",
  "Igual" = "#E9C46A",
  "Malos tiempos" = "#974D55",
  "Buenos tiempos" = "#8FB933",
  "Mayormente malos tiempos" = "#974D55",
  "Mayormente buenos tiempos" = "#8FB933",
  "Es un mal momento" = "#974D55",
  "Es un buen momento" = "#8FB933"
)

# Color de los porcentajes y de las referencias (oscuro, sobre colores claros)
color_texto_dona <- "#1B1F23"

# Tamaño único de los porcentajes (igual para todas las categorías, sin importar el %)
tamanio_texto_dona <- 16

# Tamaño de las referencias (leyenda) que van abajo de cada gráfico
tamanio_leyenda_dona <- 13

# Dónde va el porcentaje en el gráfico interactivo: "outside" (afuera del anillo, siempre se ve
# completo, incluso en las porciones chicas) o "inside" (adentro; en porciones muy finitas
# puede quedar tapado por la porción vecina)
posicion_porcentaje_dona <- "outside"

# Orden en que aparecen las categorías de cada pregunta
# (se muestran en sentido antihorario, arrancando desde las 12 hs hacia la izquierda)
niveles_preguntas <- list(
  Q1 = c("Peor", "Mejor", "Igual"),
  Q2 = c("Peor", "Mejor", "Igual"),
  Q3 = c("Malos tiempos", "Buenos tiempos"),
  Q4 = c("Mayormente malos tiempos", "Mayormente buenos tiempos"),
  Q5 = c("Es un mal momento", "Es un buen momento")
)

# Texto de la fuente (el mismo que usa el gráfico de la serie)
texto_fuente <- "Fuente: Elaboración en base a datos de la Universidad Austral"

# Categorías que entran en el score de cada pregunta:
# SCORE = % favorables - % desfavorables + 100
# (Q1 y Q2: las respuestas "Igual" no entran en la cuenta)
categorias_score <- list(
  Q1 = c(favorable = "Mejor",                    desfavorable = "Peor"),
  Q2 = c(favorable = "Mejor",                    desfavorable = "Peor"),
  Q3 = c(favorable = "Buenos tiempos",           desfavorable = "Malos tiempos"),
  Q4 = c(favorable = "Mayormente buenos tiempos", desfavorable = "Mayormente malos tiempos"),
  Q5 = c(favorable = "Es un buen momento",       desfavorable = "Es un mal momento")
)

# Tabla resumen (categoría, n, porcentaje y etiqueta) de una pregunta.
# Queda ordenada según los niveles que se definen en niveles_preguntas.
resumir_pregunta <- function(datos, col, niveles) {
  datos |>
    # Los NA (y cualquier valor fuera de las categorías) se descartan: ni los gráficos
    # ni los porcentajes ni el score los consideran
    mutate(cat = factor(trimws(as.character(.data[[col]])), levels = niveles)) |>
    filter(!is.na(cat)) |>
    group_by(cat) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(cat) |>
    mutate(porcentaje = n / sum(n) * 100,
           etiqueta = paste0(round(porcentaje, 0), "%"))
}

# Score de una pregunta: % favorables - % desfavorables + 100.
# Los porcentajes se calculan sobre las respuestas válidas (sin NA) y sin redondear;
# solo el resultado final se redondea a número entero.
calcular_score <- function(datos, col, niveles, favorable, desfavorable) {
  res <- resumir_pregunta(datos, col, niveles)
  if (nrow(res) == 0) return(NA_real_)
  p_fav <- sum(res$porcentaje[res$cat == favorable])
  p_des <- sum(res$porcentaje[res$cat == desfavorable])
  p_fav - p_des + 100
}

# Texto que se muestra en la tarjeta (ej: "SCORE: 138")
texto_score <- function(datos, id) {
  cats <- categorias_score[[id]]
  sc <- calcular_score(datos, id, niveles_preguntas[[id]], cats[["favorable"]], cats[["desfavorable"]])
  if (is.na(sc)) "SCORE: s/d" else paste0("SCORE: ", round(sc, 0))
}

# Donut estático en ggplot (se usa para la descarga en PNG):
# porcentaje adentro, referencias abajo
donut_ggplot <- function(df, titulo, score_txt = NULL) {
  ggplot(df, aes(x = 2, y = n, fill = cat)) +
    geom_col(width = 1, color = "white") +
    geom_text(aes(label = etiqueta),
              position = position_stack(vjust = 0.5),
              color = color_texto_dona, fontface = "bold", size = 5) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = colores_graficos_dona, name = NULL) +
    labs(title = titulo, subtitle = score_txt, caption = texto_fuente) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12, color = color_texto_dona),
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(size = 12, color = "#8A8D91", hjust = 0.5,
                                       margin = margin(t = 4, b = 6)),
          plot.caption = element_text(size = 9, color = "#333333", hjust = 0,
                                      margin = margin(t = 10)),
          plot.caption.position = "plot",
          plot.margin = margin(10, 10, 10, 10))
}

# Donut interactivo en plotly (el que se ve en la app):
# solo el porcentaje, del mismo tamaño en todas las categorías, y referencias abajo
donut_plotly <- function(df) {
  # Si no quedó ninguna respuesta válida (todo NA), se muestra un aviso en vez de fallar
  if (nrow(df) == 0) {
    return(
      plot_ly() |>
        layout(
          annotations = list(text = "Sin datos para mostrar", showarrow = FALSE,
                             font = list(size = 14, color = "#666666")),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
          paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)"
        ) |>
        config(displayModeBar = FALSE)
    )
  }
  # Plotly con direction = "clockwise" y los datos invertidos deja la primera categoría
  # justo a la izquierda de las 12 hs y las siguientes en sentido antihorario
  # (igual que el ggplot). La leyenda se invierte de nuevo más abajo para que
  # las referencias salgan en el orden de los levels.
  df <- df[rev(seq_len(nrow(df))), ] |>
    mutate(
      cat_chr = as.character(cat),
      texto = paste0("<b>", etiqueta, "</b>"),
      color = unname(colores_graficos_dona[cat_chr]),
      hover = paste0(cat_chr, "<br>", n, " productores (", round(porcentaje, 0), "%)")
    )
  
  plot_ly() |>
    add_trace(
      type = "pie",
      labels = df$cat_chr,
      values = df$n,
      # sort = FALSE: respeta el orden de los datos (si no, plotly ordena de mayor a menor)
      sort = FALSE,
      direction = "clockwise",
      hole = 0.45,
      text = df$texto,
      textinfo = "text",
      textposition = posicion_porcentaje_dona,
      insidetextorientation = "horizontal",
      insidetextfont = list(color = color_texto_dona, size = tamanio_texto_dona),
      outsidetextfont = list(color = color_texto_dona, size = tamanio_texto_dona),
      automargin = TRUE,
      marker = list(colors = df$color,
                    line = list(color = "white", width = 2)),
      hovertext = df$hover,
      hoverinfo = "text",
      showlegend = TRUE
    ) |>
    layout(
      showlegend = TRUE,
      # Referencias abajo del gráfico, en una fila centrada y en el orden de los levels
      legend = list(orientation = "h", x = 0.5, xanchor = "center",
                    y = -0.1, yanchor = "top",
                    traceorder = "reversed",
                    font = list(size = tamanio_leyenda_dona, color = color_texto_dona)),
      # Tamaño de letra fijo para todos los porcentajes
      uniformtext = list(minsize = tamanio_texto_dona, mode = "show"),
      # Márgenes con aire para que entren los porcentajes que van afuera
      margin = list(l = 40, r = 40, t = 40, b = 90),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    ) |>
    config(displayModeBar = FALSE)
}

# Tarjeta de una pregunta (mismo estilo que las tarjetas de variaciones)
tarjeta_pregunta <- function(id, titulo) {
  div(
    class = "card-sutil",
    div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
      div(class = "card-title-custom",
          style = "margin-bottom: 0; border-bottom: none; padding-bottom: 0;", titulo),
      downloadButton(paste0("descargar_", id), "",
                     class = "btn btn-sm btn-outline-secondary", title = "Descargar")
    ),
    # Score: alineado con el título, más chico y más claro
    uiOutput(paste0("score_", id)),
    plotlyOutput(paste0("grafico_", id), height = "400px"),
    # Fuente al pie de la tarjeta (mismo estilo que en el gráfico de la serie)
    div(
      style = "text-align: left; margin-top: 10px; font-size: 0.85rem; color: #333333; line-height: 1.4;",
      texto_fuente
    )
  )
}





####################################################
# User Interface
####################################################

ui <- page_navbar(
  # Logo a la izquierda; el título y el logo del Ag Barometer van a la derecha vía JS (ver footer)
  title = tags$img(src = "logo_dir/logo_austral_ceag_transparente.png", height = "44px",
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
      /* Tarjeta del número de productores encuestados (mismos efectos, fondo propio) */
      .card-encuestados {
        background: #CBA6AA;
        margin-bottom: 10px;
        padding: 18px 22px;
      }
      .card-encuestados .encuestados-titulo {
        font-size: 0.95rem;
        font-weight: 700;
        color: #1B1F23;
        margin-bottom: 6px;
      }
      .card-encuestados .encuestados-numero {
        font-size: 2.4rem;
        font-weight: 800;
        line-height: 1.1;
        color: #1B1F23;
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
      /* Texto de la pestaña Bienvenida: tipografía un poco más gruesa.
         Segoe UI no tiene un peso intermedio real, por eso se suma un trazo finito.
         Para más grosor, subí el 0.25px; para menos, bajalo (o ponelo en 0). */
      .texto-bienvenida {
        font-weight: 500;
        -webkit-text-stroke: 0.25px currentColor;
      }
      .texto-bienvenida p {
        margin-bottom: 12px;
      }
      /* Filtro de período: el inicio queda fijo, se oculta su punto para que no parezca movible */
      #selector_rango_fechas_ui .irs-handle.from {
        display: none !important;
        pointer-events: none !important;
      }
      /* Filtro de período más limpio: sin la línea gris horizontal,
         marcas más suaves y fechas más espaciadas
         (el padding de las fechas es la separación: más padding = menos fechas) */
      #selector_rango_fechas_ui .irs-line {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
      }
      /* Oculta la línea gris que marca el límite mínimo (Nov 2019); el límite sigue funcionando igual */
      #selector_rango_fechas_ui .irs-shadow {
        display: none !important;
      }
      #selector_rango_fechas_ui .irs-grid-pol {
        opacity: 0.3;
      }
      #selector_rango_fechas_ui .irs-grid-text {
        padding: 0 22px;
        font-size: 0.75rem;
        color: #8a8a8a;
      }
    "))
  ),
  
  # Título + logo del Ag Barometer a la derecha de la barra (mismo alto que el logo de la izquierda, no agranda la banda)
  footer = tags$script(HTML("
    $(document).ready(function() {
      var tituloHtml = '<div class=\"navbar-titulo-derecha\"><span style=\"font-weight: bold; letter-spacing: 0.5px; color: white; font-size: 1.25rem;\">Ag Barometer Austral</span><img src=\"logo_dir/logo_agbarometer_blanco.png\" height=\"38px\" style=\"margin-left: 10px; background: transparent;\"></div>';
      $('.navbar-nav').last().after(tituloHtml);
    });
  ")),
  
  # ------------------------------------------------------------- #
  # PESTAÑA 1: bienvenida (texto directamente sobre el lienzo, sin tarjeta)
  # ------------------------------------------------------------- #
  nav_panel(
    title = "Bienvenida",
    fluidRow(
      column(
        width = 10, offset = 1,
        div(
          style = "padding: 26px 24px 24px 24px;",
          div(
            class = "texto-bienvenida",
            style = "font-size: 1.05rem; line-height: 1.75; color: #222222; text-align: justify;",
            p("El ", tags$em("Ag Barometer Austral"), " es un índice que elabora el Centro de Agronegocios y Alimentos de la Universidad Austral (CEAg). El mismo proporciona bimestralmente medidas nacionales sobre las percepciones del sector agropecuario en la Argentina que muestran la evolución de su confianza a lo largo del año."),
            div(
              style = "text-align: center; margin: 25px 0;",
              tags$img(src = "logo_dir/logo_agbarometer_color.png", style = "height: 110px !important; width: auto;")
            ),
            p("El relevamiento recoge las respuestas de encuestas a 400 productores, cuyo valor bruto de producción es igual o mayor a USD 200.000 y es realizada por Map of Agriculture."),
            p("La muestra se ha estratificado basada en datos de INDEC y Map of Agriculture, para que los resultados representen correctamente el universo total."),
            p("En cada relevamiento se efectúan las siguientes cinco preguntas fijas a los productores:"),
            p(tags$b("Pregunta 1:"), " ¿Ud. diría que su operación actualmente está mejor, peor o igual financieramente con respecto a un año atrás?"),
            p(tags$b("Pregunta 2:"), " Mirando hacia adelante: de acá a un año ¿considera que su explotación estará mejor, peor o igual financieramente?"),
            p(tags$b("Pregunta 3:"), " Pensando ahora en la economía agropecuaria en general ¿considera que los próximos meses serán: buenos tiempos hablando financieramente o malos tiempos?"),
            p(tags$b("Pregunta 4:"), " Mirando hacia adelante ¿qué situación diría que es más probable, el sector agropecuario argentino en los próximos 5 años, tendrá mayormente buenos tiempos o mayormente malos tiempos?"),
            p(tags$b("Pregunta 5:"), " Pensando en inversiones importantes – como maquinaria, compra de tierras, cabezas de ganado, capacidad de almacenamiento, bioenergía- en general ¿Ud. considera que hoy es un buen momento o mal momento para realizar inversiones?"),
            p("Para más información acerca del relevamiento, consultar el siguiente ",
              tags$a("link",
                     href = "https://www.austral.edu.ar/rosario/agronegocios/investigacion-y-transferencia/barometer/",
                     target = "_blank",
                     style = "color: #7D212B; font-weight: 600;"),
              ".")
          ),
          hr(),
          div(
            style = "text-align: center; margin-top: 10px;",
            p(style = "font-size: 1.05rem; font-weight: 700; color: #7D212B; margin-bottom: 20px;",
              "Nos acompañan en la construcción de los índices:"),
            div(
              style = "display: flex; align-items: center; justify-content: center; gap: 40px; flex-wrap: wrap;",
              tags$img(src = "logo_dir/logo_basf.jpg", style = "height: 80px !important; width: auto;"),
              tags$img(src = "logo_dir/logo_john_deere.png", style = "height: 80px !important; width: auto;")
            )
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
    
    # FILA SUPERIOR: tarjeta de productores encuestados (izquierda) + descarga de la base (derecha)
    div(
      class = "row align-items-center",
      column(
        width = 4,
        uiOutput("tarjeta_encuestados")
      ),
      column(
        width = 8,
        div(
          style = "display: flex; align-items: center; justify-content: flex-end; gap: 10px; margin-bottom: 10px;",
          tags$span(style = "font-size: 0.85rem; font-weight: 700; color: #7D212B; text-transform: uppercase; letter-spacing: 1px;", "Descargar base:"),
          downloadButton("btn_descargar_xlsx", "Excel (.xlsx)", icon = icon("file-excel"),
                         class = "btn btn-custom-primary"),
          downloadButton("btn_descargar_csv", "CSV (.csv)", icon = icon("file-csv"),
                         class = "btn btn-sm btn-outline-secondary")
        )
      )
    ),
    
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
          )
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
          plotlyOutput("grafico_evolucion", height = "500px"),
          div(
            style = "text-align: left; margin-top: 15px; font-size: 0.85rem; color: #333333; line-height: 1.4;",
            uiOutput("referencias_UI")
          )
        )
      )
    ),
    
    # ----------------------------------------------------------- #
    # RESULTADOS DE LAS PREGUNTAS
    # (todo dentro de una columna de 10/12 para que las tarjetas queden más chicas y con más margen)
    # ----------------------------------------------------------- #
    fluidRow(
      column(
        width = 12,
        div(class = "section-title-clean", "Resultados")
      )
    ),
    
    fluidRow(
      column(
        width = 10, offset = 1,
        # Preguntas 1 y 2
        fluidRow(
          column(width = 6, tarjeta_pregunta("Q1", "Pregunta 1")),
          column(width = 6, tarjeta_pregunta("Q2", "Pregunta 2"))
        ),
        # Preguntas 3 y 4
        fluidRow(
          column(width = 6, tarjeta_pregunta("Q3", "Pregunta 3")),
          column(width = 6, tarjeta_pregunta("Q4", "Pregunta 4"))
        ),
        # Pregunta 5 (centrada)
        fluidRow(
          column(width = 6, offset = 3, tarjeta_pregunta("Q5", "Pregunta 5"))
        )
      )
    )
  )
)

####################################################
# Server
####################################################

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data = {
      archivo_a_leer <- if (file.exists(ruta_data)) ruta_data else file.path("Datos", "agbarometer_ARG_imputado.xlsx")
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
  
  # ----------------------------------------------------------- #
  # Tarjeta: número de productores encuestados en el bimestre más reciente
  # ----------------------------------------------------------- #
  
  output$tarjeta_encuestados <- renderUI({
    bimestre_reciente <- max(rv$data$Bimestre, na.rm = TRUE)
    
    div(
      class = "card-sutil card-encuestados",
      div(class = "encuestados-titulo",
          paste0("Número de productores encuestados en ", etiquetas_bimestres(bimestre_reciente))),
      div(class = "encuestados-numero",
          format(nrow(datos_preguntas), big.mark = ".", decimal.mark = ","))
    )
  })
  
  # ----------------------------------------------------------- #
  # Descarga de la base (Bimestre + las 3 series)
  # ----------------------------------------------------------- #
  
  base_descarga <- reactive({
    rv$data |>
      arrange(Bimestre) |>
      select(Bimestre, `Ag Barometer Austral`, `Condiciones presentes`, `Expectativas futuras`)
  })
  
  output$btn_descargar_xlsx <- downloadHandler(
    filename = function() {
      "ag_barometer_austral.xlsx"
    },
    content = function(file) {
      write_xlsx(base_descarga(), path = file)
    }
  )
  
  output$btn_descargar_csv <- downloadHandler(
    filename = function() {
      "ag_barometer_austral.csv"
    },
    content = function(file) {
      write.csv(base_descarga(), file, row.names = FALSE)
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
  
  armar_grafico_estatico <- function() {
    graficar_eventos_app(
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
  }
  
  objeto_grafico_reactivo <- reactive({
    ggplotly(armar_grafico_estatico(), tooltip = "text") |>
      layout(
        hovermode = "x unified",
        # Leyenda anclada debajo del área del gráfico con margen inferior fijo.
        # autoexpand = FALSE evita que plotly agrande el margen cuando la leyenda crece
        # (más series), así el área del gráfico mantiene siempre el mismo tamaño.
        # xanchor = "left" fija el inicio de la leyenda en el borde izquierdo del gráfico (antes quedaba corrida y se cortaba).
        # El margen es lo bastante alto para que entren las referencias completas (incluso en 2 o 3 filas)
        legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.25, yanchor = "top"),
        margin = list(b = 170, autoexpand = FALSE),
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
      plotlyOutput("grafico_zoom_modal", height = "600px"),
      footer = modalButton("Cerrar"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  output$grafico_zoom_modal <- renderPlotly({
    objeto_grafico_reactivo()
  })
  
  # ----------------------------------------------------------- #
  # Descargas (Gráfico de evolución)
  # ----------------------------------------------------------- #
  
  output$btn_descargar_grafico <- downloadHandler(
    filename = function() {
      paste0("evolucion_ag_barometer_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = armar_grafico_estatico(), width = 11, height = 8.5, dpi = 150, device = "png")
    }
  )
  
  # ----------------------------------------------------------- #
  # Resultados de las preguntas: donuts (interactivos + descarga PNG)
  # ----------------------------------------------------------- #
  
  for (q in names(niveles_preguntas)) {
    local({
      id <- q
      niveles <- niveles_preguntas[[id]]
      titulo <- paste("Pregunta", sub("Q", "", id))
      
      output[[paste0("score_", id)]] <- renderUI({
        div(style = "font-size: 0.9rem; font-weight: 500; color: #8A8D91; letter-spacing: 0.5px; margin-bottom: 6px;",
            texto_score(datos_preguntas, id))
      })
      
      output[[paste0("grafico_", id)]] <- renderPlotly({
        donut_plotly(resumir_pregunta(datos_preguntas, id, niveles))
      })
      
      output[[paste0("descargar_", id)]] <- downloadHandler(
        filename = function() {
          paste0("pregunta_", sub("Q", "", id), "_", Sys.Date(), ".png")
        },
        content = function(file) {
          p <- donut_ggplot(resumir_pregunta(datos_preguntas, id, niveles), titulo,
                            score_txt = texto_score(datos_preguntas, id))
          ggsave(file, plot = p, width = 7, height = 7, dpi = 150, device = "png", bg = "white")
        }
      )
    })
  }
}

shinyApp(ui, server)





