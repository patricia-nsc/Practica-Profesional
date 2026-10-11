# ----------------------------------------------------------- #
# Librerias
# ----------------------------------------------------------- #

library(shiny)      # div, tags, tagList
library(dplyr)
library(ggplot2)
library(lubridate)  # year() en armar_boxplot




#############################################################
# funciones para la app
#############################################################


# -------------------------------------------------------------- #
# Funcion 1
# -------------------------------------------------------------- #


formatear_fecha_amigable <- function(fecha_date) {
  meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  m_idx <- as.numeric(format(fecha_date, "%m"))
  anio_fmt <- format(fecha_date, "%y")
  paste(meses_es[m_idx], anio_fmt)
}


# -------------------------------------------------------------- #
# Funcion 2
# -------------------------------------------------------------- #


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




# -------------------------------------------------------------- #
# Funcion 3
# -------------------------------------------------------------- #


graficar_eventos_app <- function(data, 
                                 elecciones_df = NULL, 
                                 ancho_sombreado = 3,
                                 tiempo_col = Bimestre, 
                                 series_seleccionadas = c("Ag Barometer Austral"), 
                                 colores_series = c("Ag Barometer Austral" = "#8FB933"),
                                 fuente = NULL) {  
  
  tcol <- rlang::as_name(rlang::ensym(tiempo_col))
  
  # Datos con columna 'fecha' tipo Date primero
  data <- data |> dplyr::mutate(fecha = as.Date(.data[[tcol]]))
  
  cols_a_pivotear <- intersect(series_seleccionadas, names(data))
  if (length(cols_a_pivotear) == 0) cols_a_pivotear <- "Ag Barometer Austral"
  
  data_larga <- data |>  
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols_a_pivotear),
      names_to = "Indice",
      values_to = "valor"
    )
  
  max_valor_datos <- max(data_larga$valor, na.rm = TRUE)
  min_valor_datos <- min(data_larga$valor, na.rm = TRUE)
  
  y_min <- min(30, floor(min_valor_datos / 10) * 10 - 10)
  y_max <- max(190, ceiling(max_valor_datos / 10) * 10 + 10)
  
  # Desplaza fechas n meses (n puede ser negativo)
  mover_meses <- function(f, n) {
    as.Date(vapply(f, function(x) {
      as.character(seq(x, by = paste(n, "months"), length.out = 2)[2])
    }, character(1)))
  }
  
  # Hitos
  hitos_proc <- NULL
  if (!is.null(elecciones_df) && nrow(elecciones_df) > 0) {
    hitos_proc <- elecciones_df |> 
      dplyr::mutate(fecha = as.Date(.data[[tcol]])) |> 
      dplyr::arrange(fecha) |> 
      dplyr::mutate(
        Tag     = as.character(dplyr::row_number()),
        xmin    = mover_meses(fecha, -ancho_sombreado),
        xmax    = mover_meses(fecha,  ancho_sombreado),
        ymin    = y_min,
        ymax    = y_max,
        x_texto = fecha + round(ancho_sombreado * 30.4 / 2),
        Y_pos   = y_max - 8
      )
    
    texto_hitos <- paste(
      paste0(hitos_proc$Tag, ": ", hitos_proc$Descripcion),
      collapse = "\n"
    )
    leyenda_completa <- paste("Eventos:\n", texto_hitos, sep = "")
  } else {
    leyenda_completa <- ""
  }
  
  if (!is.null(fuente) && fuente != "") {
    leyenda_completa <- if (leyenda_completa == "") fuente else paste0(leyenda_completa, "\n", fuente)
  }
  
  p <- ggplot2::ggplot(
    data_larga,
    ggplot2::aes(x = fecha, y = valor, color = Indice, group = Indice,
                 text = paste0("<b>", Indice, "</b><br>Fecha: ",
                               format(fecha, "%b %Y"), "<br>Valor: ", round(valor, 1)))
  ) +
    ggplot2::geom_hline(yintercept = 100, linetype = "dashed", color = "#B3B3B3", linewidth = 0.7) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1.3)
  
  if (!is.null(hitos_proc) && nrow(hitos_proc) > 0) {
    p <- p +
      ggplot2::geom_rect(
        data = hitos_proc,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill = "#B3B3B3", alpha = 0.35, inherit.aes = FALSE
      ) +
      ggplot2::geom_vline(
        data = hitos_proc,
        ggplot2::aes(xintercept = fecha),
        color = "#666666", linetype = "dashed", linewidth = 0.6, inherit.aes = FALSE
      ) +
      ggplot2::geom_text(
        data = hitos_proc,
        ggplot2::aes(x = x_texto, y = Y_pos, label = Tag),
        fontface = "bold", color = "black", size = 5, inherit.aes = FALSE
      )
  }
  
  p +
    ggplot2::scale_color_manual(values = colores_series) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_width(20),
      limits = c(y_min, y_max),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = "Tiempo", y = "Índice", caption = leyenda_completa) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray20", lineheight = 1.2),
      plot.caption.position = "plot"
    )
}


# mas funciones





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
  
  previo <- df  |> 
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
