
# Librerias
library(forecast)
library(tidyverse)
library(ggplot2)
library(tsibble)
library(ggrepel)
library(dplyr)
library(TSA)            # 3 librerias q uso p crear ccf_preblanqueadas() + ggplot2
library(dplyr)
library(rlang)
library(ggplot2)   # para la fc 3 de la app
library(dplyr)
library(tidyr)
library(tsibble)
library(ggrepel)





#############################################################
# funciones para el informe
#############################################################




# -------------------------------------------------------------- #
# Funcion 1
# -------------------------------------------------------------- #

# sirve para estimar la ccf entre las series pre-blanqueadas
ccf_preblanqueadas <- function(data, x, y, lag_max = 18, lab_x = NULL, lab_y = NULL, fuente = NULL) {
  
  x_var <- enquo(x)
  y_var <- enquo(y)
  
  x_vec <- data |> pull({{ x }})
  y_vec <- data |> pull({{ y }})
  
  pw <- prewhiten(x = x_vec, y = y_vec, lag.max = lag_max, plot = FALSE)
  
  df <- data.frame(
    lag = as.numeric(pw$ccf$lag),
    ccf = as.numeric(pw$ccf$acf)
  )
  
  ic <- qnorm(0.975) / sqrt(pw$ccf$n.used) # 1.96/sqrt(# obs usadas en ccf)
  
  if (is.null(lab_x)) lab_x <- as_label(x_var)
  if (is.null(lab_y)) lab_y <- as_label(y_var)
  
  caption <- paste0("x = ", lab_x, "\ny = ", lab_y)
  if (!is.null(fuente)) caption <- paste0(caption, "\n", fuente)
  
  ggplot(df, aes(lag, ccf)) +
    geom_col(width = 0.15, fill = "black") +
    geom_hline(yintercept = c(-ic, ic), linetype = "dashed", color = "#666666") +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ylim(-1, 1) +
    scale_x_continuous(breaks = seq(-lag_max, lag_max, by = 3)) +
    labs(
      x = "Retardo",
      y = "FCCM",
      caption = caption
    ) +
    theme_bw()
}





# -------------------------------------------------------------- #
# Funcion 2
# -------------------------------------------------------------- #

correlaciones_preblanqueadas <- function(df, y_col, x_cols, fuente = NULL) {
  
  # Para que sirve: sirve para ver la relacion entre la Y_t y cada X_t, presentado
  # todos los graficos de dispersion entre Y_t vs cada X_t. 
  # En vez de hacer esos graficos con las series originales, primero aplica
  # prewhitening a todas las series y grafica las sesries transformadas porque
  # asumo que todas las X y la Y son no estacionarias, ents antes de 
  
  # Argumentos entrada:
  # df: dataframe con las series
  # y_col: str: nombre de la columna de la variable interes
  # x_cols: names(df)[3:ncol(df)] , suponiendo q desde la col 3 hasta la ultima 
  # son todas las covariables
  # fuente: str (opcional): texto de la fuente, se agrega en una nueva linea
  # debajo del caption del grafico
  
  # Para obtener el grafico se hace: 
  # correlaciones_preblanqueadas(df, y_col, x_cols)$grafico
  
  lista_preblanqueada <- list()
  y_original <- df[[y_col]]
  
  for (col in x_cols) {
    x <- df[[col]]
    validos <- complete.cases(x, y_original)
    
    # ajusto ARIMA a X y uso eso misma transformacion para Y
    fit_x <- auto.arima(x[validos], seasonal = TRUE, ic = "aicc")
    x_whitened <- residuals(fit_x)
    
    fit_y <- Arima(y_original[validos], model = fit_x)
    y_whitened <- residuals(fit_y)
    
    lista_preblanqueada[[col]] <- tibble(
      Fecha = df$Fecha[validos],
      variable = col,
      x_pre = x_whitened,
      y_pre = y_whitened
    )
  }
  
  d_pre_long <- bind_rows(lista_preblanqueada)
  
  # calculo correlaciones
  df_cor_pre <- d_pre_long |>
    group_by(variable) |>
    summarise(r = cor(x_pre, y_pre, method = "pearson")) |>
    arrange(r) |>
    mutate(variable = factor(variable, levels = variable))
  
  # caption del gráfico
  caption <- "r: coeficiente de correlación de Pearson estimado"
  if (!is.null(fuente)) caption <- paste0(caption, "\n", fuente)
  
  # gráfico de correlaciones
  p <- d_pre_long |>
    left_join(df_cor_pre, by = "variable") |>
    mutate(label = paste0(variable, "\nr = ", round(r, 2)),
           label = reorder(label, -r)) |>
    ggplot(aes(x = x_pre, y = y_pre)) +
    geom_point(alpha = 1, color = "black", size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, color = "#B17A80", linewidth = 0.8) +
    facet_wrap(~label, scales = "free", ncol = 4) +
    labs(
      y = paste0(y_col),
      caption = caption
    ) +
    theme_bw(base_size = 10) +
    theme(axis.title.x = element_blank())
  
  return(list(datos = d_pre_long, correlaciones = df_cor_pre, grafico = p))
}



# -------------------------------------------------------------- #
# Funcion 3
# -------------------------------------------------------------- #

# grafico serie + hitos con: raya, sombreado, numero y leyenda

graficar_elecciones <- function(data, 
                                elecciones_df = NULL, 
                                ancho_sombreado = 3,
                                tiempo_col = Bimestre, 
                                serie = `Ag Barometer Austral`, 
                                color_serie = "#6D9DC5",
                                fuente = NULL) {  
  
  # Manejo seguro si no hay hitos
  if (is.null(elecciones_df) || nrow(elecciones_df) == 0) {
    elecciones_df <- data.frame(t_dummy = numeric(), Tag = character(), Descripcion = character())
    names(elecciones_df)[1] <- rlang::as_string(rlang::ensym(tiempo_col))
  }
  
  # Procesamiento dinámico del sombreado usando el nombre de la columna de tiempo elegida
  if (nrow(elecciones_df) > 0) {
    hitos_proc <- elecciones_df |> 
      mutate(
        xmin = {{ tiempo_col }} - ancho_sombreado,
        xmax = {{ tiempo_col }} + ancho_sombreado,
        Y_pos = 155
      )
    
    texto_hitos <- paste(
      paste0(hitos_proc$Tag, ": ", hitos_proc$Descripcion),
      collapse = "\n"
    )
    leyenda_completa <- paste("Ganador de cada elección presidencial:", texto_hitos, sep = "\n")
  } else {
    leyenda_completa <- ""
  }
  
  # Agrego la fuente en una nueva línea (si se especificó)
  if (!is.null(fuente)) {
    leyenda_completa <- if (leyenda_completa == "") fuente else paste0(leyenda_completa, "\n", fuente)
  }
  
  # Gráfico base
  p <- data |>  
    ggplot(aes(x = {{ tiempo_col }}, y = {{ serie }})) +
    
    # Línea de referencia horizontal (100)
    geom_hline(yintercept = 100, linetype = "dashed", color = "#B3B3B3", linewidth = 0.7) +
    
    # Serie principal con el color personalizado
    geom_line(color = color_serie) +
    geom_point(color = color_serie, size = 1)
  
  # Capas de hitos condicionales
  if (nrow(elecciones_df) > 0) {
    p <- p +
      geom_rect(
        data = hitos_proc,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = "#B3B3B3", alpha = 0.4, inherit.aes = FALSE # sombreado
      ) +
      geom_vline(
        data = hitos_proc, 
        aes(xintercept = {{ tiempo_col }}), 
        color = "#666666", linetype = "dashed", linewidth = 0.6 # raya
      ) +
      geom_text_repel(
        data = hitos_proc, 
        aes(x = {{ tiempo_col }}, y = Y_pos, label = Tag),
        fontface = "bold", color = "black", size = 3.5,
        point.padding = 0.1, box.padding = 0.2, segment.color = NA,
        inherit.aes = FALSE
      )
  }
  
  # Escalas, etiquetas y tema
  p +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = scales::breaks_width(20)) +
    labs(
      x = "Tiempo", 
      y = rlang::as_label(rlang::enquo(serie)), # Usa automáticamente el nombre de la variable como título del eje
      caption = leyenda_completa
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      plot.caption = element_text(hjust = 0, size = 8.5, color = "gray20", lineheight = 1.2),
      plot.caption.position = "plot"
    )
}

# -------------------------------------------------------------- #
# Funcion 4
# -------------------------------------------------------------- #


# lo mismo pero cuando quiero graficar dos series (grafico eventos globales)

graficar_hitos_globales <- function(data, 
                                    hitos_df = NULL, 
                                    ancho_sombreado = 3,
                                    tiempo_col = Fecha, 
                                    colores_series = c("Ag Barometer Austral" = "#6D9DC5", 
                                                       "Ag Economy Barometer" = "#BF211E"),
                                    fuente = NULL) {
  
  # Manejo seguro si no hay hitos
  if (is.null(hitos_df) || nrow(hitos_df) == 0) {
    hitos_df <- data.frame(t_dummy = numeric(), Tag = character(), Descripcion = character())
    names(hitos_df)[1] <- rlang::as_string(rlang::ensym(tiempo_col))
  }
  
  # Procesamiento dinámico del sombreado
  if (nrow(hitos_df) > 0) {
    hitos_proc <- hitos_df |> 
      mutate(
        xmin = {{ tiempo_col }} - ancho_sombreado,
        xmax = {{ tiempo_col }} + ancho_sombreado,
        Y_pos = 180
      )
    
    texto_hitos <- paste(
      paste0(hitos_proc$Tag, ": ", hitos_proc$Descripcion),
      collapse = "\n"
    )
    leyenda_completa <- paste("Hitos globales/bélicos:", texto_hitos, sep = "\n")
  } else {
    leyenda_completa <- ""
  }
  
  # Agrego la fuente en una nueva línea (si se especificó)
  if (!is.null(fuente)) {
    leyenda_completa <- if (leyenda_completa == "") fuente else paste0(leyenda_completa, "\n", fuente)
  }
  
  # Transformación de la data (pivot_longer dentro de la función)
  data_larga <- data |> 
    as_tibble() |> 
    tidyr::pivot_longer(
      cols = c(`Ag Barometer Austral`, `Ag Economy Barometer`), 
      names_to = "Índice", 
      values_to = "valor"
    ) |> 
    tidyr::drop_na(valor)
  
  # Gráfico base
  p <- data_larga |> 
    ggplot(aes(x = {{ tiempo_col }}, y = valor, color = Índice, group = Índice)) +
    
    # Línea de referencia en 100
    geom_hline(yintercept = 100, linetype = "dashed", color = "#666666", linewidth = 0.7)
  
  # Capas de hitos (sombreado, líneas verticales y etiquetas)
  if (nrow(hitos_df) > 0) {
    p <- p +
      geom_rect(
        data = hitos_proc,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        fill = "#B3B3B3", alpha = 0.4, inherit.aes = FALSE
      ) +
      geom_vline(
        data = hitos_proc, 
        aes(xintercept = {{ tiempo_col }}), 
        color = "#666666", linetype = "dashed", linewidth = 0.6,
        inherit.aes = FALSE
      ) +
      geom_text_repel(
        data = hitos_proc, 
        aes(x = {{ tiempo_col }}, y = Y_pos, label = Tag),
        fontface = "bold", color = "black", size = 3.8,
        point.padding = 0.1, box.padding = 0.2, segment.color = NA,
        inherit.aes = FALSE
      )
  }
  
  # Series, escalas y temas
  p +
    geom_line() +
    geom_point(size = 1) +
    scale_x_yearmonth(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(breaks = scales::breaks_width(20)) +
    scale_color_manual(values = colores_series) +
    labs(
      x = "Tiempo", 
      y = "Ag Barometer",
      caption = leyenda_completa
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(hjust = 0, size = 8, color = "gray20", lineheight = 1.2),
      plot.caption.position = "plot"
    )
}

# -------------------------------------------------------------- #
# Funcion 5
# -------------------------------------------------------------- #

resultado_test <- function(test, pvalue){
  
  # argumentos:
  # test = str: adf, kpss o ljung
  # pvalue = nro
  
  resultado <- ""
  
  if(test == "adf"){
    resultado <- if(pvalue <= 0.05) "Estacionaria" else "No Estac."
    
  } else if(test == "kpss"){
    resultado <- if(pvalue <= 0.05) "No Estac." else "Estacionaria"
    
  } else if(test == "ljung"){
    resultado <- if(pvalue <= 0.05) "No ruido b." else "Ruido blanco"
    
  } else {
    resultado <- "Test no válido"
  }
  
  return(resultado)
}


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
  
  y_min <- 30
  y_max <- 190
  tcol  <- rlang::as_name(rlang::ensym(tiempo_col))
  
  # Desplaza fechas n meses (n puede ser negativo)
  mover_meses <- function(f, n) {
    as.Date(vapply(f, function(x) {
      as.character(seq(x, by = paste(n, "months"), length.out = 2)[2])
    }, character(1)))
  }
  
  # Datos con columna 'fecha' tipo Date (evita problemas de yearmonth en plotly)
  data <- data |> dplyr::mutate(fecha = as.Date(.data[[tcol]]))
  
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
        # Número: a la derecha de la raya, a mitad de camino del borde del sombreado
        x_texto = fecha + round(ancho_sombreado * 30.4 / 2),
        Y_pos   = y_max - 8
      )
  }
  
  cols_a_pivotear <- intersect(series_seleccionadas, names(data))
  if (length(cols_a_pivotear) == 0) cols_a_pivotear <- "Ag Barometer Austral"
  
  data_larga <- data |>  
    tidyr::pivot_longer(
      cols = dplyr::all_of(cols_a_pivotear),
      names_to = "Indice",
      values_to = "valor"
    )
  
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
    ggplot2::labs(x = "Tiempo", y = "Índice") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
}