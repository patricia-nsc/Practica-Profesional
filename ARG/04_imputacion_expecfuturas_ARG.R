library(readxl)
library(tsibble)
library(tidyverse)
library(imputeTS)
library(forecast)

# ------------------------------------------------------------- #
# Carga de datos - Serie IRREGULAR (no se fuerza frecuencia)
# ------------------------------------------------------------- # 

d <- readxl::read_excel("agbarometerARG_enero2026.xlsx") |>
  mutate(Bimestre = yearmonth(Bimestre)) |>
  as_tsibble(index = Bimestre, regular = FALSE)

# Trabajamos directamente sobre el VECTOR NUMÉRICO de valores.
# No usamos ts() con frecuencia inventada — eso sería mentirle a los métodos.
valores_originales <- d$`Expectativas futuras`
n <- length(valores_originales)

# Para los métodos que internamente necesitan un objeto ts (seadec, seasplit, kalman),
# usamos frequency=1 (sin estructura estacional). Esto es honesto: le decimos
# "no tenés información estacional confiable porque la serie es irregular".
# Si quisieras probar frequency=2 o =3, podés cambiar FREQ abajo.
FREQ <- 1  # <-- ajustá si querés probar con otra frecuencia

# ------------------------------------------------------------- #
# Función de imputación sobre un vector con NAs
# Devuelve vector imputado. Usa tryCatch para métodos que pueden fallar.
# ------------------------------------------------------------- #

imputar_todos <- function(vec_na, freq = FREQ) {
  ts_na <- ts(vec_na, frequency = freq)
  
  safe_impute <- function(expr) {
    tryCatch(as.numeric(expr), error = function(e) rep(NA_real_, length(vec_na)))
  }
  
  list(
    random        = safe_impute(na_random(vec_na)),
    replace       = safe_impute(na_replace(vec_na)),
    mean          = safe_impute(na_mean(vec_na, option = "mean")),
    median        = safe_impute(na_mean(vec_na, option = "median")),
    mode          = safe_impute(na_mean(vec_na, option = "mode")),
    ilinear       = safe_impute(na_interpolation(vec_na, option = "linear")),
    ispline       = safe_impute(na_interpolation(vec_na, option = "spline")),
    istine        = safe_impute(na_interpolation(vec_na, option = "stine")),
    masimple      = safe_impute(na_ma(vec_na, weighting = "simple")),
    malinear      = safe_impute(na_ma(vec_na, weighting = "linear")),
    maexponential = safe_impute(na_ma(vec_na, weighting = "exponential")),
    locf          = safe_impute(na_locf(vec_na, option = "locf")),
    nocb          = safe_impute(na_locf(vec_na, option = "nocb")),
    seadec        = safe_impute(na_seadec(ts_na, algorithm = "interpolation")),
    seasplit      = safe_impute(na_seasplit(ts_na)),
    kalmans       = safe_impute(na_kalman(ts_na, model = "StructTS")),
    kalmana       = safe_impute(na_kalman(ts_na, model = "auto.arima"))
  )
}

# ------------------------------------------------------------- #
# Métricas de error (solo en las posiciones con NA simulado)
# ------------------------------------------------------------- #

calcular_errores <- function(imputado, original, idx_na) {
  diff  <- imputado[idx_na] - original[idx_na]
  rmse  <- sqrt(mean(diff^2, na.rm = TRUE))
  mae   <- mean(abs(diff), na.rm = TRUE)
  mape  <- mean(abs(diff) / abs(original[idx_na]) * 100, na.rm = TRUE)
  c(rmse = rmse, mae = mae, mape = mape)
}

# ------------------------------------------------------------- #
# Simulación: 100 iteraciones, 10% de datos faltantes aleatorios
# ------------------------------------------------------------- #

porcentaje_faltantes <- 0.10
n_iteraciones        <- 100
set.seed(123)

nombres_metodos <- c("random", "replace", "mean", "median", "mode",
                     "ilinear", "ispline", "istine",
                     "masimple", "malinear", "maexponential",
                     "locf", "nocb", "seadec", "seasplit",
                     "kalmans", "kalmana")
n_metodos <- length(nombres_metodos)

# Matrices de acumulación: filas = iteraciones, columnas = métodos
acum_rmse <- matrix(0, nrow = n_iteraciones, ncol = n_metodos,
                    dimnames = list(NULL, nombres_metodos))
acum_mae  <- matrix(0, nrow = n_iteraciones, ncol = n_metodos,
                    dimnames = list(NULL, nombres_metodos))
acum_mape <- matrix(0, nrow = n_iteraciones, ncol = n_metodos,
                    dimnames = list(NULL, nombres_metodos))

#cat("Corriendo", n_iteraciones, "iteraciones...\n")
pb <- txtProgressBar(min = 0, max = n_iteraciones, style = 3)

for (i in 1:n_iteraciones) {
  # 1. Simular NAs sobre el VECTOR (no sobre ts con frecuencia falsa)
  idx_na <- sample(seq_len(n), size = round(n * porcentaje_faltantes))
  vec_na <- valores_originales
  vec_na[idx_na] <- NA
  
  # 2. Imputar con los 17 métodos
  imputaciones <- imputar_todos(vec_na, freq = FREQ)
  
  # 3. Calcular errores para cada método
  for (metodo in nombres_metodos) {
    errs <- calcular_errores(imputaciones[[metodo]], valores_originales, idx_na)
    acum_rmse[i, metodo] <- errs["rmse"]
    acum_mae[i,  metodo] <- errs["mae"]
    acum_mape[i, metodo] <- errs["mape"]
  }
  
  setTxtProgressBar(pb, i)
}
close(pb)

# ------------------------------------------------------------- #
# Resultados: promedios por método + ranking
# ------------------------------------------------------------- #

resultados_promedio <- data.frame(
  Metodo = nombres_metodos,
  RMSE   = colMeans(acum_rmse, na.rm = TRUE),
  MAE    = colMeans(acum_mae,  na.rm = TRUE),
  MAPE   = colMeans(acum_mape, na.rm = TRUE),
  row.names = NULL
) |>
  arrange(MAPE)

#print(resultados_promedio)