library(tsibble)
library(readxl)
library(tidyverse)
library(tsibbledata)
library(fable)
library(fpp3)
library(forecast)
library(ggplot2)
library(ggfortify)
library(tseries)
library(imputeTS)
library(tidyr)
library(dplyr)
library(zoo)


# ------------------------------------------------------------- #
# Carga de datos Expectativas futuras ARG
# ------------------------------------------------------------- #


# Trabajo con la serie Expectativas futuras como si estuviera completa
d <- readxl::read_excel("ARG/agbarometerARG_enero2026.xlsx") |>
  mutate(Bimestre = yearmonth(Bimestre)) |>
  as_tsibble(index = Bimestre, regular = FALSE)

datosts <- ts(d$`Expectativas futuras`, frequency = 6) # (ene,mar,may,jul,sep,nov)
datostsNA <- datosts


# ------------------------------------------------------------- #
# Encontrar el mejor metodo de imputacion entre 17
# ------------------------------------------------------------- #

Box.test(datosts, lag = 30, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0)

errores_imputacion <- function(datosts, datostsNA, indices_na) {
  datosts_mean <- na_mean(datostsNA, option = "mean")
  rmse_media <- sqrt(mean((datosts_mean[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_media <- mean(abs(datosts_mean[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_media <- mean(abs(datosts_mean[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_median <- na_mean(datostsNA, option = "median")
  rmse_mediana <- sqrt(mean((datosts_median[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_mediana <- mean(abs(datosts_median[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_mediana <- mean(abs(datosts_median[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_mode <- na_mean(datostsNA, option = "mode")
  rmse_mode <- sqrt(mean((datosts_mode[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_mode <- mean(abs(datosts_mode[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_mode <- mean(abs(datosts_mode[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_replace <- na_replace(datostsNA)
  rmse_replace <- sqrt(mean((datosts_replace[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_replace <- mean(abs(datosts_replace[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_replace <- mean(abs(datosts_replace[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_random <- na_random(datostsNA)
  rmse_random <- sqrt(mean((datosts_random[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_random <- mean(abs(datosts_random[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_random <- mean(abs(datosts_random[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_ilinear <- na_interpolation(datostsNA, option = "linear")
  rmse_ilinear <- sqrt(mean((datosts_ilinear[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_ilinear <- mean(abs(datosts_ilinear[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_ilinear <- mean(abs(datosts_ilinear[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_ispline <- na_interpolation(datostsNA, option = "spline")
  rmse_ispline <- sqrt(mean((datosts_ispline[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_ispline <- mean(abs(datosts_ispline[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_ispline <- mean(abs(datosts_ispline[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_istine <- na_interpolation(datostsNA, option = "stine")
  rmse_istine <- sqrt(mean((datosts_istine[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_istine <- mean(abs(datosts_istine[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_istine <- mean(abs(datosts_istine[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_kalmans <- na_kalman(datostsNA, model = "StructTS")
  rmse_kalmans <- sqrt(mean((datosts_kalmans[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_kalmans <- mean(abs(datosts_kalmans[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_kalmans <- mean(abs(datosts_kalmans[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_kalmana <- na_kalman(datostsNA, model = "auto.arima")
  rmse_kalmana <- sqrt(mean((datosts_kalmana[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_kalmana <- mean(abs(datosts_kalmana[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_kalmana <- mean(abs(datosts_kalmana[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_locf <- na_locf(datostsNA, option = "locf")
  rmse_locf <- sqrt(mean((datosts_locf[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_locf <- mean(abs(datosts_locf[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_locf <- mean(abs(datosts_locf[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_nocb <- na_locf(datostsNA, option = "nocb")
  rmse_nocb <- sqrt(mean((datosts_nocb[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_nocb <- mean(abs(datosts_nocb[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_nocb <- mean(abs(datosts_nocb[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_masimple <- na_ma(datostsNA, weighting = "simple")
  rmse_masimple <- sqrt(mean((datosts_masimple[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_masimple <- mean(abs(datosts_masimple[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_masimple <- mean(abs(datosts_masimple[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_malinear <- na_ma(datostsNA, weighting = "linear")
  rmse_malinear <- sqrt(mean((datosts_malinear[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_malinear <- mean(abs(datosts_malinear[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_malinear <- mean(abs(datosts_malinear[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_maexponential <- na_ma(datostsNA, weighting = "exponential")
  rmse_maexponential <- sqrt(mean((datosts_maexponential[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_maexponential <- mean(abs(datosts_maexponential[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_maexponential <- mean(abs(datosts_maexponential[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_seadec <- na_seadec(datostsNA, algorithm = "interpolation")
  rmse_seadec <- sqrt(mean((datosts_seadec[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_seadec <- mean(abs(datosts_seadec[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_seadec <- mean(abs(datosts_seadec[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  datosts_seasplit <- na_seasplit(datostsNA)
  rmse_seasplit <- sqrt(mean((datosts_seasplit[indices_na] - datosts[indices_na])^2, na.rm = TRUE))
  mae_seasplit <- mean(abs(datosts_seasplit[indices_na] - datosts[indices_na]), na.rm = TRUE)
  mape_seasplit <- mean(abs(datosts_seasplit[indices_na] - datosts[indices_na])/abs(datosts[indices_na])*100, na.rm = TRUE)
  
  rmse_resultados = c(rmse_random, rmse_replace, rmse_media, rmse_mediana, rmse_mode, rmse_ilinear, rmse_ispline, rmse_istine,
                      rmse_masimple, rmse_malinear, rmse_maexponential, rmse_locf, rmse_nocb, rmse_seadec, rmse_seasplit,
                      rmse_kalmans, rmse_kalmana)
  mae_resultados = c(mae_random, mae_replace, mae_media, mae_mediana, mae_mode, mae_ilinear, mae_ispline, mae_istine,
                     mae_masimple, mae_malinear, mae_maexponential, mae_locf, mae_nocb, mae_seadec, mae_seasplit,
                     mae_kalmans, mae_kalmana)
  mape_resultados = c(mape_random, mape_replace, mape_media, mape_mediana, mape_mode, mape_ilinear, mape_ispline, mape_istine,
                      mape_masimple, mape_malinear, mape_maexponential, mape_locf, mape_nocb, mape_seadec, mape_seasplit,
                      mape_kalmans, mape_kalmana)
  
  resultados <- list(rmse = rmse_resultados, mae = mae_resultados, mape = mape_resultados)
  return(resultados)
}

################################### Mismo porcentaje ##################################

porcentaje_faltantes <- 0.1
set.seed(123)
n_iteraciones <- 100

acumulador_rmse <- rep(0, 17)
acumulador_mae <- rep(0, 17)
acumulador_mape <- rep(0, 17)

for (i in 1:n_iteraciones) {
  indices <- sample(1:length(datosts), size = round(length(datosts) * porcentaje_faltantes))
  datostsNA <- datosts
  datostsNA[indices] <- NA
  
  resultados <- errores_imputacion(datosts, datostsNA, indices)
  
  acumulador_rmse <- acumulador_rmse + resultados$rmse
  acumulador_mae <- acumulador_mae + resultados$mae
  acumulador_mape <- acumulador_mape + resultados$mape
}

promedio_rmse <- acumulador_rmse / n_iteraciones
promedio_mae <- acumulador_mae / n_iteraciones
promedio_mape <- acumulador_mape / n_iteraciones

resultados_promedio <- data.frame(
  Metodo = c("random", "replace", "mean", "median", "mode", "ilinear", "ispline", "istine",
             "masimple", "malinear", "maexponential", "locf", "nocb", "seadec", "seasplit",
             "kalmans", "kalmana"),
  RMSE = promedio_rmse,
  MAE = promedio_mae,
  MAPE = promedio_mape
)

#print(resultados_promedio) |> 
#  arrange(MAPE)

