# Armado de la base de datos


# ------------------------------------------------------ #
# Librerias
# ------------------------------------------------------ #
library(tsibble)
library(lubridate)      # ymd()
library(dplyr)          # mutate()
library(tidyr)
library(imputeTS)


# ------------------------------------------------------ #
# Argentina
# ------------------------------------------------------ #



# IMPUTO LAS 3 SERIES
# 1. Cargo las 3 series, cada una con sus dos faltantes
# 2. Relleno los 2 faltantes q tienen cada una de mis 3 series
# 3. Elimino oct2018 y abril2020
# 4. Ahora el data tiene 3 series de periodicidad regular (bimensual)

  # 1.
d_conNA <- readxl::read_excel("Datos/Crudos/agbarometerARG_enero2026_conNA.xlsx") |>
  mutate(Bimestre = yearmonth(Bimestre)) |> 
  as_tsibble(index = Bimestre,
             regular = F) |> 
  # 2.
  mutate(
    `Ag Barometer` = na_interpolation(`Ag Barometer`, option = 'linear'),
    `Condiciones presentes` = na_interpolation(`Condiciones presentes`, option = 'linear'),
    `Expectativas futuras` = na_interpolation(`Expectativas futuras`, option = 'linear')
  ) |> 
  
  # 3.
  filter(!Bimestre %in% yearmonth(c("2018 Oct", "2020 Apr"))) |> 
  
  # 4.
  as_tsibble(
    index = Bimestre,
    regular = T
  )

# exporto el data imputado a datos/prodecesados 

d_conNA |>
  as_tibble() |>               # 1. Lo volvemos a formato tabla simple
  mutate(Bimestre = as.Date(Bimestre)) |> # 2. yearmonth a Date
  writexl::write_xlsx("Datos/Procesados/agbarometer_ARG_imputado.xlsx") # 3. Guardamos












