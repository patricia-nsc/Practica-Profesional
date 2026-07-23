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
    `Ag Barometer Austral` = na_interpolation(`Ag Barometer Austral`, option = 'linear'),
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
  as_tibble() |>               
  mutate(Bimestre = as.Date(Bimestre)) |> 
  writexl::write_xlsx("Datos/Procesados/agbarometer_ARG_imputado.xlsx") 


# importo los datos de Arg imputados
d_arg <- readxl::read_excel("Datos/Procesados/agbarometer_ARG_imputado.xlsx") |> 
  mutate(Bimestre = yearmonth(Bimestre)) |> 
  as_tsibble(index = Bimestre) |> 
  rename(Fecha = Bimestre,               # Nombre nuevo = Nombre viejo
         `Condiciones presentes Arg` = `Condiciones presentes`,
         `Expectativas futuras Arg` = `Expectativas futuras`)

# ------------------------------------------------------ #
# EEUU
# ------------------------------------------------------ #

d_eeuu <- readxl::read_excel("Datos/Crudos/agbarometerUSA_marzo2026.xlsx") |> 
  rename(
         `Ag Economy Barometer` = 3,
         `Condiciones presentes EEUU` = 4,
         `Expectativas futuras EEUU` = 5,
         `Inversion de capital agricola` = 6
         ) |> 
  mutate(
    Fecha = yearmonth(sprintf("%d-%02d", Year, match(Month, month.name)))) |> 
  relocate(Fecha) |> 
  as_tsibble(index = Fecha) |> 
  select(-Month, -Year, -`Inversion de capital agricola`)
  
# ------------------------------------------------------ #
# Covariables
# ------------------------------------------------------ #

covs <- readxl::read_excel("Datos/Crudos/covariables.xlsx",
                           sheet = "Monthly Prices") |> 
  mutate(Fecha = yearmonth(stringr::str_replace(Fecha, "M", "-"))) |> 
  as_tsibble(index = Fecha) 


# ------------------------------------------------------ #
# Unificacion: d_arg + d_eeuu + covs
# ------------------------------------------------------ #
  
datos <- d_arg |> 
  full_join(d_eeuu, by = "Fecha") |> 
  full_join(covs, by = "Fecha") |> 
  as.data.frame() |>                     # le saco el formato de tsibble
  mutate(Fecha = as.Date(Fecha)) |>      # para q Excel no modifique el formato
  writexl::write_xlsx("Datos/Procesados/base_construida.xlsx")



