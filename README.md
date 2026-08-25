# Práctica Profesional 


## Introducción y Objetivo


Este repo describe la práctica profesional realizada en el 
Centro de Agronegocios y Alimentos de la Universidad Austral sede Rosario (CEAg), 
enfocada en el análisis comparativo de los índices
de confianza de productores agropecuarios de Argentina (*Ag Barometer Austral*) y 
Estados Unidos (*Ag Economy Barometer*).



## Estructura

```text
.
├── datos/                                     
│   ├── 01_crudos/                             # Datos originales
│   └── 02_resultado/                          # Resultado final
├── notebooks/
│   ├── 01_descriptivo_univariado.ipynb        # Análisis exploratorio univariado
│   ├── 02_descriptivo_bivariado.ipynb         # Análisis exploratorio bivariado
│   └── 03_modelos.ipynb                       # Entrenamiento, evaluación de modelos y generación de resultados
├── scripts/
│   └── funciones.py                           # Funciones auxiliares
├── .gitignore
├── NOTAS.md                                   # Criterios y decisiones del análisis
├── pyproject.toml    
├── README.md                                  # Este archivo
└── uv.lock                                   
```

<!-- ## Ambiente de trabajo -->

<!-- Este proyecto usa `uv` para administrar Python y las dependencias. -->


### Instrucciones para clonar el repositorio



1. En la terminal de RStudio escribir:

```bash
git clone https://github.com/patricia-nsc/Practica-Profesional-Privado.git
cd Practica-Profesional-Privado
```

2. En la consola de R instalar el siguiente paquete (en caso de no tenerlo):

```r
install.packages("renv")
```

3. Para disponer de todos los paquetes que fueron utilizados para hacer el análisis
correr en la consola de R:


```r
renv::restore()
```








## Análisis


El análisis completo se puede ver en `Informe/Informe.pdf` y puede 
reproducirse desde el archivo `Informe/Informe.qmd`. 


Los datos crudos están en la carpeta `Datos/Crudos/` y los procesados en la carpeta 
`Datos/Procesados/`. 

En 


Puede reproducirse 


Las *notebooks* que se encuentran en la carpeta `notebooks/` se ejecutan en orden secuencial:

1. `01_descriptivo_univariado.ipynb`: se realiza un análisis exploratorio univariado de `temporada1.parquet`.

2. `02_descriptivo_bivariado.ipynb`: se realiza un análisis exploratorio bivariado de `temporada1.parquet`.

3. `03_modelos.ipynb`: con los datos de `temporada1.parquet`
    - se entrenan y comparan distintos modelos,
    - se selecciona el modelo final según las métricas de evaluación,
    - se aplica el modelo final sobre `temporada2.parquet`,
    - se genera el archivo `validacion.parquet` con el ID de cada *pitcheo* de la temporada 2 y la probabilidad estimada de que el bateador haga *swing*. Se guarda el resultado en:

    ```text
    datos/02_resultado/
    ```




