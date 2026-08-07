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

1. Instalar `uv` (si no lo tenés)

2. Clonar el repositorio

```bash
git clone https://github.com/Marianela773/tp_baseball.git
cd tp_baseball
```
3. Crear el entorno e instalar dependencias

```bash
uv sync
```
4. Solicitar datos
Los archivos `temporada1.parquet` y `temporada2.parquet` no están incluidos en este repositorio. Es necesario solicitarlos y colocarlos manualmente en la carpeta:

```text
datos/01_crudos/
```






## Análisis

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




