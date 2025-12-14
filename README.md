# uso-del-tiempo-cultura
**Estudio análisis de uso del tiempo y relación con perfiles de participación cultural**

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/active.svg)](STATUS.md) [![License](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/mit.svg)](LICENSE-MIT.md) [![License](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/cc_by_4_0.svg)](LICENSE-CC.md) [![Empiria](https://raw.githubusercontent.com/Empiria-Lab/uso-del-tiempo-cultura/main/badges/empirialab.svg)](https://empirialab.cl/)

## Resumen

Este repositorio es parte del "Estudio de análisis de uso del tiempo y relación con perfiles de participación cultural", contratado por el **Departamento de Estudios de la Subsecretaría de las Culturas y las Artes**. El objetivo general del estudio es analizar y contextualizar la Encuesta Nacional de Uso del Tiempo (II ENUT 2023) en relación con la Encuesta Nacional de Participación Cultural y Comportamiento Lector (ENPCCL 2024), con el fin de caracterizar perfiles de uso del tiempo de distintos grupos de interés para las políticas culturales.

Para asegurar la reproducibilidad del estudio, todas las transformaciones y análisis se han documentado en este repositorio que contiene los scripts completos en R, la estructura de carpetas, los archivos de configuración y las especificaciones detalladas del diseño del estudio. Los perfiles y matrices analíticas se han almacenado en formato columnar Parquet, lo que facilita su lectura eficiente en las distintas etapas del flujo de trabajo.

## Metadatos y preservación

Este repositorio se ha creado con control de versiones Git. Además, el entorno de trabajo en R se ha gestionado con `renv`, lo que permite congelar y restaurar las versiones de los paquetes utilizados. Paralelamente, se ha generado un registro sistemático de dependencias y referencias bibliográficas asociadas a los paquetes empleados utilizando `grateful`, fortaleciendo la transparencia del ecosistema de software.

### Archivos relevantes

A continuación se ofrece una breve descripción de los archivos relevantes de este repositorio:

- **code/stage_1_ENUT_profiles.R**. Script de R que preprocesa los datos de II ENUT 2023 y guarda cada perfil generado utilizando control de versiones y el paquete `pins`.

- **code/stage_2_ENUT_plots.R**. Script de R que convierte los perfiles generales de II ENUT 2023 en el archivo CSV `data/tidy/enut_profiles_overall.csv` y en las figuras del reporte disponibles en `results/figures`, incluyendo descriptivos y gráficos de correlaciones.

- **code/stage_3_ENUT_disaggregation.R**. Script de R que convierte los perfiles desagregados de II ENUT 2023 en archivos CSVs desagregados por nivel socioeconómico, grupos etarios, nivel educacional y sexo disponibles en `data/tidy`.

- **code/stage_4_ENUT_pca.R**. Script de R que realiza el Análisis de Componentes Principales (ACP) y genera los gráficos de sedimentación para el anexo.

- **code/stage_5_ENUT_models.R**. Script de R que ajusta los modelos OLS, genera las tablas y ejecuta un prototitpo de modelo bayesiano para propagar el error de las estimaciones reajustando los intervalos de confianza de los coeficientes de los modelos.

- **code/stage_1_ENPCCL_profiles.R**. Script de R que preprocesa los datos de ENPCCL 2024 y guarda cada perfil generado utilizando control de versiones y el paquete `pins`.

- **code/stage_2_ENPCCL_plots.R**. Script de R que convierte los perfiles generales de ENPCCL 2024 en el archivo CSV `data/tidy/enpccl_profiles_overall.csv` y en las figuras del reporte disponibles en `results/figures`, incluyendo descriptivos y gráfico de correlaciones.

- **code/stage_3_ENPCCL_disaggregation.R**. Script de R que convierte los perfiles desagregados de ENPCCL 2024 en archivos CSVs desagregados por nivel socioeconómico, grupos etarios, nivel educacional y sexo disponibles en `data/tidy`.

- **code/stage_4_ENPCCL_pca.R**. Script de R que realiza el ACP y genera el gráfico de sedimentación para el anexo.

- **code/stage_5_ENPCCL_models.R**. Script de R que ajusta los modelos OLS y genera las tablas.

- **README.md**. La descripción general principal de este repositorio que proporciona metadatos, información sobre conservación e instrucciones para replicar los análisis.

- **renv.lock**. El archivo de bloqueo que especifica las versiones exactas de los paquetes utilizados en este proyecto, lo que garantiza un entorno computacional coherente para reproducir el estudio.

> [!TIP] 
> El archivo [mini-codebook.md](data/tidy/mini_codebook.md) contiene las categorías utilizadas en los CSVs para los perfiles generales y desagregados.

### Carpetas relevantes

El repositorio también contiene las siguientes carpetas con archivos relevantes:

- **data/code**. Contiene los scripts en R descritos previamente.

- **data/raw**. Contiene las bases de datos originales de II ENUT 2023 `250403-ii-enut-bdd-r-v2.RDS` y ENPCCL 2024 `enpccl_puf.RData`. ENPCLL 2024 pesa 148MB, por tanto, solo se encuentra disponible de forma local y el archivo debe ser añadido manualmente.

> [!IMPORTANT]
> ENPCCL 2024 `enpccl_puf.RData` se ha incorporado a `.gitignore` por su tamaño y los límites de LFS, por tanto, debe ser descargada desde https://www.cultura.gob.cl/participacioncultural/resultados-enpccl-2024/. II ENUT 2023 se está respaldada en el repositorio, sin embargo, también se puede descargar desde https://www.ine.gob.cl/estadisticas/sociales/genero/uso-del-tiempo. 

- **data/tidy**. Contiene los archivos agregados de perfiles procesados con las bases de datos originales de II ENUT 2023 y ENPCCL 2024.

- **data/tidy/enpccl_profiles_board**. Contiene las estimaciones de perfiles individuales de la ENPCCL 2024 en formato Parquet con el paquete `pins`.

- **data/tidy/enut_profiles_board**. Contiene las estimaciones de perfiles individuales de la II ENUT 2023 en formato Parquet con el paquete `pins`.

- **results/figures**. Contiene las figuras del reporte generadas con los scripts de R en formato PDF y PNG (300 DPI).

- **results/models**. Contiene los modelos ajustados con las estimaciones de la II ENUT y ENPCCL 2024 en formato RDS con el paquete `pins`.

- **results/tables**. Contiene las tablas de los modelos en formato HTML y TeX.

## Cómo empezar

El código original se escribió entre noviembre y diciembre de 2025 utilizando `R v4.5.0 -- How About a Twenty-Six`. Es importante señalar que `renv.lock` es el archivo de bloqueo del proyecto, y que todos los paquetes R instalados y necesarios se registran allí. Además, se puede revisar el reporte de los paquetes utilizados en [grateful-report.md](grateful-report.md).

Para replicar los principales resultados del artículo, deben ejecutarse los siguientes archivos:

- [stage_1_ENUT_profiles.R](code/stage_1_ENUT_profiles.R)
- [stage_2_ENUT_plots.R](code/stage_2_ENUT_plots.R)
- [stage_3_ENUT_disaggregation](code/stage_3_ENUT_disaggregation.R)
- [stage_4_ENUT_pca.R](code/stage_4_ENUT_pca.R)
- [stage_5_ENUT_models.R](code/stage_5_ENUT_models.R)
- [stage_6_ENPCCL_profiles.R](code/stage_6_ENPCCL_profiles.R)
- [stage_7_ENPCCL_plots.R](code/stage_7_ENPCCL_plots.R) 
- [stage_8_ENPCCL_disaggregation.R](code/stage_8_ENPCCL_disaggregation.R)
- [stage_9_ENPCCL_pca.R](code/stage_9_ENPCCL_pca.R)
- [stage_10_ENPCCL_models.R](code/stage_10_ENPCCL_models.R) 

## Licencia

El contenido de este proyecto está protegido por una licencia [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE-CC.md), y el código subyacente utilizado para dar formato y mostrar dicho contenido está protegido por una [licencia MIT](LICENSE-MIT.md).

## Autor

**Dr. Bastián González-Bustamante.** Coordinador de Estudios de [Empiria Lab](https://empirialab.cl/) y Profesor Asociado de la Facultad de Administración y Economía de la Universidad Diego Portales. También es investigador postdoctoral en Ciencias Sociales Computacionales en la Universidad de Leiden y Coordinador de Investigación del grupo de Ciencia Política Computacional de la Universidad de Oxford. Es Doctor en Ciencia Política por la Universidad de Oxford y Magíster en Ciencia Política, Administrador Público y Licenciado en Ciencias Políticas y Gubernamentales por la Universidad de Chile. Email: bastian.gonzalezbustamante@empirialab.cl. 

## Agradecimientos

Agradecimientos al equipo de la Subsecretaría de las Culturas y las Artes, especialmente a Henry Caro y Julián Suzarte. También para el equipo de [Empiria Lab](https://empirialab.cl/), en particular para Carla Cisternas y Nando Verelst.

## Empiria Lab

[Empiria Lab](https://empirialab.cl/) es un equipo especializado que ofrece soluciones y asesorías para los ámbitos público y privado. Nació como una idea en Oxford y, en 2024, se convirtió en un grupo de consultoría, con sede en Santiago y colaboradores en distintas partes del mundo. Esperamos que nuestros clientes disfruten de nuestros productos y servicios tanto como nosotros disfrutamos ofreciéndolos.

### Última revisión

14 de diciembre de 2025.